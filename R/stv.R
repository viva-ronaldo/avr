#' Single transferable vote
#'
#' @param votes A list of order-of-preference vote vectors, or a list of ballot
#' objects.
#' @param nseats How many seats to fill.
#' @param verbose Print out intermediate information or not
#'
#' @return An STV object, containing:
#'   \describe{
#'     \item{winners:}{the winning entries in order of preference}
#'     \item{More to come!}{}
#'   }
#' @export
#' @examples
#' votes <- list(
#'   dex = c("Ice Skating", "Unihoc", "Food"),
#'   dean = c("Ice Skating", "Unihoc", "Food"),
#'   paul = c("Whiskey Tasting", "Established"),
#'   james = c("Ice Skating", "Unihoc", "Food")
#' )
#'
#' stv(votes, 2)
#'
#' map <- c("e", "f", "i", "u", "w")
#' votes <- list(
#'   ballot(0, 3, 1, 2, 0, map = map),
#'   ballot(0, 3, 1, 2, 0, map = map),
#'   ballot(2, 0, 0, 0, 1, map = map),
#'   ballot(0, 3, 1, 2, 0, map = map)
#' )
#' stv(votes, 2)
stv <- function(votes, nseats, use_fps_for_final_tie=TRUE, transfer_surplus=TRUE,
                verbose=FALSE, 
                getMatrix=FALSE, 
                report=FALSE, report_path=ifelse(report,'stv_single_report.html',NULL),
                getTable=ifelse(report,TRUE,FALSE)) {
    
    if (report) {
        #Need ggplot2, reshape2, kableExtra, formattable, circlize to make report
        if (length(intersect(c('ggplot2','reshape2','kableExtra','formattable','circlize'),
                             row.names(installed.packages()))) < 5) report <- FALSE
        if (report) getMatrix <- TRUE
    }
    
    winners <- c()
    out <- c()
    used_random <- FALSE
    nballots <- length(votes)
    nvotes <- nballots
    weights <- rep(1, nvotes)
    quota <- droop_quota(nvotes, nseats)
    if (verbose) message('Quota is ',quota)
    running <- get_all_entries(votes)
    if (getMatrix) transfer_matrix <- data.frame() else transfer_matrix <- NULL
    if (getTable) count_table <- data.frame(candidate=running) else count_table <- NULL
    can_drop_bottom <- 1
    #get order by pref1s, pref2s, etc in case need to break a tie later, including random tie breaker
    pref_counts_df <- data.frame(cand = running, stringsAsFactors = FALSE)
    for (i in 1:length(running)) {
        pref_counts_df[,paste0('pref',i)] <- sapply(running, function(p) 
            sum(sapply(votes, function(l) l[i]) == p, na.rm=TRUE))
    }
    pref_counts_df$random_split <- sample.int(nrow(pref_counts_df))
    order_for_ties <- pref_counts_df[do.call(order, -pref_counts_df[,2:(ncol(pref_counts_df))]), 'cand']
    
    if (getTable | verbose) rnd_num <- 1
    while (length(winners) < nseats) {
        if (verbose) message(sprintf('Round %i',rnd_num))
        fps <- get_first_preferences(votes)
        through_this_rnd <- get_above_quota(fps, quota, weights)
        stillIn <- setdiff(running, union(winners, out))
        if (verbose) print(get_current_table(stillIn, fps, weights))
        if (getTable & can_drop_bottom == 1) {
            count_table <- merge(count_table, get_current_table(stillIn, fps, weights), 
                                 by='candidate', all.x=T)
            names(count_table)[ncol(count_table)] <- paste0('round_',rnd_num)
            rnd_num <- rnd_num + 1
        } else if (verbose) {
            rnd_num <- rnd_num + 1  
        }
        
        if (length(fps) == 0) {
            break
        }
        if (length(stillIn) <= (nseats - length(winners))) {
            # Everyone left is in
            winners <- c(winners, stillIn)
            if (verbose) message('Rest are in')
            break
        }
        
        # Check for winner, unless we need to finish a multiple elimination at bottom
        if (can_drop_bottom == 1 & length(through_this_rnd) > 0) {
            
            #At end, may need to just take top few of remaining
            if (length(winners) + length(through_this_rnd) > nseats) {
                howManyIn <- nseats - length(winners)
                votesEach <- c()
                for (winner in through_this_rnd) {
                    votesEach <- c(votesEach, get_nvotes(fps, winner, weights))
                }
                if (verbose) message(votesEach)
                if (verbose) print(order(votesEach,decreasing=TRUE)[1:howManyIn])
                #reduce to the number of free seats
                through_this_rnd <- through_this_rnd[order(votesEach,decreasing=TRUE)[1:howManyIn]]
            }
            
            # Move everyone's votes up and discount weights for each winner
            votesForTransfer <- 0
            for (winner in through_this_rnd) {
                nvotes <- get_nvotes(fps, winner, weights)
                excess <- round(nvotes - quota, 3) #avoid precision errors
                if (!transfer_surplus) excess <- 0  #only transfer from losers in this case
                transfer_ratio <- excess / nvotes
                weights[fps == winner] <- weights[fps == winner] * transfer_ratio
                
                if (verbose) message(sprintf('-> %s elected with %g (%g to transfer)\n',
                                             winner,nvotes,excess))
                votesForTransfer <- votesForTransfer + excess
                cand_out <- winner
            }
            winners <- c(winners, through_this_rnd)
            votes <- remove_prefs(votes, through_this_rnd)
        } else {
            # Eliminate someone, update votes with no change to weights
            if (length(stillIn) > 2) {
                can_drop_bottom <- get_number_to_eliminate(rev(get_current_table(stillIn, fps, weights)$votes), quota)
                #if (can_drop_bottom > 1) { message(sprintf('Can drop bottom %i',can_drop_bottom)) }
            }
            loser_info <- get_stv_loser(fps, stillIn, weights, order_for_ties, nseats-length(winners),
                                        use_fps_for_final_tie=use_fps_for_final_tie)
            loser <- loser_info$loser
            if (loser_info$used_random) used_random <- TRUE
            
            votesForTransfer <- get_nvotes(fps, loser, weights)
            if (verbose) message(sprintf('-> %s eliminated with %g\n',loser,votesForTransfer))
            cand_out <- loser
            votes <- remove_prefs(votes, loser)
            out <- c(out, loser)
            # Check if we can safely eliminate more: done only to simplify the count table
            #TODO what was this?
        }
        
        if (getMatrix) {
            stillIn <- setdiff(running, union(winners,out)) #update early, for output
            prevVotes <- vector('integer', length=length(stillIn))
            for (i in seq(length(stillIn))) {
                prevVotes[i] <- get_nvotes(fps, stillIn[i], weights)
            }
        }
        
        vw <- drop_empty_votes_and_update_weights(votes, weights) #keep their lengths consistent
        votes <- vw[[1]]
        weights <- vw[[2]]
        
        if (getMatrix) transfer_matrix <- add_row_to_transfer_matrix(votes,weights,running,
                                                                     stillIn,cand_out,prevVotes,
                                                                     votesForTransfer,
                                                                     verbose,transfer_matrix)
    }
    if (getTable) count_table <- finalise_count_table(count_table, winners)
    if (getMatrix) {
        for (cand in setdiff(running, row.names(transfer_matrix))) {
            #print(paste('Blank for',cand))
            transfer_matrix[cand, ] <- NA
        }
    }
    
    stv_single_results <- structure(
        list(ballots = ballots,
             nballots = nballots,
             candidates = running,
             nseats = nseats,
             quota = quota,
             winners = winners,
             used_random = used_random,
             transfer_matrix = transfer_matrix,
             count_table = count_table),
        class = "STV"
    )
    
    if (report) {
        summ_res <- run_all_methods(ballots, nseats=nseats)$summary_table
        #overwrite the stv column to make sure it matches this result in the event of used_random
        summ_res$elected_stv <- summ_res$candidate %in% winners
        
        #stv_single_results$points_table_formatted <- get_points_table_formatted(count_table, winners)
        stv_single_results$points_table_formatted <- get_points_table_formatted(
            stv_single_results$count_table, 
            winners,
            'round_1', 'palegreen')

        saveRDS(stv_single_results, file='tmp_stv_single_results.rds')
        stv_single_results$points_table_formatted <- NULL
        #report_text <- get_report_text(ensemble=FALSE)
        #cat(sprintf(report_text, 'tmp_stv_single_results.rds'), file='tmp_stv_single_report.rmd')
        report_text <- get_generic_report_text(method='stv', ensemble=FALSE)
        cat(sprintf(report_text,
                    'tmp_stv_single_results.rds',
                    'Single Transferable Vote',
                    sprintf('* The quota was **%i**\n', stv_single_results$quota),
                    compare_result_by_method('STV', summ_res)),
            file='tmp_stv_single_report.rmd')
        capture.output(suppressMessages(rmarkdown::render('tmp_stv_single_report.rmd', 
                                                          output_file=report_path, quiet=TRUE)))
        #system('rm tmp_stv_single_results.rds tmp_stv_single_report.rmd')
        message(sprintf('Report written to %s',report_path))
    }
    
    return(stv_single_results)
}

#' Return the names of the candidates who are over the current quota
get_above_quota <- function(fps, quota, weights) {
  ufps <- unique(fps)
  sum_wts <- sapply(ufps, function(fp) sum(weights[fps == fp]))

  ufps[sum_wts >= quota]
}

#' Get the number of votes for a given candidate
get_nvotes <- function(fps, candidate, weights) {
  sum(weights[fps == candidate])
}

#' Return the lowest-voted candidate by FPs, resolving ties randomly
#' Updated so as not to miss candidates on 0 FPs
get_stv_loser <- function(fps, stillIn, weights, order_for_ties, 
                          nseats_left, use_fps_for_final_tie=FALSE) {
  sum_wts <- sapply(stillIn, function(fp) sum(weights[fps == fp]))
  #losers <- ufps[sum_wts == min(sum_wts)] #seem to get precision error when transfers were previously involved
  losers <- stillIn[abs(sum_wts - min(sum_wts)) < 1e-9] 
  #if have (nseats_remaining + 1) candidates tied, can use fps instead to split
  if (use_fps_for_final_tie & length(losers) == (nseats_left+1) & 
      length(stillIn) == (nseats_left+1)) {
      #message('Breaking tie by FPs')
      return(list(loser=losers[which.max(sapply(losers, function(x) which(order_for_ties == x)))],
                  used_random=FALSE))
  } else {
      return(list(loser=sample(losers, 1),
                  used_random=TRUE))   
  }
}

#' For verbose output, print the current votes leaderboard
get_current_table <- function(stillIn, fps, weights) {
    fpframe <- data.frame()
    for (p in stillIn) {
        fpframe <- rbind(fpframe, data.frame(candidate=p, 
                                             votes=round(get_nvotes(fps,p,weights),2)))
    }
    fpframe[order(fpframe$votes, decreasing=TRUE), ]
}


#' Add an entry to the transferMatrix, for cand_out
add_row_to_transfer_matrix <- function(votes,weights,running,stillIn,cand_out,
                                       prevVotes,votesForTransfer,verbose,transfer_matrix) {
    if (verbose & votesForTransfer > 0) message('Transfers:')
    fps <- get_first_preferences(votes)
    transfer_fracs <- list()
    for (i in seq_along(stillIn)) {
        transfers <- get_nvotes(fps,stillIn[i],weights) - prevVotes[i]
        if (transfers > 0 & verbose) {
            message(sprintf('%g to %s (%.2f)',transfers,stillIn[i],transfers/votesForTransfer))
        }
        if (votesForTransfer > 0) transfer_fracs[[stillIn[i]]] <- transfers/votesForTransfer
    }
    transfer_frame <- data.frame(lapply(running, function(x) ifelse(x %in% names(transfer_fracs),
                                                                    transfer_fracs[[x]], NA)))
    names(transfer_frame) <- running
    row.names(transfer_frame) <- c(cand_out)
    if (votesForTransfer > 0) transfer_matrix <- rbind(transfer_matrix, transfer_frame)
    if (verbose & votesForTransfer > 0) message('')
    transfer_matrix
}
    
drop_empty_votes_and_update_weights <- function(votes, weights) {
    keepInds <- vector('logical', length=length(votes))
    for (v in seq_along(votes)) {
        if (length(votes[[v]] > 0)) keepInds[v] <- TRUE
    }
    list(votes[keepInds], weights[keepInds])
}

#' Return the number of bottom candidates that can be dropped in this round
get_number_to_eliminate <- function(ordered_current_votes, quota) {
    number_still_in <- length(ordered_current_votes)
    could_drop_group <- sapply(seq(2,number_still_in-1), 
                               function(x) sum(ordered_current_votes[1:x]) < quota &
                                   sum(ordered_current_votes[1:x]) < ordered_current_votes[x+1])
    #print(could_drop_group)
    if (sum(could_drop_group) >= 1) {
        return(min(which(could_drop_group)) + 1)
    } else {
        return(1)
    }
}

#' Prepare the count table for viewing
finalise_count_table <- function(count_table, winners) {
    if (ncol(count_table) > 2) {
        count_table[,2:ncol(count_table)] <- apply(count_table[,2:ncol(count_table)],2,
                                                   function(c) ifelse(is.na(c), ifelse(count_table$candidate %in% winners, 'E', ' '), c))
    }
    count_table <- cbind(count_table, data.frame(elected=count_table$candidate %in% winners))
    if (ncol(count_table) > 3) count_table <- count_table[ do.call(order, count_table[,2:(ncol(count_table)-1)]), ]
    count_table <- count_table[rev(row.names(count_table)),]
    row.names(count_table) <- NULL
    return(count_table)
}

#' @export
print.STV <- function(stv) {
  message("An avr stv object.")
  winners <- stv$winners
  message("Winners:")
  for (i in seq_along(winners)) {
    str <- paste0("Round ", i, ":\t", winners[i])
    message(str)
  }
  invisible()
}
