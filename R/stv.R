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
stv <- function(votes, nseats, verbose=FALSE, getMatrix=FALSE) {
  winners <- c()
  out <- c()
  nvotes <- length(votes)
  weights <- rep(1, nvotes)
  
  quota <- droop_quota(nvotes, nseats)
  if (verbose) message('Quota is ',quota)
  
  running <- get_all_entries(votes)
  if (getMatrix) transfer_matrix <- data.frame() else transfer_matrix <- NULL

  rnd_num <- 1
  while (length(winners) < nseats) {
    if (verbose) message(sprintf('Round %i',rnd_num))
    rnd_num <- rnd_num + 1
    fps <- get_first_preferences(votes)
    through_this_rnd <- get_above_quota(fps, quota, weights)
    stillIn <- setdiff(running, union(winners, out))
    if (verbose) print_current_table(stillIn, fps, weights)
    
    if (length(fps) == 0) {
      break
    }
    
    if (length(stillIn) <= (nseats - length(winners))) {
        # Everyone left is in
        winners <- c(winners, stillIn)
        if (verbose) message('Rest are in')
        break
    }

    if (length(through_this_rnd) > 0) {
        
        #At end, may need to just take top few of remaining
        if (length(winners) + length(through_this_rnd) > nseats) {
            howManyIn <- nseats - length(winners)
            votesEach <- c()
            for (winner in through_this_rnd) {
                votesEach <- c(votesEach, get_nvotes(fps, winner, weights))
            }
            if (verbose) print(votesEach)
            if (verbose) print(order(votesEach,decreasing=TRUE)[1:howManyIn])
            #reduce to the number of free seats
            through_this_rnd <- through_this_rnd[order(votesEach,decreasing=TRUE)[1:howManyIn]]
        }
        
      # Move everyone's votes up and discount weights for each winner
      votesForTransfer <- 0
      for (winner in through_this_rnd) {
        nvotes <- get_nvotes(fps, winner, weights)
        excess <- round(nvotes - quota, 3) #avoid precision errors
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
      loser <- get_stv_loser(fps, stillIn, weights)
      votesForTransfer <- get_nvotes(fps, loser, weights)
      if (verbose) message(sprintf('-> %s eliminated with %g\n',loser,votesForTransfer))
      cand_out <- loser
      votes <- remove_prefs(votes, loser)
      out <- c(out, loser)
    }
    
    if (getMatrix) {
        stillIn <- setdiff(running, union(winners,out))  #update early, for output
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

  structure(
    list(winners = winners,
         transfer_matrix = transfer_matrix),
    class = "STV"
  )
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
get_stv_loser <- function(fps,stillIn,weights) {
  sum_wts <- sapply(stillIn, function(fp) sum(weights[fps == fp]))
  #losers <- ufps[sum_wts == min(sum_wts)] #seem to get precision error when transfers were previously involved
  losers <- stillIn[abs(sum_wts - min(sum_wts)) < 1e-9] 
  return(sample(losers, 1))
}


#' For verbose output, print the current votes table
print_current_table <- function(stillIn, fps, weights) {
    fpframe <- data.frame()
    for (p in stillIn) {
        fpframe <- rbind(fpframe,
                         data.frame(cand=p, votes=get_nvotes(fps,p,weights)))
    }
    print(fpframe[order(fpframe$votes, decreasing=TRUE), ])
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
