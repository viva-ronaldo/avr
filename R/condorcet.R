#Technically a Condorcet winner must win all pairings. Otherwise we are doing a
#  tie-breaking method of sorts. Copeland's method ranks by pairing wins minus
#  pairing losses.
#  Second order Copeland

#Ties- use two-method or single method to resolve
#Two-method: in event of Condorcet tie, can go to Borda count. 
#  Harder way is to take subset and run something on that.
#Single method: e.g. Copeland's method, Minimax, Nanson's method, ranked pairs

#Minimax - find candidate with smallest number for number of votes against in 
#  biggest pairwise net votes defeat (zero if no defeats);
#  or smallest number for biggest margin of defeat (allowing negative if no defeats)
#Minimax selects as the winner the candidate whose greatest pairwise defeat is smaller than the greatest pairwise defeat of any other candidate.



get_condorcet_pair_results <- function(ballots) {
    #identify candidates
    cands <- unique(unlist(ballots))
    
    #for each pair, reduce ballots to just these and count favourites
    pair_results <- data.frame()
    for (c1 in cands) {
        for (c2 in cands[cands != c1]) {
            c1_votes <- sum(sapply(ballots, function(b) b[b %in% c(c1, c2)][1] == c1), na.rm=T)
            c2_votes <- sum(sapply(ballots, function(b) b[b %in% c(c1, c2)][1] == c2), na.rm=T)
            pair_results <- rbind(pair_results, data.frame(c1=c1, c2=c2, 
                                                           c1_votes=c1_votes, c2_votes=c2_votes, 
                                                           result=ifelse(c1_votes>c2_votes,'c1', ifelse(c2_votes>c1_votes, 'c2', 'tie')),
                                                           stringsAsFactors = FALSE))
        }
    }
    pair_results
}

#For printing in report
form_condorcet_grid <- function(pair_results) {
    pair_results$net_votes <- sprintf('%+i',pair_results$c1_votes - pair_results$c2_votes)
    results_grid <- reshape(pair_results[,c('c1','c2','net_votes')], direction='wide', v.names='net_votes', idvar='c1', timevar='c2')
    names(results_grid) <- as.character(sapply(names(results_grid), function(n) gsub('net_votes.','',n)))
    row.names(results_grid) <- results_grid$c1
    results_grid$c1 <- NULL
    results_grid[is.na(results_grid)] <- ''
    results_grid[,row.names(results_grid)]
}

condorcet <- function(ballots,
                      report = FALSE, 
                      report_path = ifelse(report,'condorcet_single_report.html',NULL)) {
    win_method <- 'outright'
    
    pair_results <- get_condorcet_pair_results(ballots)
    pairs_grid <- form_condorcet_grid(pair_results)
    
    win_results <- aggregate(cbind(h2h_wins=I(result=='c1'), 
                                   h2h_ties=I(result=='tie'),
                                   h2h_losses=I(result=='c2')) ~ c1, 
                             data=pair_results, FUN=sum)
    #win_results <- win_results[order(win_results$wins, win_results$ties, decreasing=TRUE),]
    #above seems to be the same as this, Copeland's method:
    win_results <- win_results[order(win_results$h2h_wins-win_results$h2h_losses, decreasing=TRUE),]
    row.names(win_results) <- NULL
    
    if (win_results$h2h_wins[1] == win_results$h2h_wins[2] & 
        win_results$h2h_ties[1] == win_results$h2h_ties[2]) {
        #have at least the top two tied
        
        #Try Minimax
        votes_against_in_biggest_defeat <- aggregate(cbind(I(ifelse(result=='c2',c2_votes,0))) ~ c1,  
                                                     data=pair_results, FUN=max)
        votes_against_in_biggest_defeat <- votes_against_in_biggest_defeat[order(votes_against_in_biggest_defeat$V1), ]
        
        if (votes_against_in_biggest_defeat$V1[1] < votes_against_in_biggest_defeat$V1[2]) {
            win_method <- 'minimax'
            message(sprintf('Winner by Minimax tie-break is %s (wins %i out of %i pairings; worst pair result is %s',
                            votes_against_in_biggest_defeat$c1[1],
                            win_results$h2h_wins[1], nrow(win_results)-1, 
                            ifelse(votes_against_in_biggest_defeat$V1 == 0, 'tie',
                                   sprintf('%i votes against', votes_against_in_biggest_defeat$V1))))
        } else {
            win_method <- 'borda'
            borda_results <- borda(ballots, nseats=1, variant='standard')$points_table
        }
        
    } else {
        win_method <- ifelse(win_results$h2h_wins[1] == nrow(win_results)-1,
                             'outright', 
                             'copeland')
    }
    names(win_results)[1] <- 'candidate'
    winners <- ifelse(win_method=='borda',
                      borda_results$candidate[1],
                      win_results$candidate[1])
    
    condorcet_results <- structure(
        list(ballots = ballots,
             nballots = length(ballots),
             candidates = unique(unlist(ballots)),
             nseats = 1,
             winners = winners,
             win_method = win_method,
             used_random = ifelse(win_method == 'borda', borda_results$used_random, FALSE),
             points_table = win_results,
             pairs_grid = pairs_grid),
        class = "Condorcet"
    )
    
    if (report) {
        summ_res <- run_all_methods(ballots, nseats=1)
        
        win_method_text <- list(
            'outright'='There was an outright winner (wins all head-to-heads)',
            'copeland'='Copeland\'s method was used to find the winner',
            'minimax'='Minimax was used as a tie-break',
            'borda'='The result was tied, so resorted to Borda count to find a winner'
        )

        condorcet_results$points_table_formatted <- get_points_table_formatted(
            condorcet_results$points_table[, names(condorcet_results$points_table) != 'h2h_losses'],
            winners,  
            'h2h_wins', 'lightskyblue')
        print(condorcet_results$points_table_formatted)

        #note, transposing grid, so scores are column name minus row name
        condorcet_results$pairs_grid_formatted <- formattable::format_table(data.frame(t(condorcet_results$pairs_grid)),
            list(formattable::area(col = condorcet_results$candidates) ~ formattable::color_tile('palevioletred1','palegreen')), 
            align='c')
        saveRDS(condorcet_results, file='tmp_condorcet_single_results.rds')
        condorcet_results$pairs_grid_formatted <- NULL
        condorcet_results$points_table_formatted <- NULL
        
        report_text <- get_generic_report_text(method='condorcet', ensemble=FALSE)
        cat(sprintf(report_text, 
                    'tmp_condorcet_single_results.rds',
                    'the Condorcet method',
                    sprintf('* %s\n',win_method_text[[win_method]]),
                    compare_result_by_method('Condorcet', summ_res$summary_table)), 
            file='tmp_condorcet_single_report.rmd')
        
        capture.output(suppressMessages(rmarkdown::render('tmp_condorcet_single_report.rmd', 
                                                          output_file=report_path, quiet=TRUE)))
        system('rm tmp_condorcet_single_results.rds tmp_condorcet_single_report.rmd')
        message(sprintf('Report written to %s',report_path))
    }
    
    return(condorcet_results)
}

print.Condorcet <- function(condorcet) {
    message("An avr Condorcet object.")
    message("Winners:")
    for (i in seq_along(condorcet$winners)) message(condorcet$winners[i])
    invisible()
}

