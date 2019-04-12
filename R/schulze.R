#For printing in report
form_schulze_paths_grid <- function(pair_results) {
    paths_grid <- reshape(pair_results[,c('c1','c2','path')], direction='wide', v.names='path', idvar='c1', timevar='c2')
    names(paths_grid) <- as.character(sapply(names(paths_grid), function(n) gsub('path.','',n)))
    row.names(paths_grid) <- paths_grid$c1
    paths_grid$c1 <- NULL
    paths_grid[is.na(paths_grid)] <- ''
    paths_grid[,row.names(paths_grid)]
}

#TODO
#'
#' @export
schulze <- function(ballots, nseats = 1,
                    report = FALSE, 
                    report_path = ifelse(report,'schulze_single_report.html',NULL)) {
    
    cands <- unique(unlist(ballots))
    
    pair_results <- get_condorcet_pair_results(ballots)
    pair_results[pair_results$c1_votes < pair_results$c2_votes, 'c1_votes'] <- 0
    pair_results <- pair_results[,c('c1','c2','c1_votes')]
    
    pair_results$path <- pair_results$c1_votes
    for (i in cands) {
        for (j in cands[cands != i]) {
            for (k in cands[cands != i & cands != j]) {
                pair_results$path[pair_results$c1 == j &
                                      pair_results$c2 == k] <- max(
                                          pair_results$path[pair_results$c1 == j &
                                                                pair_results$c2 == k],
                                          min(pair_results$path[pair_results$c1 == j &
                                                                    pair_results$c2 == i],
                                              pair_results$path[pair_results$c1 == i &
                                                                    pair_results$c2 == k])
                                      )
            }
        }
    }
    
    paths_grid <- form_schulze_paths_grid(pair_results)
    
    path_wins <- sapply(cands, function(c1) sum(sapply(cands[cands!=c1], 
                                                       function(c2) if (pair_results$path[pair_results$c1==c1 & pair_results$c2==c2] > 
                                                                        pair_results$path[pair_results$c1==c2 & pair_results$c2==c1]) 1 else 0)))
    points_table <- data.frame(candidate=names(path_wins),path_wins=as.numeric(path_wins),
                               stringsAsFactors = FALSE)
    points_table$random_split <- sample.int(nrow(points_table))
    points_table <- points_table[order(points_table$path_wins,
                                       points_table$random_split, decreasing=TRUE), ]
    row.names(points_table) <- NULL
    
    used_random <- FALSE
    if (nseats < nrow(points_table)) {
        if (points_table$path_wins[nseats] == points_table$path_wins[nseats+1]) used_random <- TRUE
    }

    schulze_results <- structure(
        list(ballots = ballots,
             nballots = length(ballots),
             candidates = cands,
             nseats = nseats,
             winners = points_table$candidate[1:nseats],
             used_random = used_random,
             points_table = points_table,
             paths_grid = paths_grid),
        class = "Schulze"
    )
    
    if (report) {
        summ_res <- run_all_methods(ballots, nseats=nseats)
        
        schulze_results$points_table_formatted <- get_points_table_formatted(
            schulze_results$points_table[, names(schulze_results$points_table) != 'random_split'], 
            schulze_results$winners, 
            'path_wins', 'lightskyblue')
        #note, transposing grid, so paths are from column name to row name
        schulze_results$pairs_grid_formatted <- formattable::format_table(data.frame(t(schulze_results$paths_grid)),
            list(formattable::area(col = schulze_results$candidates) ~ formattable::color_tile('palevioletred1','palegreen')), 
            align='c')
        saveRDS(schulze_results, file='tmp_schulze_single_results.rds')
        schulze_results$points_table_formatted <- NULL
        schulze_results$pairs_grid_formatted <- NULL
        
        report_text <- get_generic_report_text(method='schulze', ensemble=FALSE)
        cat(sprintf(report_text, 
                    'tmp_schulze_single_results.rds',
                    'the Schulze method',
                    '',
                    compare_result_by_method('Condorcet', summ_res$summary_table)),
            file='tmp_schulze_single_report.rmd')
        capture.output(suppressMessages(rmarkdown::render('tmp_schulze_single_report.rmd', 
                                                          output_file=report_path, quiet=TRUE)))
        system('rm tmp_schulze_single_results.rds tmp_schulze_single_report.rmd')
        message(sprintf('Report written to %s',report_path))
    }
    
    return(schulze_results)
}

print.Schulze <- function(schulze) {
    message("An avr Schulze object.")
    message("Winners:")
    for (i in seq_along(schulze$winners)) message(schulze$winners[i])
    invisible()
}
