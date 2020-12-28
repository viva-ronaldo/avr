#Borda count - gives points in reverse order of preferences; 
#  can either start with n_cands or n_cands-1 (this difference doesn't affect result);
#  or Dowdall method gives points 1, 1/2, 1/3, etc (does affect result).
#  For truncated ballots probably assign minimum point value to unmarked cands. This means in standard method
#  a partially filled ballot can be better for getting your first pref elected than a full ballot.
#In modified Borda (MBC), only give points based on how many prefs are ranked - can't run this
#  on the side of an STV vote as voters are penalised for incomplete ballots in MBC.
#Borda is a consensus or utilitarian method, and doesn't necessarily elect a candidate that 
#  more than half of voters have put as first preference (if many also rank the candidate low).
#Dowdall gives more weight to strong preferences, whereas standard Borda favours consistent medium prefs
#For multiple seats, there is the Quota Borda method but here just using the ordered points list.

#' Borda count
#'
#' Count votes using the Borda method. This uses the simple \code{nseats > 1} version rather than 
#' Quota Borda Count (using 1st pref quotas), because that doesn't generalise to \code{nseats = 1}.
#' @param ballots The list of ballots.
#' @param nseats The number of seats available.
#' @param variant The variant of Borda to use. Must be one of '\code{standard}', '\code{modified}', or '\code{dowdall}'.
#' See \url{https://en.wikipedia.org/wiki/Borda_count}.
#' @param report Generate an HTML report of the vote.
#' @param report_path When \code{report} is TRUE, the filepath to which to save the generated report.
#' @export
borda <- function(ballots, nseats=1, variant = 'standard',
                  report = FALSE, 
                  report_path = ifelse(report,'borda_single_report.html',NULL)) {
    variant <- tolower(variant)
    if (!(variant %in% c('standard','modified','dowdall'))) {
        message('Don\'t recognise that variant; resorting to standard Borda count')
        variant <- 'standard'
    }
    
    cands <- unique(unlist(ballots))
    n_cands <- length(cands)
    
    if (variant == 'standard') {
        #The n_cands-1 method, giving 0 to unmarked prefs:
        borda_points <- sapply(ballots, function(b) 
            sapply(cands, function(c) ifelse(c %in% b, n_cands-which(b == c), 0)))
    } else if (variant == 'modified') {
        #Modified Borda - encourages full ballots
        borda_points <- sapply(ballots, function(b)
            sapply(cands, function(c) ifelse(c %in% b, length(b) - which(b == c) + 1, 0)))
    } else if (variant == 'dowdall') {
        #Dowdall system
        borda_points <- sapply(ballots, function(b)
            sapply(cands, function(c) ifelse(c %in% b, 1/which(b == c), 1/n_cands)))
    }
    borda_points <- data.frame(borda_points)
    if (variant == 'dowdall') borda_points <- round(borda_points,2)
    
    borda_points$total_points <- rowSums(borda_points)
    borda_points$num_firsts <- rowSums(sapply(ballots, function(b) sapply(row.names(borda_points), function(c) sum(b[1]==c))))
    borda_points$random_split <- sample.int(nrow(borda_points))
    borda_points <- borda_points[order(borda_points$total_points, 
                                       borda_points$num_firsts, 
                                       borda_points$random_split, decreasing=TRUE), ]
    #print(borda_points)
    
    used_random <- FALSE
    if (nseats < nrow(borda_points)) {
        if (borda_points$total_points[nseats] == borda_points$total_points[nseats+1] &
            borda_points$num_firsts[nseats] == borda_points$num_firsts[nseats+1]) used_random <- TRUE
    }
    
    borda_points$candidate <- row.names(borda_points)
    row.names(borda_points) <- NULL
    
    borda_results <- structure(
        list(ballots = ballots,
             nballots = length(ballots),
             candidates = cands,
             nseats = nseats,
             winners = borda_points$candidate[1:nseats],
             variant = variant,
             used_random = used_random,
             points_table = borda_points[, c('candidate','total_points','num_firsts')]),
        class = "Borda"
    )
    
    if (report) {
        summ_res <- run_all_methods(ballots, nseats=nseats)
        #overwrite the borda column to make sure it matches this result in the event of used_random
        summ_res$elected_borda <- summ_res$candidate %in% borda_results$winners
        
        borda_results$points_table_formatted <- get_points_table_formatted(
            borda_results$points_table, 
            borda_results$winners,
            'total_points', 'pink')
        
        report_path <- path.expand(report_path)  #first convert tilde, if there is one
        if (substr(report_path,1,2) == './') report_path <- substring(report_path,3)
        report_path <- paste(getwd(), report_path, sep='/')
        owd <- setwd(tempdir())
        
        saveRDS(borda_results, file='tmp_borda_single_results.rds')
        borda_results$points_table_formatted <- NULL
        
        report_text <- get_generic_report_text('borda', ensemble=FALSE)
        cat(sprintf(report_text, 
                    'tmp_borda_single_results.rds',
                    sprintf('the Borda method (%s)', variant), 
                    '',
                    compare_result_by_method('Borda', summ_res$summary_table)),
                    #ifelse(used_random, 'The result was tied, so resorted to random split to find a winner','')),
            file='tmp_borda_single_report.rmd')
        capture.output(suppressMessages(rmarkdown::render('tmp_borda_single_report.rmd', 
                                                          output_file=report_path, quiet=TRUE)))
        system('rm tmp_borda_single_results.rds tmp_borda_single_report.rmd')
        setwd(owd)
        message(sprintf('Report written to %s',report_path))
    }
    
    return(borda_results)
}

#' @export
print.Borda <- function(borda) {
    message("An avr Borda object.")
    message("Winners:")
    for (i in seq_along(borda$winners)) message(borda$winners[i])
    invisible()
}