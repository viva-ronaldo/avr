get_points_table_formatted <- function(points_table, winners, col2_name, bar_colour) {
    points_table_formatted <- points_table
    points_table_formatted$elected <- points_table_formatted$candidate %in% winners
    points_table_formatted <- formattable::format_table(points_table_formatted,
                                                        list(formattable::area(col = col2_name) ~ formattable::color_bar(bar_colour, formattable::proportion), 
                                                             elected = elected_formatter))
    points_table_formatted
}

get_candidate_ballot_profile <- function(votes_as_ballots, c, no_preference_value) {
    sapply(votes_as_ballots, function(b) if (c %in% b) which(b==c) else no_preference_value)
}

get_stv_points_table_formatted_gt <- function(points_table, votes_as_ballots) {
    points_table %>% 
        rename(Candidate = candidate, Result = elected) %>%
        rename_with(substr, .cols=starts_with('round'), 7, 7) %>%
        mutate(Preferences = purrr::map(Candidate, function(c) get_candidate_ballot_profile(votes_as_ballots, c, length(count_result$candidates))),
               Preferences = purrr::map(Preferences, kableExtra::spec_hist, col='lightgray',
                                        breaks = seq(0.5, length(count_result$candidate)+0.5, 1),
                                        lim = c(0.5, length(count_result$candidate)+0.5)),
               Preferences = purrr::map(Preferences, 'svg_text'),
               Preferences = purrr::map(Preferences, gt::html),
               .after=Candidate) %>%
        mutate(`1` = as.numeric(`1`),
               Result = ifelse(Result, 'Elected', '')
        ) %>%
        arrange(-`1`) %>%
        gt() %>% 
        cols_width(
            gt::matches('\\d') ~ px(30)
        ) %>%
        cols_align(
            align = "center",
            columns = gt::matches('\\d')
        ) %>%
        text_transform(locations = gt::cells_body(gt::matches('\\d')), 
                       function(x) ifelse(x=='E', sprintf("<span style='font-weight: bold'>%s</span>",x), x)) %>%
        data_color(
            columns = vars(`1`),
            colors = scales::col_numeric(
                palette = c('snow','pink'),
                domain = NULL
            ),
            alpha = 1
        ) %>%
        tab_spanner(
            label = "STV rounds",
            columns = gt::matches('\\d')
        ) %>%
        tab_header(title='STV count details') %>%
        tab_style(
            style = list(gt::cell_text(font = "Bitter", align = "left")),
            location = list(gt::cells_body(columns = gt::vars(Candidate)))
        ) %>%
        tab_style(locations = gt::cells_body(columns = gt::vars(Result)),
                  style = list(gt::cell_text(color='green', weight='bold'))) %>%
        tab_options(
            heading.align = "left",
            heading.title.font.size = 20,
            table.border.top.color = "transparent",
            table.border.bottom.color = "black",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "black",
            table_body.hlines.color = "#ededed"
        ) %>%
        gt::as_raw_html()
}

#Return true if a vote result didn't prefer any with fewer 1ps to one with more 1ps
check_maximized_first_prefs <- function(ballots, winners) {
    cands <- unique(unlist(ballots))
    first_prefs <- sort(table(sapply(ballots, function(b) b[1])), decreasing = TRUE)
    first_prefs[cands[!(cands %in% names(first_prefs))]] <- 0
    winners_lowest_fps <- min(first_prefs[winners])
    nonwinners_highest_fps <- max(0, first_prefs[!(names(first_prefs) %in% winners)])
    return(nonwinners_highest_fps <= winners_lowest_fps)
}

#This will be only for non-ensemble mode
compare_result_by_method <- function(primary_method, summary_table) {
    other_methods <- setdiff(c('STV','Borda','Condorcet'), primary_method)
    if (sum(summary_table$elected_always) == sum(summary_table$elected_stv)) {
        comp_string <- sprintf('The result would have been the same under %s and %s.\n',
                               other_methods[1], other_methods[2])
    } else {
        comp_string <- 'Other methods would have given a different result:\n\n'
        primary_winners <- as.character(summary_table$candidate[summary_table[,paste0('elected_',
                                                                                      tolower(primary_method))]])
        st_excl_primary <- summary_table[,c('candidate',
                                            paste0('elected_',tolower(other_methods[1])),
                                            paste0('elected_',tolower(other_methods[2])))]
        #print(st_excl_primary)
        other_winners <- unique(as.character(st_excl_primary$candidate[which(rowSums(st_excl_primary[,2:3])>0)]))
        other_winners <- setdiff(other_winners, primary_winners)
        #print(primary_winners)
        #print(other_winners)
        for (winner in c(primary_winners, other_winners)) {
            if (sum(as.numeric(st_excl_primary[st_excl_primary$candidate==winner, 2:3])) == 2) {
                comp_string <- paste0(comp_string, 
                                      sprintf('* **%s** would be elected under both %s and %s\n',
                                              winner, other_methods[1], other_methods[2]))
            } else if (sum(as.numeric(st_excl_primary[st_excl_primary$candidate==winner, 2:3])) == 0) {
                comp_string <- paste0(comp_string, 
                                      sprintf('* **%s** would _not_ be elected under either %s or %s\n',
                                              winner, other_methods[1], other_methods[2]))
            } else {
                winner_ind <- which(as.logical(st_excl_primary[st_excl_primary$candidate==winner, 2:3]))
                comp_string <- paste0(comp_string, 
                                      sprintf('* **%s** would be elected under %s but _not_ under %s\n',
                                              winner, other_methods[winner_ind], other_methods[-winner_ind]))
            }
        }
    }
    return(comp_string)
}

#Compare 4 methods:
run_all_methods <- function(ballots, nseats = 1,
                            borda_variant = 'standard') {
    summary_table <- data.frame(candidate = unique(unlist(ballots)))
    
    res_stv <- stv(votes=ballots, nseats=nseats)
    summary_table$elected_stv <- summary_table$candidate %in% res_stv$winners
    
    res_bor <- borda(ballots, nseats=nseats, variant=borda_variant)
    summary_table$elected_borda <- summary_table$candidate %in% res_bor$winners
    
    if (nseats == 1) {
        res_con_sch <- condorcet(ballots)
    } else {
        res_con_sch <- schulze(ballots, nseats=nseats)
    }
    summary_table$elected_condorcet <- summary_table$candidate %in% res_con_sch$winners
    unanimous <- setequal(c(res_stv$winners, res_bor$winners, res_con_sch$winners), res_stv$winners)
    #print(unanimous)
    
    summary_table <- summary_table[order(-rowSums(summary_table[,2:4])),]
    summary_table$elected_always <- summary_table$elected_stv & 
        summary_table$elected_borda & summary_table$elected_condorcet
    
    return(list(stv_results = res_stv,
                borda_results = res_bor,
                condorcet_results = res_con_sch,
                summary_table = summary_table))
}
