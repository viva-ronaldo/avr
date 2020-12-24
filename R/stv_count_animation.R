#' Take the stv count table with numeric columns and return a tweenr
#' animated data frame to produce the dot plots for the count
#' @param num_count_table The count table output from function stv.
#' @param candidates The list of candidates; also output from function stv.
#' @return A list containing
#'   \describe{
#'     \item{animation_data:}{tweenr animated data frame}
#'     \item{votes:}{a ballots data frame of dimension nballots rows by num_rounds columns}
#'   }
get_anim_positions_from_count_table <- function(num_count_table, candidates, quota) {
    
    votes <- data.frame(id = seq(sum(num_count_table$round_1)),
                        round_1 = unlist(lapply(1:length(candidates), 
                                                function(i) rep(num_count_table$candidate[i], 
                                                                num_count_table$round_1[i])))
    )
    
    #Have to go round by round to keep the continuity for the non-transferring candidate rows
    num_rounds <- ncol(num_count_table)-1
    for (rn in seq(2, num_rounds)) {
        votes[,sprintf('round_%d', rn)] <- votes[,rn]
        cands_losing_votes <- num_count_table$candidate[num_count_table[,rn+1] < num_count_table[,rn]]
        cands_gaining_votes <- num_count_table$candidate[num_count_table[,rn+1] > num_count_table[,rn]]
        #sometimes get a 'dead' round with a 0 knocked out or someone reaching quota exactly; if not, do this:
        if (length(cands_losing_votes) > 0 | length(cands_gaining_votes) > 0) {
            gains_by_cand <- pmax(0, num_count_table[,rn+1] - num_count_table[,rn])
            
            #if no-one seems to be losing votes, or if the total has gone up from previous round,
            #  it must be someone at the quota for which I repeated the pre-quota values
            if (length(cands_losing_votes)==0 | sum(num_count_table[,rn+1]) > sum(num_count_table[,rn])) {
                cands_above_quota <- num_count_table$candidate[num_count_table[,rn+1] > quota]
                #move the last ones in each case
                inds_to_change <- c()
                for (cand in cands_above_quota) {
                    inds_voting_for_cand <- which(votes[,rn] == cand)
                    inds_to_change <- append(inds_to_change, inds_voting_for_cand[(quota+1):length(inds_voting_for_cand)])
                }
            } else {
                inds_to_change <- which(votes[,rn] %in% cands_losing_votes)
            }
            
            new_values <- unlist(lapply(seq_along(candidates), function(i) rep(num_count_table$candidate[i], gains_by_cand[i])))
            #pad with NAs whenever votes are being dropped (not transferred)
            votes[inds_to_change, rn+1] <- c(new_values, rep(NA, length(inds_to_change)-sum(gains_by_cand)))
        }
    }
    
    votes <- votes %>% dplyr::group_by(round_1) %>% dplyr::mutate(pos_x1 = seq(dplyr::n())) %>% dplyr::ungroup() 
    for (i in seq(2,num_rounds)) {
        votes <- votes %>% dplyr::arrange(change_from_last_round = (dplyr::across(i+1) != dplyr::across(i))) %>%
            dplyr::group_by(dplyr::across(i+1)) %>% dplyr::mutate(!!sprintf('pos_x%d',i) := seq(dplyr::n())) %>% dplyr::ungroup()
    }
    
    for (i in seq(1,num_rounds)) {
        votes <- votes %>% dplyr::group_by(dplyr::across(i+1)) %>% dplyr::mutate(tmp_tot = ifelse(!is.na(dplyr::cur_group()), dplyr::n(),0)[,1]) %>% dplyr::ungroup() %>% 
            dplyr::arrange(-tmp_tot, dplyr::across(i+1)) %>% dplyr::mutate(!!sprintf('pos_y%d',i) := cumsum(!duplicated(dplyr::across(i+1))))
    }
    votes <- dplyr::select(votes, -tmp_tot)
    
    animation_data <- votes %>% dplyr::select(id, pos_y=pos_y1, pos_x=pos_x1, candidate=round_1) %>% dplyr::mutate(round_name='Round 1') %>%
        dplyr::mutate(.id=id, .phase='static', .frame=1) %>%
        tweenr::keep_state(6)
    for (i in seq(2,num_rounds)) {
        animation_data <-  animation_data %>% 
            tweenr::tween_state(votes %>% dplyr::select(id, 
                                         pos_y = !!dplyr::sym(sprintf('pos_y%d',i)), 
                                         pos_x = !!dplyr::sym(sprintf('pos_x%d',i)), 
                                         candidate = !!dplyr::sym(sprintf('round_%d',i))) %>% 
                                    dplyr::mutate(round_name=sprintf('Round %d',i)), 
                                ease='cubic-in-out', nframes=15, id=id) %>%
            tweenr::keep_state(7)
    }
    animation_data <- animation_data %>% tweenr::keep_state(7)
    
    return(list(animation_data = animation_data,
                votes = votes))
}

#' STV count animated gif
#'
#' Plot an animation of an STV count and save a gif
#' @param count_result The count_result output from function stv, or any list containing count_table, candidates, and quota.
#' @param out_gif_path Filepath to which to save the animated gif.
#' @param candidate_colour_dict An optional named character vector of candidates and colour values to pass to `ggplot`.
#' @param frame_delay_ms The gap between frames passed to the convert function, in milliseconds.
#' @param plot_width The output gif width, in inches.
#' @param plot_height The output gif height, in inches.
#' 
#' @export
create_stv_count_gif <- function(count_result, out_gif_path, 
                                 candidate_colour_dict = NULL,
                                 frame_delay_ms = 10, plot_width = 3, plot_height = 2.5) {
    if (!requireNamespace(c("tweenr", "animation"), quietly = TRUE)) {
        stop('You must install packages "tweenr" and "animation" in order to use this function')
    }
    if (is.null(candidate_colour_dict)) {
        if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
            stop('You must install package "RColorBrewer", or specify your own candidate_colour_dict, in order to use this function')
        }   
    }
    
    count_table <- count_result$count_table
    candidates <- count_result$candidates
    quota <- count_result$quota
    
    if (is.null(candidate_colour_dict)) candidate_colour_dict <- setNames(RColorBrewer::brewer.pal(length(candidates), 'Set1'), candidates)
    
    if (any(sapply(count_table[,2:(ncol(count_table)-1)], function(c) any(grepl('\\.',c))))) {
        warning('Some non-integer values in count table (partial transfers) - currently coercing to integer')
    }
    
    num_count_table <- count_table %>% dplyr::select(-elected)
    num_count_table[num_count_table == ' '] <- '0'
    num_count_table <- num_count_table %>% dplyr::mutate(dplyr::across(-candidate, function(c) as.integer(ifelse(c=='E',NA,c))))
    row_maxes <- num_count_table[,2:ncol(num_count_table)] %>% dplyr::rowwise() %>% 
        dplyr::transmute(m = max(dplyr::across(),na.rm=T)) %>% dplyr::pull(m)
    for (i in seq_along(num_count_table$candidate)) {
        inds_to_replace <- which(is.na(num_count_table[i, 1:ncol(num_count_table)]))
        if (length(inds_to_replace) > 0) num_count_table[i, inds_to_replace] <- row_maxes[i]
    }
    
    returnstuff <- get_anim_positions_from_count_table(num_count_table, candidates, quota)
    animation_data <- returnstuff[[1]]
    votes <- returnstuff[[2]]

    plot_max_x_val <- max(votes %>% dplyr::select(matches('pos_x')) %>% max(), quota)
    
    #path may be given as relative to cwd; convert to absolute before moving to tempdir()
    #(can't use normalizePath because the path doesn't exist yet)
    out_gif_path <- path.expand(out_gif_path)  #first convert tilde, if there is one
    if (substr(out_gif_path,1,2) == './') out_gif_path <- substring(out_gif_path,3)
    out_gif_path <- paste(getwd(), out_gif_path, sep='/')
    
    owd <- setwd(tempdir())
    for (i in 1:max(animation_data$.frame)) {
        tmp <- animation_data %>% dplyr::filter(.frame==i) %>% dplyr::filter(!is.na(candidate))
        #Because sometimes pos_y will be mid-move in animation_data, need to go back to votes to get the candidate y order
        round_num_char <- substring(dplyr::first(tmp$round_name), nchar(dplyr::first(tmp$round_name)))
        ordered_candidates <- votes %>% dplyr::select(sprintf('round_%s', round_num_char), sprintf('pos_y%s', round_num_char))
        names(ordered_candidates) <- c('candidate','pos_y')
        ordered_candidates <- ordered_candidates %>% dplyr::arrange(pos_y) %>% 
            dplyr::filter(!duplicated(.), !is.na(candidate)) %>% dplyr::pull(candidate)
        ordered_candidates <- c(ordered_candidates, setdiff(candidates, ordered_candidates))

        p <- ggplot2::ggplot(tmp) + 
            ggplot2::geom_vline(xintercept = quota, linetype = 2) +
            ggplot2::annotate('text', x=quota*0.98, y=0.5, label='Quota', size=2, hjust=1) +
            ggplot2::geom_point(ggplot2::aes(pos_x, pos_y, colour=candidate), size=3) +
            ggplot2::scale_colour_manual(values = candidate_colour_dict) +
            ggplot2::scale_y_reverse(breaks=seq_along(candidates), labels=ordered_candidates, 
                            limits=c(length(candidates)+0.5, 0.5)) +
            ggplot2::scale_x_continuous(limits=c(0.5, plot_max_x_val*1.25)) +
            ggplot2::labs(y='', x='Number of votes', 
                 title=sprintf('Votes after %s', dplyr::first(subset(animation_data, .frame==i)$round_name)),
                 subtitle = if (plot_max_x_val <= 50) 'Each circle = 1 vote (excluding fractional votes)' else NULL) +
            ggplot2::guides(colour='none') +
            ggplot2::theme_minimal() +
            ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), 
                           panel.grid.major.x = ggplot2::element_blank(),
                           text = ggplot2::element_text(size=6))
        
        #find who is elected - over quota, or on the last round, use count_table$elected
        elected_check <- tmp %>% dplyr::group_by(candidate) %>% dplyr::summarise(max_x = max(pos_x), 
                                                                                 pos_y = dplyr::first(pos_y), .groups='drop')
        if (as.integer(round_num_char) == ncol(count_table)-2) {
            elected_check <- elected_check %>% dplyr::filter(candidate %in% subset(count_table, elected)$candidate)
        } else {
            elected_check <- elected_check %>% dplyr::filter(max_x >= quota)
        }
        
        for (j in seq_along(elected_check$candidate)) {
            p <- p + ggplot2::annotate('label', x=elected_check$max_x[j] + quota*0.20, y=elected_check$pos_y[j], 
                                       label='Elected', size=2, colour='hotpink')
        }
        p
        ggplot2::ggsave(filename=sprintf('frame%03d.png',i), width=plot_width, height=plot_height)
    }
    animation::ani.options(interval = frame_delay_ms/100)  #this is =delay/100, which means interval=10 is 1s
    
    animation::im.convert("frame*.png", output = out_gif_path, convert='convert')
    #need to delete pngs from tmpdir, in case a second, shorter gif is made in the same session
    system('rm ./frame*.png')
    
    setwd(owd)
    
    invisible()
}
