get_generic_report_text <- function(method, ensemble=TRUE, unanimous=NULL) {
string_parts <- vector(mode='character')

string_parts[1] <- "---
title: \"Preference vote results report\"
output: html_document
---

```{r include=FALSE}
vote_results <- readRDS(\'%s\')
```

### Results\n\nAn election with `r vote_results$nseats` `r if (vote_results$nseats > 1) {\"seats\"} else {\"seat\"}` was run using %s."

string_parts[2] <- "\n\n* There were **`r vote_results$nballots`** votes cast
* There were **`r length(vote_results$candidates)`** candidates
%s* The result was `r ifelse(vote_results$used_random, \"non-deterministic: consider running in ensemble mode\", \"deterministic\")`

Elected `r if (vote_results$nseats > 1) {\"were\"} else {\"was\"}` <span style=\"font-size:150%%\">**`r vote_results$winners`**</span>.\n\n
"

#TODO what other plots can I show for these?

if (method == 'stv') {
  string_parts[3] <- "Results were:
  ```{r echo=FALSE, results=\'asis\'}
  vote_results$points_table_formatted
  ```\n"
} else {
  string_parts[3] <- "Results were:
  ```{r echo=FALSE, results=\'asis\'}
  kableExtra::kable_styling(vote_results$points_table_formatted, bootstrap_options=c(\"hover\"), full_width=F)
  ```\n"
}

string_parts[4] <- "\n\n### Alternative methods\n\n%s"

if (method == 'condorcet') {
    string_parts[5] <- "\n### Details\n\nPairwise results were (\'column name plays row name\'):
```{r echo=FALSE, results=\'asis\'}
kableExtra::kable_styling(vote_results$pairs_grid_formatted, bootstrap_options=c(\"hover\"), full_width=F)
```"
} else if (method == 'schulze') {
    string_parts[5] <- "\n### Details\n\nStrongest path strengths were:
```{r echo=FALSE, results=\'asis\'}
kableExtra::kable_styling(vote_results$pairs_grid_formatted, bootstrap_options=c(\"hover\"), full_width=F)
```"
} else if (method == 'stv') {
    string_parts[5] <- "\n### Details\n\n```{r include=FALSE}
row.cols <- c('#1B9E77', '#D95F02', '#7570B3', '#E7298A', '#66A61E', '#E6AB02', '#A6761D', '#666666')
    row.cols <- row.cols[c(1,6,3,4,5,2,8,7)]
    if (is.element(\'count_table\', names(vote_results))) {
    if (nrow(vote_results$count_table) <= 8) row.cols <- row.cols[1:nrow(vote_results$count_table)] else row.cols <- NULL
}
if (is.element(\'pathways\', names(vote_results))) npathways <- length(vote_results$pathways)
```

Actual transfers were:\n\n
```{r echo=FALSE, fig.height=6, fig.width=6}
vote_results$tm_circplot <- vote_results$transfer_matrix
vote_results$tm_circplot[is.na(vote_results$tm_circplot)] <- 0
circlize::circos.par(gap.degree=15)
circlize::chordDiagram(as.matrix(vote_results$tm_circplot), directional=1,
direction.type='arrows+diffHeight',
row.col=row.cols, grid.col=row.cols, transparency=0.4)
circlize::circos.clear()
```"

#Need to turn votes lists into tra_votes which is ballots as rows, cands as columns
string_parts[5] <- paste0(string_parts[5], "\n\nTransfer fractions, looking at full ballots 
and weighting towards higher-preference transfers, were:\n\n
```{r echo=FALSE}
tra_votes <- transpose_votes(vote_results$votes, vote_results$candidates)

#Weight 1-2 transfer twice as much as 2-3, etc. Ignore second last to last.
transfers_full <- data.frame()
for (b in seq(vote_results$nballots)) {
for (i in seq(length(vote_results$candidates)-2)) {
downweight_factor <- 1 / 2**(i-1)
from <- names(tra_votes)[which(tra_votes[b,]==i)]
if (length(from) == 0) break

left <- names(tra_votes)[which(tra_votes[b,]>i)]

for (potential in left) {
if (is.na(tra_votes[b,potential])) {
transfer_score <- 0
} else {
transfer_score <- 1 / 2**(tra_votes[b,potential]-i-1)
}
val <- transfer_score * downweight_factor
transfers_full <- rbind(transfers_full,
data.frame(Candidate=from, Target=potential, val=val, possible=downweight_factor))
}
}
}

transfers_full <- dplyr::summarise(dplyr::group_by(transfers_full, Candidate, Target),
fraction_transferred=sum(val)/sum(possible))

#normalise: as method is rough, we get outgoing fractions not summing to 1
out_fractions <- dplyr::summarise(dplyr::group_by(transfers_full, Candidate), tot_from=sum(fraction_transferred))
transfers_full <- merge(transfers_full, out_fractions, by=\'Candidate\')
transfers_full$fraction_transferred <- transfers_full$fraction_transferred / transfers_full$tot_from

transfers_full$Candidate <- factor(transfers_full$Candidate,
                                   levels=rev(sort(unique(transfers_full$Candidate))))
transfers_full$Target <- factor(transfers_full$Target,
                                levels=rev(sort(unique(transfers_full$Target))))
ggplot2::ggplot(transfers_full, ggplot2::aes(Target,Candidate)) +
  ggplot2::geom_tile(ggplot2::aes(fill=fraction_transferred)) +
  ggplot2::scale_fill_distiller(palette='Spectral',direction=-1) + ggplot2::theme_classic()

```")
} else {
    string_parts[5] <- '\n'
}

return(paste(string_parts,collapse=''))
}

elected_formatter <- formattable::formatter("span",
                                            style = x ~ formattable::style(color = ifelse(x,"green","red"),
                                                                           width="50px"), 
                                            x ~ formattable::icontext(ifelse(x,"ok","remove")))