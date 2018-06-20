transpose_votes <- function(votes, candidates) {
    tra_votes <- data.frame(dum=seq(length(votes)))
    for (cand in candidates) {
        tra_votes[,cand] <- as.integer(sapply(votes, function(x) which(x==cand)))
    }
    tra_votes$dum <- NULL
    tra_votes
}


get_report_text <- function(ensemble=TRUE, unanimous=NULL) {
string_parts <- vector(mode='character', length=7)

string_parts[1] <- "---
title: \"STV results report\"
output: html_document
---

```{r include=FALSE}
stv_out <- readRDS(\'%s\')
row.cols <- c('#1B9E77', '#D95F02', '#7570B3', '#E7298A', '#66A61E', '#E6AB02', '#A6761D', '#666666')
row.cols <- row.cols[c(1,6,3,4,5,2,8,7)]
if (is.element(\'count_table\', names(stv_out))) {
if (nrow(stv_out$count_table) <= 8) row.cols <- row.cols[1:nrow(stv_out$count_table)] else row.cols <- NULL
}
if (is.element(\'pathways\', names(stv_out))) npathways <- length(stv_out$pathways)
```

An STV election with `r stv_out$nseats` `r if (stv_out$nseats > 1) {\"seats\"} else {\"seat\"}` was "

if (ensemble) {
    string_parts[2] <- "run in ensemble mode, with `r stv_out$nensemble` iterations."
} else {
    string_parts[2] <- "run."
}

string_parts[3] <- " There were `r stv_out$nballots` votes cast; the quota was `r stv_out$quota`.

There were `r nrow(stv_out$count_table)` candidates, with first preferences as follows:
```{r echo=FALSE}
print(if(is.element(\'count_table\',names(stv_out))) stv_out$count_table[,1:2] else stv_out$pathways[[1]][,1:2])
```
"

if (ensemble) {
    if (unanimous) {
        string_parts[4] <- "The result was deterministic: elected were `r stv_out$ensemble_result$candidate`.\n\n"
    } else {
        string_parts[4] <- "Overall elected frequencies were:
```{r echo=FALSE}
print(stv_out$ensemble_result)
#todo use class print
```
"
    }
} else {
    string_parts[4] <- "Elected `r if (stv_out$nseats > 1) {\"were\"} else {\"was\"}` `r stv_out$winners`.\n\n"    
}

string_parts[5] <- "```{r echo=FALSE}
elected_formatter <- formattable::formatter(\"span\",
style = x ~ formattable::style(color = ifelse(x,\"green\",\"red\"),width=\"50px\"), 
x ~ formattable::icontext(ifelse(x,\"ok\",\"remove\")))
pretty_print_count_table <- function(count_table) {
kableExtra::kable_styling(knitr::kable(dplyr::mutate(count_table, 
Round_1=formattable::normalize_bar(\"pink\")(Round_1),
Elected=elected_formatter(Elected)), \"html\", escape=F, align=\"r\"),
bootstrap_options=c(\"hover\"), full_width=F)
}
```
"

if (ensemble) {
    string_parts[6] <- "Due to random resolution of ties, there were `r npathways` possible pathways.
```{r echo=FALSE, results=\'asis\'}
cat(\"\n\")
for (p in seq_along(stv_out$pathways)) {
cat(paste0(\"Pathway \",p,\":\n\"))
print(pretty_print_count_table(stv_out$pathways[[p]]))
}
```
"
} else {   
    string_parts[6] <- "The count was as follows:
```{r echo=FALSE, results=\'asis\'}
cat(\"\n\")
print(pretty_print_count_table(stv_out$count_table))
```
"
}

#TODO also print in unanimous ensemble
if (!ensemble) {
    string_parts[7] <- "Actual transfers were:\n\n
```{r echo=FALSE, fig.height=6, fig.width=6}
stv_out$tm_circplot <- stv_out$transfer_matrix
stv_out$tm_circplot[is.na(stv_out$tm_circplot)] <- 0
circlize::circos.par(gap.degree=15)
circlize::chordDiagram(as.matrix(stv_out$tm_circplot), directional=1,
direction.type='arrows+diffHeight',
row.col=row.cols, grid.col=row.cols, transparency=0.4)
circlize::circos.clear()
```
"
} else { string_parts[7] <- "" }

#Need to turn votes lists into tra_votes which is ballots as rows, cands as columns
string_parts[8] <- "\n\nTransfer fractions, considering full ballots 
and weighting towards higher-preference transfers, were:\n\n
```{r echo=FALSE}
tra_votes <- transpose_votes(stv_out$votes, stv_out$candidates)

#Weight 1-2 transfer twice as much as 2-3, etc. Ignore second last to last.
transfers_full <- data.frame()
for (b in seq(stv_out$nballots)) {
  for (i in seq(length(stv_out$candidates)-2)) {
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
                                   levels=rev(sort(levels(transfers_full$Candidate))))
transfers_full$Target <- factor(transfers_full$Target,
                                levels=rev(sort(levels(transfers_full$Target))))
ggplot2::ggplot(transfers_full, ggplot2::aes(Target,Candidate)) +
  ggplot2::geom_tile(ggplot2::aes(fill=fraction_transferred)) +
  ggplot2::scale_fill_distiller(palette='Spectral',direction=-1) + ggplot2::theme_classic()

```
"

return(paste(string_parts,collapse=''))
}
