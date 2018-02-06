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

string_parts[3] <- "There were `r stv_out$nballots` votes cast; the quota was `r stv_out$quota`.

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

string_parts[8] <- "\n\nTransfer fractions considering full ballots were:\n\n
```{r echo=FALSE}
stv_out$transfer_matrix$Donor <- row.names(stv_out$transfer_matrix)
tra_grid_plot <- reshape2::melt(stv_out$transfer_matrix,id.vars='Donor',
variable.name='Recipient',value.name='Fraction')
tra_grid_plot$Donor <- factor(tra_grid_plot$Donor, 
levels=levels(tra_grid_plot$Recipient))
print(ggplot2::ggplot(tra_grid_plot,ggplot2::aes(Recipient,Donor)) + ggplot2::geom_tile(ggplot2::aes(fill=Fraction)) +
ggplot2::scale_fill_distiller(palette='Spectral',direction=-1) + ggplot2::theme_bw())
```
"

return(paste(string_parts,collapse=''))
}
