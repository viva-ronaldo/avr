# avr
Alternative voting systems in R. 

Currently supports [instant runoff voting](https://en.wikipedia.org/wiki/Instant-runoff_voting), with [single transferable vote](https://en.wikipedia.org/wiki/Single_transferable_vote) coming soon™.

```R
votes <- list(
  dex = c("Ice Skating", "Unihoc", "Food"),
  dean = c("Ice Skating", "Unihoc", "Food"),
  paul = c("Whiskey Tasting", "Established"),
  james = c("Ice Skating", "Unihoc", "Food")
)

irv_soln <- irv(votes)
irv_soln$winner
# [1] "Ice Skating"
```
