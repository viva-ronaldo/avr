# avr
Alternative voting systems in R.

Currently supports [instant runoff voting](https://en.wikipedia.org/wiki/Instant-runoff_voting), with [single transferable vote](https://en.wikipedia.org/wiki/Single_transferable_vote) coming soon™.

```R
votes <- list(
  n1 = c("a", "b", "c", "d"),
  n2 = c("a", "b", "c", "d"),
  n3 = c("a", "b", "c", "d"),
  n4 = c("b", "a", "c", "d"),
  n5 = c("b", "a", "c", "d"),
  n6 = c("c", "b", "c", "d"),
  n7 = c("c", "b", "c", "d"),
  n8 = c("d", "c", "c", "d")
)

irv_soln <- irv(votes)
irv_soln
# Winner: a
# ...in 3 rounds.
# Dropped in round 0:     None
# Dropped in round 1:     d
# Dropped in round 2:     b
# Dropped in round 3:     c
```
