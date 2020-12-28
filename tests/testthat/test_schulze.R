context('Schulze method tests')

#Check function basics
test_that("Schulze basics are correct", {
    votes <- c(
        replist("Orange",                4),
        replist(c("Pear", "Orange"),     2),
        replist(c("Choc", "Strawberry"), 8),
        replist(c("Choc", "Candy"),      4),
        replist("Strawberry",            1),
        replist("Candy",                 1)
    )
    results_1seat <- schulze(votes, nseats=1)
    expect_equal(results_1seat$nballots, 20)
    expect_equal(sort(results_1seat$candidates), c('Candy','Choc','Orange','Pear','Strawberry'))
    expect_equal(results_1seat$winners, c('Choc'))
    
    results_2seat <- schulze(votes, nseats=2)
    expect_equal(sort(results_2seat$winners), c('Choc','Strawberry'))
})


#Schulze check from Wikipedia
test_that("Schulze method correct for Wikipedia example", {
    test_cands <- c('A','B','C','D','E')
    test_ballots <- c(rep(list(ballot(c(1,3,2,5,4), map=test_cands)), 5),
                      rep(list(ballot(c(1,5,4,2,3), map=test_cands)), 5),
                      rep(list(ballot(c(4,1,5,3,2), map=test_cands)), 8),
                      rep(list(ballot(c(2,3,1,5,4), map=test_cands)), 3),
                      rep(list(ballot(c(2,4,1,5,3), map=test_cands)), 7),
                      rep(list(ballot(c(3,2,1,4,5), map=test_cands)), 2),
                      rep(list(ballot(c(5,4,2,1,3), map=test_cands)), 7),
                      rep(list(ballot(c(3,2,5,4,1), map=test_cands)), 8))
    
    results_1seat <- schulze(test_ballots, nseats=1)
    expect_equal(results_1seat$winner, 'E')
    expect_equal(length(results_1seat$winner), 1)
})

#Completely tied case
test_that("Schulze uses random in complete tie situation", {
    test_ballots <- c(list(c('A','B','C')), list(c('B','A','C')))
    
    results_tied <- schulze(test_ballots, nseats=1)
    expect_true(results_tied$winner %in% c('A','B'))
    
    results_not_tied <- schulze(test_ballots, nseats=2)
    expect_equal(sort(results_not_tied$winner), c('A','B'))
})
