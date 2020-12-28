context('Borda method tests')

test_that("Borda basics are correct", {
    votes <- c(
        replist("Orange",                4),
        replist(c("Pear", "Orange"),     2),
        replist(c("Choc", "Strawberry"), 8),
        replist(c("Choc", "Candy"),      4),
        replist("Strawberry",            1),
        replist("Candy",                 1)
    )
    results_1seat <- borda(votes, nseats=1)
    expect_equal(results_1seat$nballots, 20)
    expect_equal(sort(results_1seat$candidates), c('Candy','Choc','Orange','Pear','Strawberry'))
    expect_equal(results_1seat$winners, c('Choc'))
    expect_false(results_1seat$used_random)
    
    results_2seat <- borda(votes, nseats=2)
    expect_equal(sort(results_2seat$winners), c('Choc','Strawberry'))
    results_2seat_m <- borda(votes, nseats=2, variant='modified')
    expect_equal(sort(results_2seat_m$winners), c('Choc','Strawberry'))
    results_2seat_d <- borda(votes, nseats=2, variant='dowdall')
    expect_equal(sort(results_2seat_d$winners), c('Choc','Orange')) #!
})

#Borda check from Wikipedia
test_that("Borda is correct for Wikipedia example", {
    test_cands <- c('Andrew','Catherine','Brian','David')
    test_ballots <- c(rep(list(ballot(c(1,2,3,4), map=test_cands)), 51),
                      rep(list(ballot(c(4,1,2,3), map=test_cands)), 5),
                      rep(list(ballot(c(4,2,1,3), map=test_cands)), 23),
                      rep(list(ballot(c(4,2,3,1), map=test_cands)), 21))
    
    results_1seat <- borda(test_ballots, nseats=1, variant='standard')
    expect_equal(results_1seat$winners, 'Catherine')
    expect_equal(borda(test_ballots, nseats=1)$points_table$total_points, c(205,153,151,91))
    
    results_1seat_d <- borda(test_ballots, nseats=1, variant='dowdall')
    expect_equal(results_1seat_d$winners, 'Andrew')
    expect_equal(results_1seat_d$points_table$candidate[2], 'Catherine')
})

#Completely tied case
test_that("Borda uses random in complete tie situation", {
    test_ballots <- c(list(c('A','B','C')), list(c('B','A','C')))
    
    results_tied <- borda(test_ballots, nseats=1)
    expect_true(results_tied$winners %in% c('A','B'))
    
    results_not_tied <- borda(test_ballots, nseats=2)
    expect_equal(sort(results_not_tied$winner), c('A','B'))
})
