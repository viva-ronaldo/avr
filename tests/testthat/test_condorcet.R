context('Condorcet tests')

test_that("Condorcet basics are correct", {
    votes <- c(
        replist("Orange",                4),
        replist(c("Pear", "Orange"),     2),
        replist(c("Choc", "Strawberry"), 8),
        replist(c("Choc", "Candy"),      4),
        replist("Strawberry",            1),
        replist("Candy",                 1)
    )
    results_1seat <- condorcet(votes)
    expect_equal(results_1seat$nballots, 20)
    expect_equal(sort(results_1seat$candidates), c('Candy','Choc','Orange','Pear','Strawberry'))
    expect_equal(results_1seat$winners, c('Choc'))
    expect_equal(results_1seat$win_method, 'outright')
})

#Use Borda Wikipedia example. Andrew wins by Condorcet as wins all 3 head to heads.
#Borda check from Wikipedia
test_that("Condorcet resolves Borda Wikipedia example correctly", {
    test_cands <- c('Andrew','Catherine','Brian','David')
    test_ballots <- c(rep(list(ballot(c(1,2,3,4), map=test_cands)), 51),
                      rep(list(ballot(c(4,1,2,3), map=test_cands)), 5),
                      rep(list(ballot(c(4,2,1,3), map=test_cands)), 23),
                      rep(list(ballot(c(4,2,3,1), map=test_cands)), 21))
    
    results_1seat <- condorcet(test_ballots)
    expect_equal(sort(results_1seat$candidates), c('Andrew','Brian','Catherine','David'))
    expect_equal(results_1seat$winners, c('Andrew'))
    expect_equal(results_1seat$win_method, 'outright')
})
