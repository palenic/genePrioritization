context("getter methods")
test_that("input validation", {
    x <- matrix(c(4,6,8,2,1,3,1,8,3,1,2,3,4,2,6), nrow=5, byrow=TRUE)
    rownames(x) <- c("g1", "g2", "g3", "g4", "g5")
    colnames(x) <- c("s1","s2", "s3")
    seed <- c("g1", "g2")
    cand <- c("g3", "g5")
    cand2 <- function(y){mean(y)}
    res <- prioritizeCandidates(x, seed, cand)

    expect_error(getRanks(cand2), regexp="RankedGeneList object")
    expect_error(getSeed(cand2), regexp="RankedGeneList object")
    expect_error(getCandidates(seed), regexp="RankedGeneList object")
    expect_error(getResults(seed), regexp="RankedGeneList object")

})

test_that("correctly get coexpression and rank matrix", {
    x <- matrix(c(4,6,8,2,1,3,1,8,3,1,2,3,4,2,6), nrow=5, byrow=TRUE)
    rownames(x) <- c("g1", "g2", "g3", "g4", "g5")
    colnames(x) <- c("s1","s2", "s3")
    seed <- c("g1", "g2")
    cand <- c("g3", "g5")
    sortedCand <- c("g5","g3")


    expectedCor <- matrix(c(0.5,0.277,1,-0.693), nrow=2, byrow=FALSE)
    expectedRanks <- matrix(c(2.5,1,4,4),
                            nrow=2,
                            byrow=TRUE)

    res1 <- prioritizeCandidates(x, seed, cand)

    expect_equivalent(expectedCor, getCoexpression(res1),
                        tolerance=0.001)
    expect_equivalent(expectedRanks, getRanks(res1))
    expect_equivalent(seed, getSeed(res1))
    expect_equivalent(sortedCand, getCandidates(res1))
})
