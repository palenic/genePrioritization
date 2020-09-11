context("results of candidateScoring")
test_that("correct scoring of candidate genes", {
    x <- matrix(c(4,6,8,2,1,3,1,8,3,1,2,3,4,2,6), nrow=5, byrow=TRUE)
    rownames(x) <- c("g1", "g2", "g3", "g4", "g5")
    colnames(x) <- c("s1","s2", "s3")
    seed <- c("g1", "g2")
    cand <- c("g3", "g5")

    corrMatrix <- findCoexpression(x, seed)
    rankMatrix <- rankGenes(corrMatrix)
    rankMatrix2 <- rankGenes(corrMatrix, antiCorrelation=TRUE)

    expectedScores <- c(0.15625,1)
    expectedScores2 <- c(0.15625,0.5)
    expectedScores3 <- c(0.15625,0.15625,1)
    expectedScores4 <- c(0.15625,0.21875,0.5)

    expect_equivalent(expectedScores, candidateScoring(rankMatrix, cand))
    expect_equivalent(expectedScores2, candidateScoring(rankMatrix2, cand))
    expect_equivalent(expectedScores3, candidateScoring(rankMatrix))
    expect_equivalent(expectedScores4, candidateScoring(rankMatrix2))

})
