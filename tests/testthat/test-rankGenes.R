context("results of rankGenes")
test_that("correct ranking of all genes", {
    x <- matrix(c(4,6,8,2,1,3,1,8,3,1,2,3,4,2,6), nrow=5, byrow=TRUE)
    rownames(x) <- c("g1", "g2", "g3", "g4", "g5")
    colnames(x) <- c("s1","s2", "s3")
    seed <- c("g1", "g2")
    corrMatrix <- findCoexpression(x, seed)
    expectedRanks <- matrix(c(NA,2.5,4,1,2.5,2.5,NA,4,2.5,1),
                            nrow=5,
                            byrow=FALSE)
    expectedRanks2 <- matrix(c(NA,2.5,4,1,2.5,3.5,NA,2,3.5,1),
                            nrow=5,
                            byrow=FALSE)

    expect_equivalent(expectedRanks, rankGenes(corrMatrix))
    expect_equivalent(expectedRanks2, rankGenes(corrMatrix,
                                                antiCorrelation=TRUE))

})
