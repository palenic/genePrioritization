context("results of findCoexpression")
test_that("correct results", {
    ## writing a simple matrix to check the results
    x <- matrix(c(4,6,8,2,1,3,1,8,3,1,2,3,4,2,6), nrow=5, byrow=TRUE)
    rownames(x) <- c("g1", "g2", "g3", "g4", "g5")
    colnames(x) <- c("s1","s2", "s3")
    ## trying a variety of formats for input
    seed1 <- c("g1", "g2")
    seed2 <- c("g1")
    seed2_1 <- "g1"
    seed1_1 <- list("g1","g2")
    seed1_2 <- list(c("g1","g2"))[[1]]
    expectedCor1 <- matrix(c(1,0.5,0.277,1,0.5,0.5,1,-0.693,0.5,1),
                            nrow=5, byrow=FALSE)
    expectedCor2 <- matrix(c(1,0.5,0.277,1,0.5), nrow=5,
                            byrow=FALSE)

    expect_equivalent(findCoexpression(x, seed1), expectedCor1, tolerance=0.001)
    expect_equivalent(findCoexpression(x, seed1_1), expectedCor1,
                        tolerance=0.001)
    expect_equivalent(findCoexpression(x, seed1_2), expectedCor1,
                        tolerance=0.001)

    expect_equivalent(findCoexpression(x, seed2), expectedCor2, tolerance=0.001)
    expect_equivalent(findCoexpression(x, seed2_1), expectedCor2,
                        tolerance=0.001)
})
