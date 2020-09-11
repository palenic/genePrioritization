context("results of prioritizeCandidates")
test_that("correct output format",{
    x <- matrix(c(4,6,8,2,1,3,1,8,3,1,2,3,4,2,6), nrow=5, byrow=TRUE)
    rownames(x) <- c("g1", "g2", "g3", "g4", "g5")
    colnames(x) <- c("s1","s2", "s3")
    seed <- c("g1", "g2")
    cand <- c("g3", "g5")
    res <- prioritizeCandidates(x, seed, cand)
    expect_s4_class(res, "RankedGeneList")
})

test_that("correct scoring of candidate genes", {
    x <- matrix(c(4,6,8,2,1,3,1,8,3,1,2,3,4,2,6), nrow=5, byrow=TRUE)
    rownames(x) <- c("g1", "g2", "g3", "g4", "g5")
    colnames(x) <- c("s1","s2", "s3")
    seed <- c("g1", "g2")
    cand <- c("g3", "g5")

    expectedScores <- c(0.15625,1)
    expectedScores2 <- c(0.15625,0.5)
    expectedScores3 <- c(0.15625,0.15625,1)
    expectedScores4 <- c(0.15625,0.21875,0.5)

    res1 <- prioritizeCandidates(x, seed, cand)
    res2 <- prioritizeCandidates(x, seed, cand, antiCorrelation=TRUE)
    res3 <- prioritizeCandidates(x, seed)
    res4 <- prioritizeCandidates(x, seed, antiCorrelation=TRUE)

    expect_equivalent(expectedScores, res1@results)
    expect_equivalent(expectedScores2, res2@results)
    expect_equivalent(expectedScores3, res3@results)
    expect_equivalent(expectedScores4, res4@results)

})

test_that("correct seed/candidate lists", {
    x <- matrix(c(4,6,8,2,1,3,1,8,3,1,2,3,4,2,6), nrow=5, byrow=TRUE)
    rownames(x) <- c("g1", "g2", "g3", "g4", "g5")
    colnames(x) <- c("s1","s2", "s3")
    seed <- c("g1", "g2", "g2")
    cand <- c("g3", "g5")

    expectedSeed <- c("g1","g2")
    expectedCand <- c("g5","g3")
    expectedCand2 <- c("g4","g5","g3")

    res1 <- prioritizeCandidates(x, seed, cand)
    res3 <- prioritizeCandidates(x, seed)

    expect_equivalent(expectedCand, res1@candidates)
    expect_equivalent(expectedCand2, res3@candidates)
    expect_equivalent(expectedSeed, res1@seed)
    expect_equivalent(expectedSeed, res3@seed)

})

test_that("correct coexpression and rank matrix", {
    x <- matrix(c(4,6,8,2,1,3,1,8,3,1,2,3,4,2,6), nrow=5, byrow=TRUE)
    rownames(x) <- c("g1", "g2", "g3", "g4", "g5")
    colnames(x) <- c("s1","s2", "s3")
    seed <- c("g1", "g2")
    cand <- c("g3", "g5")

    expectedCor <- matrix(c(0.5,0.277,1,-0.693), nrow=2, byrow=FALSE)
    expectedRanks <- matrix(c(2.5,1,4,4),
                            nrow=2,
                            byrow=TRUE)
    res1 <- prioritizeCandidates(x, seed, cand)

    expect_equivalent(expectedCor, res1@coexpr, tolerance=0.001)
    expect_equivalent(expectedRanks, res1@ranks)

})

