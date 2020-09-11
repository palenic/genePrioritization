context("Validation of input matrix")
test_that("Not providing matrix", {
    x <- seq(1, 10)
    seed <- c("gene1")
    expect_error(prioritizeCandidates(x, seed), regexp="not a matrix")
    expect_error(findCoexpression(x, seed), regexp="not a matrix")
    expect_error(rankGenes(x), regexp="not a matrix")
    expect_error(candidateScoring(x), regexp="not a matrix")
})

test_that("unnamed matrix", {
    x <- matrix(seq(1, 10), nrow=5)
    seed <- c("gene1")
    expect_error(prioritizeCandidates(x, seed), regexp="Unnamed")
    expect_error(findCoexpression(x, seed), regexp="Unnamed")
    expect_error(rankGenes(x), regexp="Unnamed")
    expect_error(candidateScoring(x), regexp="Unnamed")
})

test_that("wrong matrix shape", {
    x <- matrix(seq(1, 10), nrow=2)
    rownames(x) <- c("g1", "g2")
    colnames(x) <- c("s1", "s2", "s3", "s4", "s5")
    seed <- c("g1")
    cand <- c("g2")
    expect_message(checkCounts(x),
        regexp="There are more samples than genes")
    colnames(x) <- c("g1", "g2", "g3", "g4", "g5")
    expect_error(rankGenes(x),
        regexp="There can't be more seed genes than the total number of genes.")
    expect_error(candidateScoring(x, cand),
        regexp="There can't be more seed genes than the total number of genes.")
})

