context("Validation of input genes")
test_that("seed genes not a vector", {
    x <- matrix(seq(1, 10), nrow=5)
    seed <- function(y){mean(y)}
    rownames(x) <- c("g1", "g2", "g3", "g4", "g5")
    colnames(x) <- c("s1", "s2")
    expect_error(prioritizeCandidates(counts=x, seedGenes=seed),
        regexp="seedGenes parameter is not a list or vector")
    expect_error(findCoexpression(counts=x, seedGenes=seed),
                 regexp="seedGenes parameter is not a list or vector")
    list1 <- list(c("g1", "g2"))
    ## this does not work because list1[1] is a list with 2 genes.
    ## It would if you provided a list("g1", "g2")
    expect_error(prioritizeCandidates(counts=x, seedGenes=list1),
                 regexp="All seed genes are not present")
})

test_that("too many genes", {
    x <- matrix(seq(1, 10), nrow=5)
    rownames(x) <- c("g1", "g2", "g3", "g4", "g5")
    colnames(x) <- c("s1","s2")
    list1 <- c(rownames(x), "g6")
    list2 <- c("g1", "g2")
    expect_error(prioritizeCandidates(counts=x, seedGenes=list1),
        regexp="seedGenes parameter cannot have more elements")
    ## maybe change with the helper functions!
    expect_error(findCoexpression(counts=x, seedGenes=list1),
                 regexp="seedGenes parameter cannot have more elements")
    expect_error(prioritizeCandidates(counts=x,seedGenes=list2,
                                      candidateGenes=list1),
        regexp="candidateGenes parameter cannot have more elements")
})

test_that("missing or wrong genes",{
    x <- matrix(seq(1, 10), nrow=5)
    allgenes <- c("g1", "g2", "g3", "g4", "g5")
    rownames(x) <- allgenes
    colnames(x) <- c("s1","s2")
    list2 <- c("g7", "g8", "g10")
    list1 <- c("g3", "g4")
    expect_error(prioritizeCandidates(counts=x, seedGenes=list2),
        regexp="All seed genes are not present")
    expect_error(prioritizeCandidates(counts=x, seedGenes=list1,
                                      candidateGenes=list2),
        regexp="All candidate genes are not present")
    list3 <- c("g5", "g3")
    expect_error(prioritizeCandidates(counts=x, seedGenes=list1,
                                      candidateGenes=list3),
        regexp="Seed and candidate genes cannot have any overlap")
    list4 <- c("g1", "g2", "g8")
    expect_warning(checkGenes(allgenes, list4, "test"),
        regexp="not present in the expression matrix")
})
