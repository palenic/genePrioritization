#' Candidate gene ranking
#'
#' Takes as input a ranked coexpression list between a set of
#' genes and a set of seed genes, computes a score for each candidate gene
#' as the product of their relative ranks (rank/(nGenes-1)), and finally ranks
#' them according to their
#' increasing score: lower scores indicate a higher coexpression, and thus a
#' higher probability of being involved in the given phenotype. Rank(seed, seed)
#' should be NA: if not, throws a warning. Used
#' alternatively to \code{\link{prioritizeCandidates}}, after
#' \code{\link{rankGenes}} as part of \code{\link{genePrioritization}} workflow.
#'
#' @usage candidateScoring(rankMatrix, candidateGenes=NULL)
#'
#' @param rankMatrix A rank matrix with shape (nGenes, nSeed). Both rows and
#' columns have to be named with gene symbols or IDs. Rank(seed, seed) should
#' be set to NA.
#' @param candidateGenes (default NULL) A vector or list of candidate gene
#' symbols or IDs. If a list, each gene has to be a list element. If NULL,
#' all other genes in the matrix which are not part of the seed genes
#' will be considered candidates. If supplied, least one candidate gene
#' has to be present in the rank matrix rownames. Missing or duplicated genes
#' are removed.
#' @return A named vector of scores for the candidates, sorted from lowest
#' (most correlated to seed genes) to highest (least correlated).
#' @author Chiara Paleni\cr Politecnico di Milano\cr Maintainer: Chiara Paleni
#' \cr E-Mail: <chiara.paleni@@polimi.it>
#' @references \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2935433/}\cr
#' Piro, Rosario M et al. “Candidate gene prioritization based on spatially
#' mapped gene expression: an application to XLMR.” Bioinformatics (Oxford,
#' England) vol. 26,18 (2010): i618-24. doi:10.1093/bioinformatics/btq396
#' @seealso \code{\link{prioritizeCandidates}}, \code{\link{findCoexpression}},
#' \code{\link{rankGenes}}\cr
#' @examples
#' a <- matrix(c(1,2,3,2,4,6,8,6,4,5,2,8,7,1,5),
#' nrow=5, ncol=3,byrow=TRUE)
#' colnames(a) <- c('sample1','sample2','sample3')
#' rownames(a) <- c('gene1','gene2','gene3','gene4','gene5')
#' seed <- c('gene1')
#' candidates <- c('gene2','gene4')
#' x <- findCoexpression(counts=a, seedGenes=seed)
#' y <- rankGenes(x)
#' z <- candidateScoring(y, candidates)
#' @export

candidateScoring <- function(rankMatrix, candidateGenes=NULL){
    ## input validation
    checkMatrix(rankMatrix, "rank")
    seedGenes <- colnames(rankMatrix)
    allGenes <- rownames(rankMatrix)
    ## candidate validation
    candidateGenes <- checkCandidates(allGenes, seedGenes, candidateGenes)
    ## get relative ranks
    ## checking seed genes
    count <- 0
    N <- nrow(rankMatrix)-1
    for(s in seedGenes)
        if(! is.na(rankMatrix[s,s]))
            count <- count+1
    if(count > 0){
        warning("Rank(seed, seed) should be NA. Please run function
prioritizeCandidates or rankGenes.")
        N <- nrow(rankMatrix)
    }
    rankMatrix <- rankMatrix/N
    ## multiply the scores on the rows for each candidate
    scores <- apply(X = rankMatrix[candidateGenes,,drop=FALSE], FUN = prod,
                    MARGIN = 1)
    scores <- sort(scores)
    return(scores)
}
