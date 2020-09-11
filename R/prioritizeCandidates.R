#' Candidate gene prioritization
#'
#' Finds potential disease-related genes
#' based on their coexpression with known disease-related ("seed") genes.
#' Takes as input a gene expression matrix, a list of seed genes and
#' (optionally) a list of candidate genes; computes a ranked co-expression
#' list for each seed gene; assigns to each candidate gene the product of its
#' ranks within these lists as a total score; ranks them according to their
#' increasing score. Lower scores indicate a higher coexpression, and thus a
#' higher probability of being involved in the given phenotype. The function
#' performs all steps of the analysis and returns a \code{\link{RankedGeneList}}
#' object summarizing the
#' results, but they can be performed manually by the user with the functions
#' \code{\link{findCoexpression}}, \code{\link{rankGenes}}
#' and \code{\link{candidateScoring}}.
#'
#' @usage prioritizeCandidates(counts, seedGenes, candidateGenes=NULL,
#' antiCorrelation=FALSE)
#'
#' @param counts A gene expression matrix (genes in rows, samples in columns):
#' can be a count matrix, a microarray result, as long as it's numeric. Rows
#' have to be named with the gene symbol or ID.
#' If desired, normalization has to be previously performed be the user.
#' @param seedGenes A vector or list of seed gene symbols or IDs. If a list,
#' each gene has to be a list element. At least one seed gene has to be present
#' in the counts matrix rownames. Missing or duplicated genes are removed.
#' @param candidateGenes (default NULL) A vector or list of candidate gene
#' symbols or IDs. If a list, each gene has to be a list element. If NULL,
#' all other genes in the matrix which are not part of the seed genes
#' will be considered candidates. If supplied, least one candidate gene
#' has to be present in the rank matrix rownames. Missing or duplicated genes
#' are removed.
#' @param antiCorrelation A logical value (default FALSE). If TRUE,
#' anti-correlation will be considered as a significant correlation, thus
#' strongly anti-correlated genes will have a high rank.
#' @return A RankedGeneList object
#' @author Chiara Paleni\cr Politecnico di Milano\cr Maintainer: Chiara Paleni
#' \cr E-Mail: <chiara.paleni@@polimi.it>
#' @references \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2935433/}\cr
#' Piro, Rosario M et al. “Candidate gene prioritization based on spatially
#' mapped gene expression: an application to XLMR.” Bioinformatics (Oxford,
#' England) vol. 26,18 (2010): i618-24. doi:10.1093/bioinformatics/btq396
#' @seealso \code{\link{findCoexpression}}, \code{\link{rankGenes}},
#' \code{\link{candidateScoring}}, \code{\link{RankedGeneList}}\cr
#' @examples
#' a <- matrix(c(1,2,3,2,4,6,8,6,4,5,2,8,7,1,5),
#' nrow=5, ncol=3,byrow=TRUE)
#' colnames(a) <- c('sample1','sample2','sample3')
#' rownames(a) <- c('gene1','gene2','gene3','gene4','gene5')
#' seed <- c('gene1')
#' candidates <- c('gene2','gene4')
#' z <- prioritizeCandidates(counts=a, seedGenes=seed,
#' candidateGenes=candidates, antiCorrelation=FALSE)
#' @export

prioritizeCandidates <- function(counts, seedGenes, candidateGenes=NULL,
                                antiCorrelation=FALSE){
    checkCounts(counts)
    allGenes <- rownames(counts)
    if(!is.logical(antiCorrelation)){
        stop("antiCorrelation parameter is not a logical value")
    }
    seedGenes <- checkGenes(allGenes, seedGenes, "seed")
    candidateGenes <- checkCandidates(allGenes, seedGenes, candidateGenes)
    ## technically the seedGenes check is also performed by findCoexpression,
    ## however I think it's best to stop the function immediately if there is a
    ## problem with the candidates, and i need the seedGenes to test that
    corrMatrix <- findCoexpression(counts, seedGenes)
    rankMatrix <- rankGenes(corrMatrix, antiCorrelation)
    scores <- candidateScoring(rankMatrix, candidateGenes)
    sortedCand <- names(scores)
    result <- RankedGeneList(seed=seedGenes, candidates=sortedCand,
                            results=scores,
                            coexpr=corrMatrix[sortedCand,,drop=FALSE],
                            ranks=rankMatrix[sortedCand,,drop=FALSE])
    return(result)
}
