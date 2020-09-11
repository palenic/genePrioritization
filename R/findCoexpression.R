#' Seed coexpression
#'
#' Computes coexpression of each gene in a gene expression matrix with each of
#' the provided "seed" genes as the Pearson correlation. Used alternatively to
#' \code{\link{prioritizeCandidates}} as first step of
#' \code{\link{genePrioritization}} worflow.
#'
#'
#' @usage findCoexpression(counts, seedGenes)
#'
#' @param counts A gene expression matrix (genes in rows, samples in columns):
#' can be a count matrix, a microarray result, as long as it's numeric. Rows
#' have to be named with the gene symbol or ID.
#' If desired, normalization has to be previously performed be the user.
#' @param seedGenes A vector or list of seed gene symbols or IDs. If a list,
#' each gene has to be a list element. At least one seed gene has to be present
#' in the counts matrix rownames. Missing or duplicated genes are removed.
#' @return A named matrix of shape (nGenes, nSeed) with correlation values
#' between each seed and each gene in the matrix.
#' @author Chiara Paleni\cr Politecnico di Milano\cr Maintainer: Chiara Paleni
#' \cr E-Mail: <chiara.paleni@@polimi.it>
#' @references \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2935433/}\cr
#' Piro, Rosario M et al. “Candidate gene prioritization based on spatially
#' mapped gene expression: an application to XLMR.” Bioinformatics (Oxford,
#' England) vol. 26,18 (2010): i618-24. doi:10.1093/bioinformatics/btq396
#' @seealso \code{\link{prioritizeCandidates}}, \code{\link{rankGenes}},
#' \code{\link{candidateScoring}}\cr
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
#' @importFrom stats cor

findCoexpression <- function(counts, seedGenes){
    ## input validation
    checkCounts(counts)
    allGenes <- rownames(counts)
    seedGenes <- checkGenes(allGenes, seedGenes, "seed")

    ## defining the function to apply to each seed

    seedCoexpr <- NULL
    seedCoexpr <- function(seed, counts){
        ## helper function that computes correlation btw each row of counts
        ## (using MARGIN=1) and the row corresponding to the seed gene we are
        ## considering
        c <- apply(X=counts, FUN=cor, y=counts[seed,], MARGIN=1)
        return(c)
    }

    ## applies seedCoexpr to each seed gene in the seed list. each call to apply
    ## returns a vector of size = number of genes. Returns a matrix with dim
    ## (nGenes, nSeed)

    corrMatrix <- vapply(X=seedGenes, FUN=seedCoexpr, counts=counts,
                        FUN.VALUE=rep(1,nrow(counts)))
    return(corrMatrix)
}
