#' Accessor methods for class \code{\link{RankedGeneList}}
#'
#' @importFrom methods is
#' @name RankedGeneList-accessors
#' @param object A RankedGeneList object
#' @return A specific slot of the object
#' @seealso \code{\link{RankedGeneList}},
#' \code{\link{prioritizeCandidates}}\cr
#' @examples
#' a <- matrix(c(1,2,3,2,4,6,8,6,4,5,2,8,7,1,5),
#' nrow=5, ncol=3,byrow=TRUE)
#' colnames(a) <- c('sample1','sample2','sample3')
#' rownames(a) <- c('gene1','gene2','gene3','gene4','gene5')
#' seed <- c('gene1')
#' candidates <- c('gene2','gene4')
#' z <- prioritizeCandidates(counts=a, seedGenes=seed,
#' candidateGenes=candidates, antiCorrelation=FALSE)
#' getSeed(z)
NULL
#' @rdname RankedGeneList-accessors
#' @export
getCandidates <- function(object){
    if(is(object,"RankedGeneList"))
        return(object@candidates)
    else
        stop("did not receive a RankedGeneList object")
}
#' @rdname RankedGeneList-accessors
#' @export
getSeed <- function(object){
    if(is(object,"RankedGeneList"))
        return(object@seed)
    else
        stop("did not receive a RankedGeneList object")
}
#' @rdname RankedGeneList-accessors
#' @export
getResults <- function(object){
    if(is(object,"RankedGeneList"))
        return(object@results)
    else
        stop("did not receive a RankedGeneList object")
}
#' @rdname RankedGeneList-accessors
#' @export
getCoexpression <- function(object){
    if(is(object,"RankedGeneList"))
        return(object@coexpr)
    else
        stop("did not receive a RankedGeneList object")
}
#' @rdname RankedGeneList-accessors
#' @export
getRanks <- function(object){
    if(is(object,"RankedGeneList"))
        return(object@ranks)
    else
        stop("did not receive a RankedGeneList object")
}
