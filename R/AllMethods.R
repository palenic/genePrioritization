#' Methods for class RankedGeneList
#'
#' @name RankedGeneList-methods
#' @author Chiara Paleni\cr Politecnico di Milano\cr Maintainer: Chiara Paleni
#' \cr E-Mail: <chiara.paleni@@polimi.it>
#' @seealso \code{\link{prioritizeCandidates}}, \code{\link{RankedGeneList}}\cr
#' @docType methods
#' @param object A RankedGeneList object
#' @importFrom methods show
#' @importFrom utils head
#' @importFrom utils tail
#' @return Meaningful representations
NULL
#' @rdname RankedGeneList-methods
#' @exportMethod show
setMethod("show",
            signature(object="RankedGeneList"),
            function(object) {
                NC = length(object@candidates)
                NS = length(object@seed)
                cat("RankedGeneList object with",length(object@seed),
                    "seed genes and",NC,
                    "candidates\n")
                if(NS<20)
                    cat("Seed genes:",object@seed,"\n")
                else
                    cat("Seed genes",head(object@seed,4),
                        "[and", NS-4,"more]")
                if(NC<20)
                    cat("Ranked candidate genes:",object@candidates,"\n")
                else
                    cat("Ranked candidate genes:",head(object@candidates,4),
                        "[and", NC-4,"more]")
            })

#' @rdname RankedGeneList-methods
#' @exportMethod summary
setMethod("summary",
            signature(object="RankedGeneList"),
            function(object) {
                NC = length(object@candidates)
                NS = length(object@seed)
                cat("Candidate disease genes analysis\n\n")
                if(NS<20){
                    cat("Seed genes:\n")
                    cat(object@seed)
                    cat("\n\n")
                }
                else{
                    cat("Seed genes:\n")
                    print(head(object@seed, 4))
                    cat("[and",NS-4 ,"more]\n\n")
                }
                if(NC<20){
                    cat("Candidate gene scores:\n")
                    print(object@results)
                    cat("\n")
                    cat("Ranks of candidate genes:\n")
                    print(object@ranks)
                    cat("\n")
                    cat("Coexpression matrix of candidate genes:\n")
                    print(object@coexpr)
                }
                else{
                    cat("Candidate gene scores:\n")
                    print(head(object@results, 4))
                    cat("[and",NC-4 ,"more]\n\n")
                    cat("Ranks of candidate genes:\n")
                    print(head(object@ranks, 4))
                    cat("...\n")
                    print(tail(object@ranks, 4))
                    cat("\n")
                    cat("Coexpression matrix of candidate genes:\n")
                    print(head(object@coexpr, 4))
                    cat("...\n")
                    print(tail(object@coexpr, 4))
                    cat("\n")
                }
            })
