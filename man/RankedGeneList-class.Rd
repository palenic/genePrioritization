% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllClasses.R
\docType{class}
\name{RankedGeneList-class}
\alias{RankedGeneList-class}
\alias{RankedGeneList}
\title{Class RankedGeneList}
\description{
A S4 class to store results of a genePrioritization analysis.
}
\section{Slots}{

\describe{
\item{\code{results}}{A named vactor of candidate scores sorted according to
increasing score}

\item{\code{seed}}{The seed genes}

\item{\code{candidates}}{The candidate genes sorted according to
increasing score}

\item{\code{coexpr}}{A named matrix of shape (nCandidates, nSeed) with
coexpression values between each seed and each candidate.
Rows are sorted according to increasing candidate score.}

\item{\code{ranks}}{A named matrix of shape (nCandidates, nSeed) with ranks of
each candidate in each seed gene coexpression list. Candidates are ranked
according to their decreasing coexpression. Rows are sorted according to
increasing candidate score}
}}

\examples{
a <- matrix(c(1,2,3,2,4,6,8,6,4,5,2,8,7,1,5),
nrow=5, ncol=3,byrow=TRUE)
colnames(a) <- c('sample1','sample2','sample3')
rownames(a) <- c('gene1','gene2','gene3','gene4','gene5')
seed <- c('gene1')
candidates <- c('gene2','gene4')
x <- findCoexpression(counts=a, seedGenes=seed)
y <- rankGenes(x)
z <- candidateScoring(y, candidates)
result <- RankedGeneList(seed=seed, candidates=candidates,
                        results=z,
                        coexpr=x[candidates,,drop=FALSE],
                        ranks=y[candidates,,drop=FALSE])

}
\seealso{
\code{\link{prioritizeCandidates}}\cr
}
