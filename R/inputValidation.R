## this file contains "helper" functions for input validation which will not
## be exported

checkCounts <- function(counts){
    ## this is used as input control for prioritizeCandidates and
    ## findCoexpression which should take as input a matrix of shape (nGenes,
    ## nSamples). idc if the samples are not named but the genes have to.
    ## throws error if: not matrix; unnnamed. message if I suspect the matrix
    ## shape is wrong (but it still goes on with the analysis)

    if(!is.matrix(counts)){
        stop("counts parameter is not a matrix")
    }
    if(is.null(rownames(counts))){
        stop("Unnamed counts matrix. Please provide gene names as rownames")
    }
    if(nrow(counts) < ncol(counts)){
        message("There are more samples than genes. Make sure your input
matrix has shape (nGenes, nSamples)")
    }
}

checkGenes <- function(allGenes, geneList, geneListName){
    ## this is used as input control for prioritizeCandidates and
    ## findCoexpression. Works for both seed and candidates.
    ## It checks that the gene list is iterable and that the
    ## genes are actually in the matrix, if not, removes them with a warning.
    ## If no gene is present in the count matrix it throws an error

    if(!is.vector(geneList) & !is.list(geneList)){
        stop(geneListName, "Genes parameter is not a list or vector")
    }
    geneList <- unique(geneList)
    if(length(geneList) > length(allGenes)){
        stop(geneListName, "Genes parameter cannot have more elements than
the total number of genes")
    }
    missingGenes <- geneList[!geneList %in% allGenes]
    if(length(missingGenes) == length(geneList)){
        stop("All ", geneListName, " genes are not present in the count matrix.
Please provide a different set")
    }
    else if(length(missingGenes) > 0){
        warning("Gene/s", missingGenes, "not present in the expression matrix:
removing from", geneListName,"genes list")
        geneList <- geneList[!geneList %in% missingGenes]
    }
    return(geneList)
}

checkCandidates <- function(allGenes, seedGenes, candidateGenes=NULL){
    ## this is used as input control for prioritizeCandidates and
    ## candidateScoring. It checks if there are candidate genes and fills the
    ## list if there are none. Else, it checks that candidateGenes is iterable
    ## and that the candidateGenes are actually in the matrix, if not, removes
    ## them. If no candidate gene is present in the count matrix it throws an
    ## error. If there are more seed than candidates it prints a message.

    if (is.null(candidateGenes)){
        message("No candidate genes were provided. Setting all non-seed genes
as candidates.")
        candidateGenes<-allGenes[!allGenes %in% seedGenes]
    }
    else{
        candidateGenes <- checkGenes(allGenes, candidateGenes, "candidate")
        if(length(candidateGenes) < length(seedGenes)){
            message("There are more seed than candidates. Make sure you provided
them correctly.")
        }
        if(length(intersect(seedGenes, candidateGenes))>0){
            stop("Seed and candidate genes cannot have any overlap. Make sure
you provided them correctly.")
        }
    }
    return(candidateGenes)
}

checkMatrix <- function(aMatrix, matrixName){
    ## this is used as input control for rankGenes and candidateScoring
    ## which should take as input a matrix with shape (nGenes, nSeed).
    ## both rows and cols have to be named
    if(!is.matrix(aMatrix)){
        stop(matrixName, "parameter is not a matrix")
    }
    if((is.null(rownames(aMatrix))) | (is.null(colnames(aMatrix)))){
        stop("Unnamed", matrixName, "matrix. Please provide gene names
as rownames and seed names as colnames")
    }
    if(nrow(aMatrix) < ncol(aMatrix)){
        stop("There can't be more seed genes than the total number of genes.
Make sure your input matrix has shape (nGenes, nSeed)")
    }
}

