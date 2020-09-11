#' genePrioritization - Find potential disease-related genes
#'
#' The \code{genePrioritization} package finds potential disease-related genes
#' based on their coexpression with known disease-related "seed" genes,
#' starting from
#' a gene expression count matrix. The package computes a ranked co-expression
#' list for each seed gene; each candidate gene is assigned the product of its
#' ranks within these lists as a total score, according to which the candidates
#' are finally ranked. Higher ranked genes should be functionally linked to the
#' disease related genes and can thus have a role in the disease.
#'
#' \tabular{ll}{
#' Package: \tab genePrioritization\cr
#' Type: \tab Package\cr
#' Version: \tab 0.99.4\cr
#' Date: \tab 2020-08-26\cr
#' License: \tab GPL-3\cr
#' }
#'
#' @name genePrioritization-package
#' @aliases genePrioritization-package genePrioritization
#' @docType package
#' @author Chiara Paleni [aut, cre]\cr
#' Politecnico di Milano\cr
#' Maintainer: Chiara Paleni\cr
#' E-Mail: <chiara.paleni@@mail.polimi.it>
NULL
