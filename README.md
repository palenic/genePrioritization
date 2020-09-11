# genePrioritization
genePrioritization - Find potential disease-related genes  

The genePrioritization package finds potential disease-related genes based on their coexpression with known disease-related "seed" genes, starting from a gene expression count matrix. The package computes a ranked co-expression list for each seed gene; each candidate gene is assigned the product of its ranks within these lists as a total score, according to which the candidates are finally ranked. Higher ranked genes should be functionally linked to the disease related genes and can thus have a role in the disease.

## References
Piro, Rosario M et al. “Candidate gene prioritization based on spatially mapped gene expression: an application to XLMR.” Bioinformatics (Oxford, England) vol. 26,18 (2010): i618-24. doi:10.1093/bioinformatics/btq396
