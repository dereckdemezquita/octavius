#' Extract top statistically significant lg2fc genes
#'
#' Extracts all top significantly differentially expressed genes from res object of DESeq2. The results will be automatically ordered per lowest alpha, then reordered per highest absolute value for the lg2fc column  of the res object.
#'
#' Allows for setting an alpha threshold. Note that this function will use the dfRmvNA function to remove any NA rows found in the results object. Note that NA results in the padj column of the DESeq2 results object are normal; consult the DESeq2 documentation.
#'
#' Was made to work with res object of DESeq2.
#'
#' @param res A DESeq2 results type object; data frame type object.
#' @param alpha Alpha threshold, default set to 0.1.
#' @export
topSigExtract <- function(res, alpha = 0.1) {
	res <- dfRmvNA(res, col = "padj")
	topSig <- res[res$padj <= alpha,]

	topSig <- topSig[order(-abs(topSig$log2FoldChange), topSig$padj),]
	return(topSig)
}
