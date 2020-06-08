#' Deconcatenate rows of a data frame
#'
#' Made for deconcatenating the rows of the DESeq2 results object. Works well on other data frames, feed the data frame and the pattern to match.
#'
#' It duplicates the data for each row and sets the separated names values to rownames, separately.
#'
#' @param df A data frame type object.
#' @param pattern A string to use a separator for the rows.
#' @export
dfDeconcat <- function(df, pattern) {
	split_genes <- strsplit(rownames(df), pattern, fixed = TRUE)

	split_genes_lengths <- sapply(split_genes, length)
	res_cat_tmp <- res[rep(1:nrow(df), split_genes_lengths), ]
	rownames(res_cat_tmp) <- unlist(split_genes)

	df <- res_cat_tmp
	return(df)
}
