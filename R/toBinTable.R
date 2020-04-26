#' Convert TRUE/FALSE table to binary table
#'
#' Converts a data frame object that contains TRUE and FALSE; also NAs to a binary table.
#'
#' Good for subsequently passing onto the `venn` package for visualisation.
#'
#' A sanitation check is done on the convertNA input to ensure it the resulting table is indeed binary.
#'
#' @param df A data frame object of TRUE/FALSE values; may also contain NAs.
#' @param convertNA A numeric value to which convert NAs.
#' @export
toBinTable <- function (df, convertNA = 1) {
	if (convertNA != (0 || 1)) {
		stop("convertNA should only be 0 or 1.")
	}
	for (i in 1:ncol(df)) {
		if (is.logical(df[, i]) == TRUE) {
			df[, i] <- as.numeric(df[, i])
		}
	}
	if (any(is.na(df)) == TRUE) {
		df[, 1:ncol(df)][is.na(df[, 1:ncol(df)])] = convertNA
		message(glue::glue("Converted NAs to {convertNA}."))
	}
	return(df)
}
