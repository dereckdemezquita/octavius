#' Remove NA rows from data frame.
#'
#' Reliably removes all rows that include NA values for a given column in a data frame object.
#'
#' Was made to work with res object of DESeq2.
#'
#' @param df A data frame type object.
#' @param col A string for selecting a column.
#' @export
dfRmvNA <- function(df, col = "") {
	map <- !is.na(df$col)
	df <- df[map,]
	return(df)
}
