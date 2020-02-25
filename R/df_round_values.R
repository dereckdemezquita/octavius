#' Function for rounding all values in a data frame keeping them in the numeric type.
#'
#' Takes two variables, a data frame and a number to round to.
#' @param df a data frame input.
#' @param digits a number to which the values will be rounded to.
#' @export
df_round_values <- function(df, digits) {
	# round all numeric variables
	# df: data frame
	# digits: number of digits to round enter numeric type
	numeric_columns <- sapply(df, mode) == 'numeric'
	df[numeric_columns] <-  round(df[numeric_columns], digits)
	df
}
