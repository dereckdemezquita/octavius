#' Regex replace/remove from all rows of data frame
#'
#' A function for easily replacing/removing up to 3 different regular expressions from all rows in a data frame.
#' Takes a data frame object and outputs a data frame object. All regex set to `strn = ""` by default.
#' @param df A data frame type input.
#' @param str1 Regex to search for.å
#' @param str2 Regex to search for.
#' @param str3 Regex to search for.
#' @param rep1 Replacement string.
#' @param rep2 Replacement string.
#' @param rep3 Replacement string.å
#' @export
dfRmvStr <- function(df, str1 = "", str2 = "", str3 = "", rep1 = "", rep2 = "", rep3 = "") {
	df <- lapply(df, sub, pattern = str1, replacement = rep1) %>% as.data.frame()
	df <- lapply(df, sub, pattern = str2, replacement = rep2) %>% as.data.frame()
	df <- lapply(df, sub, pattern = str3, replacement = rep3) %>% as.data.frame()
}
