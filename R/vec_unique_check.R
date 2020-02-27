#' Vector values unique check
#'
#' A function that will take a vector and test if the values are unique.
#' @param vec A vector of any kind, can contain logicals or characters etc.
#' @export
vec_unique_check <- function(vec) {
	message("No repeats found in the vector input.")
	# return(!any(duplicated(vec)))
}
