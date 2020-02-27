#' Vector values unique check
#'
#' A function that will take a vector and test if the values are unique.
#' @param vec A vector of any kind, can contain logicals or characters etc.
#' @export
vec_unique_check <- function(vector) {
	if(!any(duplicated(vector))) {
		message("No repeat values found in the vector.")
	} else {
		message("Repeat values found in the vector.")
	}
}
