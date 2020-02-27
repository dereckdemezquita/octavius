#' Update/re-install octavius
#'
#' Will quickly remove and install Octavius in one function. No inputs required just call the function.
#' @export
octavius <- function() {
	remove.packages("octavius")
	devtools::install_github("dereckdemezquita/octavius", force = TRUE)
	library("octavius")
}
