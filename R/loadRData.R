#' Load .RData into var
#'
#' Allows to load an .RData file into a variable name, not the global environment.
#'
#' Experimental; unstable.
#'
#' @param file
#' @example var <- loadRData("~/path/file.RData")
#' @export
loadRData <- function(file){
	load(file)
	get(ls()[ls() != "file"])
}
