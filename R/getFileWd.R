#' Get path of current file
#'
#' A function for easily getting the path of the current file you're working in.
#'
#' This function can only be executed from the editor, not the console; it uses rstudioapi. It will not work independently of R-studio.
#' Takes no inputs, and can have it's string results set to a variable.
#' @export
getFileWd <- function() {
	wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
	if(nchar(wd) == 0) {
		message("This function can only be executed in the editor, please run from file context.")
	} else if(nchar(wd) > 0) {
		message(glue::glue("Got wd as: \"{dirname(rstudioapi::getActiveDocumentContext()$path)}\""))
	}
	return(wd)
}
