#' Check if variable is defined
#'
#' Easily check if a variable has been previously instantiated and defined. Does not require quotations.
#'
#' @param var variable to check; no quotations.
#' @examples isDefined(a) # FALSE
#' @examples a <- 10
#' @examples isDefined(a) # TRUE
#' @source https://stackoverflow.com/questions/9368900/how-to-check-if-object-variable-is-defined-in-r
#' @export
isDefined <- function(var) {
	var <- deparse(substitute(var));
	env <- parent.frame();
	exists(var, env);
}
