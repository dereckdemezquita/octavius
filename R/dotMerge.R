#' Merge CARNIVAL dot networks
#'
#' Merges two CARNIVAL networks, removing any EXACT interactions that are in double.
#'
#' Use readLines() to read the networks as a string object, each line should be a new element in an array.
#'
#' @param dot1 A .dot network network.
#' @param dot1 A .dot network network.
#' @export
dotMerge <- function(dot1, dot2) {
	dot1 <- dot1[-1]
	dot1 <- dot1[-length(dot1)]

	dot2 <- dot2[-1]
	dot2 <- dot2[-length(dot2)]

	dot <- unique(c("digraph {", dot1, dot2, "}"))
	return(dot)
}
