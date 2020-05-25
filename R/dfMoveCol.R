#' Move data frame columns with natural language
#'
#' A function for easily moving cols in df with natural language.
#' Takes the names() of the data frame and a move command in natural language; 4 options.
#' #' dfMoveCol(names(df), "colname20 first")
#'
#' @param vec Get the names(df) of data frame; vector.
#' @param movecommand a string with the name of the column followed by the move command.
#' @details "after", "before", "first", "last"; are possible as string commands for movement. The column names should be input as a vector; use names(df) command.
#'
#' @export
dfMoveCol <- function (vec, movecommand) {
	movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]],
								   ",|\\s+"), function(x) x[x != ""])
	movelist <- lapply(movecommand, function(x) {
		Where <- x[which(x %in% c("before", "after", "first",
								  "last")):length(x)]
		ToMove <- setdiff(x, Where)
		list(ToMove, Where)
	})
	myVec <- vec
	for (i in seq_along(movelist)) {
		temp <- setdiff(myVec, movelist[[i]][[1]])
		A <- movelist[[i]][[2]][1]
		if (A %in% c("before", "after")) {
			ba <- movelist[[i]][[2]][2]
			if (A == "before") {
				after <- match(ba, temp) - 1
			}
			else if (A == "after") {
				after <- match(ba, temp)
			}
		}
		else if (A == "first") {
			after <- 0
		}
		else if (A == "last") {
			after <- length(myVec)
		}
		myVec <- append(temp, values = movelist[[i]][[1]], after = after)
	}
	myVec
}
