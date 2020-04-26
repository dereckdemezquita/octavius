#' Compare uneven data frames
#'
#' Compare two data frames of differing heights.
#'
#' Good for scoring two data frame of different heights; normally on obtains a truth table and can score the similarity between two data frames by using `df1 == df2`. When they are not of equal heights this is not possible.
#'
#' Here I used the `%in%` operator to check the similarity between two uneven data frames and return a pseudo score along with a pseudo truth table.
#'
#' Note that scores and truth tables returned are not guaranteed to be representative of the similarity of the two data frames input. This is because of the nature of the `%in%` operator.
#'
#' A sanitation check is done to prevent the input of two objects of different widths.
#'
#' @param net1 A data frame type object, of any height; must be equal in width to the second input.
#' @param net2 A data frame type object, of any size; must be equal in width to the first input.
#' @export
dfUnevenCheck <- function(net1, net2) {
	check <- dim(net1)[2] == dim(net2)[2]
	if(!check) {
		stop("There seems to be a corruption in the networks please check them manually; the number of columns are not equal.")
	}

	dots <- list(large = "", small = "")

	if(dim(net1)[1] == dim(net2)[1]) {
		message("Found that net1 input is equal to net2 input in size.")
		dots$large <- net1
		dots$small <- net2
	} else if (dim(net1)[1] > dim(net2)[1]) {
		message("Found that net1 input is larger than net2 inpu in size.")
		dots$large <- net1
		dots$small <- net2
	} else {
		message("Found that net2 input is larger than net1 input in size.")
		dots$large <- net2
		dots$small <- net1
	}
	dim(dots$large); dim(dots$small)

	# The bigger network should be used first to compare the smaller one
	# The order of the networks should have been done previously as per the first column
	df <- data.frame(matrix(NA, nrow = nrow(dots$large), ncol = ncol(dots$large)))
	falsies <- 0
	for (i in 1:ncol(dots$large)) {
		df[,i] <- dots$large[,i] %in% dots$small[,i]

		falsies = falsies + sum(!df[,i])
	}
	colnames(df) <- colnames(dots$large)

	score <- (falsies/(dim(dots$large)[1]*dim(dots$large)[2]))*100; score <- paste0(round(score, 4), " %")
	res <- list(table = df, score = score)

	message(glue::glue("There is a {score} difference between the two dot network files.\n A list object was returned, the score and a truth table comparing the networks."))
	invisible(res)
}
