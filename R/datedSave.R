#' Save time stamped file
#'
#' A function for easily saving time stamped objects, and data frames.
#' Takes a variable r-object, one of the following save functions, "txt, "csv", "excel" or "robject", data frame or r-objects and then followed by a character name.
#' @param save_function A file format either "excel" or "robject"; must be class of character.
#' @param input A data frame.
#' @param name A name for the file.
#' @export
datedSave <- function(input, save_function, name) {
	save_function_class <- class(save_function)
	name_class <- class(name)
	if(save_function != "character" && name_class != "character") {
		stop("Please use character types for save_function and name inputs.")
	}

	dated_name <- datedName(name)

	print(glue::glue("You saved {dated_name}"))

	if(save_function == "txt") {
		dated_txt_name <- dated_name %>% paste0(".txt")
		write.table(input, file = dated_txt_name, quote = F, row.names = F, col.names = F)
	} else if(save_function == "csv") {
		dated_csv_name <- dated_name %>% paste0(".csv")
		write.csv(input, file = dated_csv_name)
	} else if(save_function == "excel") {
		dated_excel_name <- dated_name %>% paste0(".xlsx")
		write.xlsx(input, file = dated_excel_name, colNames = TRUE, rowNames = TRUE)
	} else if(save_function == "robject") {
		dated_robject_name <- dated_name %>% paste0(".RData")
		save(input, file = dated_robject_name)
	} else {
		message("Please select either \"txt\", \"csv\", \"excel\", or \"robject\" for a \"save_function\".")
	}
}
