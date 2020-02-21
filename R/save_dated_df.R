#' A function for easily saving time stamped data frames.
#'
#' Takes one of two save functions, "excel" or "robject", data frame, and name
#' @param save_function A file format either "excel" or "robject"; must be class of character
#' @param df A data frame
#' @param name A name for the file
#' @export
save_dated_df <- function(save_function, df, name) {
	save_function_class <- class(save_function)
	name_class <- class(name)
	if(save_function != "character" && name_class != "character") {
		stop("Please use character types for save_function and name inputs.")
	}

	generate_dated_name <- function(name) {
		date_time <- Sys.time() %>% as.character() %>% str_replace(" ", "_") %>% str_replace_all(":", "-")
		date_time <- str_remove_all(date_time, "-")
		dated_name <- paste(date_time, name, sep = "_")

		return(dated_name)
	}

	if(save_function == "excel") {
		dated_excel_name <- generate_dated_name(name) %>% paste(".xlsx", sep = "")
		write.xlsx(df, file = dated_excel_name, colNames = TRUE, rowNames = TRUE, borders = "surrounding")
	} else if(save_function == "robject") {
		dated_robject_name <- generate_dated_name(name) %>% paste(".RData", sep = "")
		save(df, file = dated_robject_name)
	} else {
		message("Please select either \"excel\" or \"robject\" for a \"save_function\".")
	}
}
