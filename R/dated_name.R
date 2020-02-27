#' Time stamped string
#'
#' Function for generating a string preceeded by a time stamp: `YYYYMMDD_HHMMSS_filename`.
#' Takes a character input
#' @param name A name to concatenate as a suffix.
#' @export
dated_name <- function(name) {
	date_time <- Sys.time() %>% as.character() %>% str_replace(" ", "_") %>% str_replace_all(":", "-")
	date_time <- str_remove_all(date_time, "-") # Remove "-" from date names
	dated_name <- paste(date_time, name, sep = "_")

	return(dated_name)
}
