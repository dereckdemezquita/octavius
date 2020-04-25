#' Knit .Rhtml launch localhost; show in viewer
#'
#' This function Knits an .Rhtml file to .html, within the current working directory of the working file, if no wd specified. It gets this working directory by the `getFileWd()` function of `octavius`.
#'
#' A localhost server is launched on `port: 6969`; port forward to make this server externally accessible: `http://localhost:6969`
#'
#' A nice feature is to set this function to `cmd + shift + s` as this could have it knit and preview automatically on save.
#'
#' Finally the local viewer (R-studio's) is launched; note that the default preview window for knitting .Rhtml is not launched.
#' @param save set to save the current working file by default; this easily allows for you to set this function to a hotkey.
#' @param input set to "index.Rhtml" by default.
#' @param output set to "index.html" by default.
#' @param clear_cache set to TRUE by default.
#' @param wd set to "" by default, if left will recover the wd of the current working file.
#' @param port set to "6969" by default, allows you to set the port on localhost.
#' @export
servrKnitRhtml <- function(save = TRUE, input = "index.Rhtml", clear_cache = TRUE, wd = "", port = "6969") {
	if(input != "index.Rhtml") {
		output <- strsplit(input, split = ".", fixed = TRUE)[[1]][1];
		output <- glue::glue("{output}.html");
	} else {
		output <- "index.html";
		output <- glue::glue("{output}.html");
	};
	if(save == TRUE) {
		rstudioapi::documentSave();
	};
	if(nchar(wd) == 0) {
		wd <- getFileWd()
	};

	servr::daemon_stop(which = daemon_list()); # Kills all currently open servers

	knitr::clean_cache(clean = clear_cache); # Will clear cache by default
	knitr::knit(
		input = input,
		output = output,
		tangle = FALSE,
		text = NULL,
		quiet = TRUE,
		envir = parent.frame(),
		encoding = "UTF-8");

	localhost <- glue::glue("http://localhost:{port}")
	servr::httw(dir = wd, # Launches server on: http://localhost:6969 default
				watch = wd,
				pattern = output,
				all_files = TRUE,
				port = port);

	rstudioapi::viewer(glue::glue("{localhost}/{output}"));

	message(glue::glue("File exported to: {output},\n Server launched on: {localhost}\n Current wd set to: {wd}."))
}
