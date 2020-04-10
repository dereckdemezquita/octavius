#' Parse .dot graph files to data frames
#'
#' A function for parsing .dot type files (specifically those produced by the saez/CARNIVAL package), into data frames.
#' Good for analysing and comparing results; in human readable formats or otherwise.
#' Takes a file path or name as a string; be sure to be in the correct working directory.
#' Will output two variables, `dot_data`, and `dot_meta` both concatenated with the raw `dim()[1]` size of the input; this is the number of lines in the file.
#' Will print names of output variables.
#' @param file A file path as a string.
#' @export
parseDot <- function(file) { # Will output two vars globally: dot_data, dot_meta; both concatenated with the dim size of the recovered file
	dot <- read.csv(file, header = FALSE, sep = "-")
	dim <- eval({dim(dot)[1]}) %>% as.character()

	dot_data_nm <- paste("dot_data", dim, sep = "_")
	dot_meta_nm <- paste("dot_meta", dim, sep = "_")

	colnames(dot) <- c("origin", "target")

	dot_data <- dot[-which(dot$target == ""), ]
	dot_meta <- dot[which(dot$target == ""), ] %>% head(-1) %>% tail(-1)
	dot_meta$target <- NULL

	dot_data_tmp <- dfRmvStr(dot_data, str1 = ">", str2 = " \\[", str3 =  "\\]", rep2 = ", ")

	dot_data <- separate(dot_data_tmp, col = "target", into = c("target", "penwidth", "colour", "arrow"), sep = ",", remove = FALSE) # Final data output
	dot_data <-dfRmvStr(dot_data, str1 = "penwidth=", str2 = "color=", str2 = "arrowhead=", rep1 = "", rep2 = "", rep3 = "")

	assign(dot_data_nm, value = dot_data, pos = 1) # Assign in the outer envir

	dot_meta_tmp <- dfRmvStr(dot_meta, str1 = " \\[", str2 = "\\];", rep1 = ", ", rep2 = "")

	dot_meta_all <- separate(dot_meta_tmp, col = "origin", into = c("gene", "style", "colour", "fill", "shape"), sep = ",", remove = TRUE)

	dot_meta_top <- dot_meta_all[rowSums(!is.na(dot_meta_all)) == 5,]
	dot_meta_btm <- dot_meta_all[rowSums(!is.na(dot_meta_all)) != 5,]

	dot_meta_btm[,4:5] <- NULL
	names(dot_meta_btm)[names(dot_meta_btm) == "colour"] <- "fill"

	dot_meta <- bind_rows(dot_meta_top[c("gene", "style", "colour", "fill", "shape")], dot_meta_btm[c("gene", "style", "fill")]) # Final meta data output
	dot_meta <- dfRmvStr(dot_meta, str1 = "style=", str2 = "color=", str2 = "fillcolor=", rep1 = "", rep2 = "", rep3 = "") %>% dfRmvStr(str1 = "shape=", rp1 = "")

	assign(dot_meta_nm, value = dot_meta, pos = 1) # Assign in the outer envir

	message(glue::glue("Succesfully created the following output variables: {dot_data_nm}, {dot_meta_nm}."))
}
