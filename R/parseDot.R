#' Parse .dot graph files to data frames
#'
#' A function for parsing .dot type files (specifically those produced by the saez/CARNIVAL package), into data frames.
#'
#' Good for analysing and comparing results; in human readable formats or otherwise.
#'
#' Takes a file path or name as a string; be sure to be in the correct working directory.
#' Sets and instatiates a class, returns to variable; use `@` to access the sets relating to this functions results.
#'
#' @param file A file path as a string.
#' @export
parseDot <- function(file) {
	data <- read.csv(file, header = FALSE, sep = "-")
	colnames(data) <- c("origin", "target")

	.PreprocessorCache <- new.env()

	setClass(Class = "dotInfo",
			 where = .PreprocessorCache,
			 slots = c(data = "data.frame", meta = "data.frame"))

	dotInfo <- new("dotInfo")

	dotInfo@data <- data[-which(data$target == ""), ]
	dotInfo@meta <- data[which(data$target == ""), ] %>% head(-1) %>% tail(-1)
	dotInfo@meta$target <- NULL


	data_tmp <- dfRmvStr(dotInfo@data, str1 = ">", str2 = " \\[", str3 =  "\\]", rep2 = ", ")
	data_tmp <- dfRmvStr(data_tmp, str1 = " penwidth=", str2 = " color=", str3 = " arrowhead=")

	dotInfo@data <- separate(data_tmp, col = "target", into = c("target", "penwidth", "colour", "arrow"), sep = ",", remove = FALSE) # Final data output



	meta_tmp <- dfRmvStr(dotInfo@meta, str1 = " \\[", str2 = "\\];", rep1 = ", ", rep2 = "")
	meta_tmp <- dfRmvStr(meta_tmp, str1 = " style=", str2 = " color=", str3 = " fillcolor=") %>% dfRmvStr(str1 = " shape=")

	meta_all <- separate(meta_tmp, col = "origin", into = c("gene", "style", "colour", "fill", "shape"), sep = ",", remove = TRUE)

	meta_top <- meta_all[rowSums(!is.na(meta_all)) == 5,]
	meta_btm <- meta_all[rowSums(!is.na(meta_all)) != 5,]

	meta_btm[,4:5] <- NULL
	names(meta_btm)[names(meta_btm) == "colour"] <- "fill"

	dotInfo@meta <- bind_rows(meta_top[c("gene", "style", "colour", "fill", "shape")], meta_btm[c("gene", "style", "fill")]) # Final meta data output

	message(glue::glue("Instantiated as class parts. Use @ to access."))
	print(head(dotInfo@data)); print(head(dotInfo@meta))
	return(dotInfo)
}
