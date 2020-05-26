#' Minify CARNIVAL network to list matches
#'
#' Takes a .dot network file and vector list of gene names, and matches the node pairs (origin target) to the list.
#'
#' Adds upstream interactions and downstream interactions to the matching list to get the largest subnetworks and all indirect interactions to the gene of interest.
#'
#' @param file A .dot network graph file from CARNIVAL.
#' @param matchList A vector list of gene names as strings to match for; reduce the network to sub networks.
#' @export
dotMinify <- function(file = "", matchList = "") {
	data <- read.csv(file, header = FALSE, sep = "-")
	colnames(data) <- c("origin", "target")

	top_init <- data[-which(data$target == ""), ]
	btm_init <- data[which(data$target == ""), ] %>% head(-1) %>% tail(-1)
	btm_init$target <- NULL

	map <- matchList %in% top_init$origin
	matchList <- c(matchList[map], matchList)

	map <- top_init$origin %in% matchList
	top <- top_init[map,]
	miniTop <- paste(top$origin, top$target, sep = "-")

	top_tmp <- separate(top_init, col = "target", into = c("target", "info"), sep = " \\[", remove = TRUE)
	top_tmp$target <- lapply(top_tmp$target, sub, pattern = ">", replacement = "") %>% as.character()

	map <- matchList %in% top_tmp$target
	matchList <- c(matchList[map], matchList)

	map <- top_tmp$target %in% matchList
	top_tmp <- top_tmp[map,]

	top_tmp <- na.omit(top_tmp)

	miniTop_tmp <- paste(top_tmp$origin, top_tmp$target, sep = "->") %>% as.data.frame()
	miniTopOrigTar <- paste(miniTop_tmp$`.`, top_tmp$info, sep = " [")

	btm <- separate(btm_init, col = "origin", into = c("origin", "info"), sep = " \\[", remove = TRUE)
	map <- btm$origin %in% matchList
	btm <- btm[map,]
	miniBtm <- paste(btm$origin, btm$info, sep = " [")

	miniDot <- c("digraph {", miniTop, miniTopOrigTar, miniBtm, "}")
	return(miniDot)
}
