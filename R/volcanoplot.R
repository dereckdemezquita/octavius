#' Volcano plot
#'
#' Creates simple volcano plot based on base R graphics. Highly customisable and allows for deeper arguments.
#'
#' Takes a minimal data frame; col-1 log2FoldChange, col-2 padj. Padj can contain NAs these will be removed by internal function.
#'
#' Note that the function ALSO returns an object with the differentially expressed genes, rownames are genes, column one is log2FoldChange and column two is the padj values. Simply set the function to a variable to save the object.
#'
#' @export
volcanoplot <- function(df, pcutoff = 0.05, lcutoff = 1, nlabs = 10, xlim = NULL, ylim = NULL, ptype = 20, psize = 1, main = "Volcano plot\n Cutoffs: padj < 0.05, abs(lg2fc) > 1", sub = "", ldispmethod = "SANN", lcex = 1, lpos = 2, loffset = 0.75, xnsig = 0, ynsig = -10, nsigcex = 1) {
	pcutoff_init <- pcutoff
	pcutoff <- log10(pcutoff)

	df <- dfRmvNA(df, col = "padj")

	### define interesting genes
	top <- octavius::topSigExtract(df)
	top <- head(top, nlabs)
	interesting_genes <- rownames(top)

	### define colours (grey, blue, red, green)
	palette <- c(
		rgb(0, 0, 0, max = 255, alpha = 65),
		rgb(0, 0, 255, max = 255, alpha = 125),
		rgb(255, 0, 0, max = 255, alpha = 125),
		rgb(0, 255, 0, max = 255, alpha = 175)
	)

	point_type = ptype
	point_size = psize


	fold_changes <- df[,1]
	pvalues <- log10(df[,2])

	fc_interesting_genes <- top[,1]
	pval_interesting_genes <- log10(top[,2])

	# find deferentially expressed genes to colour
	left_biased <- fold_changes < -lcutoff
	right_biased <- fold_changes > lcutoff
	top_biased <- pvalues < pcutoff

	### plot corpus
	plot(
		fold_changes,
		pvalues,
		pch = point_type,
		cex = point_size,
		col = palette[1],
		xlab = "Fold Change (log2)",
		ylab = "log10(padj)",
		xlim = xlim,
		ylim = ylim,
		yaxt = "n",
		main = main,
		sub = sub
	)
	aty <- axTicks(2)
	labels <- sapply(aty, function(i)
		as.expression(bquote(10 ^ .(i))))
	axis(2, at = aty, labels = labels)

	abline(v = lcutoff, lwd = 3, col = palette[3], lty = 2)
	abline(v = -lcutoff, lwd = 3, col = palette[2], lty = 2)
	abline(h = pcutoff, lwd = 3, col = palette[4], lty = 2)


	### plot deferentially expressed genes in colour + genes on interest with black
	points(fold_changes[left_biased], pvalues[left_biased],
		   pch = point_type, cex = point_size, col = palette[2])
	points(fold_changes[right_biased], pvalues[right_biased],
		   pch = point_type, cex = point_size, col = palette[3])

	points(fold_changes[left_biased & top_biased],
		   pvalues[left_biased & top_biased], pch = point_type, cex = point_size, col = palette[4])
	points(fold_changes[right_biased & top_biased],
		   pvalues[right_biased & top_biased], pch = point_type, cex = point_size, col = palette[4])

	points(fc_interesting_genes, pval_interesting_genes, pch = point_type, cex = point_size, col = "purple")

	# n <- octavius::topSigExtract(df, alpha = 0.05)
	n <- filter(df, abs(log2FoldChange) > lcutoff & padj < pcutoff_init)
	text(xnsig, ynsig, glue::glue("{dim(n)[1]}: pval < {pcutoff_init}\n && lg2fc > {lcutoff}\n (green/purple)"), xpd = NA, cex = nsigcex)

	# library("maptools")
	# library("basicPlotteR")
	# https://blog.revolutionanalytics.com/2009/05/make-text-stand-out-with-outlines.html
	shadowtext <- function(x, y = NULL, labels, col = "black", bg = "white",
						   theta = seq(pi/4, 2*pi, length.out = 8), r = 0.00000001, ...) {
		xy <- xy.coords(x, y)
		xo <- r * strwidth("A")
		yo <- r * strheight("A")

		for (i in theta) {
			pointLabel(
				xy$x + cos(i) * xo,
				xy$y + sin(i) * yo,
				labels = labels,
				cex = lcex,
				method = ldispmethod,
				pos = lpos,
				# offset = loffset,
				xpd = NA,
				col = bg
			)
			# text(xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, pos = lpos, offset = loffset)
		}
		# text(xy$x, xy$y, labels, col=col, pos = lpos, offset = loffset)
		pointLabel(
			xy$x,
			xy$y,
			labels = labels,
			cex = lcex,
			method = ldispmethod,
			pos = lpos,
			# offset = loffset,
			xpd = NA,
			col = col
		)
	}
	# shadowtext(fc_interesting_genes,pval_interesting_genes, interesting_genes, col = "black")

	pointLabel(
		fc_interesting_genes,
		pval_interesting_genes,
		labels = interesting_genes,
		cex = lcex,
		method = ldispmethod,
		pos = lpos,
		offset = loffset,
		xpd = NA
	)
	message("Returns siginificant genes as per set lcutoff and pcutoff; save to variable.")
	return(n)
}

