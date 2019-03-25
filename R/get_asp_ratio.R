get_asp_ratio <- function(x, is.projected = NA, width=700, height=700, res=100) {
	if (inherits(x, "tmap")) {
		tmp <- tempfile(fileext = ".png")
		png(tmp, width=width, height=height, res = res)
		asp <- print(x, return.asp = TRUE, mode = "plot")
		dev.off()
	} else {
	    if (inherits(x, c("Spatial", "Raster", "sf", "sfc"))) {
	        bbx <- bb(x)
	        if (is.na(is.projected)) is.projected <- is_projected(x)
	    } else {
	        bbx <- bb(x)
	        if (is.na(is.projected)) is.projected <- !maybe_longlat(bbx)
	    }

	    xlim <- bbx[c(1,3)]
	    ylim <- bbx[c(2,4)]

	    asp <- if (diff(xlim)==0 || diff(ylim)==0) {
	        1
	    } else unname((diff(xlim)/diff(ylim)) * ifelse(is.projected, 1, cos((mean(ylim) * pi)/180)))
	}
	asp
}
