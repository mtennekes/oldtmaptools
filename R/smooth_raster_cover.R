#' Get a smoothed cover of a raster object
#'
#' Get a smoothed cover of a raster object. From all non-missing values of a raster object, a 2D kernal density is applied. The output is a \code{sf} object of spatial polygons. Used by \code{\link{smooth_map}}. Note that this function supports \code{sf} objects, but still uses sp-based methods (see details). Note that this function supports \code{sf} objects, but still uses sp-based methods (see details).
#'
#' For the estimation of the 2D kernal density, code is borrowed from \code{\link[KernSmooth:bkde2D]{bkde2D}}. This implemention is slightly different: \code{\link[KernSmooth:bkde2D]{bkde2D}} takes point coordinates and applies linear binning, whereas in this function, the data is already binned, with values 1 if the values of \code{var} are not missing and 0 if values of \code{var} are missing.
#'
#' This function supports \code{\link[sf:sf]{sf}} objects, but still uses sp-based methods, from the packages sp, rgeos, and/or rgdal.
#'
#' @param shp raster object, from either \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}} or \code{\link[raster:Raster-class]{Raster}} class.
#' @param var name of the variable from which missing values are flagged. If unspecified, the first variable will be taken.
#' @param bandwidth single numeric value or vector of two numeric values that specifiy the bandwidth of the kernal density estimator. See details.
#' @param threshold numeric value between 0 and 1 that determines which part of the estimated 2D kernal density is returned as cover.
#' @param output class of the returned object. One of: \code{polygons} (\code{\link[sf:sf]{sf}} object), \code{lines} (\code{sf} object), or a \code{\link[raster:Raster-class]{raster}}. A vector of class names results in a list of output objects.
#' @importFrom raster raster extent
#' @importMethodsFrom raster as.matrix
#' @export
smooth_raster_cover <- function(shp, var=NULL, bandwidth=NA, threshold=.6, output="polygons") {

	# convert to rasterlayer
	if (!inherits(shp, "RasterLayer")) {
		if (missing(var)) var <- names(shp)[1]
		shp <- raster(shp, layer=var)
	}
	ncol <- ncol(shp)
	nrow <- nrow(shp)


	# get shape properties
	bbx <- bb(shp)
	prj <- get_projection(shp)

	if (is.na(bandwidth[1])) {
		bandwidth <- 3 * (bbx[3:4] - bbx[1:2]) / c(ncol, nrow)
	} else {
		# make sure bandwith is a vector of 2
		bandwidth <- rep(bandwidth, length.out=2)
	}

	## find non-NA areas
	shp$N__NA <- !is.na(shp[])
	m_nna <- as.matrix(raster(shp, layer="N__NA"))
	m_nna <- matrix(as.integer(m_nna), ncol = ncol(m_nna))
	x_nna <- kde2D(m_nna, bandwidth = bandwidth, gridsize=c(ncol, nrow), range.x=list(bbx[c(1,3)], bbx[c(2,4)]))

	# normalize results
	x_nna$fhat <- x_nna$fhat * (sum(shp$N__NA[]) / sum(x_nna$fhat, na.rm=TRUE))

	# append to shape
	shp$NNA__VALUES <- as.vector(x_nna$fhat[, ncol(x_nna$fhat):1])

	# find contour lines with one level (at threshold) and convert to spatial polygons
	cl_nna <- grDevices::contourLines(x_nna$x1, x_nna$x2, x_nna$fhat, levels=threshold)
	if (!length(cl_nna)) stop("No contour lines are found. Try to decrease the bandwidth.")
	cl2_nna <- contour_lines_to_SLDF(cl_nna, proj4string = CRS(prj))



	rect <- as(extent(bbx[c(1,3,2,4)]), "SpatialPolygons")
	rect <- as(set_projection(rect, current.projection = prj), "Spatial")

	cp_nna <- lines2polygons(ply = rect, lns = cl2_nna, rst = raster(shp, "NNA__VALUES"), lvls = threshold)[2,]

	### OUTPUTS

	# Lines
	SL <- st_as_sf(as(cl2_nna, "SpatialLines"))

	# SpatialPolygons
	SP <- st_as_sf(as(cp_nna, "SpatialPolygons"))

	# SpatialGridDataFrame
	SG <- as(shp$NNA__VALUES, "SpatialGridDataFrame")
	SG@data <- data.frame(cover=SG$NNA__VALUES >= threshold, drop=FALSE)

	# RasterLayer
	RL <- raster(SG, layer="cover")

	if (length(output)==1) {
		switch(output,
			   lines=SL,
			   polygons=SP,
			   raster=RL)
	} else {
		names(output) <- output
		lapply(output, function(out) {
			switch(out,
				   lines=SL,
				   polygons=SP,
				   raster=RL)
		})
	}
}
