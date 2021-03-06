#' Create a smooth map
#'
#' Create a smooth map from a shape object. A 2D kernel density estimator is applied to the shape, which can be a spatial points, polygons, or raster object. Various format are returned: a smooth raster, contour lines, and polygons. The covered area can be specified, i.e., the area outside of it is extracted from the output. Note that this function supports \code{sf} objects, but still uses sp-based methods (see details).
#'
#' For the estimation of the 2D kernal density, code is borrowed from \code{\link[KernSmooth:bkde2D]{bkde2D}}. This implemention is slightly different: \code{\link[KernSmooth:bkde2D]{bkde2D}} takes point coordinates and applies linear binning, whereas in this function, the data is already binned, with values 1 if the values of \code{var} are not missing and 0 if values of \code{var} are missing.
#'
#' This function supports \code{\link[sf:sf]{sf}} objects, but still uses sp-based methods, from the packages sp, rgeos, and/or rgdal.
#'
#' @param shp shape object of class \code{\link[sp:Spatial]{Spatial}}, \code{\link[raster:Raster-class]{Raster}}, or \code{\link[sf:sf]{sf}}. Spatial points, polygons, and grids are supported. Spatial lines are not.
#' @param var variable name. Not needed for \code{\link[sp:SpatialPoints]{SpatialPoints}}. If missing, the first variable name is taken. For polygons, the variable should contain densities, not absolute numbers.
#' @param nrow number of rows in the raster that is used to smooth the shape object. Only applicable if shp is not a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}} or \code{\link[raster:Raster-class]{Raster}}
#' @param ncol number of rows in the raster that is used to smooth the shape object. Only applicable if shp is not a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}} or \code{\link[raster:Raster-class]{Raster}}
#' @param N preferred number of points in the raster that is used to smooth the shape object. Only applicable if shp is not a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}} or \code{\link[raster:Raster-class]{Raster}}
#' @param unit unit specification. Needed when calculating density values. When set to \code{NA}, the densities values are based on the dimensions of the raster (defined by \code{nrow} and \code{ncol}). See also \code{unit.size}.
#' @param unit.size size of the unit in terms of coordinate units. The coordinate system of many projections is approximately in meters while thematic maps typically range many kilometers, so by default \code{unit="km"} and \code{unit.size=1000} (meaning 1 kilometer equals 1000 coordinate units).
#' @param smooth.raster logical that determines whether 2D kernel density smoothing is applied to the raster shape object. Not applicable when \code{shp} is a \code{\link[sp:SpatialPoints]{SpatialPoints}} object (since it already requires a 2D kernel density estimator). Other spatial objects are converted to a raster, which is smoothed when \code{smooth.raster=TRUE}.
#' @param nlevels preferred number of levels
#' @param style method to cut the color scale: e.g. "fixed", "equal", "pretty", "quantile", or "kmeans". See the details in \code{\link[classInt:classIntervals]{classIntervals}}.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param bandwidth single numeric value or vector of two numeric values that specifiy the bandwidth of the kernal density estimator. By default, it is 1/50th of the shortest side in units (specified with \code{unit.size}).
#' @param threshold threshold value when a 2D kernel density is applied. Density values below this threshold will be set to \code{NA}. Only applicable when \code{shp} is a \code{\link[sp:SpatialPoints]{SpatialPoints}} or \code{smooth.raster=TRUE}.
#' @param cover.type character value that specifies the type of raster cover, in other words, how the boundaries are specified. Options: \code{"original"} uses the same boundaries as \code{shp} (default for polygons), \code{"smooth"} calculates a smooth boundary based on the 2D kernal density (determined by \code{\link{smooth_raster_cover}}), \code{"rect"} uses the bounding box of \code{shp} as boundaries (default for spatial points and grids).
#' @param cover \code{\link[sp:SpatialPolygons]{SpatialPolygons}} shape that determines the covered area in which the contour lines are placed. If specified, \code{cover.type} is ignored.
#' @param cover.threshold numeric value between 0 and 1 that determines which part of the estimated 2D kernal density is returned as cover. Only applicable when \code{cover.type="smooth"}.
#' @param weight single number that specifies the weight of a single point. Only applicable if \code{shp} is a \code{\link[sp:SpatialPoints]{SpatialPoints}} object.
#' @param extracting.method Method of how coordinates are extracted from the kernel density polygons. Options are: \code{"full"} (default), \code{"grid"}, and \code{"single"}. See details. For the slowest method \code{"full"}, \code{\link[raster:extract]{extract}} is used. For \code{"grid"}, points on a grid layout are selected that intersect with the polygon. For \code{"simple"}, a simple point is generated with \code{\link[rgeos:gPointOnSurface]{gPointOnSurface}}.
#' @param buffer.width Buffer width of the iso lines to cut kernel density polygons. Should be small enough to let the polygons touch each other without space in between. However, too low values may cause geometric errors.
#' @param to.Raster not used anymore, since the "raster" output is always a \code{\link[raster:Raster-class]{RasterLayer}} as of version 2.0
#' @return List with the following items:
#' \describe{
#' \item{\code{"raster"}}{A smooth raster, which is either a \code{\link[sp:SpatialGridDataFrame]{SpatialGridDataFrame}} or a \code{\link[raster:Raster-class]{RasterLayer}} (see \code{to.Raster})}
#' \item{\code{"iso"}}{Contour lines, which is an \code{\link[sf:sf]{sf}} object of spatial lines.}
#' \item{\code{"polygons"}}{Kernel density polygons, which is an \code{\link[sf:sf]{sf}} object of spatial polygons}
#' \item{\code{"bbox"}}{Bounding box of the used raster}
#' \item{\code{"ncol"}}{Number of rows in the raster}
#' \item{\code{"nrow"}}{Number of columns in the raster}
#' }
#' @importFrom raster raster extent couldBeLonLat extract extend rasterToContour brick nlayers fromDisk colortable projectExtent
#' @importMethodsFrom raster as.vector
#' @importFrom rgeos gConvexHull gUnaryUnion gPointOnSurface gContains gIsValid gIntersection gArea gBuffer gDifference
#' @importFrom KernSmooth bkde2D
#' @importFrom grDevices contourLines colorRampPalette dev.off png col2rgb colors rgb
#' @importFrom methods as slot slotNames
#' @importFrom stats na.omit runif dnorm fft
#' @importFrom utils download.file head setTxtProgressBar tail txtProgressBar
#' @import RColorBrewer
#' @importFrom classInt classIntervals findCols
#' @example ./examples/smooth_map.R
#' @export
smooth_map <- function(shp, var=NULL, nrow=NA, ncol=NA, N=250000, unit="km", unit.size=1000, smooth.raster=TRUE, nlevels=5, style = ifelse(is.null(breaks), "pretty", "fixed"), breaks = NULL, bandwidth=NA, threshold=0, cover.type=NA, cover=NULL, cover.threshold=.6, weight=1, extracting.method="full", buffer.width=NA, to.Raster=NULL) {
    if (!missing(to.Raster)) warning("to.Raster is not used anymore, since the \"raster\" output is always a raster object as of version 2.0")

    is_sf <- inherits(shp, c("sf", "sfc"))
    if (is_sf) shp <- as(shp, "Spatial")

	bbx <- as.vector(bb(shp))
	prj <- get_projection(shp)
#	asp <- get_asp_ratio(shp)

	pb <- txtProgressBar(style=3)

	if (!inherits(shp, c("SpatialPoints", "SpatialPolygons", "SpatialGrid", "Raster"))) {
		stop("shp is not a Raster nor a SpatialPoints, -Polygons, or -Grid object")
	}

	if (!inherits(shp, c("SpatialPoints"))) {
		if (inherits(shp, "Spatial") && !("data" %in% slotNames(shp))) stop("No data found in shape.")
		if (missing(var)) var <- names(shp)[1]
	}


	## determine bounding box and grid size
	if (inherits(shp, c("SpatialPoints", "SpatialPolygons"))) {
		bbx <- as.vector(bb(bbx, ext=-1.05))
		shp@bbox <- matrix(bbx, ncol=2)
		asp <- get_asp_ratio(shp)
		if (is.na(nrow) || is.na(ncol)) {
			nrow <- round(sqrt(N/asp))
			ncol <- round(N / nrow)
		}

	} else {
		#if (inherits(shp, "Raster")) shp <- as(shp, "SpatialGridDataFrame")
		if (!inherits(shp, "RasterLayer")) shp <- raster(shp, layer=var)
		# if (inherits(shp, "SpatialGrid")) {
		# 	shp <- raster(shp, layer=var)
		# } else {
		# 	shp <- raster(shp, layer=var)
		# }
			# shp <- as(shp, "SpatialGridDataFrame")
		ncol <- ncol(shp)
		nrow <- nrow(shp)

		#ncol <- shp@grid@cells.dim[1]
		#nrow <- shp@grid@cells.dim[2]
	}
	N <- nrow * ncol

	# determine cell area (needed to calculate densities)
	if (!is_projected(shp) || is.na(unit)) {
		warning("shp is not projected; therefore density values cannot be calculated", call. = FALSE)
		cell.area <- 1
	} else {
		cell.width <- (bbx[3] - bbx[1]) / (unit.size * ncol)
		cell.height <- (bbx[4] - bbx[2]) / (unit.size * nrow)
		cell.area <- cell.width * cell.height
	}



	# edit bandwidth
	if (is.na(bandwidth[1])) {
		#bandwidth <- 3 * (bbx[,2] - bbx[,1]) / c(ncol, nrow)
		short_side <- min((bbx[3:4] - bbx[1:2]) / unit.size)
		bandwidth <- rep(short_side/100, 2)
	} else {
		# make sure bandwith is a vector of 2
		bandwidth <- rep(bandwidth, length.out=2)
	}

	# create an empty grid
	cover_r <- raster(extent(bbx[c(1,3,2,4)]), nrows=nrow, ncols=ncol, crs=prj)

	setTxtProgressBar(pb, .1)

	## process cover: fill the empty grid with T/F that indicates whether grid cells are inside or not
	if (is.na(cover.type)) cover.type <- ifelse(inherits(shp, "SpatialPolygons"), "original", "rect")
	if (missing(cover)) {

		if (cover.type=="rect") {
			cover <- as(extent(bbx[c(1,3,2,4)]), "SpatialPolygons")
			if (!is.na(prj)) cover <- as(set_projection(cover, current.projection = prj), "Spatial")
			cover_r[] <- TRUE
		} else if (cover.type=="original") {
			if (inherits(shp, "Raster")) {
				warning("cover.type=\"original\" only applied to raster output")
				cover <- as(extent(bbx[c(1,3,2,4)]), "SpatialPolygons")
				if (!is.na(prj)) cover <- as(set_projection(cover, current.projection = prj), "Spatial")

				cover_r <- shp

			} else {
				if (inherits(shp, "SpatialPoints")) {
					cover <- gConvexHull(shp)
				} else if (inherits(shp, "SpatialPolygons")) {
					cover <- gUnaryUnion(shp)
				}
				cover <- checknfix_sp(cover)
				cover@bbox <- matrix(bbx, ncol=2)
				cover_r <- poly_to_raster(cover, nrow = nrow, ncol = ncol)
			}
		}  else if (cover.type=="smooth") {
			if (!inherits(shp, "Raster")) stop("Raster shape required when cover.type=\"smooth\"")
			cover_list <- smooth_raster_cover(shp, var=var, bandwidth = bandwidth*unit.size, threshold = cover.threshold, output=c("raster", "polygons"))
			cover_r <- cover_list$raster
			cover_r[!cover_r[]] <- NA
			cover <- as(cover_list$polygons, "Spatial")
		}
	} else {
	    if (inherits(cover, c("sf", "sfc"))) cover <- as(cover, "Spatial")

		cover <- gUnaryUnion(cover)
		cover <- spTransform(cover, CRS(prj))

		cover <- checknfix_sp(cover)

		cover_r <- poly_to_raster(cover, nrow = nrow, ncol = ncol)
		bbc <- as.vector(bb(cover))
		bbx[1:2] <- pmin(bbx[1:2], bbc[1:2])
		bbx[3:4] <- pmin(bbx[3:4], bbc[3:4])
	}
	setTxtProgressBar(pb, .3)


	if (inherits(shp, "SpatialPoints")) {
		# for spatial points, use the 2d binned kernel density estimator from KernSmooth
		co <- coordinates(shp)
		x <- bkde2D(co, bandwidth=bandwidth*unit.size, gridsize=c(ncol, nrow), range.x=list(bbx[c(1,3)], bbx[c(2,4)]))

		var <- "count"
	} else {
		# otherwise, the shape is converted to a raster (if it isn't already)
		if (inherits(shp, "SpatialPolygons")){
			shp@data <- shp@data[,var, drop=FALSE]
			shp <- poly_to_raster(shp, nrow = nrow, ncol=ncol, copy.data = TRUE)
		}
		# get raster with selected variable
		# shpr <- raster(shp, layer=var)
		# browser()

		if (smooth.raster) {
			# apply 2d kernel density estimator, similar to bkde2D, but without binning (since this is already binned data)
			m <- as.matrix(shp)
			x <- kde2D(m, bandwidth = bandwidth*unit.size, gridsize=c(ncol, nrow), range.x=list(bbx[c(1,3)], bbx[c(2,4)]))

		} else {
			# copy raster (without 2d kernel density) and deterine levels
			r <- shp
			lvls <- num2breaks(as.vector(r[]), n=nlevels, style=style, breaks=breaks)$brks
		}
	}
	setTxtProgressBar(pb, .5)


	apply2kde <- inherits(shp, "SpatialPoints") || smooth.raster

	if (apply2kde) {
		# fill raster values
		r <- raster(extent(bbx[c(1,3,2,4)]), nrows=nrow, ncols=ncol, crs=prj)
		r[] <- as.vector(x$fhat[, ncol(x$fhat):1])
		names(r) <- var

		# apply cover
		r[is.na(cover_r[])] <- NA

		# normalize r and x$fhat
		if (inherits(shp, "SpatialPoints")) {
			norm_weight <- length(shp) * weight / sum(r[], na.rm=TRUE) / cell.area
		} else {
			norm_weight <- sum(shp[], na.rm=TRUE) / sum(r[], na.rm=TRUE)
		}
		r[] <- r[] * norm_weight #/ cell.area
		x$fhat <- x$fhat * norm_weight #/ cell.area

		#x$fhat[x$fhat < threshold] <- NA
#browser()

		lvls <- num2breaks(as.vector(x$fhat), n=nlevels, style=style, breaks=breaks)$brks
		#brks <- fancy_breaks(lvls, intervals=TRUE)

		# check if threshold can be applied (i.e. if values < threshold => NA)
		thresLevel <- (lvls[1]==0 && lvls[2] > threshold && threshold != 0)
		if (thresLevel) {
		    lvls_orig <- lvls
		    lvls <- c(lvls[1], threshold, lvls[-1])
		}

		cl <- contourLines(x$x1, x$x2, x$fhat, levels=lvls)
		if (length(cl) < 1L) {
		    warning("No iso lines found")
		    cl2 <- NULL
		} else {
		    if (length(cl) > 10000) stop(paste("Number of iso lines over 10000:", length(cl)))
		    cl2 <- contour_lines_to_SLDF(cl, proj4string = CRS(prj))
		    if (thresLevel) levels(cl2$level) <- c(0, levels(cl2$level)[-1])
		}

		#cl2$levelNR <- as.numeric(as.character(cl2$level))
	} else {
	    # no 2d kernel density has been applied. Instead contour lines are found from the original raster
	    thresLevel <- FALSE

		bbr <- as.vector(bb(r))

		# extend raster to prevent bleeding (due to isolines that do not reach the boundary)
		rxtra <- (floor(nrow(r) / 10) + 1) * 2
		cxtra <- (floor(ncol(r) / 10) + 1) * 2
		bbr2 <- as.vector(bb(bbr, width=(ncol(r)+cxtra)/ncol(r),
				   height=(nrow(r)+rxtra)/nrow(r),
				   relative=TRUE))

		r2 <- extend(r, extent(bbr2[c(1,3,2,4)]))
		r2[1:(rxtra/2),(cxtra/2+1):(ncol(r2)-cxtra/2)] <- r[1,]
		r2[(nrow(r2)-(rxtra/2)+1):nrow(r2),(cxtra/2+1):(ncol(r2)-cxtra/2)] <- r[nrow(r),]

		r2[,1:(cxtra/2)] <- r2[,cxtra/2+1]
		r2[,(ncol(r2)-cxtra/2+1):ncol(r2)] <- r2[,ncol(r2)-cxtra/2]

		cl2 <- rasterToContour(r2, maxpixels = length(r2), levels=lvls)
	}
	setTxtProgressBar(pb, .7)



	# make sure lines are inside poly
	cp <- suppressWarnings(lines2polygons(ply = cover, lns = cl2, rst = r, lvls=lvls, extracting.method="full", buffer.width = buffer.width))
	if (thresLevel) {
	    ids <- as.integer(cp$level)
	    ids[ids==1] <- NA
	    ids <- ids - 1L
	    cp$level <- factor(ids, levels=1:(length(lvls_orig)-1), labels=fancy_breaks(lvls_orig, intervals=TRUE), ordered = TRUE)
	}


	cp <- as(cp, "sf")

	attr(cp, "kernel_density") <- TRUE

	setTxtProgressBar(pb, .9)

	if (!is.null(cl2)) {
	    lns <- SpatialLinesDataFrame(suppressWarnings(gIntersection(cover, cl2, byid = TRUE)), data=cl2@data, match.ID = FALSE)
	    lns <- as(lns, "sf")
	    attr(lns, "isolines") <- TRUE
	} else {
	    lns <- NULL
	}

	setTxtProgressBar(pb, 1)

	if (apply2kde && thresLevel) r[][r[]<threshold] <- NA

	list(raster = r,
		 iso = lns,
		 polygons = cp,
		 bbox = bbx,
		 nrow = nrow,
		 ncol = ncol,
		 cell.area=cell.area,
		 bandwidth = bandwidth)
}

contour_lines_to_SLDF <- function (cL, proj4string = CRS(as.character(NA)))
{
	.contourLines2LineList <- function (cL)
	{
		n <- length(cL)
		res <- vector(mode = "list", length = n)
		for (i in 1:n) {
			crds <- cbind(cL[[i]][[2]], cL[[i]][[3]])
			res[[i]] <- Line(coords = crds)
		}
		res
	}
	cLstack <- tapply(1:length(cL), sapply(cL, function(x) x[[1]]),
					  function(x) x, simplify = FALSE)
	df <- data.frame(level = factor(names(cLstack), levels=names(cLstack), ordered=TRUE))
	m <- length(cLstack)
	res <- vector(mode = "list", length = m)
	IDs <- paste("C", 1:m, sep = "_")
	row.names(df) <- IDs
	for (i in 1:m) {
		res[[i]] <- Lines(.contourLines2LineList(cL[cLstack[[i]]]),
						  ID = IDs[i])
	}
	SL <- SpatialLines(res, proj4string = proj4string)
	SpatialLinesDataFrame(SL, data = df)
}


buffer_width <- function(bbx) {
	prod(bbx[3:4] - bbx[1:2]) / 1e12
}


lines2polygons <- function(ply, lns, rst=NULL, lvls, extracting.method="full", buffer.width=NA) {

    if (inherits(ply, c("sf", "sfc"))) ply <- as(ply, "Spatial")
    if (inherits(lns, c("sf", "sfc"))) lns <- as(lns, "Spatial")

	prj <- get_projection(ply)

	# add a little width to lines
	if (is.na(buffer.width)) buffer.width <- buffer_width(bb(ply))

	if (is.null(lns)) blpi <- NULL else suppressWarnings(blpi <- gBuffer(lns, width = buffer.width))
	suppressWarnings(ply <- gBuffer(ply, width = 0))

	# cut the poly with isolines
	dpi <- if (is.null(lns)) ply else gDifference(ply, blpi)

	dpi <- checknfix_sp(dpi)


	if (missing(rst)) {
		dpi
	} else {
		# place each polygon in different SpatialPolygon
		ps <- lapply(dpi@polygons[[1]]@Polygons, function(poly) {
			SpatialPolygons(list(Polygons(list(poly), ID = "1")), proj4string = CRS(prj))
		})

		# find holes
		holes <- sapply(dpi@polygons[[1]]@Polygons, function(poly) poly@hole)

		if (all(holes)) stop("All polygons are holes.")

		# create poly id (put each polygon in different feature, and append all holes)
		polyid <- cumsum(!holes)

		if (any(holes)) {
			ps_holes <- do.call("sbind", ps[holes])
			ps_solid <- do.call("sbind", ps[!holes])

			is_parent <- gContains(ps_solid, ps_holes, byid=TRUE)
			suppressWarnings(areas <- gArea(ps_solid, byid = TRUE))
			parents <- apply(is_parent, MARGIN=1, function(rw) {
				id <- which(rw)
				id[which.min(areas[id])]
			})
			parents <- which(!holes)[parents]

			polyid[holes] <- polyid[parents]
		}

		m <- max(polyid)

		dpi2 <- SpatialPolygons(lapply(1:m, function(i) {
			Polygons(dpi@polygons[[1]]@Polygons[which(polyid==i)], ID=i)
		}), proj4string = CRS(prj))

		if (extracting.method=="single") {
			pnts <- gPointOnSurface(dpi2, byid = TRUE)
			values <- extract(rst, pnts)
		} else if (extracting.method=="grid") {
			values <- sapply(1:m, function(i) {
				p <- dpi2[i,]
				rs <- as(raster(extent(p), nrows=10, ncols=10), "SpatialPoints")
				rs@proj4string <- CRS(prj)
				rs <- gIntersection(rs, p)
				if (is.null(rs)) rs <- gPointOnSurface(p) else {
					rs <- sbind(rs, gPointOnSurface(p))
				}
				mean(extract(rst, rs))
			})
		} else {
			# extracting.method=="full"
			values <- sapply(extract(rst, dpi2), function(x)if (is.null(x)) NA else mean(x, na.rm=TRUE))
		}


		if (length(lvls)==1) {
			lvls <- c(-Inf, lvls, Inf)
		}

		# just in case...
		values[is.na(values) | is.nan(values)] <- lvls[1]

		brks <- fancy_breaks(lvls, intervals=TRUE)

		ids <- cut(values, lvls, include.lowest=TRUE, right=FALSE, labels = FALSE)

		if (any(is.na(ids))) stop("raster values not in range")
		if (length(ids)==1) warning("Only one polygon created. Probably threshold value too low.")


		res <- lapply(1:(length(lvls)-1), function(i) {
			if (any(ids==i)) {
				s <- gUnaryUnion(dpi2[ids==i,])
				SpatialPolygonsDataFrame(s, data.frame(level=factor(brks[i], levels=brks, ordered = TRUE)), match.ID = FALSE)
			} else NULL
		})
		res <- res[!sapply(res, is.null)]

		x <- do.call("sbind", res)
	}
}

checknfix_sp <- function(x) {
    if (!suppressWarnings(gIsValid(x))) gBuffer(x, width = 0) else x
}

