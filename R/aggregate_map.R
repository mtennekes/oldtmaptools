first <- function(x, na.rm=FALSE) {
	if (!na.rm) x[1] else x[which(!is.na(x))[1]]
}

last <- function(x, n=length(x), na.rm=FALSE) {
	if (!na.rm) x[n] else x[tail(which(!is.na(x)), 1)]
}

modal <- function(x, na.rm=FALSE) {
	ux <- unique(x)
	if (na.rm) ux <- na.omit(ux)
	ux[which.max(tabulate(match(x, ux)))]
}

weighted.modal <- function(x, w, na.rm=FALSE) {
	isf <- is.factor(x)
	isn <- is.numeric(x)
	if (!isf) x <- as.factor(x)

	addNA <- !na.rm && any(is.na(x))

	mx <- max(tapply(w, x, sum), na.rm=TRUE)
	wmx <- which.max(tapply(w, x, sum))
	cat <- names(wmx)

	if (addNA) {
		nas <- sum(w[is.na(x)])
		if (nas>mx) cat <- NA
	}

	if (isf) {
		factor(cat, levels=levels(x))
	} else if (isn) {
		as.numeric(cat)
	} else cat
}

#' Aggregate map
#'
#' Aggregate spatial polygons, spatial lines or raster objects. For spatial polygons and lines, the units will be merged with the \code{by} variable. For rasters, the \code{fact} parameter determined how many rasters cells are aggregated both horizontally and vertically. Per data variable, an aggregation formula can be specified, by default mean for numeric and modal for categorical varaibles. Note that this function supports \code{sf} objects, but still uses sp-based methods (see details).
#'
#' This function is similar to \code{\link[raster:aggregate]{aggregate}} from the \code{raster} package. However, the aggregation can be specified in more detail: weights can be used (e.g. polygon area sizes). Also, an aggregation function can be specified per variable or raster layer. It is also possible to specify a general function for numeric data and a function for categorical data.
#'
#' By default, the data is not aggregated. In this case, this function is similar to \code{unionSpatialPolygons} from the \code{maptools} package. The only difference is way the aggregate-by variable is specified. When using \code{unionSpatialPolygons}, the values have to be assigned to \code{IDs} whereas when using \code{aggregate_map} the data variable name can be assigned to \code{by}.
#'
#' The underlying functions of \code{aggregate_map} for \code{\link[sp:sp]{sp}} objects are \code{\link[rgeos:gUnaryUnion]{gUnaryUnion}}, \code{\link[rgeos:gUnionCascaded]{gUnionCascaded}}, and \code{\link[rgeos:gLineMerge]{gLineMerge}}. For \code{Raster} objects, the \code{\link[raster:aggregate]{aggregate}} is used.
#'
#' This function supports \code{\link[sf:sf]{sf}} objects, but still uses sp-based methods, from the packages sp, rgeos, and/or rgdal. Alternatively, the \code{\link[sf:tidyverse]{tidyverse}} methods \code{group_by} and \code{summarize} can be used.
#'
#' @param shp shape object, which is one of
#' \enumerate{
#'  \item{\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialLinesDataFrame]{SpatialLines(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPixelsDataFrame]{SpatialPixels(DataFrame)}}}
#'  \item{\code{\link[raster:Raster-class]{RasterLayer, RasterStack, or RasterBrick}}}
#'  \item{\code{\link[sf:sf]{sf}} object if it can be coerced to an \code{\link[sp:sp]{sp}} object}
#' }
#' @param by variable by which polygons or lines are merged. Does not apply to raster objects.
#' @param fact number that specifies how many cells in both horizontal and vertical direction are merged. Only applied to raster objects.
#' @param agg.fun aggregation function(s). One of the following formats:
#' \enumerate{
#' \item{One function (name) by which all variables are aggregated.}
#' \item{A vector of two function names called \code{"num"} and \code{"cat"} that determine the functions by which numeric respectively categorical variables are aggregated. For instance \code{c(num="mean", cat="modal")}, which calculates the mean and mode for numeric respectively categorical variables.}
#' \item{A list where per variable the (names of the) function(s) are provided. The list names should correspond to the variable names.}
#' }
#' These predefined functions can be used: \code{"mean"}, \code{"modal"}, \code{"first"}, and \code{"last"}.
#' @param weights name of a numeric variable in \code{shp}. The values serve as weights for the aggregation function. If provided, these values are passed on as second argument. Works with aggregation functions \code{"mean"} and \code{"modal"}. Use \code{"AREA"} for polygon area sizes.
#' @param na.rm passed on to the aggregation function(s) \code{agg.fun}.
#' @param ... other arguments passed on to the aggregation function(s) \code{agg.fun}.
#' @import sp
#' @import sf
#' @importFrom stats weighted.mean
#' @importFrom raster raster extent rasterize couldBeLonLat crop projectRaster
#' @importFrom rgdal CRSargs make_EPSG checkCRSArgs getPROJ4VersionInfo
#' @importFrom rgeos gArea gIntersection
#' @importFrom units set_units as_units
#' @importFrom lwgeom st_transform_proj st_geod_area
#' @importFrom XML xmlChildren xmlRoot xmlAttrs xmlTreeParse xmlValue
#' @return A shape object, in the same format as \code{shp}
#' @example ./examples/aggregate_map.R
#' @export
aggregate_map <- function(shp, by=NULL, fact=NULL, agg.fun=NULL, weights=NULL, na.rm=FALSE, ...) {
	weighted.mean <- NULL

	agg.data <- !missing(agg.fun)

	is_f <- inherits(shp, c("sf", "sfc"))

	if (is_f) shp <- as(shp, "Spatial")

	# process aggregation functions
	is_raster <- (inherits(shp, c("Raster", "SpatialPixels", "SpatialGrid")))
	shpnms <- names(shp)

	if (!is.null(agg.fun)) {
		if (is.list(agg.fun)) {
			nms <- names(agg.fun)
			if (!(all(nms %in% shpnms))) stop("not all list names of agg.fun are shape variable names")
			aggmethod <- "list"
		} else if (is.character(agg.fun) && length(agg.fun)>1) {
			if (!setequal(names(agg.fun), c("num", "cat"))) stop("the names of agg.fun should be \"num\" and \"cat\"; please use a named list to specify functions per variable")
			aggmethod <- "numcat"
		} else {
			aggmethod <- "one"
		}
	} else {
		if (is_raster) stop("for raster shapes, agg.fun should be defined")
		aggmethod <- "none"
	}


	if (is_raster) {
		if (missing(fact)) stop("fact is missing")
		if (!missing(weights)) stop("weights are not used for raster shapes")

		# keep original class (to reconvert afterwards)
		cls <- class(shp)

		# determine levels and set to raster brick
		lvls <- get_raster_levels(shp)
		if (!inherits(shp, "RasterBrick")) shp <- brick(shp)

		# subset layers
		if (aggmethod == "list") {
			ids <- match(nms, shpnms)
			shp <- raster::subset(shp, ids, drop=FALSE)
			lvls <- lvls[ids]
		}

		# retrieve build-in functions
		get_function <- function(fun) {
			# predefined functions
			if (is.character(fun)) {
				# assign locally defined functions
				if (fun=="first") fun <- first
				else if (fun=="last")
					fun <- last
				else if (fun=="modal") fun <- modal
			}
			fun
		}

		# which layers are factors?
		isf <- !sapply(lvls, is.null)

		# aggregate raster
		if (aggmethod=="numcat") {

			if (any(isf)) {
				shp_cat <- raster::subset(shp, subset=which(isf), drop=FALSE)
				shp_cat2 <- raster::aggregate(shp_cat, fact=fact, fun=get_function(agg.fun["cat"]), na.rm=na.rm, ...)
			}

			if (any(!isf)) {
				shp_num <- raster::subset(shp, subset=which(!isf), drop=FALSE)
				shp_num2 <- raster::aggregate(shp_num, fact=fact, fun=get_function(agg.fun["num"]), na.rm=na.rm, ...)
			}

			if (all(isf)) {
				shp2 <- shp_cat2
			} else if (all(!isf)) {
				shp2 <- shp_num2
			} else {
				# restore order
				o <- order(c(which(!isf), which(isf)))
				rlayers <- c(lapply(1:nlayers(shp_num), function(i) raster(shp_num2, layer=i)),
							 lapply(1:nlayers(shp_cat), function(i) raster(shp_cat2, layer=i)))[o]
				shp2 <- do.call("brick", rlayers)
			}

		} else if (aggmethod=="list") {
			rlayers <- mapply(function(var, fun, i) {
				raster::aggregate(raster(shp, i), fact=fact, fun=get_function(fun), na.rm=na.rm, ...)
			}, names(agg.fun), agg.fun, 1L:length(agg.fun), SIMPLIFY = FALSE, USE.NAMES = FALSE)
			shp2 <- do.call("brick", rlayers)
		} else {
			# so aggmethod=="one"
			shp2 <- raster::aggregate(shp, fact=fact, fun=get_function(agg.fun), na.rm=na.rm, ...)
		}

		# reset the factor levels as the may have gone lost
		if (any(isf)) shp2 <- set_raster_levels(shp2, lvls)

		# reset original class and variable names
		if (cls!="RasterBrick") {
			shp2 <- as(shp2, cls)
			names(shp2) <- names(shp)
		}
		shp2
	} else {
	  if (inherits(shp, "SpatialPoints")) stop("SpatialPoints cannot be aggregated")
		if (missing(by)) stop("by is missing")
		if (!missing(fact)) warning("fact is only used for spatial grid or raster objects")

		if (!(by %in% names(shp))) stop(by, " is not an existing variable name")

		IDs_orig <- shp[[by]]

		IDs_char <- if (storage.mode(IDs_orig) != "character")
			as.character(IDs_orig)
		else IDs_orig

		IDs_fact <- if (is.factor(IDs_orig))
			IDs_orig
		else factor(IDs_orig, levels=unique(IDs_orig))

		lvls <- levels(IDs_fact)


		data <- attr(shp, "data")

		data[[by]] <- NULL

		shp2 <- if (inherits(shp, "SpatialLines")) {
		    rgeos::gLineMerge(spgeom = shp, byid=TRUE, id = IDs_char)
		} else if (rgeos::version_GEOS0() < "3.3.0") {
			rgeos::gUnionCascaded(spgeom = shp, id = IDs_char)
		} else rgeos::gUnaryUnion(spgeom = shp, id = IDs_char)

		# restore order
		ids2 <- get_IDs(shp2)

		# get selection of levels that are present
		lsel <- lvls %in% ids2


		shp2 <- shp2[match(lvls[lsel], ids2), ]

		if (agg.data) {
		  IDs2_char <- get_IDs(shp2)
		  IDs2_orig <- if (is.factor(IDs_orig)) factor(IDs2_char, levels=lvls) else IDs2_char

		  if (aggmethod=="none") {
		    data2 <- data.frame(IDs2_orig, stringsAsFactors = FALSE)
		    names(data2) <- by
		  } else {
		    #IDs2_fact <- factor(IDs2_char, levels=IDs2_char)

		    if (missing(weights)) {
		      w <- NULL
		    } else {
		      if (!is.character(weights) && !weights %in% names(data)) stop("weights should be a shape variable")
		      if (length(weights)>1) {
		        warning("only one variable can be used for weights")
		        weights <- weights[1]
		      }
		      if (weights=="AREA") {
		        w <- approx_areas(shp)
		      } else {
		        w <- shp[[weights[1]]]
		        if (!is.numeric(w)) stop("weights variable is not numeric")
		      }
		      if (any(is.na(w))) stop("weights variable contains missing values")
		      # normalize weights
		      w <- w / sum(w)
		    }


		    # retrieve build-in functions
		    get_function <- function(fun) {
		      if (!is.function(fun) && !is.character(fun)) stop("invalid function found in agg.fun")
		      if (!is.null(w) && is.character(fun)) {
		        # assign locally defined functions
		        if (fun=="mean") {
		          fun <- stats::weighted.mean
		        } else  if (fun=="modal") {
		          fun <- weighted.modal
		        }
		      } else if (identical(fun, mean)) {
		          fun <- stats::weighted.mean
		      }
		      fun
		    }

		    if (aggmethod=="numcat") {
		      isnum <- sapply(data, is.numeric)
		      agg.fun <- lapply(1:ncol(data), function(i) {
		        agg.fun[ifelse(isnum[i], "num", "cat")]
		      })
		      names(agg.fun) <- names(data)
		    } else if (aggmethod=="one") {
		        agg.fun <- lapply(1:ncol(data), function(i) {
		            agg.fun
		        })
		        names(agg.fun) <- names(data)
		    }

		    vars <- mapply(function(var, fun) {
		      dv <- data[[var]]
		      v <- if (is.null(w)) {
		        as.vector(tapply(X = dv, INDEX = IDs_fact, FUN = get_function(fun), na.rm=na.rm))[lsel]
		      } else {
		        dvs <- split(as.integer(dv), IDs_fact)
		        ws <- split(w, IDs_fact)
		        ws <- lapply(ws, function(wss) {
		            wss <- wss / sum(wss)
		        })
		        unlist(mapply(dvs, ws, FUN = get_function(fun), MoreArgs = c(list(na.rm=na.rm), list(...)), SIMPLIFY = FALSE, USE.NAMES = FALSE))[lsel]
		      }
		      if (is.factor(dv)) factor(v, levels=1L:nlevels(dv), labels=levels(dv)) else v
		    }, names(agg.fun), agg.fun, SIMPLIFY = FALSE, USE.NAMES = FALSE)

		    lst2 <- c(list(IDs2_orig), vars, list(FALSE))
		    names(lst2) <- c(by, names(agg.fun), "stringsAsFactors")

		    data2 <- do.call(data.frame, lst2)
		  }
		  shp2 <- append_data(shp2, data=data2, fixed.order=TRUE)
		}
		if (is_f) as(shp2, "sf") else shp2
	}
}
