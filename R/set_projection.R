set_projection <- function(shp, projection=NA, current.projection=NA, overwrite.current.projection=FALSE) {
	shp.name <- deparse(substitute(shp))

	cls <- class(shp)
	is_sp <- inherits(shp, "Spatial")
	is_sp_raster <- inherits(shp, c("SpatialGrid", "SpatialPixels"))
	if (is_sp) shp <- (if (is_sp_raster) brick(shp) else as(shp, "sf"))

	shp.crs <- get_projection(shp, output="crs")
	current.crs <- get_proj4(current.projection, output = "crs")
	proj.crs <- get_proj4(projection, output = "crs")

	if (is.na(shp.crs)) {
		if (is.na(current.crs)) {
			stop("Currect projection of shape object unknown. Please specify the argument current.projection. The value \"longlat\", which stands for Longitude-latitude (WGS84), is most commonly used.")
		} else {
			if (inherits(shp, "sf")) {
				st_crs(shp) <- current.crs
			} else {
				shp@crs <- get_proj4(current.crs, output = "CRS")
			}
			#current.projection <- current.proj4
		}
	} else {
		if (!is.na(current.crs)) {
			if (identical(current.crs, shp.crs)) {
				warning("Current projection of ", shp.name, " already known.", call. = FALSE)
			} else {
				if (overwrite.current.projection) {
					warning("Current projection of ", shp.name, " differs from ", current.crs$proj4string, ", but is overwritten.", call. = FALSE)
					if (inherits(shp, "sf")) {
					    st_crs(shp) <- current.crs
					} else {
						shp@crs <- get_proj4(current.crs, output = "CRS")
					}

				} else {
					stop(shp.name, " already has projection: ", shp.crs$proj4string, ". This is different from the specified current projection ", current.crs$proj4string, ". If the specified projection is correct, use overwrite.current.projection=TRUE.", call. = FALSE)
				}
			}
		} else {
			current.crs <- shp.crs
		}
	}


	if (!is.na(proj.crs)) {
		PROJ4_version_nr <- get_proj4_version()

		if (length(grep("+proj=wintri", current.crs$proj4string, fixed = TRUE)) && PROJ4_version_nr < 491) {
			stop("Unable to reproject a shape from the Winkel Tripel projection with PROJ.4 version < 4.9.1")
		}


		if (inherits(shp, "Raster")) {
		    proj.CRS <- get_proj4(proj.crs, output = "CRS")

			#raster_data <- get_raster_data(shp)
			has_color_table <- (length(colortable(shp))>0)

			# get factor levels (to be restored later)
			lvls <- get_raster_levels(shp, 1:nlayers(shp))
			# override factor levels with colortable values
			if (has_color_table) {
				lvls <- list(colortable(shp))
#				raster_data <- data.frame(PIXEL__COLOR=getValues(shp[[1]])+1L)
			}
			isnum <- sapply(lvls, is.null)
			new_ext <- suppressWarnings(projectExtent(shp, crs = proj.CRS))
			if (any(isnum) && !all(isnum)) {
				shp_num <- raster::subset(shp, subset=which(isnum))
				shp_cat <- raster::subset(shp, subset=which(!isnum))
				shp_num2 <- suppressWarnings(projectRaster(shp_num, to=new_ext, crs=proj.CRS, method="bilinear"))
				shp_cat2 <- suppressWarnings(projectRaster(shp_cat, to=new_ext, crs=proj.CRS, method="ngb"))

				# restore order
				o <- order(c(which(isnum), which(!isnum)))
				rlayers <- c(lapply(1:nlayers(shp_num), function(i) raster(shp_num2, layer=i)),
							 lapply(1:nlayers(shp_cat), function(i) raster(shp_cat2, layer=i)))[o]
				shp <- do.call("brick", rlayers)
			} else if (all(isnum)) {
				shp <- suppressWarnings(projectRaster(shp, to=new_ext, crs=proj.CRS, method="bilinear"))
			} else {
				shp <- suppressWarnings(projectRaster(shp, to=new_ext, crs=proj.CRS, method="ngb"))
			}

			# new_raster_data <- as.data.frame(mapply(function(d, l) {
			# 	if (!is.null(l) && !is.factor(d)) factor(d, levels=1:length(l), labels=l) else d
			# }, get_raster_data(shp), lvls, SIMPLIFY=FALSE))

			if (any(!isnum)) shp <- set_raster_levels(shp, lvls)
			# shp@data@isfactor <- !isnum
			# dfs <- mapply(function(nm, lv) {
			# 	df <- data.frame(ID=1:length(lv), levels=factor(lv, levels=lv))
			# 	if (cls=="RasterBrick") names(df)[2] <- nm
			# 	df
			# }, names(which(!isnum)), lvls[!isnum], SIMPLIFY=FALSE)
			# shp@data@attributes <- dfs
		} else {
			shp <- st_transform2(shp, proj.crs)
		}
		if (is_sp_raster) {
			shp <- as(shp, cls)
			names(shp) <- names(isnum)
		}
	}

	shp

	#if (is_sp && !is_sp_raster) as(shp, cls) else shp
}

st_transform2 <- function(x, crs, ...) {
    args <- list(...)
    y <- tryCatch(do.call(sf::st_transform, c(list(x=x, crs=crs), args)),
             error = function(e) NULL,
             warning = function(w) NULL)
    if (is.null(y)) {
        y <- tryCatch(do.call(lwgeom::st_transform_proj, c(list(x=x, crs=crs), args)),
                 error = function(e) {
                      stop("Unable to set the projection to ", crs$proj4string, ".", call. = FALSE)
                 } )
    }
    y
}






get_projection <- function(shp, guess.longlat=FALSE,
                           output = c("character", "crs", "epsg", "CRS")) {
    p <- if (inherits(shp, c("sf", "sfc"))) {
        st_crs(shp)
    } else if (inherits(shp, "Spatial")) {
        st_crs(attr(attr(shp, "proj4string"), "projargs"))
    } else if (inherits(shp, "Raster")) {
        st_crs(attr(attr(shp, "crs"), "projargs"))
    } else {
        stop("shp is neither a sf, sp, nor a raster object")
    }

    output <- match.arg(output)

    switch(output,
           character = p$proj4string,
           crs = p,
           epsg = p$epsg,
           CRS = CRS(ifelse(is.na(p$proj4string), "", p$proj4string))
    )
}


