raster_colors <- function(x, use.colortable = FALSE) {

	n <- nrow(x)

	# get alpha transparency
	if (ncol(x)==4) {
		a <- x[,4]
		x <- x[,1:3]
	} else {
		a <- NULL
	}

	if (!use.colortable) {
		if (is.null(a)) {
			cols <- rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
		} else {
			cols <- rgb(x[,1], x[,2], x[,3], x[,4], maxColorValue = 255)
		}
		return(factor(cols))
	}


	storage.mode(x) <- "integer"
	v <- x[, 1] * 1e6L + x[, 2] * 1e3L + x[, 3]

	isna <- is.na(v)
	if (!is.null(a)) isna <- isna & (a==255)

	v <- v[!isna]
	u <- unique(v)
	nu <- length(u)
	m <- match(v, u)
	nc <- (min(256, nu))
	ta <- tabulate(m, nbins = nu)
	mo <- order(ta, decreasing = TRUE)
	ids <- mo[1:nc]

	r <- floor(u/1e6L)
	g <- floor((u-r*1e6L)/1e3L)
	b <- (u - r * 1e6L - g * 1e3L)
	rs <- r[ids]
	gs <- g[ids]
	bs <- b[ids]

	RGB <- cbind(r, g, b)
	RGBs <- cbind(rs, gs, bs)


	dists <- apply(RGBs, MARGIN = 1, function(rw) {
		sqrt((rw[1]-RGB[,1])^2 + (rw[2]-RGB[,2])^2 + (rw[3]-RGB[,3])^2)
	})

	ids2 <- apply(dists, MARGIN = 1, which.min)

	m2 <- ids2[m]

	ind <- integer(length=n)


	ind[!isna] <- m2
	ind[isna] <- NA

	cols <- rgb(rs, gs, bs, maxColorValue = 255)

	factor(ind, labels=cols)
}



extract_raster_data <- function(nm, isf, d, a){
	if (isf) {
		if (class(a)=="list") a <- a[[1]]
		id <- a$ID
		a$ID <- NULL
		a$COUNT <- NULL
		alist <- lapply(a, function(ai) {
			if (is.numeric(ai)) {
				ai[d]
			} else {
				factor(d, levels=id, labels=as.character(ai))
			}
		})
		as.data.frame(alist)
	} else {
		df <- data.frame(d)
		names(df) <- ifelse(nm == "", "layer", nm)
		df
	}
}

get_raster_layer_data <- function(rl) {
	extract_raster_data(nm = rl@data@names, isf = rl@data@isfactor, d = rl@data@values, a = rl@data@attributes)
}

get_raster_data <- function(shp, show.warnings = TRUE) {
	cls <- class(shp)
	if (fromDisk(shp)) {
		data <- raster::as.data.frame(shp)
		layerID <- 1L:ncol(data)
	} else if (inherits(shp, "RasterLayer")) {
		data <- get_raster_layer_data(shp)
		layerID <- rep(1L, ncol(data))
	} else if (inherits(shp, c("RasterStack", "RasterBrick"))) {

		if (inherits(shp, "RasterStack")) {
			datalayers <- lapply(shp@layers, get_raster_layer_data)
		} else {
			nms <- shp@data@names
			if (nms[1]=="") nms <- colnames(shp@data@values)

			nl <- length(nms)

			isfactor <- shp@data@isfactor

			data <- as.list(as.data.frame(shp@data@values))

			atb <- shp@data@attributes
			atb <- atb[vapply(atb, length, integer(1))!=0]

			stopifnot(sum(isfactor)==length(atb))

			atbList <- as.list(rep(NA, nl))
			atbList[isfactor] <- atb

			datalayers <- mapply(extract_raster_data, nms, isfactor, data, atbList, SIMPLIFY=FALSE)
		}

		ks <- vapply(datalayers, ncol, integer(1))
		data <- do.call(cbind, datalayers)
		if (any(duplicated(names(data)))) {
			names(data) <- paste(unname(unlist(mapply(function(x, y) {
				rep(x, each = y)
			}, names(shp), ks, SIMPLIFY=FALSE))), names(data), sep = ".")
			if (show.warnings) warning("Raster object contains duplicated variable names. Therefore, the variables have been internally renamed to ", paste(names(data), collapse = ", "))
		}
		layerID <- unlist(mapply(rep, 1L:length(datalayers), ks, SIMPLIFY = FALSE))
	}
	attr(data, "cls") <- cls
	attr(data, "layerID") <- layerID

	mainID <- vapply(1L:max(layerID), function(i) which(layerID==i)[1], FUN.VALUE = integer(1))
	attr(data, "mainID") <- mainID
	data
}


get_data_frame_levels <- function(data) {
	lapply(data, function(x) {
		if (is.factor(x)) {
			levels(x)
		} else if (is.numeric(x)) {
			range(x, na.rm = TRUE)
		} else if (is.logical(x)) {
			c(FALSE, TRUE)
		} else {
			sort(unique(x))
		}
	})
}


set_raster_levels <- function(shp, lvls) {
    isf <- !vapply(lvls, is.null, logical(1))
    cls <- class(shp)
    if (any(isf)) {
        shp@data@isfactor <- isf
        dfs <- mapply(function(nm, lv) {
            df <- data.frame(ID=1:length(lv), levels=factor(lv, levels=lv))
            if (cls=="RasterBrick") names(df)[2] <- nm
            df
        }, names(which(isf)), lvls[isf], SIMPLIFY=FALSE)
        shp@data@attributes <- dfs
    }
    shp
}

get_RasterLayer_levels <- function(r) {
    if (r@data@isfactor) {
        dt <- r@data@attributes[[1]]
        levelsID <- ncol(dt)
        as.character(dt[[levelsID]])
    } else {
        NULL
    }
}

get_raster_names <- function(shp) {
    nms <- names(shp)

    # overwrite unknown first names with FILE__VALUES
    if (inherits(shp, "RasterStack")) {
        if (shp@layers[[1]]@data@names[1]=="") nms[1] <- "FILE__VALUES"
    } else {
        if (shp@data@names[1]=="") nms[1] <- "FILE__VALUES"
    }
    nms
}

get_raster_levels <- function(shp, layerIDs) {
    if (missing(layerIDs)) layerIDs <- 1L:nlayers(shp)

    if (inherits(shp, "Spatial")) {
        return(lapply(attr(shp, "data")[,layerIDs], levels))
    }

    shpnames <- get_raster_names(shp)[layerIDs]
    if (inherits(shp, "RasterLayer")) {
        lvls <- list(get_RasterLayer_levels(shp))
    } else if (inherits(shp, "RasterStack")) {
        lvls <- lapply(shp@layers[layerIDs], get_RasterLayer_levels)
    } else if (inherits(shp, "RasterBrick")) {
        isfactor <- shp@data@isfactor
        if (all(!isfactor)) {
            lvls <- lapply(shpnames, function(sn) NULL)
        } else {
            atb <- shp@data@attributes
            atb <- atb[vapply(atb, length, integer(1))!=0]
            stopifnot(sum(isfactor)==length(atb))
            isfactor2 <- isfactor[layerIDs]

            lvls <- rep(list(NULL), length(layerIDs))
            if (any(isfactor2)) {
                atb2 <- atb[match(layerIDs[isfactor2], which(isfactor))]

                lvls[isfactor2] <- lapply(atb2, function(a) {
                    if (class(a)=="list") a <- a[[1]]
                    levelsID <- ncol(a) # levels is always the last column of the attributes data.frame (?)
                    as.character(a[[levelsID]])
                })
            }
        }
    }
    names(lvls) <- shpnames
    lvls
}
