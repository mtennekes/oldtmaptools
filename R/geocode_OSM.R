geocode_OSM <- function(q, projection=NULL, return.first.only=TRUE, details=FALSE, as.data.frame=NA, as.sf=FALSE, server="http://nominatim.openstreetmap.org") {
	n <- length(q)
	q2 <- gsub(" ", "+", enc2utf8(q), fixed = TRUE)
	addr <- paste0(server, "/search?q=", q2, "&format=xml&polygon=0&addressdetails=0")

	project <- !missing(projection)
	if (project) projection <- get_proj4(projection, output = "crs")


	if (is.na(as.data.frame)) as.data.frame <- (n>1)
	if (as.sf) {
		as.data.frame <- TRUE
		return.first.only <- TRUE
	}


	output2 <- lapply(1:n, function(k) {
		tmpfile <- tempfile()
		suppressWarnings(download.file(addr[k], destfile = tmpfile, mode= "wb", quiet = TRUE))

		doc <- xmlTreeParse(tmpfile, encoding="UTF-8")
		unlink(tmpfile)

		res <- xmlChildren(xmlRoot(doc))

		if (length(res)==0) {
			warning(paste("No results found for \"", q[k], "\".", sep="")) #if (n==1)
			return(NULL)
		}

		idx <- if (return.first.only) 1 else 1:length(res)

		sn_names <- c("place_id", "osm_type", "osm_id", "place_rank", "display_name", "class", "type", "importance", "icon")
		output <- lapply(idx, function(i) {
			search_result <- xmlAttrs(res[[i]])

			search_result_id <- search_result[sn_names]
			names(search_result_id) <- sn_names # in case of missings
			Encoding(search_result_id) <- "UTF-8"

			search_result_loc <- as.numeric(search_result[c("lat", "lon")])
			names(search_result_loc) <- c("lat", "lon")

			search_result_bb <- as.numeric(unlist(strsplit(search_result["boundingbox"], ",")))

			if (!project) {
				names(search_result_bb) <- c("lat_min", "lat_max", "lon_min", "lon_max")
				b <- bb(xlim=search_result_bb[3:4], ylim=search_result_bb[1:2])

				coords <- search_result_loc[c("lon", "lat")]
				names(coords) <- c("x", "y")

			} else {
				b <- bb(xlim=search_result_bb[3:4], ylim=search_result_bb[1:2], current.projection = .CRS_longlat, projection=projection)

				search_result_bb <- b[c(2,4,1,3)]
				names(search_result_bb) <- c("y_min", "y_max", "x_min", "x_max")

                p <- st_sf(st_sfc(st_point(search_result_loc[2:1]), crs = .crs_longlat))

				p <- set_projection(p, projection=projection)

				coords <- as.vector(st_coordinates(p))
				names(coords) <- c("x", "y")

				search_result_loc <- as.list(coords)
				names(search_result_loc) <- c("x", "y")
			}

			res <- if (as.data.frame) {
				c(list(query=q[k]),
				  search_result_loc,
				  search_result_bb)
			} else {
				c(list(query=q[k],
					   coords=coords,
					   bbox=b))
			}

			if (details) res <- c(res, search_result_id)
			if (as.data.frame) res <- as.data.frame(res, stringsAsFactors=FALSE)
			res
		})
	})

	output3 <- do.call(c, output2)

	if (as.data.frame) {
		df <- do.call(rbind, output3)

		if (as.sf) {
			if (!project) {

			    df$x <- df$lon
			    df$y <- df$lat

			    res <- st_as_sf(df, coords = c("x","y"), crs=.crs_longlat)
			} else {
			    df$x2 <- df$x
			    df$y2 <- df$y

			    res <- st_as_sf(df, coords = c("x2","y2"), crs=.crs_longlat)
			}
			res
		} else {
			df
		}
	} else {
		if (length(output3)==1) {
			output3[[1]]
		} else output3
	}
}



rev_geocode_OSM <- function(x, y=NULL, zoom=NULL, projection=NULL, as.data.frame=NA, server="http://nominatim.openstreetmap.org") {

	project <- !missing(projection)

	if (project) projection <- get_proj4(projection, output = "CRS")

	if (inherits(x, "Spatial")) x <- as(x, "sf")

	if (inherits(x, "sf")) {
	    if (!all(st_geometry_type(x) == "POINT")) stop("sf object should only contain POINT geometries")

		isproj <- is_projected(x)

		if (is.na(isproj)) {
			if (project) {
				x <- set_projection(x, current.projection = projection, projection=.crs_longlat)
			} else {
				ll <- maybe_longlat(st_bbox(x))
				if (!ll) stop("Projection of x unknown. Please specify projection.")
				warning("Projection of SpatialPoints object unknown. Assuming longitude latitude coordinates.")
				x <- set_projection(x, current.projection = .crs_longlat)
			}
		} else {
			if (isproj) x <- set_projection(x, projection = .crs_longlat)
		}
		n <- nrow(x)
		co <- st_coordinates(x)
		lon <- x <- co[,1]
		lat <- y <- co[,2]
	} else {
	    n <- 1
		if (length(x) > 1 || length(y) > 1) {
			n <- max(length(x), length(y))
			x <- rep(x, length.out=n)
			y <- rep(y, length.out=n)
		}
		if (!project) {
			lon <- x
			lat <- y
		} else {
			projection <- get_proj4(projection, output = "CRS")
			single_point <- SpatialPoints(matrix(c(x,y), ncol=2), proj4string=projection)
			coords <- attr(set_projection(single_point, projection = .CRS_longlat), "coords")
			lon <- coords[,1]
			lat <- coords[,2]
		}
	}

	if (is.na(as.data.frame)) as.data.frame <- (n>1)

	if (missing(zoom)) {
		qzoom <- ""
		strzoom <- ""
	} else {
		qzoom <- paste0("&zoom=", zoom)
		strzoom <- paste0(", zoom = ", zoom)
	}

	addr <- paste0(server, "/reverse?format=xml&lat=", lat, "&lon=", lon, qzoom, "&addressdetails=1")


	dfs <- lapply(1:n, function(i) {
		# download query
		tmpfile <- tempfile()
		suppressWarnings(download.file(addr[i], destfile = tmpfile, mode= "wb", quiet = TRUE))
		doc <- xmlTreeParse(tmpfile, encoding="UTF-8")
		unlink(tmpfile)

		# read xml document
		res <- xmlChildren(xmlRoot(doc))

		# get name
		result_name <- xmlValue(res[[1]])
		Encoding(result_name) <- "UTF-8"

		# get osm id, location, bbox
		search_result <- xmlAttrs(res[[1]])
		search_result_id <- search_result[c("place_id", "osm_type", "osm_id", "ref")]
		names(search_result_id) <- c("place_id", "osm_type", "osm_id", "ref") # in case of missings
		Encoding(search_result_id) <- "UTF-8"
		search_result_ll <- as.numeric(search_result[c("lat", "lon")])
		names(search_result_ll) <- c("lat", "lon")
		search_result_bb <- as.numeric(unlist(strsplit(search_result["boundingbox"], ",")))
		names(search_result_bb) <- c("lat_min", "lat_max", "lon_min", "lon_max")

		# get address
		addr_result <- xmlChildren(res[[2]])
		dfnames <- names(addr_result)
		dfvalues <- lapply(1:length(addr_result), function(j) {
			v <- xmlValue(addr_result[[j]])
			Encoding(v) <- "UTF-8"
			v
		})
		names(dfvalues) <- dfnames

		c(list(x=x[i],
			 y=y[i],
			 name=result_name),
		  search_result_id,
		  search_result_ll,
		  search_result_bb,
		  dfvalues)
	})

	# cast to data.frame
	if (as.data.frame) {
		addrnames <- sort(unique(unlist(lapply(dfs, function(df) {
			names(df)[14:length(df)]
		}))))

		addrlist <- lapply(addrnames, function(a) NA)
		names(addrlist) <- addrnames

		do.call(rbind, c(lapply(dfs, function(df) {
			sel <- 14:length(df)
			addrlist[names(df)[sel]] <- df[sel]
			as.data.frame(c(df[1:13], addrlist), stringsAsFactors=FALSE)
		}), list(stringsAsFactors=FALSE)))
	} else {
		dfs
	}
}
