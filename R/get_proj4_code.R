get_proj4 <- function(x, output = c("crs", "character", "epsg", "CRS")) {
    output <- match.arg(output)
	y <- if (is.null(x)) {
		return(NULL)
	} else if (is.na(x)) {
		sf::st_crs()
	} else if (inherits(x, "crs")) {
	    x
	} else if (inherits(x, "CRS")) {
	    sf::st_crs(attr(x, "projargs"))
	} else if (!is.numeric(x) && !is.character(x)) {
		stop("x is not a character, crs object, CRS object, nor a number", call.=FALSE)
	} else {
		if (x %in% names(.proj_epsg)) {
		    create_crs(unname(.proj_epsg[x]))
		} else if (x %in% names(.proj_sc)) {
		    create_crs(unname(.proj_sc[x]))
		} else if (is_num_string(x)) {
		    sf::st_crs(x)
		} else if (substr(x, 1, 3)=="utm") {
		    if (!(nchar(x) %in% c(5,6))) stop("\"utm\" shortcut code should be utmXX or utmXXs where XX refers to the utm zone")
			sf::st_crs(paste("+proj=utm +zone=", substr(x, 4, 5), ifelse(substr(x, 6, 6)=="s", " +south", ""), " +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0", sep=""))
		} else {
			sf::st_crs(x)
		}
	}

	switch(output,
	       character = y$proj4string,
	       crs = y,
	       epsg = y$epsg,
	       CRS = CRS(ifelse(is.na(y$proj4string), "", y$proj4string)))
}

create_crs <- function(x) {
    if (is.numeric(x)) {
        sf::st_crs(x)
    } else {
        structure(list(epsg = as.integer(NA), proj4string = x), class = "crs")
    }
}

is_num_string <- function(x) {
    suppressWarnings(!is.na(as.numeric(x)))
}

.proj_epsg <- c(longlat = 4326,
                latlong = 4326,
                WGS84 = 4326,
                NAD83 = 4269,
                NAD27 = 4267,
                merc = 3857,
                laea_Eur = 3035,
                laea_NA = 2163,
                rd = 28992)

.proj_sc <- c(wintri="+proj=wintri +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  robin="+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  eck4="+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  hd="+proj=cea +lat_ts=37.5 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  gall="+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  mill="+proj=mill +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +R_A +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  eqc0="+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  eqc30="+proj=eqc +lat_ts=30 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0",
			  eqc45="+proj=eqc +lat_ts=45 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0")

# many shapefiles of the Netherlands have one of these projections, which cause problems since the +towgs84 attribute is missing (this is automatically corrected in read_shape)
.wrong_rd_projections <- c("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs",
                           "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs",
                           "+proj=sterea +lat_0=52.156161 +lon_0=5.387639 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +no_defs")
