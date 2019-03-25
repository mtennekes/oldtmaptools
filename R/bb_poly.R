bb_poly <- function(x, steps=100, stepsize=NA, projection=NULL) {
    bbx <- get_bb(x)$b
    create_sf_rect(bbx, steps=steps, stepsize=stepsize, projection=projection)
}

create_sf_rect <- function(bbx, steps=100, stepsize=NA, projection=NULL) {
    if (is.null(projection)) projection <- st_crs(bbx)

    x1 <- bbx[1]
    x2 <- bbx[3]
    y1 <- bbx[2]
    y2 <- bbx[4]

    dx <- x2-x1
    dy <- y2-y1

    if (is.na(stepsize)) stepsize <- min(dx,dy) / steps

    ny <- round(dy / stepsize + 1)
    nx <- round(dx / stepsize + 1)

    crds <- matrix(c(
        rep(x1, ny),
        seq(x1+stepsize, x2-stepsize, length.out=nx-2),
        rep(x2, ny),
        seq(x2-stepsize, x1+stepsize, length.out=nx-2),
        seq(y1, y2, length.out=ny),
        rep(y2, nx-2),
        seq(y2, y1, length.out=ny),
        rep(y1, nx-2)),
        ncol=2)

    #x <- SpatialPolygons(list(Polygons(list(Polygon(coords=crds)), ID=id)), proj4string=get_proj4(projection, as.CRS = TRUE))

    sf::st_sfc(sf::st_polygon(x=list(rbind(crds, crds[1,]))), crs=projection)
}




bb_earth <- function(projection=NULL, stepsize=1, earth.datum=4326, bbx=c(-180, -90, 180, 90), buffer=1e-6) {
    crs_datum <- get_proj4(earth.datum, output = "crs")
    if (missing(projection))
        projection <- NA
    else
        projection <- get_proj4(projection, output = "crs")

    if (buffer==0) {
        bs <- 0
    } else {
        bs <- buffer*(10^(0:6))
    }

    for (b in bs) {
        bbxb <- bbx + c(b, b, -b, -b)
        world_bb_sf <- create_sf_rect(bbx=bbxb, stepsize = stepsize, projection=crs_datum)

        res <- if (is.na(projection)) {
            world_bb_sf
        } else {
			st_transform2(world_bb_sf, crs=projection)
        }
        if (!is.na(sf::st_is_valid(res))) break
    }

    isV <- sf::st_is_valid(res)

    if (is.na(isV) || !isV) {
    	warning("Unable to determine bounding box of the earth in projection \"", projection, "\"", call. = FALSE)
    	return(NULL)
    }

    res
}
