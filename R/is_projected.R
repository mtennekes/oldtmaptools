is_projected <- function(x) {
    isP <- if (inherits(x, "Raster")) {
        !couldBeLonLat(x, warnings=FALSE)
    } else {
        if (inherits(x, c("Spatial", "sf", "sfc")))
            prj <- get_projection(x)
        else prj <- get_proj4(x)

        proj4_is_projected(prj)
    }
    if (is.na(isP) && !is.character(x)) isP <- !maybe_longlat(get_bb(x)$b)
    isP
}

proj4_is_projected <- function(p) {
    if (is.na(p))
        as.logical(NA)
    else
        (length(grep("longlat", p, fixed = TRUE))==0)
}


maybe_longlat <- function(bb) {
    (bb[1] >= -180.1 && bb[3] <= 180.1 && bb[2] >= -90.1 && bb[4] <= 90.1)
}
