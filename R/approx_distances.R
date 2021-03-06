approx_distances <- function(x, y = NULL, projection = NULL, target = NULL) {
        ## set metric and imperial to defaults: km and mi
    if (!missing(target)) {
        is_metric <- target=="metric"
        is_imperial <- target=="imperial"

        if (is_metric) target <- "km"
        if (is_imperial) target <- "mi"
    }


    if (!inherits(x, c("sf", "Spatial", "raster"))) {
        if (missing(projection)) {
            projection <- st_crs(NA)
            #stop("Please specify projection")
        } else {
            projection <- get_proj4(projection, output = "crs")
        }
    } else {
        projection <- get_projection(x, output = "crs")
    }

    if (is.na(projection)) {
        warning("unknown projection", call. = FALSE)
    }


    if (missing(y)) {

        tryCatch({
            bbx <- bb(x)
        }, error = function(e) {
            stop("x cannot be coerced to a bounding box with bb", call. = FALSE)
        })

        pW <- st_sfc(st_point(c(bbx[1], (bbx[2]+bbx[4])/2)), crs=projection)
        pE <- st_sfc(st_point(c(bbx[3], (bbx[2]+bbx[4])/2)), crs=projection)
        pS <- st_sfc(st_point(c((bbx[1]+bbx[3])/2, bbx[2])), crs=projection)
        pN <- st_sfc(st_point(c((bbx[1]+bbx[3])/2, bbx[4])), crs=projection)

        if (missing(target)) {
            list(hdist = get_distance(pW, pE),       #st_distance(pW, pE)[1,1],
                 vdist = get_distance(pS, pN))       #st_distance(pS, pN)[1,1])
        } else {
            list(hdist = set_units(get_distance(pW, pE), target, mode = "standard"),  #st_distance(pW, pE)[1,1]
                 vdist = set_units(get_distance(pS, pN), target, mode = "standard"))  #st_distance(pS, pN)[1,1]
        }

    } else {
        p1 <- st_sfc(st_point(x), crs=projection)
        p2 <- st_sfc(st_point(y), crs=projection)

        if (missing(target) || is.na(projection)) {
            st_distance(p1, p2)[1,1]
        } else {
            set_units(st_distance(p1, p2), target, mode = "standard")[1,1]
        }

    }
}


get_distance <- function(p1, p2) {
    tryCatch(st_distance(p1, p2)[1,1], error = function(e) {
        p1ll <- lwgeom::st_transform_proj(p1, crs = 4326)
        p2ll <- lwgeom::st_transform_proj(p2, crs = 4326)
        lwgeom::st_geod_distance(p1ll, p2ll)[1,1]
    })
}



#'
#'
# approx_distances <- function(x, y = NULL, projection = NULL, target="metric", orig=NA, to=NA, show.warnings=TRUE) {
#     ## set metric and imperial to defaults: km and mi
#     is_metric <- target=="metric"
#     is_imperial <- target=="imperial"
#
#     if (is_metric) target <- "km"
#     if (is_imperial) target <- "mi"
#
#     if (inherits(x, c("Spatial", "Raster", "sf", "sfc"))) {
#         ## get projection and bounding box for spatial objects
#         prj <- get_projection(x, as.CRS=FALSE, guess.longlat = TRUE)
#         if (is.na(prj) && missing(projection)) stop("shape projection unknown; please specify it")
#         if (!is.na(prj) && !missing(projection)) warning("projection already defined in shape")
#
#         if (is.na(prj)) prj <- get_projection(projection, as.CRS = FALSE)
#         bbx <- bb(x)
#         if (!missing(y)) {
#             warning("y is only used if x is a pair of coordinates")
#             y <- NULL
#         }
#     } else {
#         ## get projection and bounding box for points and bounding boxes. Guess projection
#         if (is.vector(x) && length(x)==2) {
#             if (missing(y)) stop("y is required")
#             if (!is.vector(y) || length(y)!=2) stop("y is not a vector of 2")
#             bbx <- matrix(c(x, y), ncol=2, byrow = FALSE)
#         } else {
#             bbx <- bb(x)
#             if (!missing(y)) {
#                 if (show.warnings) warning("y is only used if x is a pair of coordinates")
#                 y <- NULL
#             }
#         }
#         if (missing(projection)) {
#             if (maybe_longlat(bbx)) {
#                 prj <- get_proj4("longlat")
#             } else {
#                 if (show.warnings) warning("projection unknown")
#                 prj <- NA
#             }
#         } else {
#             prj <- get_proj4(projection, as.CRS = FALSE)
#         }
#     }
#
#     ## Get projection info
#     res <- projection_units(x=prj, target = target, orig=orig, to=to)
#     projected <- res$projected
#     target <- res$target
#     orig <- res$orig
#     to <- res$to
#
#     ## For non-projected case, units of coordinates will be meters (distGeo)
#     if (!projected) {
#         orig <- "m"
#         to <- to_m["m"] / to_m[target]
#     } else if (is.na(to)) {
#         if (show.warnings) warning("Target unit or original unit unknown. Please specify valid the arguments target and orig, or the argument to")
#         target <- "abs"
#         to <- 1
#         is_metric <- FALSE
#         is_imperial <- FALSE
#     }
#
#
#     if (is.null(y)) {
#         if (projected) {
#             vdist <- (bbx[4] - bbx[2]) * to
#             hdist <- (bbx[3] - bbx[1]) * to
#         } else {
#             # also add middle values to prevent the shortest route is reverse
#             h <- c(bbx[1], mean(c(bbx[1], bbx[3])), bbx[3])
#             v <- c(bbx[2], mean(c(bbx[2], bbx[4])), bbx[4])
#             hdist <- (geosphere::distGeo(c(h[1], v[2]), c(h[2], v[2])) + geosphere::distGeo(c(h[2], v[2]), c(h[3], v[2]))) * to
#             vdist <- (geosphere::distGeo(c(h[2], v[1]), c(h[2], v[2])) + geosphere::distGeo(c(h[2], v[2]), c(h[2], v[3]))) * to
#         }
#         if (is_metric) {
#             if (hdist < 1 || vdist < 1) {
#                 hdist <- hdist * 1000
#                 vdist <- vdist * 1000
#                 target <- "m"
#             }
#         } else if (is_imperial) {
#             if (hdist < 1 || vdist < 1) {
#                 hdist <- hdist * 5280
#                 vdist <- vdist * 5280
#                 target <- "ft"
#             }
#         }
#
#         list(unit=target,
#              hdist=hdist,
#              vdist=vdist)
#
#     } else {
#         if (projected) {
#             #to_meter <- get_shape_units(projection = prj)$to_meter
#             xd <- y[1] - x[1]
#             yd <- y[2] - x[2]
#             dist <- unname(sqrt(xd^2+yd^2)) * to
#         } else {
#             dist <- geosphere::distGeo(x, y) * to
#         }
#
#         if (is_metric) {
#             if (dist < 1) {
#                 dist <- dist * 1000
#                 target <- "m"
#             }
#         } else if (is_imperial) {
#             if (dist < 1) {
#                 dist <- dist * 5280
#                 target <- "ft"
#             }
#         }
#
#         list(unit=target,
#              dist=dist)
#     }
# }
