approx_areas <- function(shp, target="metric", total.area=NULL) {
    is_metric <- target=="metric"
    is_imperial <- target=="imperial"

    if (is_metric) target <- "km km"
    if (is_imperial) target <- "mi mi"

    nct <- nchar(target)
    if (substr(target, nct-1, nct) == "^2") target <- paste(substr(target, 1, nct-2), substr(target, 1, nct-2))

    if (inherits(shp, "Spatial")) shp <- as(shp, "sf")

    areas <- tryCatch(sf::st_area(shp),
                  error = function(e) {
                      lwgeom::st_geod_area(lwgeom::st_transform_proj(shp, crs = 4326))
                  })

    if (target == "prop") {
        areas <- areas / sum(areas)
    } else if (target == "norm") {
        areas <- areas / max(areas)
    } else {
        areas <- set_units(areas, as_units(target), mode = "standard")
        if (!is.null(total.area)) {
            fact <- total.area / sum(areas)
            areas <- areas * fact
        }
    }

    areas
}
#'
#'
# approx_areas <- function(shp, target="metric", orig=NA, to=NA, total.area=NA, show.warnings=TRUE) {
#
#     is_metric <- target=="metric"
#     is_imperial <- target=="imperial"
#
#     if (is_metric) target <- "km"
#     if (is_imperial) target <- "mi"
#
#     if (!(target %in% c("abs", "prop", "norm"))) {
#         res <- projection_units(get_projection(shp), target=target, orig=orig, to=to)
#
#         projected <- res$projected
#         newtarget <- res$target
#         orig <- res$orig
#         to <- res$to
#     } else {
#         projected <- is_projected(shp)
#         newtarget <- target
#     }
#
#     # determine area sizes and corresponding units
#     if (projected) {
#         x <- rgeos::gArea(shp, byid = TRUE)
#     } else {
#         x <- geosphere::areaPolygon(shp)
#         orig <- "m"
#         to <- to_m["m"] / to_m[newtarget]
#     }
#
#     if (!(target %in% c("abs", "prop", "norm")) && is.na(to)) {
#         if (show.warnings) warning("Target unit or original unit unknown. Please specify valid the arguments target and orig, or the argument to")
#         target <- "abs"
#     }
#
#     if (any(is.na(x)) || any(is.infinite(x))) {
#         naid <- sort(union(which(is.na(x)), which(is.infinite(x))))
#         if (show.warnings) warning("cannot determine area of polygon(s) ", paste(naid, collapse=","))
#     }
#
# 	if (is.na(total.area)) total.area <- sum(x, na.rm = TRUE) * (to^2)
#     denom <- switch(target, norm=max(x), prop=sum(x), abs=1, sum(x, na.rm = TRUE)/total.area)
#     x2 <- x / denom
#
#     # revert back to meters or feet is needed
#     if (is_metric) {
#         if (max(x2, na.rm = TRUE) < 1) {
#             x2 <- x2 * 1e6
#             newtarget <- "m"
#         }
#     } else if (is_imperial) {
#         if (max(x2, na.rm = TRUE) < 1) {
#             x2 <- x2 * 27878400
#             newtarget <- "ft"
#         }
#     }
#
#     if (!(target %in% c("abs", "prop", "norm"))) newtarget <- paste("sq", newtarget)
#     attr(x2, "unit") <- newtarget
#     x2
# }
