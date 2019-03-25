calc_densities <- function(shp, var, target="metric", total.area=NULL, suffix=NA, drop=TRUE) {
	## calculate densities
    #if (inherits(shp, c("sf", "sfc"))) shp <- as(shp, "Spatial")

    if (inherits(shp, "Spatial")) shp <- as(shp, "sf")

	areas <- approx_areas(shp, target = target, total.area=total.area)

	areas_unit <- attr(areas, "unit")

	if (is.na(suffix)) suffix <- paste("_", sub(" ", replacement = "_", areas_unit), sep = "")

	## calculate and return densities
	shp <- st_set_geometry(shp, NULL)

    if (length(var)==1 && drop) return(shp[[var]] / areas)

    data <- as.data.frame(lapply(shp[, var, drop=FALSE], function(x)x/areas))
	names(data) <- paste(var, suffix, sep="")
	data
}

