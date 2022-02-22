#' overlay relief with coloured data visualization
#'
#' overlay.relief converts DEM data into a data frame of spatial points and their associated
#' hill shade values. It merges this data with attributes associated with sf geometries. This allows
#' you to visualize sf attributes on top of a shaded relief in ggplot without masking the relief.
#'
#' @name overlay.relief
#'
#' @param map.data An sf data frame with geometries of type POLYGON or MULTIPOLYGON and associated attributes.
#' @param variables The column names of the attributes you wish to visualize as a character vector i.e c("variable_1","variable_2").
#' @param elevation.raster Digital Elevation Model (if make.hillshade = TRUE) or Shaded Relief (if make.hollshade = FALSE) data of class RasterLayer.
#' @param make.hillshade Set to TRUE if you wish to turn DEM data into hillshade, set to FALSE if you wish to pass a hillshade raster to the function directly. Default TRUE.
#' @param coordinate.system EPSG code of the projection you wish to use. Default NULL: if crs of map.data and elevation.raster do not match the latter will be projected to the crs of the former.
#' @param altitude Elevation angle of light source in degrees to calculate hill shade. Numeric value between 0 and 90.
#' @param azimuth Direction angle of light source in degrees to calculate hill shade. Numeric value between 0 and 360.
#' @param z.factor Numeric value to multiply elevation raster by to exaggerate relief. Default NULL.
#' @return A list containing 1. a data frame of points and their associated hill shade values and sf attributes 2. the EPGS projection code
#' @export


overlay.relief <- function(map.data,
                           variables,
                           elevation.raster,
                           make.hillshade = TRUE,
                           coordinate.system = NULL,
                           altitude = 45,
                           azimuth = 270,
                           z.factor = NULL) {

  if (missing(map.data)) {
    stop("missing map.data")
  } else {
    if (class(map.data)[[1]] != "sf") {
      stop("map.data not of class sf")
    }
  }

  if (missing(variables)) {
    stop("Missing variables. Please specify the names of the variables in map.data you'd like to visualize")
  } else {
    if (class(variables) != "character") {
      stop("Input for Variable is not of class character. Please input variable as character string")
    }

    if (!all(variables %in% colnames(map.data))){
      in_colnames <- variables %in% colnames(map.data)
      not_in_colnames <- paste(variables[which(!in_colnames)], collapse = " ")
      stop(paste(not_in_colnames), " not in map.data", collapse = " ")
    }
  }

  if (missing(elevation.raster)) {
    stop("missing elevation.raster")
  } else {
    if (class(elevation.raster)[[1]] != "RasterLayer") {
      stop("elevation.raster not of class RasterLayer")
    }
  }

  if(is.na(raster::crs(elevation.raster))){
    stop("no crs set for elevation.raster")
  }

  if (is.null(coordinate.system)){
    same_crs <- raster::compareCRS(map.data, elevation.raster)
    if(!same_crs){
      message("re-projecting elevation.raster")
      elevation.raster <- raster::projectRaster(elevation.raster, crs = raster::crs(map.data))
      message("elevation.raster projected")
    }
  } else {
    message("re-projecting map.data")
    map.data <- sf::st_transform(map.data, crs = coordinate.system)
    message("map.data projected")
    message("re-projecting elevation.raster")
    elevation.raster <- raster::projectRaster(elevation.raster, crs = raster::crs(map.data))
    message("elevation.raster projected")
  }

  if(make.hillshade){
    if (!is.null(z.factor)) {
      elevation.raster <- (elevation.raster * z.factor)
    }
    message("generating shaded relief")
    slope <- raster::terrain(elevation.raster, opt = "slope")
    aspect <- raster::terrain(elevation.raster, opt = "aspect")
    elevation.raster <- raster::hillShade(slope, aspect, angle = altitude, direction = azimuth)
    message("shaded relief generated")
  }

  data <- map.data
  sf::st_geometry(data) <- NULL

  message("extracting relief data for each polygon")
  relief_vals <- exactextractr::exact_extract(elevation.raster, map.data, include_xy = TRUE)
  message("relief data extracted")

  combine_data <- function(data, relief_vals){
    loop <- function(x){
      value_i <- relief_vals[[x]]

      data_i <- as.data.frame(data[x,variables])
      vars_i <- dplyr::slice(data_i,rep(1:dplyr::n(), each = nrow(value_i)))
      colnames(vars_i) <- variables

      value_i <- cbind(value_i, vars_i)
      return(value_i)
    }
    out <- purrr::map_dfr(1:length(relief_vals), ~loop(.x))
    return(out)
  }

  message("compiling data")
  out <- combine_data(data = data, relief_vals = relief_vals)
  message("data compiled")

  cs <- trimws(stringr::str_split(sf::st_crs(map.data)[[2]], "\n")[[1]])
  cs <- cs[grepl("^ID", cs)]
  cs <- cs[length(cs)]
  cs <- as.numeric(gsub("[^0-9.-]", "", cs))

  return(list(
    relief = out,
    EPSG = cs
  ))
}












