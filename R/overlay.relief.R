#' overlay relief with coloured data visualization
#'
#' This function converts DEM data into a data frame of spatial points and their associated
#' hill shade values. It merges this data with atributes associated with sf geometries. This allows
#' you to visualize sf attributes on top of a shaded relief in ggplot without masking the relief.
#' To implement this in ggplot plot the function output as a geom_raster() with x and y as point coordinates.
#' Control the hill shading with an alpha scale and the sf attribute visualization with a fill scale.
#'
#'
#' @param map.data An sf data frame with geometries of type MULTILINESTRING, POLYGON or MULTIPOLYGON and associated attributes.
#' @param variable The column names of the attributes you wish to visualize on top of the relief passed to the argument as a character vector i.e c("variable_1","variable_2").
#' @param elevation.raster Digital Elevation Model data in form of an object of class RasterLayer.
#' @param coordinate.system Four digit epsg code of the projection you wish to use. Default 4326.
#' @param altitude Elevation angle of light source in degrees to calculate hill shade. Numeric value between 0 and 90.
#' @param azimuth Direction angle of light source in degrees to calculate hill shade. Numeric value between 0 and 360.
#' @param z.factor Numeric value to multiply elevation raster by to exaggerate relief. Default NULL.
#' @return A data frame of points and their associated hill shade values and sf attributes.
#' @export

overlay.relief <- function(map.data,
                           variables,
                           elevation.raster,
                           coordinate.system = 4326,
                           altitude = 45,
                           azimuth = 270,
                           z.factor = NULL){
  if(missing(map.data)) {
    stop("missing map.data")
  } else {
    if (class(map.data)[[1]] != "sf") {
      stop("map.data not of class sf")
    } else {
      map_data <- map.data %>% sf::st_transform(., coordinate.system)
    }
  }

  if(missing(variables)) {
    stop("Missing variable. Please specify the name of the variable in map.data you'd like to visualize")
  } else {
    if (class(variables) != "character") {
      stop("Input for Variable is not of class character. Please input variable as character string")
    }
  }


  if(missing(elevation.raster)) {
    stop("missing elevation.raster")
  } else {
    if (class(elevation.raster)[[1]] != "RasterLayer") {
      stop("elevation.raster not of class RasterLayer")
    } else {
      elevation_raster <- elevation.raster%>%
        raster::projectRaster(., crs = raster::crs(map_data))
    }
  }

  if(!is.null(z.factor)) {
    raster <- (elevation_raster * z.factor)
  } else {
    raster <- elevation_raster
  }

  slope <- raster::terrain(raster, opt= "slope")
  aspect <- raster::terrain(raster, opt= "aspect")
  relief <- raster::hillShade(slope, aspect, angle= altitude, direction= azimuth)

  relief_assign <- function(x){
    crop_area <- map_data[x,]

    relief_cropped <- raster::crop(relief, crop_area)%>%
      raster::mask(., crop_area)%>%
      raster::rasterToPoints()%>%
      data.frame()

    sf::st_geometry(crop_area) <- NULL
    vars <- as.data.frame(sapply(crop_area[,variables], function(x) rep(x, nrow(relief_cropped))))
    relief_cropped <- cbind(vars, relief_cropped)
    return(relief_cropped)
  }
  relief_combined <- purrr::map_dfr(1:nrow(map.data), ~relief_assign(.x))
  return(relief_combined)
}












