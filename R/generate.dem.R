#' turn elevation raster .tif files into a DEM
#'
#' generate.dem converts elevation raster .tif files into a DEM ready to use with overlay.relief
#'
#' @name generate.dem
#'
#' @param map.data An sf data frame with geometries of type POLYGON or MULTIPOLYGON and associated attributes.
#' @param raster.files A vector of .tif elevation raster file names you wish to turn into a DEM. If empty the function will pull raster data for the map.data bounding box from AWS Terrain Tiles.
#' @param coordinate.system EPSG code of the projection you wish to use. Default NULL: if crs of map.data and the generated DEM do not match the latter will be projected to the crs of the former.
#' @param aggregate factor to aggregate the generated DEM by. Default NULL.
#' @return A raster object.
#' @export


generate.dem <-
  function(map.data,
           raster.files,
           coordinate.system = NULL,
           aggregate = NULL) {

    if (!methods::is(map.data,"sf")) {
        stop("map.data not of class sf")
      }


    if (missing(raster.files)) {
      message("downloading elevation raster")
      elevation.raster <- elevatr::get_elev_raster(map.data, z = 9)
      message("elevation raster loaded")
    } else {
      message("loading elevation raster files")
      elevation.raster <- lapply(raster.files,
                                 function(i){raster::raster(i)})
      message("combining elevation raster files")
      elevation.raster <- do.call(raster::merge, elevation.raster)
    }

    if (is.na(raster::crs(elevation.raster))) {
      raster::crs(elevation.raster) <-
        "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    }

    if (!is.null(aggregate)) {
      message("aggregating raster")
      elevation.raster <-
        raster::aggregate(elevation.raster, aggregate)
      message("raster aggregated")
    }

    if (is.null(coordinate.system)) {
      same_crs <- raster::compareCRS(map.data, elevation.raster)
      if (!same_crs) {
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

    return(elevation.raster)
  }
