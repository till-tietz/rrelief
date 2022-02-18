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


generate.dem <- function(map.data, raster.files, coordinate.system = NULL, aggregate = NULL){

  if (missing(map.data)) {
    stop("missing map.data")
  } else {
    if (class(map.data)[[1]] != "sf") {
      stop("map.data not of class sf")
    }
  }

  if (missing(raster.files)) {
    print("downloading elevation raster")
    elevation.raster <- elevatr::get_elev_raster(map.data, z = 9)
    print("elevation raster loaded")
  } else {
    print("combining elevation raster files")
    gdalUtils::gdalbuildvrt(gdalfile = raster.files,
                            output.vrt = "dem.vrt")
    print("elevation raster files combined")
    print("loading elevation raster")
    elevation.raster <- gdalUtils::gdal_translate(src_dataset = "dem.vrt",
                                                  dst_dataset = "dem.tif",
                                                  output_Raster = TRUE,
                                                  options = c("BIGTIFF=YES", "COMPRESSION=LZW"))
    print("elevation raster loaded")
  }

  if(is.na(raster::crs(elevation.raster))){
    raster::crs(elevation.raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  }

  if(!is.null(aggregate)){
    print("aggregating raster")
    elevation.raster <- raster::aggregate(elevation.raster, aggregate)
    print("raster aggregated")
  }

  if (is.null(coordinate.system)){
    same_crs <- raster::compareCRS(map.data, elevation.raster)
    if(!same_crs){
      print("re-projecting elevation.raster")
      elevation.raster <- raster::projectRaster(elevation.raster, crs = raster::crs(map.data))
      print("elevation.raster projected")
    }
  } else {
    print("re-projecting map.data")
    map.data <- sf::st_transform(map.data, crs = coordinate.system)
    print("map.data projected")
    print("re-projecting elevation.raster")
    elevation.raster <- raster::projectRaster(elevation.raster, crs = raster::crs(map.data))
    print("elevation.raster projected")
  }

  return(elevation.raster)
}
