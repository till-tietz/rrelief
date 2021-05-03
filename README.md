
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rrelief

<!-- badges: start -->

<!-- badges: end -->

rrelief allows you to conveniently build visually appealing and
informative maps with ggplot. Using this package you can now easily
visualize data over a shaded relief background without masking the
relief. This package heavily relies on the amazing raster
(<http://CRAN.R-project.org/package=raster>), sf
(<https://CRAN.R-project.org/package=sf>) and exactextractr
(<https://CRAN.R-project.org/package=exactextractr>) packages and simply
provides a convenient wrapper to integrate your normal workflow to
accomplish this task using these packages into one function.

## Installation

You can install the development version of rrelief from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("till-tietz/rrelief")
```

## Usage

This quick example will hopefully help to illustrate the general rrelief
workflow, show what the package can do and get you started on making
some nice maps. For this example we’ll be using some census tract data
for the state of Washington from
(<https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html>),
as well as some elevation data for the north western United States from
(<http://srtm.csi.cgiar.org/srtmdata/>).

``` r
library(rrelief)

#loading census_tract shape file as an sf object
census_tracts <- sf::st_read("cb_2018_53_tract_500k.shp")
census_tracts <- sf::st_transform(census_tracts, crs = 4326)

#loading .tif elevation files as raster files and merging them into one raster 
files <- list.files(recursive=TRUE, pattern=".tif$")
rasters_list <- sapply(files, raster::raster)
names(rasters_list) <- NULL
rasters_list$fun <- mean
raster <- do.call(raster::mosaic, rasters_list)
raster::crs(raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
raster <- raster::crop(raster, census_tracts)

#adding some random variables to the census tracts so we have something to map
census_tracts[,"random_var_cont"] <- sample(seq(from = 0, to = 100, by = 0.01), size = nrow(census_tracts), replace = TRUE)
census_tracts[,"random_var_cat"] <- sample(c(0,1, NA), size = nrow(census_tracts), replace = TRUE)

#running rrelief keeping the crs projection at 4326
rrelief_output <- overlay.relief(map.data = census_tracts, variables = c("random_var_cont", "random_var_cat"),
                                 elevation.raster = raster, make.hillshade = TRUE, 
                                 coordinate.system = NULL, altitude = 45, azimuth = 270, z.factor = 10)
```

overlay.relief returns a data.frame of points with their respective
hill-shade and variable values.

|     value |          x |        y | coverage\_fraction | random\_var\_cont | random\_var\_cat |
| --------: | ---------: | -------: | -----------------: | ----------------: | ---------------: |
| 0.6776151 | \-122.9262 | 47.04625 |          0.0145559 |             97.54 |                1 |
| 0.7581708 | \-122.9254 | 47.04625 |          0.2550253 |             97.54 |                1 |
| 0.7320812 | \-122.9246 | 47.04625 |          0.2350712 |             97.54 |                1 |
| 0.6898862 | \-122.9237 | 47.04625 |          0.2151171 |             97.54 |                1 |
| 0.6878825 | \-122.9229 | 47.04625 |          0.1951630 |             97.54 |                1 |
| 0.6878825 | \-122.9221 | 47.04625 |          0.1752090 |             97.54 |                1 |
| 0.6624738 | \-122.9213 | 47.04625 |          0.1552549 |             97.54 |                1 |
| 0.6783612 | \-122.9204 | 47.04625 |          0.1353008 |             97.54 |                1 |
| 0.7342287 | \-122.9196 | 47.04625 |          0.1153467 |             97.54 |                1 |
| 0.5879318 | \-122.9188 | 47.04625 |          0.0953927 |             97.54 |                1 |

Now you can simply create your base map with generate.map.

``` r

map <- generate.map(data = rrelief_output, x = "x", y = "y", hillshade = "value",
                    variable = "random_var_cont", coordinate.system = 4326)
```

![](man/figures/test_plot_1.png)

You can further customize your base map with all the ggplot options you
know and love. Let’s add a slightly more compelling colour scheme and
nicer legend.

``` r

map +
  scale_fill_viridis_c(alpha = 0.8) +
  guides(fill = guide_colourbar(title = "Random Variable",
                                label.position = "bottom",
                                frame.colour = "black",
                                barwidth = 10,
                                barheight = 1,
                                title.vjust = 1))
```

![](man/figures/test_plot_2.png)
