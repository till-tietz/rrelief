
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
provides some convenient wrappers to integrate your normal workflow to
accomplish this task into three functions.

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
some nice maps. For this example we’ll be using some voting district
data for the state of Washington from
(<https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html>),
as well as some elevation data for the north western United States from
(<http://srtm.csi.cgiar.org/srtmdata/>).

``` r
library(rrelief)

#loading census_tract shape file as an sf object and adding a random variable
voting_districts <- sf::st_read("cb_2018_53_sldu_500k.shp")
voting_districts <- sf::st_transform(voting_districts, crs = 4326)
voting_districts[,"var"] <- runif(nrow(voting_districts), 0,100)

#loading .tif elevation files and merging them into one raster 
files <- list.files(recursive=TRUE, pattern=".tif$")
raster <- rrelief::generate.dem(raster.files = files, map.data = voting_districts, aggregate = 3)

#running rrelief keeping the crs projection at 4326
rrelief_output <- rrelief::overlay.relief(map.data = voting_districts, variables = c("var"),
                                          elevation.raster = raster, make.hillshade = TRUE, 
                                          coordinate.system = NULL, altitude = 45, azimuth = 270,
                                          z.factor = 10)
```

overlay.relief returns a data.frame of points with their respective
hill-shade and variable values.

|     value |          x |        y | coverage\_fraction |      var |
| --------: | ---------: | -------: | -----------------: | -------: |
| 0.6933837 | \-122.3413 | 47.58625 |          0.0065928 | 60.99251 |
| 0.7028063 | \-122.3388 | 47.58625 |          0.4054028 | 60.99251 |
| 0.7341834 | \-122.3363 | 47.58625 |          0.4298274 | 60.99251 |
| 0.7347732 | \-122.3337 | 47.58625 |          0.4629008 | 60.99251 |
| 0.7044837 | \-122.3312 | 47.58625 |          0.4805375 | 60.99251 |
| 0.7518519 | \-122.3287 | 47.58625 |          0.4751625 | 60.99251 |
| 0.7388729 | \-122.3263 | 47.58625 |          0.4679143 | 60.99251 |
| 0.7167771 | \-122.3238 | 47.58625 |          0.4451203 | 60.99251 |
| 0.9926822 | \-122.3213 | 47.58625 |          0.4399785 | 60.99251 |
| 0.9323923 | \-122.3187 | 47.58625 |          0.1329148 | 60.99251 |

Now you can simply create your base map with generate.map.

``` r

map <- generate.map(data = rrelief_output, x = "x", y = "y", hillshade = "value",
                    variable = "var", coordinate.system = 4326)
```

![](man/figures/map_original.png)

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

![](man/figures/map.png)
