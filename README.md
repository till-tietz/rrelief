
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rrelief

<!-- badges: start -->

[![R-CMD-check](https://github.com/till-tietz/rrelief/workflows/R-CMD-check/badge.svg)](https://github.com/till-tietz/rrelief/actions)
<!-- badges: end -->

`rrelief` allows you to conveniently build visually appealing and
informative maps with ggplot. Using this package you can now easily
visualize data over a shaded relief background without masking the
relief. This package heavily relies on the amazing `raster`
(<http://CRAN.R-project.org/package=raster>), `sf`
(<https://CRAN.R-project.org/package=sf>), `exactextractr`
(<https://CRAN.R-project.org/package=exactextractr>) and `elevatr`
(<https://cran.r-project.org/web/packages/elevatr/index.html>) packages
and simply provides some convenient wrappers to implement the routine
task of mapping with minimal coding on your end.

## Installation

You can install the development version of `rrelief` from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("till-tietz/rrelief")
```

## Usage

This quick example will hopefully help to illustrate the general
`rrelief` workflow, show what the package can do and get you started on
making some nice maps. For this example we’ll be using some voting
district data for the state of Washington from
(<https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html>),
as well as some elevation data for the north western United States from
(<http://srtm.csi.cgiar.org/srtmdata/>).  
  

Generating a map in `rrelief` is split into three successive core
steps:  

1.generate a DEM  
2. generate a relief and overlay your data  
3. generate the map  

We’ll look at each step in turn.  
  

### Generating a DEM 

  

Fundamentally all you need to get started is a `sf` `data.frame` with
`polygon geometries` and the variables you wish to map. `rrelief`
utilizes `elevatr` to pull DEM raster data from Terrain Tiles on AWS
(<https://registry.opendata.aws/terrain-tiles/>) based on the bounding
box of your `sf`, so you don’t have to deal with finding suitable
elevation data.

``` r
library(rrelief)

#loading census_tract shape file as an sf object and adding a random variable
voting_districts <- sf::st_read("cb_2018_53_sldu_500k.shp")
voting_districts <- sf::st_transform(voting_districts, crs = 4326)
voting_districts[,"var"] <- runif(nrow(voting_districts), 0,100)

#generating DEM 
raster <- rrelief::generate.dem(map.data = voting_districts, aggregate = 3)
```

Should you already have suitable elevation data raster data in `.tif`
format you can generate a DEM using `rrelief` as follows.

``` r
#generating DEM 
raster <- rrelief::generate.dem(map.data = voting_districts, 
                                raster.files = c("dem_1.tif","dem_2.tif"), aggregate = 3)
```

  

### Generating a relief and overlaying Data 

  

``` r
#running rrelief keeping the crs projection at 4326
rrelief_output <- rrelief::overlay.relief(map.data = voting_districts, variables = c("var"),
                                          elevation.raster = raster, make.hillshade = TRUE, 
                                          coordinate.system = NULL, altitude = 45, azimuth = 270,
                                          z.factor = 10)
```

`overlay.relief` returns a `data.frame` of points with their respective
hill-shade and variable values.

|     value |         x |        y | coverage\_fraction |      var |
|----------:|----------:|---------:|-------------------:|---------:|
| 0.6933837 | -122.3413 | 47.58625 |          0.0065928 | 60.99251 |
| 0.7028063 | -122.3388 | 47.58625 |          0.4054028 | 60.99251 |
| 0.7341834 | -122.3363 | 47.58625 |          0.4298274 | 60.99251 |
| 0.7347732 | -122.3337 | 47.58625 |          0.4629008 | 60.99251 |
| 0.7044837 | -122.3312 | 47.58625 |          0.4805375 | 60.99251 |
| 0.7518519 | -122.3287 | 47.58625 |          0.4751625 | 60.99251 |
| 0.7388729 | -122.3263 | 47.58625 |          0.4679143 | 60.99251 |
| 0.7167771 | -122.3238 | 47.58625 |          0.4451203 | 60.99251 |
| 0.9926822 | -122.3213 | 47.58625 |          0.4399785 | 60.99251 |
| 0.9323923 | -122.3187 | 47.58625 |          0.1329148 | 60.99251 |

  

### Generating the Map 

Now you can simply create your base map with `generate.map`.

``` r
map <- generate.map(data = rrelief_output, x = "x", y = "y", 
                    hillshade = "value", variable = "var")
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
