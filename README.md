
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rrelief

<!-- badges: start -->

<!-- badges: end -->

rrelief allows you to conveniently build visually appealing and
informative maps with ggplot. Using this package you can now easily
visualize data over a shaded relief background without masking the
relief.

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

(If it is clear from looking at your map.data sf that many observations
have the same variable values you might wish to merge them into one
polygon with sf::st\_union() to cut down the run-time of overlay.relief)

``` r
library(rrelief)

#loading .tif elevation files as raster files and merging them into one raster 
files <- list.files(recursive=TRUE, pattern=".tif$")
rasters_list <- sapply(files, raster::raster)
names(rasters_list) <- NULL
rasters_list$fun <- mean
raster <- do.call(raster::mosaic, rasters_list)

#loading census_tract shape file as an sf object
census_tracts <- sf::st_read("cb_2018_53_tract_500k.shp")

#adding some random variables to the census tracts so we have something to map
census_tracts[,"random_var_cont"] <- sample(seq(from = 0, to = 100, by = 0.01), size = nrow(census_tracts), replace = TRUE)
census_tracts[,"random_var_cat"] <- sample(c(0,1, NA), size = nrow(census_tracts), replace = TRUE)

#running rrelief with epsg 5070 projection for USA 
rrelief_output <- overlay.relief(map.data = census_tracts, variables = c("random_var_cont", "random_var_cat"),
                                          elevation.raster = raster, coordinate.system = 5070, altitude = 45,
                                          azimuth = 270, z.factor = 10)
```

overlay.relief returns a data.frame of points (defined by x and y
coordinates) with their respective hill-shade (layer) and variable
values.

|     | random\_var\_cont | random\_var\_cat |         x |       y |     layer |
| :-- | ----------------: | ---------------: | --------: | ------: | --------: |
| …1  |             98.84 |                1 | \-2028476 | 2962035 | 0.7048025 |
| …2  |             98.84 |                1 | \-2028415 | 2962035 | 0.7142783 |
| …3  |             98.84 |                1 | \-2028354 | 2962035 | 0.6925296 |
| …4  |             98.84 |                1 | \-2028537 | 2961946 | 0.7062669 |
| …5  |             98.84 |                1 | \-2028476 | 2961946 | 0.6799695 |
| …6  |             98.84 |                1 | \-2028415 | 2961946 | 0.6981753 |
| …7  |             98.84 |                1 | \-2028354 | 2961946 | 0.7331790 |
| …8  |             98.84 |                1 | \-2028293 | 2961946 | 0.6970354 |
| …9  |             98.84 |                1 | \-2028231 | 2961946 | 0.5949051 |
| …10 |             98.84 |                1 | \-2028170 | 2961946 | 0.5666509 |

Now you can simply create your map by feeding the output of
overlay.relief to ggplot. You can control the look of the relief with an
alpha aesthetic and visualize your variables with a fill aesthetic. In
order to match the appearance of your fill scale between map and legend
it is important to set the alpha value of your fill scale to the central
value of your hill-shade alpha scale with guides.

``` r
ggplot()+
  geom_raster(data = rrelief_output, aes(x = round(x,5), y = round(y,5), alpha = layer, fill = random_var_cont),interpolate = TRUE)+
  scale_alpha(name = "", range = c(1, 0.4), guide = F)+
  scale_fill_gradient(low = "blue", high = "orange", na.value = "gray5")+
  labs(fill = "Random Variable")+
  theme(legend.position = "right", legend.box = "vertical", legend.title = element_text(size = 6, family = "serif"), 
        legend.text = element_text(size = 6, family = "serif"),axis.title = element_blank(),
        legend.key.size = unit(0.5,"cm"),
        panel.background = element_blank(),panel.grid = element_blank(),
        axis.line = element_blank(),axis.text = element_blank(),
        axis.ticks = element_blank())+
  #matching fill scale appearance between legend and map
  guides(fill = guide_legend(override.aes = list(alpha = 0.7)))
```

Now simply save your map.

``` r
ggsave('map_rrelief.png', width = 15, height = 10)
```

And there we are.

![](C:/Users/ttiet/Desktop/rrelief_data/map_test_new_1.png)
