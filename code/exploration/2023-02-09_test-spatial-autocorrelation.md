Test for spatial autocorrelation
================
Eleanor Jackson
09 February, 2023

``` r
library("tidyverse")
library("here")
library("sf")
library("ncf")
library("spdep")
library("sp")
```

``` r
readRDS(here::here("data", "clean", "hawthorn_plots.rds")) %>%
  select(plot, tree_id, longitude, latitude, dbh, reproductive) %>%
  filter(tree_id == "tree_0") %>%
  mutate(tree_id = as.numeric(plot)) %>% 
  left_join(readRDS(here::here("data", "raw", "fruit_drop_data.rds")), 
            by = "tree_id") %>%
  sf::st_as_sf(coords = c("latitude", "longitude"), 
               crs = 4326, remove = FALSE) %>%
  distinct(tree_id, .keep_all = TRUE) %>%
  drop_na(proportion_dropped) -> focal_trees

# Missing mapping data for trees 29 and 23,
# 34 and 35 were only added for the pollination work,
# so n = 30
```

## Correlogram

A correlogram shows the correlation coefficient for the series lagged
(in distance) by one delay at a time. e.g. at lag one you’re looking at
the correlation between a point and it’s nearest neighbour. At lag two
you’re looking at the correlation between a point and it’s second
nearest neighbour.

Values close to 1 indicate clustering while values close to -1 indicate
dispersion. A random arrangement would give a value that is close to 0.

``` r
# using 10 m increments
plot(ncf::correlog(x = focal_trees$latitude, y = focal_trees$longitude, 
                   z =  focal_trees$proportion_dropped,
                   latlon = TRUE, increment = 0.01))
```

    ## 100  of  999 200  of  999 300  of  999 400  of  999 500  of  999 600  of  999 700  of  999 800  of  999 900  of  999 

![](figures/2023-02-09_test-spatial-autocorrelation/correlogram-1.png)<!-- -->

Here, values significant at a nominal (two-sided) 5%-level are
represented by filled circles and non-significant values by open
circles.

## Moran’s I

The expected value of Moran’s I under the null hypothesis of no spatial
autocorrelation is `-1/(n-1)`, which equals `-0.03448276` for this data.

Values significantly below `-1/(n-1)` indicate negative spatial
autocorrelation and values significantly above `-1/(n-1)` indicate
positive spatial autocorrelation.

``` r
# detect nearest neighbours within a 15 km radius (i.e. whole island)
spdep::dnearneigh(focal_trees$geometry, 0, 15, 
                  row.names = focal_trees$tree_id,  
                  longlat = TRUE) -> nn
```

    ## Warning in spdep::dnearneigh(focal_trees$geometry, 0, 15, row.names =
    ## focal_trees$tree_id, : dnearneigh: longlat argument overrides object

``` r
# add spatial weights
lw <- spdep::nb2listw(nn, style = "W", zero.policy = TRUE) 

# Moran's test for spatial autocorrelation
spdep::moran.test(focal_trees$proportion_dropped, lw) 
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  focal_trees$proportion_dropped  
    ## weights: lw    
    ## 
    ## Moran I statistic standard deviate = -8.8422e-10, p-value = 0.5
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##     -3.448276e-02     -3.448276e-02      6.158268e-17

Moran’s I is below `-1/(n-1)`, indicating negative spatial correlation,
but it’s not a significant result.

## Variogram

A variogram describes the variance of the difference between values at
two locations.

Here, the x axis represents the distance between traps (km) and each
point represents a pair of observations. The distance at which the
variogram stops increasing and flattens out is the range. Traps which
are closer together than this distance are spatially autocorrelated.

``` r
sp::coordinates(focal_trees) = ~latitude+longitude

plot(
    gstat::variogram(proportion_dropped ~ 1, data = focal_trees, 
        width = 0.001, cutoff = 4000) 
)
```

![](figures/2023-02-09_test-spatial-autocorrelation/variogram-1.png)<!-- -->

Looks to me like we don’t have any spatial autocorrelation - the
semivariance is fairly flat and quite random.
