Make a map of focal trees
================
Eleanor Jackson
11 October, 2022

``` r
library("tidyverse")
library("sp")
library("ggmap")
library("geojsonio")
library("leaflet")
library("osmdata")
```

## Read in gpx file and clean up

This file has come straight from the garmin gps

``` r
plotKML::readGPX(here::here("data", "raw", "waypoints.gpx")) %>%
  map_df(~.) %>%
  rename(id = name) %>%
  mutate(taxa = case_when(sym == "Park" ~ "Prunus_spinosa",
                          sym == "Flag, Blue" ~ "Crataegus_monogyna")) %>%
  select(-sym) %>%
  arrange(id) -> waypoints
```

## Or use co-ordinates from csv

``` r
read.csv(here::here("data", "raw", "tree_id_checklist.csv")) %>%
  drop_na(long_dd, lat_dd) -> tree_list
```

## Make a static map

``` r
bbox <- make_bbox(c(min(tree_list$long_dd) - 0.001, 
                    max(tree_list$long_dd)) + 0.001, 
                  c(min(tree_list$lat_dd) - 0.001, 
                    max(tree_list$lat_dd) + 0.001))

osmdata::opq(bbox = bbox) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf() -> osm_trails

tree_list %>%
  filter(focal == TRUE) %>%
  filter(tree_id != "23" & tree_id != "26") -> tree_list_focal

get_map(bbox, source = "stamen", force = TRUE, maptype = "terrain") %>%
  ggmap() +
  geom_sf(data = osm_trails$osm_lines,
          inherit.aes = FALSE, colour = "grey60") +
  geom_point(data = tree_list_focal,
             colour = "red", shape = 4,
           aes(long_dd, lat_dd)) +
  geom_text(data = tree_list_focal,
             aes(long_dd, lat_dd, label = tree_id), hjust = -0.5) +
  theme_void()
```

![](figures/2021-05-28_create-map/ggmap-1.png)<!-- -->

## Make an interactive map

### make the data a SpatialPointsDataFrame object

``` r
SpatialPointsDataFrame(coords = select(tree_list, long_dd, lat_dd), 
                       data = tree_list, 
                       proj4string = CRS("+init=epsg:4326")) -> tree_list_sp
```

### make a map with leaflet

This is interactive in RStudio and if we knit to html, but GitHub won’t
render it.

``` r
# colours
pal <- colorFactor(c("gold", "navy"), c("FALSE", "TRUE"))

leaflet(data = tree_list_sp)%>% 
  addCircleMarkers(label = ~tree_id, color = ~pal(focal), 
                            fillOpacity = 0.8, stroke = FALSE, radius = 5) %>% 
  addTiles() %>%
  addLegend(pal = pal, values = ~focal)
```

![](figures/2021-05-28_create-map/leaflet-map-1.png)<!-- -->

### make a geoJSON file

We can export the SpatialPointsDataFrame to a geoJSON, which GitHub will
render as an interactive map, annotated with our geodata. This gives us
a browsable, online version we can refer to.

``` r
# convert to geojson
tree_list_geojson <- geojson_json(tree_list_sp)
```

    ## Warning: 'geojsonlint' not installed, skipping GeoJSON linting

``` r
# write
geojson_write(tree_list_geojson, file = "figures/2021-05-28_create-map/focal-trees-map.geojson")
```

    ## Success! File is at figures/2021-05-28_create-map/focal-trees-map.geojson

    ## <geojson-file>
    ##   Path:       figures/2021-05-28_create-map/focal-trees-map.geojson
    ##   From class: json

**view the interactive map online**
[**here**](figures/2021-05-28_create-map/focal-trees-map.geojson)
