if(!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               mapview,
               here)


# read export/vector data

# read a shapefile (e.g., ESRI Shapefile format)
# `quiet = TRUE` just for cleaner output
(sf_nc_county <- st_read(dsn = here("data/nc.shp"),
                         quiet = TRUE))

# export as shp
st_write(sf_nc_county,
         dsn = here("data/sf_nc_county.shp"),
         append = FALSE)

#export as geopackage
st_write(sf_nc_county,
         dsn = here("data/sf_nc_county.gpkg"),
         append = FALSE)

# export as rds
saveRDS(sf_nc_county,
        file = here("data/sf_nc_county.rds"))

# read rds 
sf_nc_county <- readRDS(file = here("data/sf_nc_county.rds"))

# Point data
## as is
sf_site <- readRDS(file = here("data/sf_finsync_nc.rds"))

mapview(sf_site,
        col.regions = "black", # point's fill color
        legend = FALSE) # disable legend

sf_site10 <- sf_site %>% 
  slice(1:10)

mapview(sf_site10,
        col.regions = "black", # point's fill color
        legend = FALSE) # disable legend

# line data
(sf_str <- readRDS (here("data/sf_stream_gi.rds")))
mapview(sf_str,
        color = "steelblue", # line's color
        legend = FALSE) # disable legend

## take the first 10 site
sf_str10 <- sf_str %>% 
  slice(1:10)



mapview(sf_str10,
        color = "steelblue", # line's color
        legend = FALSE) # disable legend

# polygon
mapview(sf_nc_county,
        col.regions = "tomato",
        legend = FALSE)

## pick guilford county
sf_nc_gi <- sf_nc_county %>% 
  filter(county == "guilford")

mapview(sf_nc_gi,
        col.regions = "salmon",
        legend = FALSE)

# use ggplot to visualize a map
## not a great map
ggplot() +
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_str) +
  geom_sf(data = sf_site)

## a litter better
ggplot() +
  geom_sf(data = sf_nc_gi) +
  geom_sf(data = sf_str)




# exercise 1
sf_str_as <- readRDS(here("data/sf_stream_as.rds"))

# exercise 2
# type your answer
# CRS fpr sf_str_as:
# CRS for sf_nc_county:
print(sf_str_as)
print(sf_nc_county)

# exercise 4
ggplot() +
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_str_as)

# exercise 5
sf_nc_as <- sf_nc_county %>% 
  filter(county == "ashe")

ggplot() +
  geom_sf(data = sf_nc_as) +
  geom_sf(data = sf_str_as)
