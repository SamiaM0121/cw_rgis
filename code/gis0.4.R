if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               terra,
               tidyterra,
               mapview,
               stars,
               here)


## read/export raster data

(spr_ex <- rast(here("data/spr_example.tif")))


## export geotiff
writeRaster(spr_ex,
            filename = "data/spr_elev.tif",
            overwrite = TRUE)

## mapping
ggplot() +
  geom_spatraster(data = spr_ex)

## mapview function
star_ex <- st_as_stars(spr_ex)
mapview(star_ex)

# raster data type


v_elev <- values(spr_ex)
head(v_elev, 10)

na.omit(v_elev) %>% 
  mean()

## extract data from a given location
## xy specifies longitude/latitude
xy <- cbind(6.0000, 50.0000)
extract(spr_ex, xy)

#3 xy cam be multiple sites
df_point <- tibble(lon = c(6, 5.9),
                   lat = c(50, 49.96))

extract(spr_ex,
        y = df_point)

## discrete raster
(spr_for <- rast("data/spr_forest_nc.tif"))

ggplot() +
  geom_spatraster(data = spr_for)

unique(spr_for)

v_binary <- values(spr_for)
mean(v_binary)

## discrete, coded values
spr_land <- rast("data/spr_land_reclass.tif")
unique(spr_land)

extract(spr_land, cbind(-79.8063, 36.0701))


## reclass

## create a conversation matrix
cm <- cbind(c(0, 1001, 1010, 1100),
      c(0, 1, 0, 0))

# reclass
spr_bin <- classify(spr_land,
         rcl = cm)

unique(spr_bin)

v_bin <- values(spr_bin)
mean(v_bin)


## exercise 1
(spr_prec_ncne <- rast(here("data/spr_prec_ncne.tif")))

## exercise 2
# Number of rows/columns (162 for the row & 532 for the column)
# Resolution (0.008333333 for both)
# Spatial extent (-79.89181, -75.45847, 35.24153, 36.59153)
# Coordinate Reference System (The WGS is 84 and the EPSG is 4326
# Minimum and Maximum Values (The min is 1063.1 & the max is 1501.5)

## exercise 3
ggplot() +
  geom_spatraster(data = spr_prec_ncne)

## exercise 4
(sf_site <- readRDS("data/sf_finsync_nc.rds"))
df_xy <- st_coordinates(sf_site)
df_land <- extract(spr_land,
        y = df_xy)

## exercise 5
cm <- cbind(c(0, 1001, 1010, 1100),
            c(0, 0, 0, 1))
spr_urban <- classify(spr_land,
                    rcl = cm)
v_bin <- values(spr_urban)
mean(v_bin)
