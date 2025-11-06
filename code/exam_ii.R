# Exam II
# By submitting this exam on time, you will obtain 55 points
# 15 questions in total, with each worth 3 points
# Points will be awarded if your code produces the expected result(s)

if (!require(pacman)) install.packages("pacman")
library(pacman)

# call packages -----------------------------------------------------------

# Execute the following lines of code to call packages
p_load(tidyverse,
       sf,
       terra,
       exactextractr,
       tidyterra)

# To answer the following questions, use the data below:
df_site <- read_csv("data/data_finsync_nc.csv") %>% 
  distinct(site_id, 
           lon, 
           lat)

sf_nc_county <- readRDS("data/sf_nc_county.rds")

# vector data analysis ----------------------------------------------------

# Q1. 
# `df_site` currently has no coordinate reference system (CRS). 
# Convert it to an `sf` object and assign the WGS 84 CRS (EPSG: 4326). 
# Save the resulting object as `sf_site`.
sf_site <- st_as_sf(df_site,
                    coords = c("lon", "lat"),
                    crs = 4326)

# Q2.
# From `sf_nc_county`, select only the county polygons of the following counties: 
#   "guilford", "randolph", "davidson", and "forsyth". 
# Save the result as `sf_four`.

sf_four <- sf_nc_county %>% 
  filter %>% c("guilford")

# Q3. 
# Perform a spatial join to identify sites in `sf_site` that fall within 
#   the four selected counties stored in `sf_four`. 
# Make sure that the output object is a POINT layer after spatial join.
# Remove any rows without a `county` value and save the result as `sf_site_four`.
sf_site_four <- st_join(sf_site, sf_four, join = st_within, left = FALSE)

# Q4. 
# Create a map showing the four selected counties (`sf_four`) 
#   and the sampling sites (`sf_site_four`) overlaid on the same plot. 
library(ggplot2)
ggplot()+
  geom_sf(data = sf_four, fill = "lightgray", color = "black") +
  geom_sf(data = sf_site_four, aes(color = county), size = 2) +
  theme_minimal() +
  labs(
    title = "Sampling Sites within Four North Carolina Counties",
    color = "County"
  )
# Q5. 
# Calculate the pairwise distances among all sites in `sf_site_four`
#   with the appropriate CRS, UTM Zone 17N (EPSG: 32617) 
#   so that distances are measured in meters. 
# Then, find the maximum distance among all site pairs.
# 
# ENTER YOUR ANSWER HERE: max_distance
sf_sie_four_utm <- st_transform(sf_site_four, 32617)

dist_matrix <- st_distance(sf_site_four_utm)

max_distance <- max(dist_matrix)

# raster data analysis ----------------------------------------------------

# Q6. 
# The raster file "spr_land_reclass.tif" in the "data" folder 
#   contains reclassified land-cover data, 
#   where pixel values represent land-cover types as follows:
#   1001 = forest
#   1010 = crop
#   1100 = urban
#   0 = other
# 
# Load this raster as `spr_land` and display the unique land-cover codes it contains.
library(terra)
spr_land <- rast("data/spr_land_reclass.tif")

unique_values <- unique(getValues(spr_land))
unique_values

# Q7. 
# Reclassify the raster `spr_land` to create a new raster object `spr_crop` 
#   that highlights only cropland areas. 
# Use the following reclassification rules:
#   1001 = 0 (forest)
#   1010 = 1 (crop)
#   1100 = 0 (urban)
#   0 = 0 (other)
library(terra)
rcl <- matrix(c(
  0, 0, 0,
  1001, 1001, 0,
  1010, 1010, 1,
  1100, 1100, 0
  ), ncol = 3, byrow = TRUE)

spr_crop <- classify(spr_land, rcl)

unique(values(spr_crop))

# Q8. 
# Crop the cropland raster (`spr_crop`) to the extent of the four selected counties 
# (`sf_four`: "guilford", "randolph", "davidson", and "forsyth")
# Save the resulting cropped raster as `spr_crop_four`.
library(terra)
spr_crop_four <- crop(spr_crop, extent(sf_four_proj))

# Q9. 
# Create a map showing the cropped cropland raster (`spr_crop_four`) 
#   overlaid with the four counties (`sf_four`). 
# Use a semi-transparent overlay for the counties.
ggplot() +
  geom_raster(data = spr_df, aes(x, y = y, fill = data[[raster_name]])) +
  scale_fill_viridis_c(name = "Cropland", na.values = "transparent") +
  geom_sf(data = sf_four, fill = NA, color = "black", alpha = 0.5, size = 0.7) +
  theme_minimal() +
  labs(title = "Cropped Cropland Raster with County Boundaries",
       x = "Longitude", y = "latitude") +
  coord_sf()



# Q10. Calculate the proportion of cropland pixels within the four counties 
#   from the cropped raster (`spr_crop_four`). 
# Since cropland pixels are coded as 1 and others as 0, the mean gives the proportion.
#
# ENTER YOUR ANSWER HERE:0.021
# (round your answer to third decimal places, e.g., 0.021)
cropland_proportion <- cellStats(spr_crop_four, stat = "mean")
cropland_proportions <- round(cropland_proportions, 3)

# raster-vector interaction -----------------------------------------------

# Q11.
# The raster file "spr_tmp_nc.tif" in the "data" folder contains 
#   annual mean temperature (°C) data for North Carolina. 
# Load this raster and extract the temperature values 
#   at each sampling site in `sf_site`. 
# Then, identify how many sites have temperature values greater than 16°C.
#
# ENTER YOUR ANSWER HERE:num_sites_gt16
temp_raster <- raster("data/spr_tmp_nc.tif")

site_temps <- raster::extract(temp_raster, sf_site)

num_site_gt16 <- sum(site_temps > 16, na.rm = TRUE)
# Q12. Create 3-km buffers around each site in `sf_site_four` (see Q3). 
# Be sure to first transform the coordinate reference system to UTM Zone 17N (EPSG: 32617) 
# so that the buffer distance is measured in meters.
sf_site_utm <- st_transform(sf_site_four, crs = 32617)

site_buffers <- st_buffer(sf_site_utm, dist = 3000)


# Q13. Project the cropped cropland raster (`spr_crop_four`) 
# to the same UTM coordinate reference system (EPSG: 32617). 
# Use an appropriate re-sampling method in light of the raster data type.
utm_crs <- CRS("init=EPSG:32617")

spr_crop_utm <- projectRaster(spr_crop_four,
                              crs = utm_crs,
                              method = "ngb")


# Q14. Create a map displaying the projected cropland raster (`spr_crop_proj`) 
# with 3-km site buffers (`sf_buff_proj`) overlaid.
spr_crop_df <- as.data.frame(spr_crop_proj, xy = TRUE)
colnames(spr_crop_df) <- c("x", "y", "crop")

ggplot() +
  geom_raster(data = spr_crop_df, aes(x = x, y = y, fill = factor(crop))) +
  scale_fill_viridis_d(name = "Cropland Type") +   # discrete color scale for categorical raster
  geom_sf(data = sf_buff_proj, fill = NA, color = "red", size = 0.7) +
  theme_minimal() +
  labs(title = "Projected Cropland Raster with 3-km Site Buffers",
       x = "Easting (m)",
       y = "Northing (m)") +
  coord_sf()

# Q15. Calculate the proportion of cropland within each 3-km site buffer. 
# Store the result as `df_crop_frac`, and identify the `site_id` 
# with the highest cropland fraction.
crop_values_list <- extract(spr_crop_proj, sf_buff_proj)

cropland_codes <- 1

df_crop_frac <- data.frame(
  site_id = sf_buff_proj$site_id,
  crop_fraction = sapply(crop_values_list, function(x) {
    mean(x %in% cropland_codes, na.rm = TRUE)
  })
)

max_crop_site <- df_crop_frac %>%
  filter(crop_fraction == max(crop_fraction, na.rm = TRUE))

df_crop_frac
max_crop_site
