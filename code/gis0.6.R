if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               terra,
               tidyterra,
               exactextractr,
               here)

## finsync survey site
sf_site <- readRDS(here("data/sf_finsync_nc.rds"))

## county polygons
sf_nc_county <- readRDS(here("data/sf_nc_county.rds"))

spr_nw <- rast(here("data/spr_prec_ncnw.tif"))
spr_ne <- rast(here("data/spr_prec_ncne.tif"))
spr_sw <- rast(here("data/spr_prec_ncsw.tif"))
spr_se <- rast(here("data/spr_prec_ncse.tif"))

list_spr <- list(spr_nw, spr_ne, spr_sw, spr_se)
spr_col <- sprc(list_spr)

## precipitation raster
spr_prec_nc <- merge(spr_col)                      

## pointwise extraction
ggplot() +
  geom_spatraster(data = spr_prec_nc) +
  geom_sf(data = sf_site) +
  scale_fill_viridis_c()
(sf_site_prec <- extract(x = spr_prec_nc,
                         y = sf_site,
                         bind = TRUE) %>% 
    st_as_sf())

ggplot() +
  geom_sf(data = sf_nc_county,
          fill = "grey") +
  geom_sf(data = sf_site_prec,
          aes(color = precipitation)) +
  scale_color_viridis_c() +
  theme_bw()


## zonal statistics

## transform sf_site to 32617: use st_transform()
sf_nc_county_proj <- sf_nc_county %>% 
  st_transform(crs = 32617)
                                 

## transform spr_prec_nc to 32617: use project()
spr_prec_nc_proj <- project(spr_prec_nc, 
                            y = "EPSG: 32617",
                            method = "bilinear")

df_prec_county <- exact_extract(x = spr_prec_nc_proj,
              y = sf_nc_county_proj,
              fun = "mean",
              append_cols = TRUE) %>% 
  as_tibble() %>% 
  rename(precipitation = mean)

df_prec_county_sd <- exact_extract(x = spr_prec_nc_proj,
                                y = sf_nc_county_proj,
                                fun = "stdev",
                                append_cols = TRUE) %>% 
  as_tibble()


sf_nc_county_prec <- left_join(sf_nc_county,
          df_prec_county,
          by = "county")

ggplot() +
  geom_sf(data = sf_nc_county_prec,
          aes(fill = precipitation)) +
  scale_fill_viridis_c() +
  theme_bw()

## buffer analysis

## transform CRS
sf_site_proj <- sf_site %>% 
  st_transform(crs = 32617)


## create buffers around the points
sf_site_buff_proj <- sf_site_proj %>% 
  st_buffer(dist = 10000)

ggplot() +
  geom_sf(data = sf_nc_county_proj) +
  geom_sf(data = sf_site_buff_proj) +
  geom_sf(data = sf_site_proj)

## get the mean precipitation for each site buffer
df_prec_buff <- exact_extract(x = spr_prec_nc_proj,
                            y = sf_site_buff_proj,
                            fun = "mean",
                            append_cols = TRUE) %>% 
  as_tibble() %>% 
  rename(precipitation = mean)

## link these values to site layer
sf_site_prec_buff <- sf_site %>%
  left_join(df_prec_buff,
            by = "site_id")

## map the precipitation value at each site
ggplot() +
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_site_prec_buff,
          aes(color = precipitation)) +
  scale_color_viridis_c() +
  theme_bw()
