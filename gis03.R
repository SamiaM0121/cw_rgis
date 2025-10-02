

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               mapview,
               here)




sf_site <- readRDS("data/sf_finsync_nc.rds")

## polygon vector
sf_nc_county <- readRDS(file = here("data/sf_nc_county.rds"))

## st_join() evaluates two geometry layers
sf_site_join <- st_join(x = sf_site,
                        y = sf_nc_county)
       

sf_one <- sf_site %>% 
  slice(1)

mapview(sf_nc_county) + mapview(sf_one)


sf_site_guilford <- sf_site_join %>% 
  filter(county == "guilford")

sf_nc_guilford <- sf_nc_county %>% 
  filter(county == "guilford")

sf_str_guilford <- readRDS(here("data/sf_stream_gi.rds"))

# create a map
ggplot() +
  geom_sf(data = sf_nc_guilford) +
  geom_sf(data = sf_str_guilford,
          color = "steelblue") +
  geom_sf(data = sf_site_guilford,
          color = "salmon")






df_n <- sf_site_join %>% 
  as_tibble() %>% 
  group_by(county) %>% 
  summarize(n_site = n()) %>% 
  arrange(desc(n_site))






sf_nc_n <- sf_nc_county %>% 
  left_join(df_n,
            by = "county") %>% 
  mutate(n_site = ifelse(is.na(n_site),
                         0,
                         n_site))


sf_nc_n %>% 
  ggplot() +
  geom_sf(data = sf_nc_n,
          aes(fill = n_site))





sf_str_proj <- st_transform(sf_str_guilford,
             crs = 32617)

v_str_l <- st_length(sf_str_proj)
head(v_str_l)

sf_str_w_len <- sf_str_guilford %>% 
  mutate(length = as.numeric(v_str_l))

ggplot() +
  geom_sf(data = sf_str_w_len,
          aes(color = length))


sf_nc_county_proj <- st_transform(sf_nc_county,
                                  crs = 32617)

v_area <- st_area(sf_nc_county_proj)             

sf_nc_county_w_are <- sf_nc_county %>% 
  mutate(area = as.numeric(v_area))




## exercise 1
sf_quakes <- readRDS(here("data/sf_quakes.rds"))
sf_nz <- readRDS(here("data/sf_nz.rds"))
mapview(sf_quakes) + mapview(sf_nz)
sf_quakes_join <- st_join(x = sf_quakes,
                          y = sf_nz)

sf_quakes_nz <- drop_na(sf_quakes_join, fid)

nrow(sf_quakes_nz)


## exercise 2
df_n <- sf_site_join %>% 
  as_tibble() %>% 
  group_by(county) %>% 
  summarize(n_site = n()) %>% 
  arrange(desc(n_site))


sf_n_site <- sf_nc_county %>% 
  left_join(df_n,
            by = "county") %>% 
  mutate(n_site = ifelse(is.na(n_site),
                         0,
                         n_site))

## exercise 3
sf_n10 <- sf_n_site %>% 
  filter(n_site > 10)

mapview(sf_n10)
## exercise 4
ggplot() +
  geom_sf(data = sf_n_site,
          color = "grey") +
  geom_sf(data = sf_n10,
          fill = "salmon")
