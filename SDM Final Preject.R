if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               ggeffects,
               sf,
               terra,
               tidyterra,
               exactextractr,
               mapview,
               here)

(df_finsync <- read_csv("data/data_finsync_nc.csv"))


(df_st1 <- df_finsync %>% 
    filter(site_id == "finsync_nrs_nc-10013"))

df_finsync %>% 
  pivot_wider(id_cols = c(site_id, lon, lat),
              names_from = latin,
              values_from = presence,
              values_fill = 0)


(df_rbs <- df_finsync %>% 
    mutate(presence = 1) %>% 
    pivot_wider(id_cols = c(site_id, lon, lat),
                names_from = latin,
                values_from = presence,
                values_fill = 0) %>% 
    select(site_id,
           lon,
           lat,
           "Nocomis micropogon") %>% 
    rename(y = "Nocomis micropogon")) 

sf_rbs <- st_as_sf(df_rbs,
                   coords = c("lon", "lat"),
                   crs = 4326)


(spr_tmp_nc <- rast(here("data/spr_tmp_nc.tif")))
                    

(sf_rbs_w_tmp <- extract(x = spr_tmp_nc,
                       y = sf_rbs,
                       bind = TRUE) %>% 
  st_as_sf())

ggplot() +
  geom_spatraster(data = spr_tmp_nc) +
  geom_sf(data = sf_rbs_w_tmp,
          aes(color = factor(y))) +
  scale_fill_viridis_c()


df_rbs_w_tmp <- as_tibble(sf_rbs_w_tmp)

df_rbs_w_tmp %>% 
  ggplot(aes(x = temperature,
             y = y)) +
  geom_point() +
  theme_bw()

(m_rbs <- glm(y ~ temperature,
             data = df_rbs_w_tmp,
             family = "binomial"))

summary(m_rbs)


df_pred <- ggpredict(m_rbs,
                     terms = "temperature [all]")
                     
ggplot() +
  geom_point(data = df_rbs_w_tmp,
             aes(x = temperature,
                 y = y)) +
  geom_line(data = df_pred,
            aes(x = x,
                y = predicted)) +
  geom_ribbon(data = df_pred,
              aes(x = x,
                  ymin = conf.low,
                  ymax = conf.high),
              fill = "grey",
              alpha = 0.4) +
  
  labs(x = "Air temperature",
       y = "Probability of occurance")
theme_bw()

