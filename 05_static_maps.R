## Set up ----------------------------------------------------------------------
library(tidyverse)
library(sf)

## Load data -------------------------------------------------------------------

dc <-
  st_read('data/processed/dc_processed.shp')

va <-
  st_read('data/processed/va_processed.shp')

## Produce static map of NCRC Gentrified ---------------------------------------

# Preparation for equally scaled maps, centered on city centroid

padding = 0.12

dc_centroid <-
  dc %>%
  st_union() %>%
  st_centroid() %>% 
  st_geometry()

va_centroid <-
  va %>%
  st_union() %>%
  st_centroid() %>% 
  st_geometry()

# Plot DC

dc %>%
  ggplot() +
    geom_sf(
      aes(fill = factor(gentrfd))) +
    coord_sf(xlim = c(dc_centroid[[1]][1]-padding , 
                      dc_centroid[[1]][1]+padding), 
             ylim = c(dc_centroid[[1]][2]-padding , 
                      dc_centroid[[1]][2]+padding), 
             expand = FALSE) +
    labs(
      title = "Gentrification of Census Tracts, 2010 - 2017\nWashington, DC") +
    scale_fill_manual(
      name = '', 
      guide = 'legend',
      values = c('#7a95b0', '#fe3533', '#900001'),
      labels = c('Not Gentrified', 'Gentrified in 2012', 'Gentrified in 2017')) +
    theme_void()

ggsave('www/dc_ncrc_gent.png')

# Plot VA

va %>%
  ggplot() +
  geom_sf(
    aes(fill = factor(gentrfd))) +
  coord_sf(xlim = c(va_centroid[[1]][1]-padding , 
                    va_centroid[[1]][1]+padding), 
           ylim = c(va_centroid[[1]][2]-padding , 
                    va_centroid[[1]][2]+padding), 
           expand = FALSE) +
  labs(
    title = "Gentrification of Census Tracts, 2010 - 2017\nNorfolk, VA") +
  scale_fill_manual(
    name = '', 
    guide = 'legend',
    values = c('#7a95b0', '#fe3533', '#900001'),
    labels = c('Not Gentrified', 'Gentrified in 2012', 'Gentrified in 2017')) +
  theme_void()

ggsave('www/va_ncrc_gent.png')
