## Set up ----------------------------------------------------------------------
library(tidyverse)
library(sf)
library(rmapshaper)

## Load data -------------------------------------------------------------------

# DC

dc_shp <-
  st_read('data/raw/spatial/census.shp') %>%
  
  # Filter to DC shape
  
  filter(state_name == 'DC') %>%
  
  # Keep only GEOID and geometry
  
  dplyr::select(GEOID) %>%
  
  # Project to CRS 4326
  
  st_transform(crs = 4326) %>% 
  
  # Simplify shape to reduce size
  
  rmapshaper::ms_simplify(keep = 0.10)

# Norfolk, Virginia

va_shp <-
  st_read('data/raw/spatial/census.shp') %>%
  
  # Filter to Norfolk, VA shape, subtract GEOID in ocean
  
  filter(state_name == 'Virginia',
         COUNTYFP == '710',
         GEOID != 51710990000) %>%
  
  # Keep only GEOID and geometry
  
  dplyr::select(GEOID) %>%
  
  # Project to CRS 4326
  
  st_transform(crs = 4326) %>% 
  
  # Simplify shape to reduce size
  
  rmapshaper::ms_simplify(keep = 0.10)

## Load gentrification data from NCRC interactive map --------------------------

# I could use purrr::map here, but decide not to.

# DC

dc_label <-
  read_csv(
    'data/raw/label/ncrc_dc_gentrification.csv',
    col_types =  cols(
      GEOID = col_character(),
      gentrified = col_double(),
      year_gentrified = col_double()))

# VA

va_label <-
  read_csv(
    'data/raw/label/ncrc_va_gentrification.csv',
    col_types =  cols(
      GEOID = col_character(),
      gentrified = col_double(),
      year_gentrified = col_double()))

## Load ACS features data ------------------------------------------------------

# DC

dc_features <-
  read_csv(
    'data/processed/dc_features.csv',
    col_types =  cols(
      GEOID = col_character()))

va_features <-
  read_csv(
    'data/processed/va_features.csv',
    col_types =  cols(
      GEOID = col_character()))

## Merge data to Census Tract --------------------------------------------------

# DC

dc <-
  dc_shp %>%
    left_join(
      dc_features,
      by = 'GEOID') %>%
    left_join(
      dc_label,
      by = 'GEOID') %>%
  mutate(gentrified = replace_na(gentrified, 0),
         gentrified = replace(gentrified, year_gentrified == 2017, 2)) %>%
  select(-year_gentrified)

# VA

va <-
  va_shp %>%
  left_join(
    va_features,
    by = 'GEOID') %>%
  left_join(
    va_label,
    by = 'GEOID') %>%
  mutate(gentrified = replace_na(gentrified, 0),
         gentrified = replace(gentrified, year_gentrified == 2017, 2)) %>%
  select(-year_gentrified)


## Export data -----------------------------------------------------------------

export = FALSE

if (export == TRUE){
  
  dc %>%
    st_write('data/processed/dc_processed.shp')
  
  va %>%
    st_write('data/processed/va_processed.shp')
} 
