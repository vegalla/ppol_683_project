## Set up ----------------------------------------------------------------------
library(tidyverse)
library(sf)
library(plm)

## Load data -------------------------------------------------------------------

dc <-
  st_read('data/processed/dc_processed.shp') %>%
  st_drop_geometry() %>%
  as_tibble()

va <-
  st_read('data/processed/va_processed.shp') %>%
  st_drop_geometry() %>%
  as_tibble()

## Two Way Fixed Effects Model -------------------------------------------------

dc_twfe <-
  plm(
    gentrfd ~ 
      mdn_nc_ + 
      mdn_hs_ + 
      pct_sngl_f + 
      pct_sngl_m + 
      fertlty + 
      pct_bch + 
      pct_br_ + 
      pct_n__,
    data = dc,
    index = c('GEOID', 'year'),
    model = "within",
    effect = "twoways")

summary(dc_twfe)

va_twfe <-
  plm(
    gentrfd ~ 
      mdn_nc_ + 
      mdn_hs_ + 
      pct_sngl_f + 
      pct_sngl_m + 
      fertlty + 
      pct_bch + 
      pct_br_ + 
      pct_n__,
    data = va,
    index = c('GEOID', 'year'),
    model = "within",
    effect = "twoways")

summary(va_twfe)
