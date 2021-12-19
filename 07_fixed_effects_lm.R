## Set up ----------------------------------------------------------------------
library(tidyverse)
library(sf)
library(plm)
library(pglm)
library(fixest)

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

dc_binary <-
  dc %>%
    mutate(gentrfd = replace(gentrfd, gentrfd != 0, 1))

va_binary <-
  va %>%
    mutate(gentrfd = replace(gentrfd, gentrfd != 0, 1)) 
    
# Linear Model
       
dc_twfe_lm <-
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
    dc_binary,
    index = c('GEOID', 'year'),
    model = "within",
    effect = "twoways")

summary(dc_twfe_lm)

# Linear Model

va_twfe_lm <-
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
    data = va %>%
      mutate(gentrfd = replace(gentrfd, gentrfd != 0, 1)),
    index = c('GEOID', 'year'),
    model = "within",
    effect = "twoways")

summary(va_twfe_lm)

## Attempts at two-way fixed effects logistic regression -----------------------

# pglm

# dc_twfe_glm <-
#   pglm(
#     gentrfd ~ 
#       mdn_nc_ + 
#       mdn_hs_ + 
#       pct_sngl_f + 
#       pct_sngl_m + 
#       fertlty + 
#       pct_bch + 
#       pct_br_ + 
#       pct_n__,
#     dc_binary,
#     index = c('GEOID', 'year'),
#     model = "within",
#     family = binomial("logit"),
#     effect = "twoways")
# 
# summary(dc_twfe_glm)

# fixest

# Error: The dependent variable is fully explained by the fixed-effects.
# dc_fixest <-
#   feglm(
#     gentrfd ~ 
#       mdn_nc_ + 
#       mdn_hs_ + 
#       pct_sngl_f + 
#       pct_sngl_m + 
#       fertlty + 
#       pct_bch + 
#       pct_br_ + 
#       pct_n__ | GEOID + year,
#     data = dc_binary,
#     family = 'logit')
# 
# summary(dc_fixest)
