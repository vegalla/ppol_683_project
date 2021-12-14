## Set up ----------------------------------------------------------------------
library(tidyverse)
library(sf)
library(geodist)
library(ape)

## Load data -------------------------------------------------------------------

dc <-
  st_read('data/processed/dc_processed.shp')

va <-
  st_read('data/processed/va_processed.shp')

## Analyze auto correlation ----------------------------------------------------

# DC - ever gentrified: spatial autocorrelation --------------------------------

# Convert to points:

dc_pts <-
  dc %>%
  select(GEOID, gentrfd) %>%
  mutate(gentrfd = replace(gentrfd, gentrfd != 0, 1)) %>%
  distinct() %>%
  st_centroid()

# Moran's I

dc_dist_matrix <-
  dc_pts %>% 
  st_coordinates() %>% 
  geodist::geodist() 

# calculate the inverse distance matrix

dc_inv_dist_matrix <-
  1/dc_dist_matrix

# set diagonals to zero

diag(dc_inv_dist_matrix) <- 0

ape::Moran.I(
  x = dc_pts$gentrfd,
  weight = dc_inv_dist_matrix)

# DC - 2012 gentrified: spatial autocorrelation --------------------------------

# Convert to points:

dc_pts <-
  dc %>%
  select(GEOID, gentrfd) %>%
  mutate(gentrfd = replace(gentrfd, gentrfd != 1, 0)) %>%
  distinct() %>%
  st_centroid()

# Moran's I

dc_dist_matrix <-
  dc_pts %>% 
  st_coordinates() %>% 
  geodist::geodist() 

# calculate the inverse distance matrix

dc_inv_dist_matrix <-
  1/dc_dist_matrix

# set diagonals to zero

diag(dc_inv_dist_matrix) <- 0

ape::Moran.I(
  x = dc_pts$gentrfd,
  weight = dc_inv_dist_matrix)

# DC - 2017 gentrified: spatial autocorrelation --------------------------------

# Convert to points:

dc_pts <-
  dc %>%
  select(GEOID, gentrfd) %>%
  mutate(gentrfd = replace(gentrfd, gentrfd != 2, 0)) %>%
  distinct() %>%
  st_centroid()

# Moran's I

dc_dist_matrix <-
  dc_pts %>% 
  st_coordinates() %>% 
  geodist::geodist() 

# calculate the inverse distance matrix

dc_inv_dist_matrix <-
  1/dc_dist_matrix

# set diagonals to zero

diag(dc_inv_dist_matrix) <- 0

ape::Moran.I(
  x = dc_pts$gentrfd,
  weight = dc_inv_dist_matrix)

# VA - ever gentrified: spatial autocorrelation --------------------------------

va_pts <-
  va %>%
  select(GEOID, gentrfd) %>%
  mutate(gentrfd = replace(gentrfd, gentrfd != 0, 1)) %>%
  distinct() %>%
  st_centroid()

# Moran's I 

va_dist_matrix <-
  va_pts %>% 
  st_coordinates() %>% 
  geodist::geodist() 

# calculate the inverse distance matrix

va_inv_dist_matrix <-
  1/va_dist_matrix

# set diagonals to zero

diag(va_inv_dist_matrix) <- 0

ape::Moran.I(
  x = va_pts$gentrfd,
  weight = va_inv_dist_matrix)
