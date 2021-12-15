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

calc_morans_i <-
  function(x){
    
    # Calculate Distance Matrix
    
    dist_matrix <-
      x %>% 
        st_coordinates() %>% 
        geodist::geodist() 
    
    inv_dist_matrix <-
      dist_matrix
    
    diag(inv_dist_matrix) <- 0
    
    morans_i_result <-
      ape::Moran.I(
        x = x$gentrfd,
        weight = inv_dist_matrix)
    
    morans_i_result
  }

# DC - ever gentrified: spatial autocorrelation --------------------------------

# Convert to points:

dc_ever_gentrified_pts <-
  dc %>%
  select(GEOID, gentrfd) %>%
  
  # Convert gentrified to binary value, 2 becomes 1
  
  mutate(gentrfd = replace(gentrfd, gentrfd != 0, 1)) %>%
  distinct() %>%
  st_centroid()

# Moran's I

calc_morans_i(dc_ever_gentrified_pts)

# DC - 2012 gentrified: spatial autocorrelation --------------------------------

# Convert to points:

dc_2012_gentrified_pts <-
  dc %>%
  select(GEOID, gentrfd) %>%
  
  # Convert gentrified to binary value, 2 becomes 0
  
  mutate(gentrfd = replace(gentrfd, gentrfd != 1, 0)) %>%
  distinct() %>%
  st_centroid()

# Moran's I

calc_morans_i(dc_2012_gentrified_pts)

# DC - 2017 gentrified: spatial autocorrelation --------------------------------

# Convert to points:

dc_2017_gentrified_pts <-
  dc %>%
  select(GEOID, gentrfd) %>%
  
  # Convert gentrified to binary value, 1 becomes 0
  
  mutate(gentrfd = replace(gentrfd, gentrfd != 2, 0)) %>%
  distinct() %>%
  st_centroid()

# Moran's I

calc_morans_i(dc_2017_gentrified_pts)

# VA - ever gentrified: spatial autocorrelation --------------------------------

va_ever_gentrified_pts <-
  va %>%
  select(GEOID, gentrfd) %>%
  mutate(gentrfd = replace(gentrfd, gentrfd != 0, 1)) %>%
  distinct() %>%
  st_centroid()

# Moran's I 

calc_morans_i(va_ever_gentrified_pts)

## If I needed these results in a dataframe, I would use purrr::map;
#  however, I do not, so I am satisfied which using this function several times.
