## Set up ----------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(purrr)

## Process DC Data -------------------------------------------------------------

# Income

dc_income <-
  purrr::map(
    (2010:2017),
    function(x){
      
      ## Set data path
      
      path = paste0('data/raw/acs/dc/income/ACSST5Y',as.character(x),'.S1901_data_with_overlays_2021-12-01T145310.csv')
      
      ## Read raw data
      
      dc_income_raw <-
        read_csv(path) %>%
        janitor::clean_names()
      
      ## Process data
      
      dc_income_processed <-
        dc_income_raw %>%
        
        ## Subset to GEOID and Median Income of Household
        
        select(geo_id, s1901_c01_012e) %>%
        
        ## Filter out first observation (labels)
        
        slice(2:n()) %>%
        
        ## Add year, correct values and formats
        
        mutate(
          year = x,
          GEOID = str_extract(geo_id, "[0-9]{11}$"),
          median_income_hh = as.numeric(s1901_c01_012e)) %>%
        select(GEOID, year,median_income_hh)

      }) %>% 
  bind_rows()

# House Value

dc_house_value <-
  purrr::map(
    (2010:2017),
    function(x){
      
      ## Set data path

      path = paste0('data/raw/acs/dc/house_value/ACSST5Y', 
                    as.character(x),
                    '.S2506_data_with_overlays_2021-12-01T151340.csv')
      
      ## Read raw data
      
      dc_house_value_raw <-
        read_csv(path) %>%
        janitor::clean_names()
      
      ## Process data
      
      dc_house_value_processed <-
        dc_house_value_raw %>%
        
        ## Subset to GEOID and Median Home Value of Owner-Occupied
        
        select(geo_id, s2506_c01_009e) %>%
        
        ## Filter out first observation (labels)
        
        slice(2:n()) %>%
        
        ## Add year, correct values and formats
        # Upper limit to household value of 1000000
        
        mutate(
          year = x,
          GEOID = str_extract(geo_id, "[0-9]{11}$"),
          median_house_value = as.numeric(
            s2506_c01_009e %>%
              str_replace_all(',','') %>%
              str_replace_all('\\+',''))) %>%
        select(GEOID, year,median_house_value)
      
    }) %>% 
  bind_rows()

# Social Characteristics

dc_social <-
  purrr::map(
    (2010:2017),
    function(x){
      
      ## Set data path
      
      path = paste0('data/raw/acs/dc/social/ACSDP5Y', 
                    as.character(x),
                    '.DP02_data_with_overlays_2021-11-29T151702.csv')
      
      ## Read raw data
      
      dc_social_raw <-
        read_csv(path) %>%
        janitor::clean_names()
      
      ## Process data
      
      dc_social_processed <-
        dc_social_raw %>%
        
        ## Subset to GEOID and selected social characteristics
        # DP02_0007PE - Single father
        # DP02_0009PE - Single mother
        # DP02_0039E - fertility
        # DP02_0064PE - Bachelor's degree (25 or older)
        # DP02_0088PE - Born in us 
        # DP02_0111PE - Speaks only English
        
        select(
          geo_id, 
          dp02_0007pe, 
          dp02_0009pe, 
          dp02_0039e, 
          dp02_0064pe, 
          dp02_0088pe, 
          dp02_0111pe) %>%
      
        
        ## Filter out first observation (labels)
        
        slice(2:n()) %>%
        
        ## Add year
        
        mutate(
          year = x,
          GEOID = str_extract(geo_id, "[0-9]{11}$"),
          pct_single_father = as.numeric(dp02_0007pe), 
          pct_single_mother = as.numeric(dp02_0009pe), 
          fertility = as.numeric(dp02_0039e), 
          pct_bachelors = as.numeric(dp02_0064pe), 
          pct_born_us = as.numeric(dp02_0088pe), 
          pct_only_speaks_english = as.numeric(dp02_0111pe)) %>%
          
        select(
          GEOID, year, 
          pct_single_father, 
          pct_single_mother, 
          fertility, 
          pct_bachelors,
          pct_born_us,
          pct_only_speaks_english)
      
    }) %>% 
  bind_rows()

## Merge All

dc_features <-
  dc_income %>%
    left_join(
      dc_house_value,
      by = c('GEOID', 'year')) %>%
    left_join(
      dc_social,
      by = c('GEOID', 'year'))

## Export data -----------------------------------------------------------------

# Set to False to prevent overwriting
export = FALSE

if (export == TRUE) {
  dc_features %>%
    write_csv('data/processed/dc_features.csv')
}

