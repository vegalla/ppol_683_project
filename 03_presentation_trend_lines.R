## Setup -----------------------------------------------------------------------
library(tidyverse)
library(sf)

## Load data -------------------------------------------------------------------

dc <-
  st_read('data/processed/dc_processed.shp')

colnames(dc)

## Trend Lines -----------------------------------------------------------------
# Theme_void() for trend line, axis are not important for this slide
# Graphics copied from plot panel

# Income

dc %>%
  as_tibble() %>%
  group_by(year) %>%
  summarize(
    min_income = min(mdn_nc_, na.rm = TRUE),
    mean_income = mean(mdn_nc_, na.rm = TRUE),
    max_income = max(mdn_nc_, na.rm = TRUE)) %>%
  ggplot(aes(x = year)) +
    geom_line(aes(y = min_income)) +
    geom_line(aes(y = mean_income, color = 'red')) +
    geom_line(aes(y = max_income)) +
    labs(
      y = "",
      x = "") +
    theme_void() +
    theme(legend.position = "none")

# House value

dc %>%
  as_tibble() %>%
  group_by(year) %>%
  summarize(
    min_house = min(mdn_hs_, na.rm = TRUE),
    mean_house = mean(mdn_hs_, na.rm = TRUE),
    max_house = max(mdn_hs_, na.rm = TRUE)) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = min_house)) +
  geom_line(aes(y = mean_house, color = 'red')) +
  geom_line(aes(y = max_house)) +
  labs(
    y = "",
    x = "") +
  theme_void() +
  theme(legend.position = "none")

# Percent Single Parent

dc %>%
  as_tibble() %>%
  group_by(year) %>%
  summarize(
    mean_single_father = mean(pct_sngl_f, na.rm = TRUE),
    mean_single_mother = mean(pct_sngl_m, na.rm = TRUE))%>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = mean_single_father)) +
  geom_line(aes(y = mean_single_mother, color = 'red')) +
  labs(
    y = "",
    x = "") +
  theme_void() +
  theme(legend.position = "none")

# Percent College Educated

dc %>%
  as_tibble() %>%
  group_by(year) %>%
  summarize(
    mean_college_educated = mean(pct_bch, na.rm = TRUE))%>%
  ggplot(aes(x = year)) +
    geom_line(aes(y = mean_college_educated, color = 'red')) +
    labs(
      y = "",
      x = "") +
    theme_void() +
    theme(legend.position = "none")
