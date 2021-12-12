## Set up ----------------------------------------------------------------------
library(sf)
library(tidyverse)
library(png)
library(magick)

## Load data -------------------------------------------------------------------

dc <-
  st_read('data/raw/spatial/census/census.shp') %>%
  filter(state_name == 'DC') %>%
  dplyr::select(GEOID) %>%
  st_transform(crs = 4326) %>% 
  rmapshaper::ms_simplify(keep = 0.10)

dc_prj <-
  dc %>%
  st_transform(crs = 26985)

ortho_bbox <-
  c(xmin = 389400,
    ymin = 124200.00000000042,
    xmax = 408599.9999999995,
    ymax = 148199.99999999977)

names(ortho_bbox) <-
  c("xmin", "ymin", "xmax", "ymax")

attr(ortho_bbox, "class") = "bbox"

attr(st_geometry(dc_prj), "bbox") = ortho_bbox


plot(dc)
plot(dc_prj)

ncrc_gent <-
  read_csv('data/raw/project/ncrc_dc_gentrification.csv',
          col_types =  cols(
             GEOID = col_character(),
             gentrified = col_double(),
             year_gentrified = col_double()))

gent_shp <-
  dc_prj %>%
  left_join(
    ncrc_gent %>%
      filter(year_gentrified == 2017),
    by = 'GEOID') 

crimes_raster <-
  crimes_prj %>% 
  dplyr::select(-everything()) %>% 
  st_transform(crs = st_crs(rasters)) %>%
  as_Spatial() %>% 
  raster::rasterize(
    rasters, # These are the cells that the points are rasterized to
    fun = 'count', 
    background = 0)

crimes_raster %>% 
  raster::aggregate(
    fact = 33,
    fun = sum) %>% 
  raster::plot()

plot(gent_shp)

gent_shp %>%
  select(gentrified)

dc_prj

dc_shp_raster <-
  raster::rasterize(dc_prj)

img <-
  png::readPNG('data/raw/project/img/Aerial_Photography_Image_Service_Orthophoto_2013.png')

ortho_2015 <-
  image_read('data/raw/project/img/Aerial_Photography_Image_Service_Orthophoto_2013.png')

print(ortho_2015)

img.n <- 
  readPNG('data/raw/project/img/Aerial_Photography_Image_Service_Orthophoto_2013.png', TRUE)

raster::plot(img.n)

if (exists("rasterImage")) { # can plot only in R 2.11.0 and higher
  plot(1:2, type='n')
  
  if (names(dev.cur()) == "windows") {
    # windows device doesn't support semi-transparency so we'll need
    # to flatten the image
    transparent <- img[,,4] == 0
    img <- as.raster(img[,,1:3])
    img[transparent] <- NA
    
    # interpolate must be FALSE on Windows, otherwise R will
    # try to interpolate transparency and fail
    rasterImage(img, 1.2, 1.27, 1.8, 1.73, interpolate=FALSE)
    
  } else {
    # any reasonable device will be fine using alpha
    rasterImage(img, 1.2, 1.27, 1.8, 1.73)
    rasterImage(img.n, 1.5, 1.5, 1.9, 1.8)
    
  }
}

rasters <-
  read_rds('data/raw/rasters/dc_lc.rds')

check <-
  rasters$canopy_cover

plot(as.raster(ortho_2015))

rasterImage()

check <-
  plot(dc_prj)

check$usr

rasterImage(ortho_2015, 
            383036.9,
            414963.1,
            123240.0, 
            149160.0)

ortho_2015_raster <-
  as.raster(img.n)

band1 <-
  raster::raster('data/raw/project/img/Aerial_Photography_Image_Service_Orthophoto_2015.png',
                 band = 1)

band2 <-
  raster::raster('data/raw/project/img/Aerial_Photography_Image_Service_Orthophoto_2015.png',
                 band = 2)

band3 <-
  raster::raster('data/raw/project/img/Aerial_Photography_Image_Service_Orthophoto_2015.png',
                 band = 3)


band1[band1 == 0] <- NA

band2[band2 == 0] <- NA

band3[band3 == 0] <- NA

raster::plot(band1)

raster::plot(band2)

raster::plot(band3)

raster::plot(band4)
raster::projection(band1)

st_crs(dc_prj)$proj4string

raster::crs(band1) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"

raster::plot(band1)

band1

dc_prj <-
  dc %>%
  st_transform(crs = st_crs(band1))

plot(dc_prj)

dc_prj


raster::projection(band1)
