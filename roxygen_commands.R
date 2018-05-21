library(devtools)
library(roxygen2)

# setwd("B:/github/baylisR")
setwd("~/github/baylisR")
# create("baylisR")
document()
install(".")

# install_github("pbaylis/baylisR", auth_token="ebb64529e5a9127f17624c261dbd727b4ab7c9e6")


# Below is some old code to rasterize a timezones shapefile for use in the package.
# It won't work right now, just keeping it in case I need it someday. Smarter would be to put it somewhere else.
# library(raster)
# library(fasterize)
# library(sf)
#
# db.dir <- "B:/Dropbox/"
# # db.dir <- "~/Dropbox"
# geo.dir <- file.path(db.dir, "01_Work/01_Current/99_Common/geo")
# rast.path <- file.path(db.dir, "01_Work/01_Current/20_Greenspace/data/Greenness_Measures/sample_NDVI_measurements/1km")
#
# # Load raster
# rast <- raster(file.path(rast.path, "MYD13A2.A2015249.1_km_16_days_NDVI.tif"))
#
# # Load shapefile
# tz.shp <- st_read(file.path(geo.dir, "tz_us"))
#
# tz.rast <- fasterize(tz.shp, rast, field="TZID")
# levels.tz <- levels(tz.shp$TZID)
# levels(tz.rast) <- data.frame(ID=0:(length(levels.tz)-1), TZID=levels.tz)
#
# writeRaster(tz.rast, file.path(geo.dir, "tz_us/tz_us_MODIS1k_rasterized.tif"), overwrite=T)
#
# tz.rast <- raster(file.path(geo.dir, "tz_us/tz_us_MODIS1k_rasterized.tif"))
#
# devtools::use_data(tz.rast, pkg=".", internal = TRUE, overwrite=T)
#
# tz.rast
#
# rm(list=ls())
