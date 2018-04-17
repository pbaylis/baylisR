library(devtools)
library(roxygen2)


setwd("/home/pbaylis/github/baylisR")
# setwd("B:/github/baylisR")
# create("baylisR")
document()
install(".")
#install_github("pbaylis/baylisR", auth_token="ebb64529e5a9127f17624c261dbd727b4ab7c9e6")

# rasterize tz.shp
library(raster)
library(fasterize)
library(sf)

db.dir <- "B:/Dropbox/"
# db.dir <- "~/Dropbox"
geo.dir <- file.path(db.dir, "01_Work/01_Current/99_Common/geo")
rast.path <- file.path(db.dir, "01_Work/01_Current/20_Greenspace/data/Greenness_Measures/sample_NDVI_measurements/1km")

# Load raster
rast <- raster(file.path(rast.path, "MYD13A2.A2015249.1_km_16_days_NDVI.tif"))

# Load shapefile
tz.shp <- st_read(file.path(geo.dir, "tz_us"))

tz.rast <- fasterize(tz.shp, rast, field="TZID")
levels.tz <- levels(tz.shp$TZID)
levels(tz.rast) <- data.frame(ID=0:(length(levels.tz)-1), TZID=levels.tz)

writeRaster(tz.rast, file.path(geo.dir, "tz_us/tz_us_MODIS1k_rasterized.tif"), overwrite=T)

tz.rast <- raster(file.path(geo.dir, "tz_us/tz_us_MODIS1k_rasterized.tif"))

devtools::use_data(tz.rast, pkg=".", internal = TRUE, overwrite=T)

tz.rast

rm(list=ls())

# DEBUG COMMANDS
rm(list=ls())
document()
install(".")
library(baylisR)

#tz.rast <- baylisR:::tz.rast

dt <- import_twitter_file(inFile="B:/twitter_data/usa_geo/scores/usa_geo_backup_2014-05-22_scores.csv",
                          col_types = cols_only(tweet_datetime=col_datetime(format = ""),
                                                lng=col_double(), lat=col_double(),
                                                afinn="?", vaderPos="?", vaderNeg="?"))

get_local_time(dt, debug=T)
