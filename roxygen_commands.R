data <- readRDS(file.path(DBDATA, "reg_cbsa.Rds"))
lhses <- c("afinn_s", "hedon_s", "liwc_s", "vader_s", "swear")
# List of weights to match lhs
weights <- list(afinn_s = "afinn_c",
                hedon_s = "hedon_c",
                liwc_s = "liwc_c",
                vader_s = "vader_c",
                swear = "swear_c")


rhs.list <- list(bins5plus = pastep(c("tmax_cut5", "ppt", "diurnal_range", "RelativeHumidity", "WindSpeed", "StationPressure", "OVC")),
                 bins5 = pastep(c("tmax_cut5", "ppt")),
                 bins3 = pastep(c("tmax_cut3", "ppt")),
                 bins1 = pastep(c("tmax_cut1", "ppt")))

fes.list <- list(c_m_y=list(pastep(c("cbsa.id", "month", "year"))),
                 c_m_y_d_h=list(pastep(c("cbsa.id", "month", "year", "dow", "hol"))),
                 c_ym_d_h=list(pastep(c("cbsa.id", "ym", "dow", "hol"))),
                 c_sm_y_d_h=list(pastep(c("cbsa.id", "stateXmonth", "year", "dow", "hol"))),
                 c_date=list(pastep(c("cbsa.id", "date"))))
# fes.list <- list(c_m_y_d_h=list(pastep(c("cbsa.id", "month", "year", "dow", "hol"))),
#                 c_date=list(pastep(c("cbsa.id", "date"))))
data.list <- list(all=data)
clus <- pastep(c("cbsa.id"))




# Note: the following checks make no difference in the results
# - Bin size (have tried 1, 3, 5)
# - Choice of FE between c_m_y, c_m_y_d_h, c_ym_d_h, and c_date
# - Data between using all and limiting to > 100 tweets or > 100 mean tweets by cbsa

# Concerns:
# - stateXmonth and stateXyear look wrong. With two years, probably not reasonable
# to include these, since they take a lot of residual variation.

felm.list <- list()
for (d in seq_along(data.list)) {
  for (lhs in lhses) {
    for (r in seq_along(rhs.list)) {
      for (f in seq_along(fes.list)) {
        tic <- Sys.time()
        dt <- data.list[[d]]
        dt.name <- names(data.list)[d]
        rhs <- rhs.list[[r]]
        rhs.name <- names(rhs.list)[r]
        fe <- fes.list[[f]]
        fe.name <- names(fes.list)[f]
        spec_name <- sprintf("%s_%s_%s_%s", dt.name, lhs, rhs.name, fe.name)
        fmla <- sprintf("%s ~ %s | %s | 0 | %s", lhs, rhs, fe, clus)
        print(fmla)
        # Will weights cause a problem if some are missing
        result <- strip_felm(felm(as.formula(fmla), data=dt, weights = data$cbsa_mean_tweets))
        felm.list[[spec_name]] <- result
        print(summary(result))
        print(Sys.time() - tic)
      }
    }
  }
}
saveRDS(felm.list, file.path(OUT_EST, "cbsa_estimates.Rds"))


install_github("pbaylis/baylisR", auth_token="ebb64529e5a9127f17624c261dbd727b4ab7c9e6")

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
