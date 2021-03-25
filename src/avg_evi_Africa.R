
# high res averaged months
load("C:/Users/Edward/MODIStsp/VI_Monthly_005dg_v6/Time_Series/RData/Terra/EVI/MOD13C2_EVI_32_2000_1_2021_RData.RData")
raster_ts[[c(1,33,111,200)]] %>% plot()
files <- raster_ts %>% names

years <- substr(files, 13, 16)
days <- substr(files, 18, 20)

table(years)
table(days)

jan <- mean(raster_ts[[which(days == "001")]], na.rm = TRUE)
feb <- mean(raster_ts[[which(days == "032")]], na.rm = TRUE)
mar <- mean(raster_ts[[which(days == "060" | days == "061")]], na.rm = TRUE)
apr <- mean(raster_ts[[which(days == "091" | days == "092")]], na.rm = TRUE)
may <- mean(raster_ts[[which(days == "121" | days == "122")]], na.rm = TRUE)
jun <- mean(raster_ts[[which(days == "152" | days == "153")]], na.rm = TRUE)
jul <- mean(raster_ts[[which(days == "182" | days == "183")]], na.rm = TRUE)
aug <- mean(raster_ts[[which(days == "213" | days == "214")]], na.rm = TRUE)
sep <- mean(raster_ts[[which(days == "244" | days == "245")]], na.rm = TRUE)
oct <- mean(raster_ts[[which(days == "274" | days == "275")]], na.rm = TRUE)
nov <- mean(raster_ts[[which(days == "305" | days == "306")]], na.rm = TRUE)
dec <- mean(raster_ts[[which(days == "335" | days == "336")]], na.rm = TRUE)

avg_evi_ts <- raster::brick(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
names(avg_evi_ts) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

plot(avg_evi_ts, col = jet.colors(30))


### calculate seasonality
seasonality
calc(irg, fun = seasonality, forceapply = TRUE)

plot(irg)

sr <- "+proj=utm +zone=35 +datum=WGS84 +ellps=clrk66 +units=m +no_defs" 

# Project Raster
projected_raster <- projectRaster(medium[[1]], crs = sr)
projected_raster
