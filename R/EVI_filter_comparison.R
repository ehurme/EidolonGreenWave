# load libraries
library(pacman)
p_load(tidyverse, lubridate, data.table, dplyr, rnaturalearth)

# load data
EVI <- read.csv("../../../../Dropbox/GreenWave/EidolonColonies/rawdata/rl_MOD13Q1_EVI_2000_2020_ptsB_EidolonColonies_QCOP1_buf67km_scalePix500.csv")
EVI0 <- read.csv("../../../../Dropbox/GreenWave/EidolonColonies/rawdata/rl_MOD13Q1_EVI_2000_2020_ptsB__EidolonColonies_QCNone_buf67km_scalePix500.csv")

colonies <- read.csv("../../../../Dropbox/MPI/Eidolon/Greenwave/data/colonies.csv")
colony_sum <- colonies %>% group_by(geeID, Location, Country, Lat, Long) %>% dplyr::summarise(n())

# How much data is removed by data quality filters?
nrow(EVI)/nrow(EVI0)

EVI %>% group_by(geeID) %>% dplyr::summarise(length = n())
EVI0 %>% group_by(geeID) %>% dplyr::summarise(length = n())

# add colony names to EVI
for(i in 1:nrow(colony_sum)){
  EVI$Location[EVI$geeID == i] <- colony_sum$Location[i] 
  EVI$Country[EVI$geeID == i] <- colony_sum$Country[i]
  EVI$Lat[EVI$geeID == i] <- colony_sum$Lat[i]
  EVI$Long[EVI$geeID == i] <- colony_sum$Long[i]
}

evi_sum <- EVI %>% group_by(geeID, Location, Country, Lat, Long) %>% dplyr::summarise(length =n(),
                                                                ratio = n()/480)
range(evi_sum$ratio) %>% round(2)



# where are those missing points concentrated?
plot(evi_sum$ratio, evi_sum$Lat %>% abs)
plot(evi_sum$ratio, evi_sum$Long %>% abs)

Africa <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf")
class(Africa)
ggplot(data = Africa)+ geom_sf(fill = "darkgrey") + 
  xlim(c(-20, 45))+ylim(c(-20, 20))+
  geom_point(data = evi_sum, aes(x = Long, y = Lat, col = ratio*100))+
  viridis::scale_color_viridis(name = "% complete data", option = "C")+
  xlab("Longitude")+ylab("Latitude")

# when are those points missing?
EVI$time <- ymd_hms(EVI$startDate)
EVI0$time <- ymd_hms(EVI0$startDate)

EVI$month <- month(EVI$time)

evi_time_sum <- EVI %>% group_by(geeID, Location, Country, Lat, Long, month) %>% 
  dplyr::summarise(length = n(), ratio = n()/42)
evi_time_sum[which(evi_time_sum$ratio != 1),]          
