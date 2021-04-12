# seasonality raster stack
require(raster)  
require(lubridate)
require(magrittr)
require(doParallel)
require(foreach)

load("C:/Users/Edward/MODIStsp/VI_Monthly_005dg_v6/Time_Series/RData/Terra/EVI/MOD13C2_EVI_32_2000_1_2021_RData.RData")

Africa <- rnaturalearth::ne_countries(continent = "Africa")

numCores <- detectCores()
registerDoParallel(numCores)  # use multicore, set to the number of our cores

n <- length(names(raster_ts))
medium <- stack()
medium <- foreach (i=1:n, .init = medium, .combine = addLayer , .packages = "raster") %dopar% {
  print(i)
  ras <- aggregate(raster_ts[[i]], fact = 20)
}
stopImplicitCluster()

res(medium) # 1,1
# res(small) # 2,2

dats <- seq(as.Date('2000-02-01'), length.out = nlayers(medium), by = 'months')
medium <- setZ(medium, dats)
x <- medium
getZ(x)
plot(x[[1]])

# seasonality_raster <- function(x){


# cores=detectCores()
# cl <- makeCluster(cores[1]-3) #not to overload your computer
# registerDoParallel(cl)
  

if(length(getZ(x) > 0)){
  time <- ymd(getZ(x))
}

years <- unique(year(time))
month <- month(time)
i = 1


s <- x[[1]] # seasonality
names(s) <- "seasonality"
values(s) <- NA

e <- s # entropy
names(e) <- "entropy"
p <- s # mean
names(p) <- "normalized month"
  
n <- length(values(s))
i = 6
# numCores <- detectCores()
# registerDoParallel(numCores-5)  # use multicore, set to the number of our cores


# foreach (i=1:n, .packages = "raster") %dopar% {
for(i in 1:length(values(s))){
  
  jan_idx <- c(12 * (1:(length(years)-2)))
  feb_idx <- c(1, 12 * (1:(length(years)-2))+1)
  mar_idx <- c(1, 12 * (1:(length(years)-2))+2)
  apr_idx <- c(1, 12 * (1:(length(years)-2))+3)
  may_idx <- c(1, 12 * (1:(length(years)-2))+4)
  jun_idx <- c(1, 12 * (1:(length(years)-2))+5)
  jul_idx <- c(1, 12 * (1:(length(years)-2))+6)
  aug_idx <- c(1, 12 * (1:(length(years)-2))+7)
  sep_idx <- c(1, 12 * (1:(length(years)-2))+8)
  oct_idx <- c(1, 12 * (1:(length(years)-2))+9)
  nov_idx <- c(1, 12 * (1:(length(years)-2))+10)
  dec_idx <- c(1, 12 * (1:(length(years)-2))+11)
  
  Rm <- c(mean(values(x)[i,jan_idx]), mean(values(x)[i,feb_idx]), 
          mean(values(x)[i,mar_idx]), mean(values(x)[i,apr_idx]),
          mean(values(x)[i,may_idx]), mean(values(x)[i,jun_idx]),
          mean(values(x)[i,jul_idx]), mean(values(x)[i,aug_idx]),
          mean(values(x)[i,sep_idx]), mean(values(x)[i,oct_idx]),
          mean(values(x)[i,nov_idx]), mean(values(x)[i,dec_idx]))
  
  R <- mean(Rm, na.rm = TRUE)
  
  # max month
  Rmax <- max(Rm, na.rm = TRUE)
  
  # annual sum
  Ra <- sum(Rm, na.rm = TRUE)
  
  # discrete probability distribution of monthly sum
  ## month/annual
  pm <- Rm/Ra
  raster::values(p)[i] <- Ra#R/Rmax
  
  # uniform distribution
  qm <- 1/length(Rm)
  # relative entropy
  ## quantifies the extent of EVI concentration in the growing season
  D <- sum(pm * log(pm/qm), na.rm = TRUE)
  raster::values(e)[i] <- D
  
  # seasonality  
  S <- D * (R/Rmax)
  
  raster::values(s)[i] <- S
  print(i)
}
# stopImplicitCluster()    

  #stopCluster(cl)  
#  return(Season)
# }

S <- mask(s, Africa)  
P <- mask(p, Africa)
E <- mask(e, Africa)

layout(1:3)
plot(S)
plot(P)    
plot(E)

# tmp <- data.frame(values <- c(values(s),
#                        values(p),
#                        values(e)),
#            variables <- c(rep("seasonality", n),
#                           rep("evi", n),
#                           rep("entropy", n)))

xy <- coordinates(S)
tmp2 <- data.frame(x = xy[,1], y = xy[,2], 
                   seasonality = (values(S)),
                   evi = (values(P)),
                   entropy = (values(E)))

# with(na.omit(tmp2), plot(seasonality, entropy, pch = 16,
#                          col = rgb(1-seasonality, 1-evi, 1-entropy, rep(1, n))))



library(ggchromatic)
ggplot(tmp2, aes(x,y))+geom_tile(aes(fill = cmy_spec(evi, entropy, seasonality)))


save(S,E,P, s,e,p, medium, file = "./../../../Dropbox/MPI/Eidolon/Greenwave/rdata/seasonality_raster.RData")
load("./../../../Dropbox/MPI/Eidolon/Greenwave/rdata/seasonality_raster.RData")


plot(S)
text(Lat~ Long, labels = geeID, #cex = 1.5,
     data = avg_peaks)
# add colonies
load("./../../../Dropbox/MPI/Eidolon/Greenwave/rdata/avg_peaks.RData")
load("./../../../Dropbox/MPI/Eidolon/Greenwave/rdata/colony_count.RData")
load("./../../../Dropbox/MPI/Eidolon/Greenwave/rdata/model_peaks.RData")

geeidx <- c(5,6,14,15) #c(5, 10, 14, 16)
plot(E)
text(Lat~ Long, labels = c("A", "B", "C", "D"), cex = 1.5,
     data = avg_peaks[avg_peaks$geeID %in% geeidx,])

# EVI <- read.csv("./../../../Dropbox/GreenWave/EidolonColonies/rawdata/rm_MOD13Q1_EVI_2000_2020_ptsB.csv")
# EVI$time <- as.Date(ymd_hms(EVI$startDate))
i = 4
# p <- list()
#for(i in 1:length(geeidx)){
  print(i)
  xy <- coordinates(cbind(avg_peaks$Long[geeidx[i]], avg_peaks$Lat[geeidx[i]]))
  # EVI <- raster::extract(raster_ts, xy)
  # time <- seq.Date(as.Date("2000-02-01"), as.Date("2021-01-01"), by = "month")
  g <- EVI[EVI$geeID == geeidx[i],]
  e <- g$mean # *1000
  time <- g$time
  pred <- smooth.spline(time, e)
  pred$p <- predict(pred)$y
  # plot(time, pred$p, type = "l", 
  #      ylim = c(min(e), max(e)+1000),
  #      xlim = c(as.Date("2010-01-01"),as.Date("2015-01-01")))
  # abline(v = seq.Date(as.Date("2000-01-01"), 
  #                     as.Date("2021-01-01"), by = "year"),
  #        col = "gray", lty = 2)
  irg <- c(NA, diff(pred$p))
  # lines(time, irg+3000, col = 2)
  evi <- data.frame(time, evi = pred$p, irg)
  evi_mlt <- reshape2::melt(evi, "time")
  # ggplot(evi, aes(x = time))+
  #   geom_line(aes(y = evi), col = 3)+
  #   geom_line(aes(y = irg), col = 2)+
  #   scale_y_continuous(
  #     # Features of the first axis
  #     name = "EVI",
  #     
  #     # Add a second axis and specify its features
  #     sec.axis = sec_axis(~., name="IRG")
  #   )+
  c <- reg_colonies[reg_colonies$geeID == geeidx[i],]
  
  
  pD <- 
    ggplot(evi_mlt, aes(x = as.Date(time), 
                                y = value, col = variable))+
    geom_line(data = g, aes(x = as.Date(time), y = mean), col = "lightgray", inherit.aes = FALSE)+
    geom_line()+
    ylim(c(-0.5,1))+
    xlim(c(as.Date("2014-01-01"), as.Date("2016-01-01")))+
      #c(min(reg_colonies$month[reg_colonies$geeID == geeidx[i]]), 
           # max(reg_colonies$month[reg_colonies$geeID == geeidx[i]])))+ 
    theme_classic()+
    geom_vline(xintercept = seq.Date(as.Date("2000-01-01"), 
                                     as.Date("2021-01-01"), by = "year"),
              linetype = "dashed", col = "gray")+
    geom_vline(xintercept = Peaks$EVI_spline[Peaks$geeID == geeidx[i]], col = 3)+
    geom_vline(xintercept = Peaks$IRG_spline[Peaks$geeID == geeidx[i]], col = 2)+
    geom_vline(xintercept = Peaks$date[Peaks$geeID == geeidx[i]], col = 1, linetype = "dashed")+
    geom_hline(yintercept = 0, col = "gray")+
    ggtitle(paste0(avg_peaks$Location[geeidx[i]], ", entropy: ", round(Peaks$entropy[1],2)))+ # add entropy
              # paste0(LETTERS[i], ". ", avg_peaks$Location[geeidx[i]]))+
    geom_area(data = c[!is.na(c$Observer),], 
              aes(month, y = count/max(count, na.rm = TRUE)*max(evi$evi)), 
              col = 1, alpha = 0.1)+
    scale_y_continuous(
      
      # Features of the first axis
      name = "EVI",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*max(c$count, na.rm = TRUE), name="Count")
    )+
    xlab("Time")+
    theme(legend.position = "none", text = element_text(size = 20),
          axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
# }

pD

entropy <- as.data.frame(E, xy = TRUE)
p_map <- 
  ggplot(data = entropy, aes(x,y, fill = entropy))+
  geom_raster()+
  viridis::scale_fill_viridis()+
  coord_quickmap()+
  geom_text(data = avg_peaks[avg_peaks$geeID %in% geeidx,], col = "white", cex = 10,
            aes(x = Long, y = Lat), label = LETTERS[1:4], inherit.aes = FALSE)+
  theme(legend.position = "top")

ggarrange(ggarrange(pA, pB, ncol = 1, labels = c("A", "B"), 
                    font.label = list(size = 20)), 
          p_map, 
          ggarrange(pD, pC, ncol = 1, labels = c("D", "C"),
                    font.label = list(size = 20)), 
          nrow = 1, heights = c(0.7,1.3,0.7), 
          align = "h", widths = c(1, 1.2, 1))
