# seasonality raster stack
library(pacman)
p_load(raster, lubridate, magrittr, doParallel, foreach, ggplot2, scales, ggpubr)

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


save(S,E,P, s,e,p, medium, file = "../../../../Dropbox/MPI/Eidolon/GreenWave/rdata/seasonality_raster.Rdata")
load("./../../../Dropbox/MPI/Eidolon/GreenWave/rdata/seasonality_raster.Rdata")
# add colonies
load("./../../../Dropbox/MPI/Eidolon/GreenWave/rdata/avg_peaks.RData")
load("./../../../Dropbox/MPI/Eidolon/GreenWave/rdata/colony_count.RData")


plot(S)
text(Lat~ Long, labels = geeID, #cex = 1.5,
     data = avg_peaks)

geeidx <- c(5, 10, 14, 16)
plot(S)
text(Lat~ Long, labels = c("A", "B", "C", "D"), cex = 1.5,
     data = avg_peaks[avg_peaks$geeID %in% geeidx,])

EVI <- read.csv("./../../../Dropbox/GreenWave/EidolonColonies/rawdata/rm_MOD13Q1_EVI_2000_2020_ptsB_buf67_pix250.csv")
EVI$time <- as.Date(ymd_hms(EVI$startDate))
precip <- read.csv("./../../../Dropbox/MPI/Eidolon/Greenwave/data/GEE/rm_TRMM_pr_2000_2020_ptsB.csv")
# names(precip) <- c("time", 1:17)
precip$time <- ymd_hms(precip$startDate)

cbPalette <- c("#009E73", "#D55E00", "turquoise", "#56B4E9")
fig <- list()
i = 2
for(i in 1:length(geeidx)){
  xy <- coordinates(cbind(avg_peaks$Long[geeidx[i]], avg_peaks$Lat[geeidx[i]]))
  g <- EVI[EVI$geeID == geeidx[i],]
  e <- g$mean#*1000
  pr <- precip[precip$geeID == geeidx[i],]
  time <- g$time
  pred <- smooth.spline(time, e)
  pred$p <- predict(pred)$y
  timep <- as.Date(pr$time)
  pp <- pr$mean
  plot(pp, type = "l")
  predp <- smooth.spline(timep, pp)
  plot(predp, type = "l")
  prp <- predict(predp, as.numeric(time))$y
  # plot(prp)
  # prp <- rescale(prp, new.min = 0, new.max = 0.5)
  irp <- c(NA, diff(prp))/(max(diff(prp))*3)
  
  irg <- c(NA, diff(pred$p))
  evi <- data.frame(time, evi = pred$p, irg, irp,
                    precip = prp)
  
  evi_mlt <- reshape2::melt(evi, "time")
  c <- reg_colonies[reg_colonies$geeID == geeidx[i],]
  
  fig[[i]] <- 
    ggplot(evi_mlt[evi_mlt$variable != "precip",], 
           aes(time, value, col = variable, fill = variable))+
    geom_col(data = evi_mlt[evi_mlt$variable == "precip",], alpha = 0.5,
             aes(time, value), col = "lightblue", fill = "lightblue", inherit.aes = FALSE)+
    geom_line(size = 1)+
    scale_color_manual(values=cbPalette)+
    xlim(c(as.Date("2010-01-01"),
           #c(min(reg_colonies$month[reg_colonies$geeID == geeidx[i]]), 
           max(reg_colonies$month[reg_colonies$geeID == geeidx[i]])))+ 
    theme_classic()+
    geom_vline(xintercept = seq.Date(as.Date("2000-01-01"), 
                                     as.Date("2021-01-01"), by = "year"),
               linetype = "dashed", col = "gray")+
    geom_hline(yintercept = 0, col = "gray")+
    ggtitle(#paste0(LETTERS[i],". ", 
                   avg_peaks$Location[geeidx[i]])+
    geom_area(data = c, aes(month, y = count/max(count, na.rm = TRUE)), # *max(evi$evi)), 
              col = 1, alpha = 0.1, inherit.aes = FALSE)+
    theme(legend.position = "none", 
          # axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
          text = element_text(size = 15))
  fig[[i]]
}
fig[[4]]

e_df <- as.data.frame(E, xy = TRUE)

p_map <- ggplot(e_df, aes(x,y,fill = entropy))+
  geom_raster()+
  coord_quickmap()+
  geom_text(data = avg_peaks[avg_peaks$geeID %in% geeidx,], 
            col = "white", size = 10,
            aes(Long, Lat, label = LETTERS[1:4]), inherit.aes = FALSE)+
  labs(x = "Longitude", y = "Latitude")+
  viridis::scale_fill_viridis()+
  theme(legend.position = "top",
        text = element_text(size = 15))
p_map


ggarrange(fig[[1]], fig[[4]], ncol = 1)

avg_peaks

load("./../../../Dropbox/MPI/Eidolon/GreenWave/rdata/gg_examples.Rdata")

fig2 <- ggarrange(ggarrange(fig[[1]], fig[[2]], ncol = 1, labels = c("A", "B")), 
         ggarrange(ggarrange(gg_peaks, gg_entropy, align = "h"), 
                   p_map, ncol = 1, 
                   heights = c(0.3, 0.7), widths = c(1,1)),
         ggarrange(fig[[4]], fig[[3]], ncol = 1, labels = c("C", "D")), nrow = 1, 
         #labels = c("A","","C","B","B","D"),
         widths = c(c(0.3,0.4,0.3)))
fig2

ggsave(fig2, filename = "./../../../Dropbox/MPI/Eidolon/Greenwave/plots/fig2_TRMM.png", 
       width = 20, height = 11)
ggsave(fig2, filename = "./../../../Dropbox/MPI/Eidolon/Greenwave/plots/fig2.eps", 
       width = 20, height = 11, device = "eps")
