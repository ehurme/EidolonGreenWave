### figure 2 IRG map with colonies
library(raster)
library(ggplot2)

load("./../../../Dropbox/MPI/Eidolon/Greenwave/rdata/avg_peaks.RData")
load("C:/Users/Edward/MODIStsp/VI_Monthly_005dg_v6/EVIAfrica.RData")
load("../../../../Dropbox/MPI/Eidolon/Greenwave/data/avg_colony_size.RData")

avg_irg
avg_peaks

tmp<- mask(avg_irg, Africa)
plot(tmp)

mydf <- purrr::map_dfr(
  as.list(tmp), 
  ~setNames(as.data.frame(as(., "SpatialPixelsDataFrame")), c('value', 'x', 'y')), 
  .id = 'names'
)

names(mydf) <- c("month", "value", "x", "y")
mydf$month = as.numeric(as.character(mydf$month))

mydf <- mydf[order(mydf$month),]

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

mydf$month <- factor(mydf$month, levels = 1:12, labels = c("January", "February", "March", 
               "April", "May", "June",
               "July", "August", "September", 
               "October", "November", "December"))

batdf_max_mean$month <- factor(batdf_max_mean$month, levels = 1:12, labels = c("January", "February", "March", 
                                                           "April", "May", "June",
                                                           "July", "August", "September", 
                                                           "October", "November", "December"))

require(scales)
options(scipen=10000)
gg <- ggplot(mydf, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  coord_equal()+
  scale_fill_viridis_c() +
  geom_point(data = batdf_max_mean, aes(Long, Lat, size = peak_size), inherit.aes = FALSE, 
             col = "red") +
  scale_size_continuous(limits = c(min(batdf_max_mean$peak_size), max(batdf_max_mean$peak_size)), 
                        breaks = c(6000,60000, 600000, 6000000),
                        range = c(2,7))+
  labs(x = 'Longitude', y = 'Latitude', fill = "IRG")+
  theme(legend.position = "bottom")+
  guides(size = guide_legend(nrow = 2, title = "Avg Peak Size"))+
    #override.aes = list(size = c(.1,1,2,3))))+
  # ggtitle("IRG")+
  # ggthemes::theme_map()+
  facet_wrap(~month)

gg
ggsave(gg, filename = "./../../../Dropbox/MPI/Eidolon/Greenwave/plots/fig4_avg_IRG_map.png")
ggsave(gg, filename = "./../../../Dropbox/MPI/Eidolon/Greenwave/plots/fig4_avg_IRG_map.eps",
       device = "eps")
