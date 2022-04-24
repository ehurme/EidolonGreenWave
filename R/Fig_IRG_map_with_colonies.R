### figure 4 IRG map with colonies
library(raster)
library(ggplot2)
library(rnaturalearth)

load("./../../../Dropbox/MPI/Eidolon/Greenwave/rdata/avg_peaks_TRMM.RData")
load("./../../../Dropbox/MPI/Eidolon/Greenwave/rdata/EVIAfrica.RData")
load("./../../../Dropbox/MPI/Eidolon/Greenwave/data/avg_colony_size_TRMM.RData")

avg_irg
avg_peaks <- batdf_max_mean
Africa <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf")

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

batdf_max_mean$size_break <- 
  factor(cut(log(batdf_max_mean$peak_size), 5) %>% as.numeric,
         levels = c(1,2,3,5),
         labels = c(
           paste0("<", exp(10) %>% round(digits = -4)),
           paste0("<", exp(11.4) %>% round(digits = -4)),
           paste0("<400000"),#, exp(12.9) %>% round(digits = -4)),
           # exp(14.3) %>% round(digits = -5),
           paste0("<", exp(15.7) %>% round(digits = -5))))

require(scales)
require(RColorBrewer)
display.brewer.pal(4,"Reds")
options(scipen=10000)
gg <- ggplot(mydf, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  coord_equal()+
  scale_fill_viridis_c() +
  geom_point(data = batdf_max_mean, aes(Long, Lat, shape = factor(size_break), 
                                        col = factor(size_break)), 
                                        # col = "red", 
             size = 4,
             inherit.aes = FALSE) +
  scale_shape_manual(values=c(18:15))+
  scale_color_manual(values = brewer.pal(5,"Reds")[2:5])+
  # geom_point(data = batdf_max_mean, aes(Long, Lat , size = peak_size), 
  #            inherit.aes = FALSE, 
  #            col = "red") +
  # scale_size_continuous(limits = c(min(batdf_max_mean$peak_size), max(batdf_max_mean$peak_size)),
  #                       breaks = c(6000,60000, 600000, 6000000),
  #                       range = c(2,7))+
  labs(x = 'Longitude', y = 'Latitude', fill = "IRG")+
  theme(legend.position = "bottom")+
  guides(shape = guide_legend(nrow = 2, title = "Avg Peak Size"), 
         col = guide_legend(title ="Avg Peak Size"),
         text = element_text(size = 25))+
    #override.aes = list(size = c(.1,1,2,3))))+
  # ggtitle("IRG")+
  # ggthemes::theme_map()+
  facet_wrap(~month)

gg
ggsave(gg, filename = "./../../../Dropbox/MPI/Eidolon/Greenwave/plots/fig4_avg_IRG_map.png",
       width = 10, height = 7)
ggsave(gg, filename = "./../../../Dropbox/MPI/Eidolon/Greenwave/plots/fig4_avg_IRG_map.eps",
       device = "eps", width = 10, height = 7)
