#gg_scantrack <- function(zdf){
  require(ggplot2)
  require(lubridate)
  require(ggpubr)
  require(rnaturalearth)
  Africa <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf")
  
  IDs <- unique(zdf$trackId)
  i = 1
  # for(i in 1:length(IDs)){
    
    p <- ggplot(data = Africa)+ geom_sf() +
      coord_sf(xlim = c(min(zdf$location.long, na.rm = TRUE)-1, max(zdf$location.long, na.rm = TRUE)+1), 
               ylim = c(min(zdf$location.lat, na.rm = TRUE)-1, max(zdf$location.lat, na.rm = TRUE)+1))+
      geom_point(data = zdf[zdf$trackId == IDs[i],], aes(location.long, location.lat, col = yday(timestamp)))+
      geom_path(data = zdf[zdf$trackId == IDs[i],], aes(location.long, location.lat, col = yday(timestamp)))+
      # facet_wrap(~factor(tag.local.identifier)) +
      ylab("Latitude")+ggtitle(IDs[i])+
      theme(legend.position="left")+
      scale_x_continuous(name="Longitude", limits=c(22, 34), breaks = seq(24,34, by = 5))+
      # scale_color_continuous(limits = c(min(zdf$irg), max(zdf$irg)))+
      scale_color_viridis_c(name='IRG', option='B', alpha=0.7)
    
    p0 <-  ggplot(zdf[zdf$trackId == IDs[i],], aes(x = length(diff(timestamp)), 
                                                   y = diff(timestamp)))+
      geom_point()+
      xlab("Date")+ylab("Fix Rate")+
      geom_hline(yintercept = median(diff(timestamp)))+
      theme(legend.position = "none",
            text = element_text(size=8),
            axis.text.x = element_text(angle=30, hjust = 1))+
      #scale_color_continuous(limits = c(min(zdf$irg), max(zdf$irg)))+
      scale_color_viridis_c(name='Day of the Year', option='B', alpha=0.7)
    
    p1 <- ggplot(zdf[zdf$trackId == IDs[i],], aes(y = location.long, x = timestamp, col = yday(timestamp)))+
      geom_point()+
      geom_path()+
      ylab("Longitude")+xlab("Date")+
      theme_classic()+
      theme(text = element_text(size=8))+
      #scale_color_continuous(limits = c(min(zdf$irg), max(zdf$irg)))+
      scale_color_viridis_c(name='Day of the Year', option='B', alpha=0.7)
    
    p2 <- ggplot(zdf[zdf$trackId == IDs[i],], aes(y = location.lat, x = timestamp, col = yday(timestamp)))+
      geom_point()+
      geom_path()+
      ylab("Latitude")+xlab("Date")+
      theme_classic()+theme(legend.position = "none",
                            text = element_text(size=8),
                            axis.text.x = element_text(angle=30, hjust = 1))+
      # scale_color_continuous(limits = c(min(zdf$irg), max(zdf$irg)))+
      scale_color_viridis_c(name='IRG', option='B', alpha=0.7)
    
    print(ggpubr::ggarrange(p1, p0, p, p2, 
                            ncol = 2, nrow = 2, align = "hv",
                            widths = c(3, 1), heights = c(1, 3),
                            common.legend = TRUE, legend = "left"))
# }