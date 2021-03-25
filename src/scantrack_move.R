scantrack <- function(b2){
  layout.matrix <- matrix(c(2, 1, 0, 3), nrow = 2, ncol = 2)
  
  layout(mat = layout.matrix,
         heights = c(1, 2), # Heights of the two rows
         widths = c(2, 2)) # Widths of the two columns
  
  # layout.show(3)
  if(class(b2) == "move"){
    plot(b2, type = "o", xlab = "Longitude", ylab = "Latitude")  
  }
  if(class(b2) == "data.frame"){
    plot(b2$utm.easting, b2$utm.northing, type = "o", xlab = "Longitude", ylab = "Latitude")
  }
  
  
  plot(b2$timestamp, b2$utm.easting, type = "o")    
  plot(b2$timestamp, b2$utm.northing, type = "o")

}


lay_out = function(...) {    
  x <- list(...)
  n <- max(sapply(x, function(x) max(x[[2]])))
  p <- max(sapply(x, function(x) max(x[[3]])))
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))    
  
  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]], 
                                           layout.pos.col = x[[i]][[3]]))
  }
} 

gg_scantrack <- function(b2){
  require(ggplot2)
  require(ggpubr)
  p1 <- ggplot(b2, aes(utm.easting, utm.northing, col = timestamp))+
    geom_point()+
    geom_path()+
    ggtitle(b2$tag.local.identifier[1])+
    #scale_color_viridis_c(name='time', #limits=c(0,365), 
    #                      option='B', alpha=0.7)+
    theme_classic()+
    theme(legend.position = "none")
  #p1
  
  p2 <- ggplot(b2, aes(timestamp, utm.northing, col = timestamp))+
    geom_point()+
    geom_path()+
    #scale_color_viridis_c(name='time', #limits=c(0,365), 
    #                      option='B', alpha=0.7)+
    theme_classic()+
    theme(legend.position = "none")
  #p2
  
  p3 <- ggplot(b2, aes(timestamp, utm.easting, col = timestamp))+
    geom_point()+
    geom_path()+
    #scale_color_viridis_c(name='time', #limits=c(0,365), 
    #                      option='B', alpha=0.7)+
    theme_classic()+
    theme(legend.position = "right")
  #p3
  
  # print(ggarrange(p1, p2, p3, common.legend = TRUE, legend = "right"))
  p <- lay_out(list(p1, 1:2, 1:3),
          list(p2, 1:2, 4:5),
          list(p3, 3, 1:5))
  return(p)  
}


