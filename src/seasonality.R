# Changes in rainfall seasonality in the tropics
# https://www.readcube.com/library/e739b9df-b0e0-40eb-b627-b1100f04fc68:1e0c11d9-43b9-47d1-869d-f00ebe2a55cf
# https://www.readcube.com/articles/supplement?doi=10.1038%2Fnclimate1907&index=0

x <- e
seasonality <- function(x){
  require(lubridate)
  require(magrittr)
  Season <- data.frame()
  
  if(length(x$time) == 0){
    x$time <- ymd_hms(x$startDate)
  }
  if(length(x$mean) == 0){
    x$mean <- x[,2]
  }
  
  years <- unique(year(x$time))
  x$month <- month(x$time)
  i = 1
  for(i in 1:length(years)){
    idx <- which(year(x$time) == years[i])
    
    # monthly sum
    if(length(idx) == 12){
      Rm <- x$mean[idx] 
      tmp <- x[idx,]
    }
  
    if(length(idx) != 12){
      x[idx,] %>% group_by(month) %>%
       summarise(mean = mean(mean, na.rm = TRUE)) -> tmp 
      Rm <- tmp$mean
    }
    
    # monthly average
    R <- mean(Rm)
    
    # max month
    Rmax <- max(Rm)
    
    # annual sum
    Ra <- sum(Rm)
    
    # discrete probability distribution of monthly sum
    ## month/annual
    pm <- Rm/Ra
    
    # uniform distribution
    qm <- 1/nrow(tmp)
    
    # relative entropy
    D <- sum(pm * log(pm/qm), na.rm = TRUE)
    
    # seasonality  
    S <- D * (R/Rmax)   
    
    season <- data.frame(R, Rmax, Ra, qm, entropy = D, seasonality = S, year = years[i], months = nrow(tmp))
    Season <- rbind(Season, season)
  }
  return(Season)
}
