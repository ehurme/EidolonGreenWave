# pdf = FALSE
# plot = TRUE
# filename = "test.pdf"
# map = TRUE


findpeaks <- function (x, thresh = 0) 
{
  pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) < 0) + 1
  if (!missing(thresh)) {
    pks[x[pks - 1] - x[pks] > thresh]
  }
  else pks
}

# rescale values between 0 and 1
rescale <- function(x, x.min = NULL, x.max = NULL, new.min = 0, new.max = 1) {
  if(is.null(x.min)) x.min = min(x, na.rm = TRUE)
  if(is.null(x.max)) x.max = max(x, na.rm = TRUE)
  new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
}


irg_calc <- function(AEVI, zdf, sort, year_buff, spar, plot, pdf, map, filename){
  zdf$evi_peak_date <- NA
  zdf$median_evi_peak_day <- NA
  zdf$evi_trough_date <- NA
  zdf$median_evi_trough_day <- NA
  zdf$evi <- NA
  zdf$irg_peak_date <- NA
  zdf$median_irg_peak_day <- NA
  zdf$irg_trough_date <- NA
  zdf$median_irg_trough_day <- NA
  zdf$irg <- NA
  zdf$median_amp <- NA
  
  IDs <- unique(AEVI$geeID)
  
  if(sort == TRUE){
    temp <- data.frame(IDs, bat = NA, date = NA)
    for(i in 1:length(IDs)){
      temp$bat[i] <- zdf$trackId[which(zdf$geeID == IDs[i])]
      temp$date[i] <- zdf$timestamp[which(zdf$geeID == IDs[i])]
    }
    
    IDs <- IDs[order(temp$bat, temp$date)]  
  }
  
  i = 12
  
  if(pdf == TRUE){
    pdf(file = paste0("./MPI/Eidolon/Plots/", filename))
  }
  for(i in 1:length(IDs)){
    aevi <- AEVI[AEVI$geeID == IDs[i],]
    
    if(plot == TRUE){
      par(mar = c(1,4,1,1), oma = c(1,1,1,1), xpd=FALSE)
      layout(cbind(c(1,2)))
      if(map == TRUE){
        layout(rbind(c(1,2),c(1,3)))
      }
      
    }
    
    zdf_idx <- which(zdf$geeID == IDs[i])
    
    if(map == TRUE){
      require(rnaturalearth)
      require(rnaturalearthdata)
      Africa <- ne_countries(continent = "Africa", scale = "medium")
      
      bat <- zdf$trackId[zdf_idx]
      
      with(zdf[zdf$trackId == bat,], plot(location.long, location.lat, 
                                          type = "o", asp = 1, col = "orange",
                                          xlim = c(min(zdf$location.long),max(zdf$location.long)),
                                          ylim = c(min(zdf$location.lat),max(zdf$location.lat))))
      lines(Africa)
      with(zdf[zdf_idx,], points(location.long, location.lat, col = 4, pch = 16))
    }
    
    
    #subset to 3 years of data
    year_idx <- (year(zdf$timestamp[zdf_idx])-year_buff):(year(zdf$timestamp[zdf_idx])+year_buff)
    aevi <- aevi[year(aevi$time) %in% year_idx,]
    
    if(plot == TRUE){
      with(aevi, plot(time, mean, type = "o", 
                      main = paste0("Bat: ", zdf$tag.local.identifier[zdf_idx[1]], 
                                    "; geeID = ", IDs[i], "; ", 
                                    as.Date(zdf$timestamp[zdf_idx[1]]))))  
    }
    
    time <- aevi$time
    evi <- aevi$mean
    
    # smooth the 
    spl <- smooth.spline(x = time, y = evi, spar = spar) # 0.6
    pred <- predict(spl)
    aevi$pred <- pred$y
    if(plot == TRUE){
      lines(pred, col=2)
      abline(v = zdf$timestamp[zdf_idx[1]], col = "blue", lwd = 3)  
    }
    
    # first derivative, get points where slope is 0, i.e. peaks and troughs of the curves
    pred.prime <- predict(spl, deriv=1)
    # pred.prime$y_scale <- rescale(pred.prime$y, c(-1,1))
    aevi$pred.prime <- pred.prime$y
    
    EVIs <- uniroot.all(approxfun(aevi$time, aevi$pred.prime), interval = range(as.numeric(aevi$time))) %>% seconds + ymd("1970-01-01")
    
    if(plot == TRUE) abline(v = EVIs)
    
    # predict(spl, #deriv = 1, 
    #            data.frame(time = as.numeric(EVIs - 1000)))$y
    
    if(predict(spl, #deriv = 1, 
               data.frame(time = as.numeric(EVIs[1])))$y < predict(spl, #deriv = 1, 
                                                                   data.frame(time = as.numeric(EVIs[2])))$y){
      print("lowest EVI first")
      trough_EVIs <- EVIs[seq(1,length(EVIs),by = 2)]
      peak_EVIs <- EVIs[seq(2,length(EVIs),by = 2)]
      
    }
    
    if(predict(spl, #deriv = 1, 
               data.frame(time = as.numeric(EVIs[1])))$y > predict(spl, #deriv = 1, 
                                                                   data.frame(time = as.numeric(EVIs[2])))$y){
      print("peak EVI first")
      trough_EVIs <- EVIs[seq(2,length(EVIs),by = 2)]
      peak_EVIs <- EVIs[seq(1,length(EVIs),by = 2)]
      
    } 
    
    years <- unique(year(aevi$time))
    j = 3
    Peak_EVIs <- {}
    Trough_EVIs <- {}
    
    max_evi <- {}
    min_evi <- {}
    for(j in 1:length(years)){
      pE <- peak_EVIs[year(peak_EVIs) == years[j]]
      tE <- trough_EVIs[year(trough_EVIs) == years[j]]
      
      Peak_EVIs <- c(Peak_EVIs, pE[which.max(unlist(predict(spl, #deriv = 1, 
                                                            data.frame(time = as.numeric(pE)))$y))])
      Trough_EVIs <- c(Trough_EVIs, tE[which.min(unlist(predict(spl, #deriv = 1, 
                                                                data.frame(time = as.numeric(tE)))$y))])
      
      max_evi[j] <- max(aevi$mean[year(aevi$time) == years[j]])
      min_evi[j] <-   min(aevi$mean[year(aevi$time) == years[j]])
      
    }
    
    amplitude <- max_evi - min_evi
    
    if(plot == TRUE){
      points(peak_EVIs, pch = 16, cex = 2,
             predict(spl, #deriv = 1, 
                     data.frame(time = as.numeric(peak_EVIs)))$y %>% unlist,
             col = 3)
      points(trough_EVIs, pch = 16, cex = 2,
             predict(spl, #deriv = 1, 
                     data.frame(time = as.numeric(trough_EVIs)))$y %>% unlist,
             col = "brown")
      # lines(spl)
      legend("topleft", legend = c("Peak EVI", "Min EVI"), pch = 16, col = c(3, "brown"), xpd = TRUE)
      
      
      plot(aevi$time, pred.prime$y, type = "l", ylab = "scaled IRG", col = 2,
           # xlim = c((min(colony$time)-years(1)), (max(colony$time)+years(1))),
           #ylim = c(-1, 1), 
           xlab = "Date")
      abline(v = zdf$timestamp[zdf_idx[1]], col = "blue", lwd = 3)
      abline(h = 0, lty = 2)
    }
    
    ## Calculate second derivative
    pred2prime <- predict(spl, deriv = 2)
    # pred2prime$y_scale <- rescale(pred2prime$y, c(-1,1))
    aevi$pred2prime <- pred2prime$y
    
    IRGs <- uniroot.all(approxfun(aevi$time, aevi$pred2prime), interval = range(as.numeric(aevi$time))) %>% seconds + ymd("1970-01-01")
    
    if(plot == TRUE) abline(v = IRGs)
    # filter negative peaks
    ## brown down first
    
    peak_IRGs <- IRGs[which(predict(spl, deriv = 1, 
                                    data.frame(time = as.numeric(IRGs)))$y > 0)]
    
    trough_IRGs <- IRGs[which(predict(spl, deriv = 1, 
                                      data.frame(time = as.numeric(IRGs)))$y < 0)]
    years <- unique(year(aevi$time))
    j = 3
    
    Peak_IRGs <- {}
    Trough_IRGs <- {}
    for(j in 1:length(years)){
      pI <- peak_IRGs[year(peak_IRGs) == years[j]]
      tI <- trough_IRGs[year(trough_IRGs) == years[j]]
      
      Peak_IRGs <- c(Peak_IRGs, pI[which.max(unlist(predict(spl, deriv = 1, 
                                                            data.frame(time = as.numeric(pI)))$y))])
      Trough_IRGs <- c(Trough_IRGs, tI[which.min(unlist(predict(spl, deriv = 1, 
                                                                data.frame(time = as.numeric(tI)))$y))])
    }
    
    if(plot == TRUE){
      points(Peak_IRGs, pch = 16, cex = 2,
             predict(spl, deriv = 1, 
                     data.frame(time = as.numeric(Peak_IRGs)))$y %>% unlist,
             col = 3)
      points(Trough_IRGs, pch = 16, cex = 2,
             predict(spl, deriv = 1, 
                     data.frame(time = as.numeric(Trough_IRGs)))$y %>% unlist,
             col = "brown")
      legend("bottomleft", legend = c("Green Up", "Brown Down"), pch = 16, col = c(3, "brown"), xpd = TRUE)  
    }
    
    Peak_EVIs <- seconds(Peak_EVIs) + ymd("1970-01-01")
    Trough_EVIs <- seconds(Trough_EVIs) + ymd("1970-01-01")
    Peak_IRGs <- seconds(Peak_IRGs) + ymd("1970-01-01")
    Trough_IRGs <- seconds(Trough_IRGs) + ymd("1970-01-01")
    
    j = 1
    for(j in 1:length(zdf_idx)){
      zdf$evi_peak_date[zdf_idx[j]] <- Peak_EVIs[which.min(abs(Peak_EVIs - zdf$timestamp[zdf_idx[j]]))]
      zdf$median_evi_peak_day[zdf_idx[j]] <- median(yday(Peak_EVIs))
      
      zdf$evi_trough_date[zdf_idx[j]] <- Trough_EVIs[which.min(abs(Trough_EVIs - zdf$timestamp[zdf_idx[j]]))]
      zdf$median_evi_trough_day[zdf_idx[j]] <- median(yday(Trough_EVIs))
      
      zdf$evi[zdf_idx[j]] <- predict(spl, # deriv = 1, 
                                     data.frame(time = as.numeric(zdf$timestamp[zdf_idx[j]])))$y %>% unlist
      
      zdf$irg_peak_date[zdf_idx[j]] <- Peak_IRGs[which.min(abs(Peak_IRGs - zdf$timestamp[zdf_idx[j]]))]
      zdf$median_irg_peak_day[zdf_idx[j]] <- median(yday(Peak_IRGs))
      
      zdf$irg_trough_date[zdf_idx[j]] <- Trough_IRGs[which.min(abs(Trough_IRGs - zdf$timestamp[zdf_idx[j]]))]
      zdf$median_irg_trough_day[zdf_idx[j]] <- median(yday(Trough_IRGs))
      
      zdf$irg[zdf_idx[j]] <- predict(spl, deriv = 1, 
                                     data.frame(time = as.numeric(zdf$timestamp[zdf_idx[j]])))$y %>% unlist
      
      zdf$median_amp[zdf_idx[j]] <- median(amplitude)
      # integrate area under the curve?
      
    }
  }
  if(pdf == TRUE){
    dev.off()  
  }
  return(zdf)
}

# irg_calc(AEVI = AEVI, zdf = zdf, year_buff = 2, spar = 0.6, plot = TRUE, pdf = FALSE, filename = "test.pdf")  



leaflet_plot <- function(raster, title){
  require(leaflet)
  require(leafem)
  irg_col <- colorNumeric(c("#FF0000", "#52E74B", "#000000"), values(raster),
                          na.color = "transparent")
  
  leaflet() %>% addTiles() %>% 
    # addProviderTiles(providers$Esri.WorldImagery) %>% # names(providers)
    addRasterImage(raster, layerId = "layer",
                   colors = irg_col, group =  "layer",
                   opacity = 0.8) %>%
    addLegend(pal = irg_col, 
              values = values(raster),
              title = title) %>% 
    leafem::addMouseCoordinates() %>%
    leafem::addImageQuery(x = raster, type="mousemove", layerId = "layer") %>% 
    addLayersControl(overlayGroups = "layer")
}

## ggplot
# https://stackoverflow.com/questions/48538034/r-contour-plot-from-raster-dataset-with-country-borders-overlaid 

# https://en.wikipedia.org/wiki/Mean_of_circular_quantities
# http://webspace.ship.edu/pgmarr/Geo441/Lectures/Lec%2016%20-%20Directional%20Statistics.pdf
# https://gist.github.com/jonesor/132f531a520c3b331543
# circ.mean <- function(m,int){
#   rad.m    = m*(360/int)*(pi/180)
#   mean.cos = mean(cos(rad.m))
#   mean.sin = mean(sin(rad.m))
#   x.deg    = atan(mean.sin/mean.cos)*(180/pi)
#   
#   return(x.deg/(360/int))
# }

# https://stackoverflow.com/questions/32404222/circular-mean-in-r
circ.mean <- function (x){
  require(bazar)
  sinr <- sumNA(sin(x), na.rm = TRUE)
  cosr <- sumNA(cos(x), na.rm = TRUE)
  circmean <- atan2(sinr, cosr)
  circmean
}

# https://gist.github.com/jonesor/132f531a520c3b331543
circ.mean2 <- function(m,int){
  rad.m    = m*(360/int)*(pi/180)
  mean.cos = mean(cos(rad.m))
  mean.sin = mean(sin(rad.m))
  x.deg    = atan(mean.sin/mean.cos)*(180/pi)
  
  return(x.deg/(360/int))
}

# sumNA(c(NA), na.rm = TRUE)

circ.mean.yday <- function(days){
  conv <- 2*pi/365
  d <- mean(exp(conv*(days-1)*1i), na.rm = TRUE)
  direction <- Arg(d)/conv%%365  ## 'direction', i.e. average day of the year
  if(direction < 0)direction <- direction+365
  # Mod(d)                ## 'intensity'
  return(direction)
}

circ.mean.month <- function(months){
  conv <- 2*pi/12
  d <- mean(exp(conv*(months)*1i), na.rm = TRUE)
  direction <- Arg(d)/conv%%12  ## 'direction', i.e. average day of the year
  if(direction < 0)direction <- direction+12
  # Mod(d)                ## 'intensity'
  return(direction)
}


# circ.mean.yday <- function(days){
#   conv <- 2*pi/365
#   res1 <- circ.mean(conv*(days-1))/conv
#   angle <- (res1 + 365) %% 365
#   return(angle)
# }

circ.intensity.month <- function(days){
  conv <- 2*pi/12
  d <- mean(exp(conv*(days)*1i), na.rm = TRUE)
  # direction <- 365+Arg(d)/conv%%365  ## 'direction', i.e. average day of the year
  intensity <- Mod(d)                ## 'intensity'
  return(intensity)
}

circ.mean.month(11:12)
circ.intensity.month(1)


deg2rad <- function(deg) {((deg * pi) / (365/2))}

