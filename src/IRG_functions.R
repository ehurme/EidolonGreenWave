findpeaks <- function (x, thresh = 0) 
{
  pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) < 0) + 1
  if (!missing(thresh)) {
    pks[x[pks - 1] - x[pks] > thresh]
  }
  else pks
}

# x <- c(ymd("2012-12-19"), ymd("2001-12-19"))

round_month <- function(x, unit = "months"){
  require(lubridate)
  if(class(x) != "Date") x <- ymd(x)
  if(class(x) == "Date"){
    y <- year(x)
    m <- month(x)
    d <- day(x)
    dm <- days_in_month(x)
    r <- round(d/dm)
    nm <- m+r
    if(any(nm > 12)){
      idx <- which(nm > 12)
      y[idx] <- y[idx] +1 
      nm[idx] <- 1
      }
    nearest_month <- ymd(paste0(y,"-",nm,"-",01))
  }  
  return(nearest_month)
}

# rescale values between 0 and 1
rescale <- function(x, x.min = NULL, x.max = NULL, new.min = 0, new.max = 1) {
  if(is.null(x.min)) x.min = min(x, na.rm = TRUE)
  if(is.null(x.max)) x.max = max(x, na.rm = TRUE)
  new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
}

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

circ.mean.month <- function(months){
  conv <- 2*pi/12
  d <- mean(exp(conv*(months)*1i), na.rm = TRUE)
  direction <- Arg(d)/conv%%12  ## 'direction', i.e. average day of the year
  if(direction < 0)direction <- direction+12
  # Mod(d)                ## 'intensity'
  return(direction)
}

circ.intensity.month <- function(days){
  conv <- 2*pi/12
  d <- mean(exp(conv*(days)*1i), na.rm = TRUE)
  # direction <- 365+Arg(d)/conv%%365  ## 'direction', i.e. average day of the year
  intensity <- Mod(d)                ## 'intensity'
  return(intensity)
}

deg2rad <- function(deg) {((deg * pi) / (365/2))}

