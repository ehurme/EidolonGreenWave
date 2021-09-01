# compare environmental data sets

# load libraries
library(tidyverse)
library(lubridate)
library(circular)
# load circular stats functions
source("./src/IRG_functions.R")

# CHIRPS and TRMM
load("./../../../Dropbox/MPI/Eidolon/Greenwave/rdata/rs_max_sum_TRMM.Rdata")
rs_TRMM <- rs
rs_max_TRMM <- rs_max
rs_max_sum_TRMM <- rs_max_sum
load("./../../../Dropbox/MPI/Eidolon/Greenwave/rdata/rs_max_sum.Rdata")
## precip

prp_avg <- data.frame(chirps_p = rs_max_sum$precip_month,
  trmm_p = rs_max_sum_TRMM$precip_month,
  chirps_pp = rs_max_sum$precipp_month,
  trmm_pp = rs_max_sum_TRMM$precipp_month)

### raw
plot(rs$precip_spline, rs_TRMM$precip_spline)
cor(as.numeric(rs$precip_spline), as.numeric(rs_TRMM$precip_spline), use = "pairwise.complete.obs")
p <- data.frame(chirps = rs$precip_spline, trmm = rs_TRMM$precip_spline, geeID = rs$geeID)

ggplot(p, aes(x = month(chirps), y = month(trmm), col = factor(geeID)))+geom_point()+
  geom_smooth(method = "lm", alpha = 0.1)+geom_abline(slope = 1, intercept = 1)

IDs <- 1:17
ps <- {} 
ts <- {}
i = 14
for(i in 1:length(IDs)){
  pp <- p[p$geeID == IDs[i],]
  y = as.circular(pi*month(pp$chirps)/12)
  x = as.circular(pi*month(pp$trmm)/12)
  plot(x, stack = TRUE, bins = 50)
  points(y-.1, col = 2, stack = TRUE, bins = 50)
  # cppp[i] <- cor.circular(
  #   y = y, 
  #   x = x)
  # better to look at difference in months
  print(table(round(na.omit(pp$chirps - pp$trmm)/(30*24*3600),0)))
  print(t.test(difftime(pp$chirps, pp$trmm, units = "weeks")/4))
  test <- t.test(difftime(pp$chirps, pp$trmm, units = "weeks")/4)
  ps[i] <- as.numeric(test$p.value)
  ts[i] <- as.numeric(test$statistic)
  # # statistical test for difference in distribution?
  # watson.two.test(y = y, x = x,alpha = 0.05)
  # l <- list(x,y)
  # watson.williams.test(l)
}
prp_avg$p_p <- ps
prp_avg$p_t <- ts


###
rs_max
p_max <- data.frame(chirps = rs_max$precip_spline, 
                    trmm = rs_max_TRMM$precip_spline, geeID = rs$geeID)
ps <- {} 
ts <- {}
i <- 1
for(i in 1:length(IDs)){
  pp <- p_max[p_max$geeID == IDs[i],]
  # correlation isn't great. Points aren't in a line but clustered
  # cpp[i] <- cor(sin(pi*month(pp$chirps)/12), sin(pi*month(pp$trmm)/12), use = "pairwise.complete.obs")
  y = as.circular(pi*month(pp$chirps)/12)
  x = as.circular(pi*month(pp$trmm)/12)
  plot(x, stack = TRUE, bins = 100)
  points(y-.1, col = 2, stack = TRUE, bins = 100)
  print(table(round(na.omit(pp$chirps - pp$trmm)/(30*24*3600),0)))
  
  test <- t.test(difftime(pp$chirps, pp$trmm, units = "weeks")/4)
  ps[i] <- as.numeric(test$p.value)
  ts[i] <- as.numeric(test$statistic)
}
prp_avg$pp_p <- ps
prp_avg$pp_t <- ts



## IRP

### average month

rs_max_sum$precipp_month
rs_max_sum_TRMM$precipp_month

pp_max <- data.frame(chirps = (rs_max$precip_pspline), 
                     trmm = (rs_max_TRMM$precip_pspline), 
                     geeID = rs$geeID)
d_max <- {}
i <- 1
for(i in 1:length(IDs)){
  pp <- pp_max[pp_max$geeID == IDs[i],]
  # correlation isn't great. Points aren't in a line but clustered
  # cpp[i] <- cor(sin(pi*month(pp$chirps)/12), sin(pi*month(pp$trmm)/12), use = "pairwise.complete.obs")
  y = as.circular(pi*month(pp$chirps)/12)
  x = as.circular(pi*month(pp$trmm)/12)
  plot(x, stack = TRUE, bins = 100)
  points(y-.1, col = 2, stack = TRUE, bins = 100)
  print(table(round(na.omit(pp$chirps - pp$trmm)/(30*24*3600),0)))
  
  test <- t.test(difftime(pp$chirps, pp$trmm, units = "weeks")/4)
  # print(test)
  d_max[i] <- as.numeric(test$p.value)
}
hist(d_max, breaks = 17)
d_max %>% round(2)


### intensity

### HR

# Compare EVI and IRG from different data sets
## Spline vs Modeled

## EVI

avg_phen_month <- rs_max %>% group_by(geeID) %>%
  dplyr::summarise(EVI_month = round(circ.mean.month(month(round_date(EVI_spline, unit = "months"))),1),
            EVI_model = round(circ.mean.month(month(round_date(EVI_model, unit = "months"))),1),
            IRG_month = round(circ.mean.month(month(round_date(IRG_spline, unit = "months"))),1),
            IRG_model = round(circ.mean.month(month(round_date(IRG_model, unit = "months"))),1)) 

evi_ps <- {}
evi_ts <- {}
for(i in 1:length(IDs)){
  r <- rs_max[rs_max$geeID == IDs[i],]
  # correlation isn't great. Points aren't in a line but clustered
  # cpp[i] <- cor(sin(pi*month(pp$chirps)/12), sin(pi*month(pp$trmm)/12), use = "pairwise.complete.obs")
  y = as.circular(pi*month(r$EVI_spline)/12)
  x = as.circular(pi*month(r$EVI_model)/12)
  plot(x, stack = TRUE, bins = 100)
  points(y-.1, col = 2, stack = TRUE, bins = 100)
  print(table(round(na.omit(as.Date(r$EVI_spline) - r$EVI_model)/(30),0)))
  
  test <- t.test(difftime(as.Date(r$EVI_spline), r$EVI_model, units = "weeks")/4)
  # print(test)
  evi_ps[i] <- as.numeric(test$p.value)
  evi_ts[i] <- as.numeric(test$statistic)
}
avg_phen_month$evi_p <- evi_ps %>% round(4)
avg_phen_month$evi_t <- evi_ts %>% round(4)

ps_irg <- {}
ts_irg <- {}
for(i in 1:length(IDs)){
  r <- rs_max[rs_max$geeID == IDs[i],]
  # correlation isn't great. Points aren't in a line but clustered
  # cpp[i] <- cor(sin(pi*month(pp$chirps)/12), sin(pi*month(pp$trmm)/12), use = "pairwise.complete.obs")
  y = as.circular(pi*month(r$IRG_spline)/12)
  x = as.circular(pi*month(r$IRG_model)/12)
  plot(x, stack = TRUE, bins = 100)
  points(y-.1, col = 2, stack = TRUE, bins = 100)
  print(table(round(na.omit(as.Date(r$IRG_spline) - r$IRG_model)/(30),0)))
  
  test <- t.test(difftime(as.Date(r$IRG_spline), r$IRG_model, units = "weeks")/4)
  # print(test)
  ps_irg[i] <- as.numeric(test$p.value)
  ts_irg[i] <- as.numeric(test$statistic)
}
avg_phen_month$irg_p <- ps_irg %>% round(4)
avg_phen_month$irg_t <- ts_irg %>% round(4)

write.csv(avg_phen_month, file = "./../../../Dropbox/MPI/Eidolon/GreenWave/data/avg_phen_month.csv")
