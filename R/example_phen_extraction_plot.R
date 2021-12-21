# example phenology extraction plot
library(pacman)
p_load(ggplot2, lubridate, ggpubr)

getwd()
source("./src/IRG_functions.R")

set.seed(42)
n = 15
x <- seq.Date(as.Date("2010-01-01"), as.Date("2010-12-01"), length = n)
y <- rescale(sin(seq(-2.5,4.5, length = n))+runif(n, min = -0.01, max = 0.1), 
             new.min = 0.2, new.max = 0.9)
precip <- rescale(sin(seq(-1,5, length = n))+runif(n, min = -0.3, max = 0.3), 
                  new.min = 0, new.max = 0.6)
plot(precip)
y. <- c(NA, diff(y))

## add a colony
colony <- data.frame(time = seq.Date(as.Date("2010-01-01"), as.Date("2010-12-01"), length = n),
                     count = c(0,1,2,4,2,5,10,3,2,2,1,0,0,0,0)/10)

## ggcolors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols <- gg_color_hue(4)

plot(x,y, ylim = c(-0.5, 1), col = "gray", type = "l",
     xlab = "Time", ylab = "EVI", frame.plot = FALSE)
polygon(x = c(colony$time, colony$time), y = c(rep(0, 15), colony$count), col = rgb(0,0,0,.1))
fit <- smooth.spline(x,y, spar = 0.4)
lines(fit, col = cols[1], lwd = 2)
fit. <- predict(fit, deriv = 1)
fit.$y11 <- rescale(fit.$y, new.min = -0.5, new.max = 0.5)



fitp <- smooth.spline(x,precip, spar = 0.4)
plot(fitp, col = cols[1], lwd = 2, type = "l")
fitp. <- predict(fitp, deriv = 1)
fitp.$y11 <- rescale(fitp.$y, new.min = -0.5, new.max = 0.5)

lines(fit.$x, fit.$y11, col = cols[3], lwd = 2)
lines(x,precip)
abline(h = 0, lwd = 2)
abline(v = fit.$x[which.max(fit.$y)], lty = 2)
abline(v = fit$x[which.max(fit$y)], lty = 2)
# abline(v = fit.$x[which.min(fit.$y)], lty = 2)
# segments(x0 = fit$x[which.max(fit$y)], x1 = fit$x[which.max(fit$y)],
#          y0 = fit$y[which.min(fit$y)], y1 = fit$y[which.max(fit$y)],
#          lty = 2)
example_peaks = data.frame(time = x, EVI = fit$y, IRG = fit.$y11, 
                           precip, IRP = fitp.$y11,
                           bats = colony$count)
colors <- c("EVI" = "#009E73", "IRG" = "#D55E00", "Prp" = "lightblue", "IRP" = "turquoise", "Count" = "lightgray")
gg_peaks <- 
  ggplot(example_peaks, aes(x = time))+
    geom_line(aes(y = EVI, col = "EVI"), size = 1, linetype = 5)+
    geom_line(aes(y = IRG, col = "IRG"), size = 1, linetype = 6)+
    geom_col(aes(y = precip, col = "Prp"), fill = "lightblue", alpha = 0.1)+
  geom_line(aes(y = IRP, col = "IRP"), size = 1, linetype = 7)+
    geom_ribbon(aes(ymin = 0, ymax = bats, col = "Count"), alpha = 0.1)+
    geom_line(aes(y = bats), col = 1, linetype = 2, size = 1)+
    #scale_color_manual(values=cbPalette)+
    theme_classic()+
  # Count  
  geom_hline(yintercept = 0, col = "gray")+
  # EVI  
  geom_vline(xintercept = example_peaks$time[which.max(example_peaks$EVI)], 
               linetype = 5, col = "darkgreen", size = 1, alpha = 0.5, show.legend = TRUE)+
  # IRG  
  geom_vline(xintercept = example_peaks$time[which.max(example_peaks$IRG)], 
               linetype = 6, col = "darkred", size = 1, alpha = 0.5)+
  # Precip  
  geom_vline(xintercept = example_peaks$time[which.max(example_peaks$precip)], 
             linetype = 8, col = "lightblue", size = 1)+
  # Count  
  geom_vline(xintercept = example_peaks$time[which.max(example_peaks$bats)], 
             linetype = 1, col = 1, size = 1, , alpha = 0.5)+  
  # IRP  
  geom_vline(xintercept = example_peaks$time[which.max(example_peaks$IRP)], 
             linetype = 9, col = "turquoise", size = 1, alpha = 0.5)+  
    
  scale_x_date(date_labels = "%b") +
    
  labs(color = "", ylab = "value")+ylab("value")+
    
  scale_color_manual(values = colors, labels = names(colors))+
  scale_linetype_manual(values = c(1, 2,3,4,5),
                        labels = c("EVI", "IRG", "Prp", "IRP", "Count"))+
    
  theme(text = element_text(size = 20), 
          # axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
          legend.position = "top",
          legend.text=element_text(size=12))+
  guides(col=guide_legend(nrow=2,byrow=TRUE))
gg_peaks

########################
colors <- c("EVI" = "#009E73", "IRG" = "#D55E00", "Prp" = "lightblue", "IRP" = "turquoise", "Count" = "lightgray")
example_peaks_mlt <- reshape2::melt(example_peaks, id.vars = c("time"))
ggplot(example_peaks_mlt, aes(x = time, y = value))+
  geom_ribbon(data = example_peaks_mlt[example_peaks_mlt$variable == "bats",], 
              aes(ymin = 0, ymax = value, x = time,
                  col = "Count"), alpha = 0.1, inherit.aes = FALSE)+
  # geom_col(data = example_peaks_mlt[example_peaks_mlt$variable == "precip",],
  #          aes(y = precip, x = time, col = "Prp"), fill = "lightblue", alpha = 0.1,
  #          inherit.aes = FALSE)+
  geom_line(aes(col = variable, 
                linetype = variable),size = 1)+
  theme_classic()+
  # Count  
  geom_hline(yintercept = 0, col = "gray")+
  theme(text = element_text(size = 20), 
      # axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
      legend.position = "top",
      legend.text=element_text(size=12))+
  scale_color_manual(values = colors, labels = names(colors), name = "")+
  scale_linetype_manual(name="", values = 2:6, labels = names(colors))+
  guides(col=guide_legend(nrow=2,byrow=TRUE))


#### ggplot
x <- seq.Date(as.Date("2010-01-01"), as.Date("2010-12-01"), by = "week")

# Choose the mean as 2.5 and standard deviation as 0.5.
y0 <- dnorm(as.numeric(x), mean = mean(as.numeric(x)), sd = 30)
y1 <- dnorm(as.numeric(x), mean = mean(as.numeric(x)), sd = 50)
y2 <- dnorm(as.numeric(x), mean = mean(as.numeric(x)), sd = 200)

plot(x,y0, type = "l", lwd = 2)
lines(x,y1, lty = 2, lwd = 2)
lines(x,y2, lty = 3, lwd = 2)
example_EVI <- data.frame(ID = c(rep(1, length(x)), 
                                 rep(2, length(x)), 
                                 rep(3, length(x))), 
                          x = rep(x, 3), EVI = c(y0,y1,y2), 
                          entropy = NA)

for(i in 1:3){
  em <- example_EVI[example_EVI$ID == i,]
  ##calc entropy
  # em <- data.frame(x, y = EVI)
  plot(em$x, em$EVI)
  # monthly mean
  Rm <- em %>% group_by(month(x), year(x)) %>% 
    summarise(EVI = mean(EVI, na.rm = TRUE))
  # Rm$EVI <- rescale(Rm$EVI)
  R <- mean(Rm$EVI, na.rm = TRUE)
  # max month
  Rmax <- max(Rm$EVI, na.rm = TRUE)
  
  # annual sum
  Ra <- sum(Rm$EVI, na.rm = TRUE)
  
  # discrete probability distribution of monthly sum
  ## month/annual
  pm <- Rm$EVI/Ra
  
  # uniform distribution
  qm <- 1/length(Rm$EVI)
  
  # relative entropy
  ## quantifies the extent of EVI concentration in the growing season
  example_EVI$Entropy[which(example_EVI$ID == i)] <- sum(pm * log(pm/qm, base = 2), 
                                                   na.rm = TRUE)
  # # seasonality  
  # S <- D * (R/Rmax)
}  

example_EVI$entropy <- factor(c(rep("high", nrow(example_EVI)/3),
                         rep("med", nrow(example_EVI)/3),
                         rep("low", nrow(example_EVI)/3)), 
                         levels = c("high", "med","low"))
gg_entropy <-               
  ggplot(example_EVI, aes(x, rescale(EVI), col = entropy, group = ID, 
                          linetype = entropy))+
    ylab("EVI")+
    xlab("time")+
    geom_line(size = 1)+
    theme_classic()+
    scale_color_manual(values = viridis::viridis(n = 3, begin = .95, end = 0))+
  scale_x_date(date_labels = "%b") +
  theme(text = element_text(size = 20), 
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.position = "top")+
  guides(col=guide_legend(nrow=3,byrow=TRUE))

gg_entropy
# Create low and high entropy plots
# Spread out greenness
# concentrated greenness

gg_examples <- ggarrange(gg_peaks, gg_entropy, align = "h")
gg_examples
save(gg_peaks, gg_entropy, gg_examples, example_EVI, example_peaks, file = "./../../../Dropbox/MPI/Eidolon/GreenWave/rdata/gg_examples.Rdata")
load("./../../../Dropbox/MPI/Eidolon/GreenWave/rdata/gg_examples.Rdata")
