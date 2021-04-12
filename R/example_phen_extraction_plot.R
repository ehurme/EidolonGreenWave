# example phenology extraction plot
getwd()
source("./src/IRG_functions.R")

set.seed(123)
n = 30
x <- seq.Date(as.Date("2010-01-01"), as.Date("2010-12-01"), length = n)
y <- rescale(sin(seq(-2,5, length = n))+runif(n, min = -0.1, max = 0.2), 
             new.min = 0.2, new.max = 0.9)
y. <- c(NA, diff(y))

## add a colony
colony <- data.frame(time = seq.Date(as.Date("2010-01-01"), as.Date("2010-12-01"), length = n/2),
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
fit <- smooth.spline(x,y)
lines(fit, col = cols[1], lwd = 2)
fit. <- predict(fit, deriv = 1)
fit.$y11 <- rescale(fit.$y, new.min = -0.5, new.max = 0.5)
lines(fit.$x, fit.$y11, col = cols[3], lwd = 2)
abline(h = 0, lwd = 2)
abline(v = fit.$x[which.max(fit.$y)], lty = 2)
abline(v = fit$x[which.max(fit$y)], lty = 2)
# abline(v = fit.$x[which.min(fit.$y)], lty = 2)
# segments(x0 = fit$x[which.max(fit$y)], x1 = fit$x[which.max(fit$y)],
#          y0 = fit$y[which.min(fit$y)], y1 = fit$y[which.max(fit$y)],
#          lty = 2)


#### ggplot


ggplot(as.data.frame(predict(fit)), aes(as.Date(x),y))+geom_line(col = )+
  geom_line(data = as.data.frame(fit.), aes(x = as.Date(x), y = y11))



# Create low and high entropy plots
# Spread out greenness
# concentrated greenness
