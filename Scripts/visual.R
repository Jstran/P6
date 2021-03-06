### ¤¤ Intro ¤¤ ### ---------------------------------------------------------------------

rm(list=ls())
load("./Workspaces/preliminary.Rdata")
load("./Workspaces/modelling.Rdata")
load("./Workspaces/forecastModA.Rdata")
load("./Workspaces/forecastModC.Rdata")
load("./Workspaces/forecastModB.Rdata")

i <- 1
ps <- list(names = c() , var = c(),  p = c() , i = c() , h = c() , w = c())

# Størrelse til downloads af grafer: 700 x 250

### ¤¤ Pakker ¤¤ ### --------------------------------------------------------------------

library(stats)
library(ggplot2)
library(lubridate)
library(forecast)
library(astsa)
library(tseries)
library(Cairo)     # Til at gemme

### ¤¤ "Rå" data ¤¤ ### -----------------------------------------------------------------

# Plot af ukorrigerede data
p.raw <-  ggplot(data.frame(X1 = dates, 
                            X2 = DK1$Raw ), 
                 aes(x = X1 , y = X2, size = sz$l) ) +
  geom_line(aes(), color = colors[1]) +
  labs(x = "", y = " Spotpris i DKK/MWh", 
       color = "") +
  p.Y
#p.raw
ps$p[[i]] <- p.raw ; ps$names[i] <- "plotRaw" ; ps$var[i] <- "p.raw"; 
ps$h[i] <- 3; ps$w[i] <- 9
i <- i + 1

# Plot af korrigerede data
p.clean <-  ggplot(data.frame(X1 = dates, 
                              X2 = DK1$A ), 
                   aes(x = X1 , y = X2 , size = sz$l) ) +
  geom_line(aes(), color = colors[1]) +
  labs(x = "", y = " Spotpris i DKK/MWh", color = "") +
  p.Y
#p.clean
ps$p[[i]] <- p.clean ; ps$names[i] <- "plotClean" ; ps$var[i] <- "p.clean"; 
ps$h[i] <- 3; ps$w[i] <- 9
i <- i + 1


# Histogram for sæsonrensede priser
p.hist <- ggplot(data.frame(X2 = DK1$D),
                 aes(x = X2)) +
  geom_histogram(binwidth = 20, color = "white", fill = colors[1]) + 
  stat_function(fun = function(x){dnorm(x = x, 
                                  mean = mean(DK1$D), 
                                  sd = sd(DK1$D))*length(DK1$D)*24.1}, 
                 color = colors[2], size = sz$l) +
  labs(x = "Sæsonkorrigeret pris i DKK/MWh", y = "Tæthed") +
  scale_x_continuous() +
  p.th
#p.hist
ps$p[[i]] <- p.hist ; ps$names[i] <- "plotHist" ; ps$var[i] <- "p.hist"; 
ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

# Q-Q plot af sæsonrensede priser
dk1.d.stand <- (DK1$D - mean(DK1$D))/sqrt(var(DK1$D))
y <- quantile(dk1.d.stand, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
quantiles <- qqnorm(dk1.d.stand)

p.qq <- ggplot(data.frame(x = quantiles$x, y = quantiles$y), aes(x = x, y = y))+
        geom_point(col = colors[1], size = sz$p) +
        geom_abline(slope = slope, intercept = int, linetype = "dashed", col = colors[2],
                    size = sz$l) + 
        labs(x = "Standard normal teoretisk fraktil", 
             y = "Standardiseret sæsonkorrigeret pris") +
        theme(legend.position="none") +
        p.th
#p.qq    
ps$p[[i]] <- p.qq; ps$names[i] <- "plotQQ" ; ps$var[i] <- "p.qq"; 
ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1



# Plot af acf
p.acf.A <- ggplot(data = data.frame(X1 = acf(DK1$A, lag.max = 2190 , plot = FALSE)$lag,
                                    X2 = acf(DK1$A, lag.max = 2190 , plot = FALSE)$acf), 
                  aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0)) +
  geom_segment(aes(xend = X1, yend = 0)) +
  geom_hline(aes(yintercept = -ci()), 
             color = colors[1], linetype = "dotted") +
  geom_hline(aes(yintercept =  ci()), 
             color = colors[1], linetype = "dotted") +
  labs(x = "Lag", y = "ACF") +
  p.th
#p.acf.A
ps$p[[i]] <- p.acf.A ; ps$names[i] <- "plotACFAll" ; ps$var[i] <- "p.acf.A" ; 
ps$h[i] <- 3; ps$w[i] <- 9
i <- i + 1


### ¤¤ s(t) ¤¤ ### ----------------------------------------------------------

# Plotter priser hvor det deterministiske er fjernet
p.d <- ggplot(data = data.frame(X1 = dates,
                                X2 = DK1$D),
              aes(x = X1, y = X2 , size = sz$l) ) +
  geom_line(color = colors[1]) +
  labs(x = "", y = "Spotpris i DKK/MWh") + 
  p.Y
#p.d
ps$p[[i]] <- p.d ; ps$names[i] <- "plotRemDet" ; ps$var[i] <- "p.d" ;
ps$h[i] <- 3; ps$w[i] <- 9
i <- i + 1


# Plotter spotpriser med den determinstiske model lagt ovenpå
p.s.A <-  ggplot(data.frame(X1 = dates, 
                             X2 = DK1$A), 
                  aes(x = X1 , y = X2 , size = sz$l) ) +
  geom_line(aes(col = "Observationer")) +
  geom_point(data = data.frame(X1 = dates, 
                               X2 = DK1$s.pred), 
             aes(col = "s(t)", size = sz$p) )+
  labs(x = "", y = "Spotpris i DKK/MWh", title = "", color = "") +
  scale_color_manual(values = colors[1:2]) +
  p.Y +
  theme(legend.position = "top" , legend.justification = "left" , 
        legend.direction = "horizontal", legend.background = element_blank())
#p.s.A
ps$p[[i]] <- p.s.A ; ps$names[i] <- "plotSpotDet" ; ps$var[i] <- "p.s.A"; 
ps$h[i] <- 3.8; ps$w[i] <- 9
i <- i + 1


# Plot af acf for decomposed
p.d.acf <- ggplot(data = data.frame(X1 = acf(DK1$D, lag.max = 30 , plot = FALSE)$lag,
                                    X2 = acf(DK1$D, lag.max = 30 , plot = FALSE)$acf), 
                  aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0)) +
  geom_segment(aes(xend = X1, yend = 0)) +
  geom_hline(aes(yintercept = -ci()), 
             color = colors[1], linetype = "dotted") +
  geom_hline(aes(yintercept =  ci()), 
             color = colors[1], linetype = "dotted") +
  labs(x = "Lag", y = "ACF") +
  p.th
#p.d.acf
ps$p[[i]] <- p.d.acf ; ps$names[i] <- "plotACFDecomp" ; ps$var[i] <- "p.d.acf" ; 
ps$h[i] <- 3; ps$w[i] <- 9
i <- i + 1

# Histogram for detrended priser
p.d.hist <- ggplot(data.frame(X2 = DK1$D),
                   aes(x = X2)) +
  geom_histogram(binwidth = 20, color = "white", fill = colors[1]) + 
  stat_function(fun = function(x){dnorm(x = x, 
                                        mean = mean(DK1$D), 
                                        sd = sd(DK1$D))*length(DK1$D)*24.1}, 
                color = colors[2]) +
  labs(x = "Spotpris i DKK/MWh", y = "") +
  scale_x_continuous() +
  geom_vline(xintercept = mean(DK1$D), col = colors[3]) +
  p.th
#p.d.hist
ps$p[[i]] <- p.d.hist ; ps$names[i] <- "plotHistDetrend" ; ps$var[i] <- "p.d.hist" ;
ps$h[i] <- 3; ps$w[i] <- 9
i <- i + 1

### ¤¤ Model A ¤¤ ### -------------------------------------------------------------

x.pred.oos.a.s <- x.pred.oos.a[2192:2281] + s.pred.oos
nas <- rep(NaN, 32)

p.forecast.a <- ggplot(data = data.frame(X1 = dates.all[2160:2281],
                                         X2 = c(DK1$A, OOS$A)[2160:2281] ), 
                       aes(x = X1, y = X2) ) +
  geom_line(aes(col = "Observerede spotpriser", size = sz$l)) + 
  geom_line(data = data.frame(X1 = dates.all[2192:2281], 
                              X2 = x.pred.oos.a.s), 
            aes(col = "Forecastede spotpriser", size = sz$l)) +
  geom_ribbon(aes(ymin = c(nas,x.pred.oos.a.s) - pred.inter.a[2160:2281], 
                  ymax = c(nas,x.pred.oos.a.s) + pred.inter.a[2160:2281]), 
              fill = colors[6], alpha = 0.2, color = colors[6], size = sz$l) +
  scale_color_manual(values = c(colors[2], colors[1])) +
  scale_x_date(date_labels = "%b %y", breaks = pretty(dates.all[2160:2281], n = 6)) +
  scale_y_continuous() +
  theme(legend.position = "top" , legend.justification = "left" , 
        legend.direction = "horizontal", legend.background = element_blank()) +
  p.th +
  labs(x = "", y = "Spotpris i DKK/MWh", title = "", color = "")
p.forecast.a
# ps$p[[i]] <- p.forecast.a ; ps$names[i] <- "ModA/plotForecastModA" ; 
ps$var[i] <- "p.forecast.a" ; ps$h[i] <- 3.8; ps$w[i] <- 9
i <- i + 1

# Histogram for residualer Model A
p.hist.res.a <- ggplot(data.frame(X2 = res.is.a),
                       aes(x = X2)) +
  geom_histogram(binwidth = 20, color = "white", fill = colors[1]) + 
  stat_function(fun = function(x){dnorm(x = x, 
                                        mean = mean(res.is.a), 
                                        sd = sd(res.is.a))*length(res.is.a)*24.6}, 
                color = colors[2], size = sz$l) +
  labs(x = "Residualer", y = "Tæthed") +
  scale_x_continuous() +
  p.th
# p.hist.res.a
ps$p[[i]] <- p.hist.res.a ; ps$names[i] <- "ModA/plotHistResA" ; 
ps$var[i] <- "p.hist.res.a"; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

# Q-Q plot af sæsonrensede priser
std.res.a <- (res.is.a - mean(res.is.a))/sqrt(var(res.is.a))
y <- quantile(std.res.a, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
quantiles <- qqnorm(std.res.a)

p.qq.res.a <- ggplot(data.frame(x = quantiles$x, y = quantiles$y), aes(x = x, y = y))+
  geom_point(col = colors[1], size = sz$p) +
  geom_abline(slope = slope, intercept = int, linetype = "dashed", col = colors[2],
              size = sz$l) + 
  labs(x = "Standard normal teoretisk fraktil", y = "Standardiserede residualer") +
  theme(legend.position="none") +
  p.th
# p.qq.res.a
ps$p[[i]] <- p.qq.res.a; ps$names[i] <- "ModA/plotQqResA" ; ps$var[i] <- "p.qq.res.a"; 
ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

# ACF residualer Model A
p.acf.res.a <- ggplot(data.frame(X1 = acf(res.is.a, 
                                          lag.max = 2190 , 
                                          plot = FALSE)$lag[2:31],
                                 X2 = acf(res.is.a, 
                                          lag.max = 2190 , 
                                          plot = FALSE)$acf[2:31]), 
                  aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0, size = sz$l)) +
  geom_segment(aes(xend = X1, yend = 0), color = colors[1]) +
  geom_hline(aes(yintercept = -ci(res.is.a)), 
             color = colors[2], linetype = "dotted") +
  geom_hline(aes(yintercept =  ci(res.is.a)), 
             color = colors[2], linetype = "dotted") +
  labs(x = "Lag", y = "ACF") +
  p.th
# p.acf.res.a
ps$p[[i]] <- p.acf.res.a ; ps$names[i] <- "ModA/plotACFModA" ; 
ps$var[i] <- "p.acf.res.a" ; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

# Ljung-Box residualer Model A
lag.max <- 30
which.lag <- 1:lag.max
p.vals <- numeric(lag.max)
for (l in 1:lag.max){
  p.vals[l] <- Box.test(res.is.a , lag = l , type = "Ljung-Box")$p.value
}
p.lbox.res.a <- ggplot(data.frame(X1 = which.lag,
                                  X2 = p.vals), 
                       aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0, size = sz$l)) +
  geom_point(aes(), color = colors[1]) +
  geom_hline(aes(yintercept =  0.05 ), 
             color = colors[2], linetype = "dotted") +
  labs(x = "Lag", y = "P-værdi") +
  coord_cartesian(ylim=c(0,1)) +
  p.th
# p.lbox.res.a
ps$p[[i]] <- p.lbox.res.a ; ps$names[i] <- "ModA/plotLboxResA" ; 
ps$var[i] <- "p.lbox.res.a" ; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1


### ¤¤ Model B ¤¤ ### -------------------------------------------------------------------
# CCF vind og pris
p.ccf <- ggplot(data.frame(X1 = ccf(w.arma.res, dk1.d.prew, plot = FALSE)$lag,
                           X2 = ccf(w.arma.res, dk1.d.prew, plot = FALSE)$acf), 
                      aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0, size = sz$l)) +
  geom_segment(aes(xend = X1, yend = 0), color = colors[1]) +
  geom_hline(aes(yintercept = -ci()), 
             color = colors[2], linetype = "dotted") +
  geom_hline(aes(yintercept =  ci()), 
             color = colors[2], linetype = "dotted") +
  labs(x = "Lag", y = "CCF") +
  p.th
#p.ccf
ps$p[[i]] <- p.ccf ; ps$names[i] <- "ModB/plotCCF" ; ps$var[i] <- "p.ccf" ; 
ps$h[i] <- 3; ps$w[i] <- 9
i <- i + 1


x.pred.oos.b.s <- x.pred.oos.b[2192:2281] + s.pred.oos
nas <- rep(NaN, 32)

p.forecast.b <- ggplot(data = data.frame(X1 = dates.all[2160:2281],
                                         X2 = c(DK1$A, OOS$A)[2160:2281] ), 
                       aes(x = X1, y = X2) ) +
  geom_line(aes(col = "Observerede spotpriser", size = sz$l)) + 
  geom_line(data = data.frame(X1 = dates.all[2192:2281], 
                              X2 = x.pred.oos.b.s), 
            aes(col = "Forecastede spotpriser", size = sz$l)) +
  geom_ribbon(aes(ymin = c(nas,x.pred.oos.b.s) - pred.inter.b[2160:2281], 
                  ymax = c(nas,x.pred.oos.b.s) + pred.inter.b[2160:2281]), 
              fill = colors[6], alpha = 0.2, color = colors[6], size = sz$l) +
  scale_color_manual(values = c(colors[2], colors[1])) +
  scale_x_date(date_labels = "%b %y", breaks = pretty(dates.all[2160:2281], n = 6)) +
  scale_y_continuous() +
  theme(legend.position = "top" , legend.justification = "left" , 
        legend.direction = "horizontal", legend.background = element_blank()) +
  p.th +
  labs(x = "", y = "Spotpris i DKK/MWh", title = "", color = "")
# p.forecast.b
ps$p[[i]] <- p.forecast.b ; ps$names[i] <- "ModB/plotForecastModB" ;
ps$var[i] <- "p.forecast.b" ; ps$h[i] <- 3.8; ps$w[i] <- 9
i <- i + 1

# Q-Q plot af sæsonrensede priser
std.res.b <- (res.is.b - mean(res.is.b))/sqrt(var(res.is.b))
y <- quantile(std.res.b, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
quantiles <- qqnorm(std.res.b)

p.qq.res.b <- ggplot(data.frame(x = quantiles$x, y = quantiles$y), aes(x = x, y = y))+
  geom_point(col = colors[1], size = sz$p) +
  geom_abline(slope = slope, intercept = int, linetype = "dashed", col = colors[2],
              size = sz$l) + 
  labs(x = "Standard normal teoretisk fraktil", y = "Standardiserede residualer") +
  theme(legend.position="none") +
  p.th
#p.qq.res.b    
ps$p[[i]] <- p.qq.res.b; ps$names[i] <- "ModB/plotQqResB" ; ps$var[i] <- "p.qq.res.b"; 
ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

# ACF residualer Model A
p.acf.res.b <- ggplot(data.frame(X1 = acf(res.is.b, 
                                          lag.max = 2190, 
                                          plot = FALSE)$lag[2:31],
                                 X2 = acf(res.is.b, 
                                          lag.max = 2190, 
                                          plot = FALSE)$acf[2:31]), 
                      aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0, size = sz$l)) +
  geom_segment(aes(xend = X1, yend = 0), color = colors[1]) +
  geom_hline(aes(yintercept = -ci(res.is.b)), 
             color = colors[2], linetype = "dotted") +
  geom_hline(aes(yintercept =  ci(res.is.b)), 
             color = colors[2], linetype = "dotted") +
  labs(x = "Lag", y = "ACF") +
  p.th
# p.acf.res.b
ps$p[[i]] <- p.acf.res.b ; ps$names[i] <- "ModB/plotACFModB"; ps$var[i] <- "p.acf.res.b";
ps$h[i] <- 3; ps$w[i] <- 9/2 
i <- i + 1

# Histogram for residualer Model B
p.hist.res.b <- ggplot(data.frame(X2 = res.is.b),
                       aes(x = X2)) +
  geom_histogram(binwidth = 20, color = "white", fill = colors[1]) + 
  stat_function(fun = function(x){dnorm(x = x, 
                                        mean = mean(res.is.b), 
                                        sd = sd(res.is.b))*length(res.is.b)*25.85}, 
                color = colors[2], size = sz$l) +
  labs(x = "Residualer", y = "Tæthed") +
  scale_x_continuous() +
  p.th
#p.hist.res.b
ps$p[[i]] <- p.hist.res.b ; ps$names[i] <- "ModB/plotHistResB" ; 
ps$var[i] <- "p.hist.res.b"; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

# Ljung-Box residualer Model B
lag.max <- 30
which.lag <- 1:lag.max
p.vals <- numeric(lag.max)
for (l in 1:lag.max){
  p.vals[l] <- Box.test(res.is.b , lag = l , type = "Ljung-Box")$p.value
}
p.lbox.res.b <- ggplot(data.frame(X1 = which.lag,
                                  X2 = p.vals), 
                       aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0, size = sz$l)) +
  geom_point(aes(), color = colors[1]) +
  geom_hline(aes(yintercept =  0.05 ), 
             color = colors[2], linetype = "dotted") +
  labs(x = "Lag", y = "P-værdi") +
  coord_cartesian(ylim=c(0,1)) +
  p.th
# p.lbox.res.b
ps$p[[i]] <- p.lbox.res.b ; ps$names[i] <- "ModB/plotLboxResB" ; 
ps$var[i] <- "p.lbox.res.b" ; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

### ¤¤ Model C ¤¤ ### -------------------------------------------------------------

x.pred.oos.c.s <- x.pred.oos.c[2192:2281] + s.pred.oos
nas <- rep(NaN, 32)

p.forecast.c <- ggplot(data.frame(X1 = dates.all[2160:2281],
                                  X2 = c(DK1$A, OOS$A)[2160:2281] ), 
                       aes(x = X1, y = X2) ) +
  geom_line(aes(col = "Observerede spotpriser", size = sz$l)) + 
  geom_line(data = data.frame(X1 = dates.all[2192:2281], 
                              X2 = x.pred.oos.c.s), 
            aes(col = "Forecastede spotpriser", size = sz$l)) +
  geom_ribbon(aes(ymin = c(nas,x.pred.oos.c.s) - pred.inter.c[2160:2281], 
                  ymax = c(nas,x.pred.oos.c.s) + pred.inter.c[2160:2281]), 
              fill = colors[6], alpha = 0.2, color = colors[6], size = sz$l) +
  scale_color_manual(values = c(colors[2], colors[1])) +
  scale_x_date(date_labels = "%b %y", breaks = pretty(dates.all[2160:2281], n = 6)) +
  scale_y_continuous() +
  theme(legend.position = "top" , legend.justification = "left" , 
        legend.direction = "horizontal", legend.background = element_blank()) +
  p.th +
  labs(x = "", y = "Spotpris i DKK/MWh", title = "", color = "")
p.forecast.c
ps$p[[i]] <- p.forecast.c ; ps$names[i] <- "ModC/plotForecastModC" ; 
ps$var[i] <- "p.forecast.c" ; ps$h[i] <- 3.8; ps$w[i] <- 9
i <- i + 1

# Q-Q plot af sæsonrensede priser
std.res.c <- (res.is.c - mean(res.is.c))/sqrt(var(res.is.c))
y <- quantile(std.res.c, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
quantiles <- qqnorm(std.res.c)

p.qq.res.c <- ggplot(data.frame(x = quantiles$x, y = quantiles$y), aes(x = x, y = y))+
  geom_point(col = colors[1], size = sz$p) +
  geom_abline(slope = slope, intercept = int, linetype = "dashed", col = colors[2],
              size = sz$l) + 
  labs(x = "Standard normal teoretisk fraktil", y = "Standardiserede residualer") +
  theme(legend.position="none") +
  p.th
# p.qq.res.c
ps$p[[i]] <- p.qq.res.c; ps$names[i] <- "ModC/plotQqResC" ; ps$var[i] <- "p.qq.res.c"; 
ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

# ACF residualer Model A
p.acf.res.c <- ggplot(data.frame(X1 = acf(res.is.c, 
                                          lag.max = 2190 , 
                                          plot = FALSE)$lag[2:31],
                                 X2 = acf(res.is.c, 
                                          lag.max = 2190 , 
                                          plot = FALSE)$acf[2:31]), 
                      aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0, size = sz$l)) +
  geom_segment(aes(xend = X1, yend = 0), color = colors[1]) +
  geom_hline(aes(yintercept = -ci(res.is.c)), 
             color = colors[2], linetype = "dotted") +
  geom_hline(aes(yintercept =  ci(res.is.c)), 
             color = colors[2], linetype = "dotted") +
  labs(x = "Lag", y = "ACF") +
  p.th
# p.acf.res.c
ps$p[[i]] <- p.acf.res.c ; ps$names[i] <- "ModC/plotACFModC" ; 
ps$var[i] <- "p.acf.res.c" ; ps$h[i] <- 3; ps$w[i] <- 9/2 
i <- i + 1

# Histogram for residualer Model C
p.hist.res.c <- ggplot(data.frame(X2 = res.is.c),
                       aes(x = X2)) +
  geom_histogram(binwidth = 20, color = "white", fill = colors[1]) + 
  stat_function(fun = function(x){dnorm(x = x, 
                                        mean = mean(res.is.c), 
                                        sd = sd(res.is.c))*length(res.is.c)*24.45}, 
                color = colors[2], size = sz$l) +
  labs(x = "Residualer", y = "Tæthed") +
  scale_x_continuous() +
  p.th
# p.hist.res.c
ps$p[[i]] <- p.hist.res.c ; ps$names[i] <- "ModC/plotHistResC" ; 
ps$var[i] <- "p.hist.res.c"; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

# Ljung-Box residualer Model C
lag.max <- 30
which.lag <- 1:lag.max
p.vals <- numeric(lag.max)
for (l in 1:lag.max){
  p.vals[l] <- Box.test(res.is.c , lag = l)$p.value
}
p.lbox.res.c <- ggplot(data.frame(X1 = which.lag,
                                  X2 = p.vals), 
                       aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0, size = sz$l)) +
  geom_point(aes(), color = colors[1]) +
  geom_hline(aes(yintercept =  0.05 ), 
             color = colors[2], linetype = "dotted") +
  labs(x = "Lag", y = "P-værdi") +
  coord_cartesian(ylim=c(0,1)) +
  p.th
# p.lbox.res.c
ps$p[[i]] <- p.lbox.res.c ; ps$names[i] <- "ModC/plotLboxResC" ; 
ps$var[i] <- "p.lbox.res.c" ; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1


### ¤¤ Signifikanstest for A, B og C ¤¤ ### ---------------------------------------------

sign.val <- c(0.1 , 0.05 , 0.01)


est.a.crit <- rbind(est.a , rep(1,ncol(est.a)))
for(k in 1:3){
  for(l in 1:ncol(est.a) ){
    if( -abs(est.a.crit[3,l]) < qnorm(sign.val[k]/2) ){
      est.a.crit[4,l] <- sign.val[k]
    }
  }
} ; est.a.crit

est.b.crit <- rbind(est.b , rep(1,ncol(est.b)))
for(k in 1:3){
  for(l in 1:ncol(est.b) ){
    if( -abs(est.b.crit[3,l]) < qnorm(sign.val[k]/2) ){
      est.b.crit[4,l] <- sign.val[k]
    }
  }
} ; est.b.crit

est.c.crit <- rbind(est.c , rep(1,ncol(est.c)))
for(k in 1:3){
  for(l in 1:ncol(est.c) ){
    if( -abs(est.c.crit[3,l]) < qnorm(sign.val[k]/2) ){
      est.c.crit[4,l] <- sign.val[k]
    }
  }
} ; est.c.crit

### ¤¤ Procent out of bounds i forecast for A, B og C ¤¤ ### ----------------------------

inter <- 2192:2281
DK1OOS <- c(DK1$D , OOS$D)[inter]

ymin  <- x.pred.oos.a[inter] - pred.inter.a[inter] 
ymax  <- x.pred.oos.a[inter] + pred.inter.a[inter]
aout <- sum( as.numeric( (DK1OOS > ymax) | (DK1OOS < ymin) ) )/length(inter)*100

ymin  <- x.pred.oos.b[inter] - pred.inter.b[inter] 
ymax  <- x.pred.oos.b[inter] + pred.inter.b[inter]
bout <- sum( as.numeric( (DK1OOS > ymax) | (DK1OOS < ymin) ) )/length(inter)*100

ymin  <- x.pred.oos.c[inter] - pred.inter.c[inter] 
ymax  <- x.pred.oos.c[inter] + pred.inter.c[inter]
cout <- sum( as.numeric( (DK1OOS > ymax) | (DK1OOS < ymin) ) )/length(inter)*100

data.frame(a.outside = aout , b.outside = bout , c.outside = cout)

### ¤¤ Forside ¤¤ ### -------------------------------------------------------------------

yminvar <- x.pred.oos.b[2194:2281] - pred.inter.b[2194:2281] 
ymaxvar <- x.pred.oos.b[2194:2281] + pred.inter.b[2194:2281]
fppred  <- x.pred.oos.b[2194:2281]
yminvar[1:21] <- NA
ymaxvar[1:21] <- NA
fppred[1:21]  <- NA

p.forside <- ggplot(data = data.frame(X1 = dates.all[2194:2281],
                         X2 = c(DK1$D, OOS$D)[2194:2281] ), 
       aes(x = X1, y = X2) ) +
  geom_line(aes(size = sz$l),col = colors[1]) + 
  geom_line(data = data.frame(X1 = dates.all[2194:2281], 
<<<<<<< HEAD
                              X2 = x.pred.oos.b[2194:2281]), 
            aes(size = sz$l), col = colors[2]) +
  geom_ribbon(aes(ymin = x.pred.oos.b[2194:2281] - pred.inter.b[2194:2281], 
                  ymax = x.pred.oos.b[2194:2281] + pred.inter.b[2194:2281]), 
=======
                              X2 = fppred), 
            aes(size = sz$l), col = colors[2]) +
  geom_ribbon(aes(ymin = yminvar, 
                  ymax = ymaxvar),
>>>>>>> 48de8367b35c45c63ea97a101348a03897ca1ac7
              fill = colors[6], alpha = 0.2, color = colors[6], size = sz$l) +
  scale_x_date(date_labels = "%b %y", breaks = pretty(dates.all[2194:2281], n = 3)) +
  scale_y_continuous() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank()) +
  p.th +
  labs(x = "", y = "Spotpris i DKK/MWh", title = "", color = "")
 p.forside
ps$p[[i]] <- p.forside ; ps$names[i] <- "plotForside" ; ps$var[i] <- "p.forside" ; 
ps$h[i] <- 5; ps$w[i] <- 9
i <- i + 1
### ¤¤ Gemmer plots ¤¤ ### --------------------------------------------------------------
data.frame(names = ps$names , var = ps$var , w = ps$w , h = ps$h)  

# wanted.plots <- 1:length(ps$names)
wanted.plots <- 26

save.plots = TRUE

if(save.plots == TRUE){
  for(j in wanted.plots){
    print(ps$p[[j]])
    ggsave(file = paste("./Grafer/",ps$names[j],".eps" , sep = ""), 
           width = ps$w[j] , height = ps$h[j] , device = cairo_ps , dpi = 600)
  }
}
### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------



