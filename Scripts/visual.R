### ¤¤ Intro ¤¤ ### ---------------------------------------------------------------------

rm(list=ls())
load("./Workspaces/preliminary.Rdata")
load("./Workspaces/modelling.Rdata")
load("./Workspaces/forecastModA2.Rdata")
load("./Workspaces/forecastModC2.Rdata")

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

### ¤¤ ggplot af rå data ¤¤ ### ---------------------------------------------------------

# Plot af ukorrigerede data
p.raw <-  ggplot(data.frame(X1 = dates, 
                            X2 = DK1$Raw ), 
                 aes(x = X1 , y = X2, size = sz$l) ) +
  geom_line(aes(), color = colors[1]) +
  labs(x = "", y = " Spotpris i DKK/MWh", 
       color = "") +
  p.Y
#p.raw
ps$p[[i]] <- p.raw ; ps$names[i] <- "plotRaw" ; ps$var[i] <- "p.raw"; ps$h[i] <- 3; ps$w[i] <- 9
i <- i + 1

# Plot af korrigerede data
p.clean <-  ggplot(data.frame(X1 = dates, 
                              X2 = DK1$A ), 
                   aes(x = X1 , y = X2 , size = sz$l) ) +
  geom_line(aes(), color = colors[1]) +
  labs(x = "", y = " Spotpris i DKK/MWh", color = "") +
  p.Y
#p.clean
ps$p[[i]] <- p.clean ; ps$names[i] <- "plotClean" ; ps$var[i] <- "p.clean"; ps$h[i] <- 3; ps$w[i] <- 9
i <- i + 1


# Histogram for sæsonrensede priser
p.hist <- ggplot(data.frame(X2 = DK1$D),
                 aes(x = X2)) +
  geom_histogram(binwidth = 20, color = "white", fill = colors[1]) + 
  stat_function(fun = function(x){dnorm(x = x, 
                                  mean = mean(DK1$D), 
                                  sd = sd(DK1$D))*length(DK1$D)*24.1}, 
                 color = colors[2], size = sz$l) +
  labs(x = "Sæsonrensede pris i DKK/MWh", y = "Tæthed") +
  scale_x_continuous() +
  p.th
p.hist
ps$p[[i]] <- p.hist ; ps$names[i] <- "plotHist" ; ps$var[i] <- "p.hist"; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

# Q-Q plot af sæsonrensede priser
y <- quantile(DK1$D, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
quantiles <- qqnorm(DK1$D)

p.qq <- ggplot(data.frame(x = quantiles$x, y = quantiles$y), aes(x = x, y = y))+
        geom_point(col = colors[1], size = sz$p) +
        geom_abline(slope = slope, intercept = int, linetype = "dashed", col = colors[2],
                    size = sz$l) + 
        labs(x = "Standard normal teoretisk fraktil", y = "Standardiseret sæsonrensede priser") +
        labs(x = "Teoretisk kvantil", y = "Standardiserede sæsonrensede priser") +
        theme(legend.position="none") +
        p.th
p.qq    
ps$p[[i]] <- p.qq; ps$names[i] <- "plotQQ" ; ps$var[i] <- "p.qq"; ps$h[i] <- 3; ps$w[i] <- 9/2
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
p.acf.A
ps$p[[i]] <- p.acf.A ; ps$names[i] <- "plotACFAll" ; ps$var[i] <- "p.acf.A" ; ps$h[i] <- 3; ps$w[i] <- 9
i <- i + 1


### ¤¤ Regressions plot ¤¤ ### ----------------------------------------------------------

# Plotter priser hvor det deterministiske er fjernet
p.d <- ggplot(data = data.frame(X1 = dates,
                                X2 = DK1$D),
              aes(x = X1, y = X2 , size = sz$l) ) +
  geom_line(color = colors[1]) +
  labs(x = "", y = "Spotpris i DKK/MWh") + 
  p.Y
p.d
ps$p[[i]] <- p.d ; ps$names[i] <- "plotRemDet" ; ps$var[i] <- "p.d" ; ps$h[i] <- 3; ps$w[i] <- 9
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
ps$p[[i]] <- p.s.A ; ps$names[i] <- "plotSpotDet" ; ps$var[i] <- "p.s.A"; ps$h[i] <- 3.8; ps$w[i] <- 9
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
ps$p[[i]] <- p.d.acf ; ps$names[i] <- "plotACFDecomp" ; ps$var[i] <- "p.d.acf" ; ps$h[i] <- 3; ps$w[i] <- 9
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
ps$p[[i]] <- p.d.hist ; ps$names[i] <- "plotHistDetrend" ; ps$var[i] <- "p.d.hist" ; ps$h[i] <- 3; ps$w[i] <- 9
i <- i + 1

### ¤¤ Forecast plot Model A ¤¤ ### -------------------------------------------------------------

p.forecast.a <- ggplot(data = data.frame(X1 = dates.all[2160:2281],
                                       X2 = c(DK1$D, OOS$D)[2160:2281] ), 
                     aes(x = X1, y = X2) ) +
              geom_line(aes(col = "Sæsonkorigerede observationer", size = sz$l)) + 
              geom_line(data = data.frame(X1 = dates.all[2160:2281], 
                                          X2 = x.pred.oos.a[2160:2281]), 
                        aes(col = "Sæsonkorigerede forecast", size = sz$l)) +
              geom_ribbon(aes(ymin = x.pred.oos.a[2160:2281] - pred.inter.a[2160:2281], 
                              ymax = x.pred.oos.a[2160:2281] + pred.inter.a[2160:2281]), 
                          fill = colors[6], alpha = 0.4) +
              scale_color_manual(values = c(colors[2], colors[1])) +
              scale_x_date(date_labels = "%b %y", breaks = pretty(dates.all[2160:2281], n = 6)) +
              scale_y_continuous() +
              theme(legend.position = "top" , legend.justification = "left" , 
              legend.direction = "horizontal", legend.background = element_blank()) +
              p.th +
              labs(x = "", y = "Spotpris i DKK/MWh", title = "", color = "")
p.forecast.a
ps$p[[i]] <- p.forecast.a ; ps$names[i] <- "ModA/plotForecastModA" ; ps$var[i] <- "p.forecast.a" ; ps$h[i] <- 3.8; ps$w[i] <- 9
i <- i + 1

# Histogram for residualer Model A
p.hist.res.a <- ggplot(data.frame(X2 = res.is.a),
                       aes(x = X2)) +
  geom_histogram(binwidth = 20, color = "white", fill = colors[1]) + 
  stat_function(fun = function(x){dnorm(x = x, 
                                        mean = mean(res.is.a), 
                                        sd = sd(res.is.a))*length(res.is.a)*24.1}, 
                color = colors[2], size = sz$l) +
  labs(x = "Residualer", y = "Tæthed") +
  scale_x_continuous() +
  p.th
p.hist.res.a
ps$p[[i]] <- p.hist.res.a ; ps$names[i] <- "ModA/plotHistResA" ; ps$var[i] <- "p.hist.res.a"; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

# Q-Q plot af sæsonrensede priser
y <- quantile(res.is.a, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
quantiles <- qqnorm(res.is.a)

p.qq.res.a <- ggplot(data.frame(x = quantiles$x, y = quantiles$y), aes(x = x, y = y))+
  geom_point(col = colors[1], size = sz$p) +
  geom_abline(slope = slope, intercept = int, linetype = "dashed", col = colors[2],
              size = sz$l) + 
  labs(x = "Standard normal teoretisk fraktil", y = "Standardiserede residualer") +
  theme(legend.position="none") +
  p.th
p.qq.res.a    
ps$p[[i]] <- p.qq.res.a; ps$names[i] <- "ModA/plotQqResA" ; ps$var[i] <- "p.qq.res.a"; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

# ACF residualer Model A
p.acf.res.a <- ggplot(data.frame(X1 = acf(res.is.a, lag.max = 2190 , plot = FALSE)$lag[2:31],
                                 X2 = acf(res.is.a, lag.max = 2190 , plot = FALSE)$acf[2:31]), 
                  aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0, size = sz$l)) +
  geom_segment(aes(xend = X1, yend = 0), color = colors[1]) +
  geom_hline(aes(yintercept = -ci(res.is.a)), 
             color = colors[2], linetype = "dotted") +
  geom_hline(aes(yintercept =  ci(res.is.a)), 
             color = colors[2], linetype = "dotted") +
  labs(x = "Lag", y = "ACF") +
  p.th
p.acf.res.a
ps$p[[i]] <- p.acf.res.a ; ps$names[i] <- "ModA/plotACFModA" ; ps$var[i] <- "p.acf.res.a" ; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

# Ljung-Box residualer Model A
lag.max <- 20
which.lag <- 1:lag.max
p.vals <- numeric(lag.max)
for (l in 1:lag.max){
  p.vals[l] <- Box.test(res.is.a , lag = l)$p.value
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
p.lbox.res.a
ps$p[[i]] <- p.lbox.res.a ; ps$names[i] <- "ModA/plotLboxResA" ; ps$var[i] <- "p.lbox.res.a" ; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

### ¤¤ Forecast plot Model C ¤¤ ### -------------------------------------------------------------

p.forecast.c <- ggplot(data = data.frame(X1 = dates.all[2160:2281],
                                         X2 = c(DK1$D, OOS$D)[2160:2281] ), 
                       aes(x = X1, y = X2) ) +
  geom_line(aes(col = "Sæsonkorigerede observationer", size = sz$l)) + 
  geom_line(data = data.frame(X1 = dates.all[2160:2281], 
                              X2 = x.pred.oos.c[2160:2281]), 
            aes(col = "Sæsonkorigerede forecast", size = sz$l)) +
  geom_ribbon(aes(ymin = x.pred.oos.c[2160:2281] - pred.inter.c[2160:2281], 
                  ymax = x.pred.oos.c[2160:2281] + pred.inter.c[2160:2281]), 
              fill = colors[6], alpha = 0.4) +
  scale_color_manual(values = c(colors[2], colors[1])) +
  scale_x_date(date_labels = "%b %y", breaks = pretty(dates.all[2160:2281], n = 6)) +
  scale_y_continuous() +
  theme(legend.position = "top" , legend.justification = "left" , 
        legend.direction = "horizontal", legend.background = element_blank()) +
  p.th +
  labs(x = "", y = "Spotpris i DKK/MWh", title = "", color = "")
p.forecast.c
ps$p[[i]] <- p.forecast.c ; ps$names[i] <- "ModC/plotForecastModC" ; ps$var[i] <- "p.forecast.c" ; ps$h[i] <- 3.8; ps$w[i] <- 9
i <- i + 1

# Q-Q plot af sæsonrensede priser
y <- quantile(res.is.c, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
quantiles <- qqnorm(res.is.a)

p.qq.res.c <- ggplot(data.frame(x = quantiles$x, y = quantiles$y), aes(x = x, y = y))+
  geom_point(col = colors[1], size = sz$p) +
  geom_abline(slope = slope, intercept = int, linetype = "dashed", col = colors[2],
              size = sz$l) + 
  labs(x = "Standard normal teoretisk fraktil", y = "Standardiserede residualer") +
  theme(legend.position="none") +
  p.th
p.qq.res.c    
ps$p[[i]] <- p.qq.res.c; ps$names[i] <- "ModC/plotQqResC" ; ps$var[i] <- "p.qq.res.c"; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

# ACF residualer Model A
p.acf.res.c <- ggplot(data.frame(X1 = acf(res.is.c, lag.max = 2190 , plot = FALSE)$lag[2:31],
                                 X2 = acf(res.is.c, lag.max = 2190 , plot = FALSE)$acf[2:31]), 
                      aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0, size = sz$l)) +
  geom_segment(aes(xend = X1, yend = 0), color = colors[1]) +
  geom_hline(aes(yintercept = -ci(res.is.c)), 
             color = colors[2], linetype = "dotted") +
  geom_hline(aes(yintercept =  ci(res.is.c)), 
             color = colors[2], linetype = "dotted") +
  labs(x = "Lag", y = "ACF") +
  p.th
p.acf.res.c
ps$p[[i]] <- p.acf.res.c ; ps$names[i] <- "ModC/plotACFModC" ; ps$var[i] <- "p.acf.res.c" ; ps$h[i] <- 3; ps$w[i] <- 9/2 
i <- i + 1

# Histogram for residualer Model C
p.hist.res.c <- ggplot(data.frame(X2 = res.is.c),
                       aes(x = X2)) +
  geom_histogram(binwidth = 20, color = "white", fill = colors[1]) + 
  stat_function(fun = function(x){dnorm(x = x, 
                                        mean = mean(res.is.c), 
                                        sd = sd(res.is.c))*length(res.is.c)*24.1}, 
                color = colors[2], size = sz$l) +
  labs(x = "Residualer", y = "Tæthed") +
  scale_x_continuous() +
  p.th
p.hist.res.c
ps$p[[i]] <- p.hist.res.c ; ps$names[i] <- "ModC/plotHistResC" ; ps$var[i] <- "p.hist.res.c"; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

# Ljung-Box residualer Model C
lag.max <- 20
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
p.lbox.res.c
ps$p[[i]] <- p.lbox.res.c ; ps$names[i] <- "ModC/plotLboxResC" ; ps$var[i] <- "p.lbox.res.c" ; ps$h[i] <- 3; ps$w[i] <- 9/2
i <- i + 1

### ¤¤ Gemmer plots ¤¤ ### --------------------------------------------------------------
data.frame(names = ps$names , var = ps$var , w = ps$w , h = ps$h)  

wanted.plots <- 1:length(ps$names)
#wanted.plots <- 18

save.plots = TRUE
wid <- 9/2

if(save.plots == TRUE){
  for(j in wanted.plots){
    print(ps$p[[j]])
    ggsave(file = paste("./Grafer/",ps$names[j],".eps" , sep = ""), 
           width = ps$w[j] , height = ps$h[j] , device = cairo_ps , dpi = 600)
  }
}




### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------

