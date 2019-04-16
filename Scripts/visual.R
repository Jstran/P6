### ¤¤ Intro ¤¤ ### ---------------------------------------------------------------------

rm(list=ls())
load("./Workspaces/preliminary.Rdata")
load("./Workspaces/modelling.Rdata")
load("./Workspaces/forecast.Rdata")

i <- 1
ps <- list(names = c() , var = c(),  p = c() , i = c() , l = c())

# Størrelse til downloads af grafer: 700 x 250

### ¤¤ Pakker ¤¤ ### --------------------------------------------------------------------

library(stats)
library(ggplot2)
library(lubridate)
library(forecast)
library(astsa)
library(tseries)

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
ps$p[[i]] <- p.raw ; ps$names[i] <- "plotRaw" ; ps$var[i] <- "p.raw"; ps$l[i] <- FALSE
i <- i + 1

# Plot af korrigerede data
p.clean <-  ggplot(data.frame(X1 = dates, 
                              X2 = DK1$A ), 
                   aes(x = X1 , y = X2 , size = sz$l) ) +
  geom_line(aes(), color = colors[1]) +
  labs(x = "", y = " Spotpris i DKK/MWh", color = "") +
  p.Y
#p.clean
ps$p[[i]] <- p.clean ; ps$names[i] <- "plotClean" ; ps$var[i] <- "p.clean"
ps$l[i] <- FALSE ; i <- i + 1


# Histogram for priserne
p.hist <- ggplot(data.frame(X2 = DK1$A),
                 aes(x = X2)) +
  geom_histogram(binwidth = 20, color = "white", fill = colors[1]) + 
  stat_function(fun = function(x){dnorm(x = x, 
                                  mean = mean(DK1$A), 
                                  sd = sd(DK1$A))*length(DK1$A)*24.1}, 
                 color = colors[2]) +
  labs(x = "Spotpris i DKK/MWh", y = "") +
  scale_x_continuous() +
  p.th
#p.hist
ps$p[[i]] <- p.hist ; ps$names[i] <- "plotHist" ; ps$var[i] <- "p.hist"; ps$l[i] <- FALSE
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
ps$p[[i]] <- p.acf.A ; ps$names[i] <- "plotACFAll" ; ps$var[i] <- "p.acf.A"
ps$l[i] <- FALSE ; i <- i + 1


### ¤¤ Regressions plot ¤¤ ### ----------------------------------------------------------

# Plotter priser hvor det deterministiske er fjernet
p.d <- ggplot(data = data.frame(X1 = dates,
                                X2 = DK1$D),
              aes(x = X1, y = X2 , size = sz$l) ) +
  geom_line(color = colors[1]) +
  labs(x = "", y = "Spotpris i DKK/MWh") + 
  p.Y
p.d
ps$p[[i]] <- p.d ; ps$names[i] <- "plotRemDet" ; ps$var[i] <- "p.d" ; ps$l[i] <- FALSE
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
ps$p[[i]] <- p.s.A ; ps$names[i] <- "plotSpotDet" ; ps$var[i] <- "p.s.A"; ps$l[i] <- TRUE
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
ps$p[[i]] <- p.d.acf ; ps$names[i] <- "plotACFDecomp" ; ps$var[i] <- "p.d.acf"
ps$l[i] <- FALSE ; i <- i + 1

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
p.d.hist

### ¤¤ Forecast plot ¤¤ ### -------------------------------------------------------------

p.forecast <- ggplot(data = data.frame(X1 = dates.all[2000:2281],
                                       X2 = c(DK1$D, OOS$D)[2000:2281] ), 
                     aes(x = X1, y = X2) ) +
              geom_line(aes(col = "Sæsonkorigerede", size = sz$l)) + 
              scale_x_date(date_labels = "%Y", breaks = pretty(dates.all[2000:2281], n = 6)) +
              geom_line(data = data.frame(X1 = dates.all[2000:2281], 
                                          X2 = x.pred[2000:2281]), 
                        aes(col = "Sæsonkorigerende forecast", size = sz$l)) +
              geom_ribbon(aes(ymin = x.pred[2000:2281] - pred.inter, ymax = x.pred[2000:2281] + pred.inter), fill = "grey70", alpha = 0.4) +
              scale_color_manual(values = colors[1:2]) +
              p.th +
              scale_y_continuous() +
              theme(legend.position = "top" , legend.justification = "left" , 
              legend.direction = "horizontal", legend.background = element_blank()) +
              labs(x = "", y = "Spotpris i DKK/MWh", title = "", color = "");p.forecast
ps$p[[i]] <- p.forecast ; ps$names[i] <- "plotForecastModA" ; ps$var[i] <- "p.forecast" ; ps$l[i] <- T
i <- i + 1

### ¤¤ Gemmer plots ¤¤ ### --------------------------------------------------------------

wanted.plots = 1:8
save.plots = TRUE
wid <- 9

if(save.plots == TRUE){
    for(j in wanted.plots){
      if(ps$l[j] == FALSE){hei <- 3}
      else{hei <- 3.8}
      
      postscript(file = paste("./Grafer/",ps$names[j],".eps" , sep = ""), 
                 width = wid, height = hei , horizontal = FALSE)
      print(ps$p[[j]])
      dev.off()
  }
}

data.frame(names = ps$names , var = ps$var )

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------

#x <- seq(0,1,length.out = 2000)

#curve(sin(2*pi*x) , x , col = "purple")
#lines(x , sin(4*pi*x) , col = "blue")
#lines(x , sin(8*pi*x) , col = "green")
