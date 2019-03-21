### ¤¤ Intro ¤¤ ### ---------------------------------------------------------------------

rm(list=ls())
load("./Workspaces/preliminary.Rdata")
load("./Workspaces/modelling.Rdata")

i <- 1
ps <- list(names = c() , p = c() , i = c())

# Størrelse til downloads af grafer: 700 x 250

### ¤¤ Pakker ¤¤ ### --------------------------------------------------------------------

library(stats)
library(ggplot2)
library(lubridate)
library(forecast)
library(astsa)

### ¤¤ ggplot af rå data ¤¤ ### ---------------------------------------------------------

# Plot af ukorrigerede data
p.raw <-  ggplot(data.frame(X1 = dates, 
                            X2 = DK1$Raw), 
                 aes(x = X1 , y = X2, size = sz$l) ) +
  geom_line(aes(), color = colors[1]) +
  labs(x = "", y = " Spotpris i DKK/MWh", 
       color = "") +
  p.Y
p.raw
ps$p[[i]] <- p.raw ; ps$names[i] <- "plotRaw"
i <- i + 1

# Plot af korrigerede data
p.clean <-  ggplot(data.frame(X1 = dates, 
                              X2 = DK1$A), 
                   aes(x = X1 , y = X2 , size = sz$l) ) +
  geom_line(aes(), color = colors[1]) +
  labs(x = "", y = " Spotpris i DKK/MWh", color = "") +
  p.Y
#p.clean
ps$p[[i]] <- p.clean ; ps$names[i] <- "plotClean"
i <- i + 1


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
ps$p[[i]] <- p.hist ; ps$names[i] <- "plotHist"
i <- i + 1


# Plot af acf
p.acf.A <- ggplot(data = data.frame(X1 = acf(DK1$A, plot = FALSE)$lag,
                                    X2 = acf(DK1$A, plot = FALSE)$acf), 
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
ps$p[[i]] <- p.acf.A ; ps$names[i] <- "plotACFAll"
i <- i + 1


### ¤¤ Regressions plot ¤¤ ### ----------------------------------------------------------

# Plotter priser hvor det deterministiske er fjernet
p.d <- ggplot(data = data.frame(X1 = dates,
                                X2 = DK1$D),
              aes(x = X1, y = X2)) +
  geom_line(color = colors[1]) +
  labs(x = "", y = "Spotpris i DKK/MWh") + 
  p.Y
#p.d
ps$p[[i]] <- p.d ; ps$names[i] <- "plotRemDet" 
i <- i + 1


# Plotter spotpriser med den determinstiske model lagt ovenpå
p.s.A <-  ggplot(data.frame(X1 = dates, 
                             X2 = DK1$A), 
                  aes(x = X1 , y = X2 , size = sz$l) ) +
  geom_line(aes(col = "Obs")) +
  geom_point(data = data.frame(X1 = dates, 
                               X2 = DK1$s.pred), 
             aes(col = "lm model", size = sz$p) )+
  labs(x = "", y = "Spotpris i DKK/MWh", title = "", color = "") +
  scale_color_manual(values = colors[1:2]) +
  p.Y +
  theme(legend.position = c(0.13,0.9), legend.direction = "horizontal",
        legend.background = element_blank())
#p.s.A
ps$p[[i]] <- p.s.A ; ps$names[i] <- "plotSpotDet"
i <- i + 1


# Plot af acf for decomposed
p.d.acf <- ggplot(data = data.frame(X1 = acf(DK1$D, plot = FALSE)$lag,
                                    X2 = acf(DK1$D, plot = FALSE)$acf), 
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
ps$p[[i]] <- p.d.acf ; ps$names[i] <- "plotACFDecomp"
i <- i + 1



### ¤¤ Gemmer plots ¤¤ ### --------------------------------------------------------------

data.frame(names = ps$names)

wanted.plots = 1:7
save.plots = FALSE

if(save.plots == TRUE){
    for(j in wanted.plots){
      postscript(file = paste("./Grafer/",ps$names[j],".eps" , sep = ""), 
                 width = 9, height = 3 , horizontal = FALSE)
      print(ps$p[[j]])
      dev.off()
  }
}
### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------





