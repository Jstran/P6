### ¤¤ Intro ¤¤ ### ---------------------------------------------------------------------

rm(list=ls())
load("./Workspaces/preliminary.Rdata")
load("./Workspaces/modelling.Rdata")

# Størrelse til downloads af grafer: 700 x 250

### ¤¤ Pakker ¤¤ ### --------------------------------------------------------------------

library(stats)
library(ggplot2)
library(lubridate)
library(forecast)
library(astsa)

### ¤¤ ggplot af rå data ¤¤ ### ---------------------------------------------------------

# Plot af ukorrigerede data
pRaw <-  ggplot(data.frame(X1 = datesY, 
                           X2 = DK1$Raw), 
                aes(x = X1 , y = X2, size = sz$l) ) +
  geom_line(aes(), color = colors[1]) +
  labs(x = "", y = " Spotpris i DKK", 
       color = "") +
  scale_x_date(date_labels = "%Y", breaks = pretty(datesY, n = 6))  +
  p6
#pRaw

# Plot af korrigerede data
pClean <-  ggplot(data.frame(X1 = datesY, 
                             X2 = DK1$A), 
                  aes(x = X1 , y = X2 , size = sz$l) ) +
  geom_line(aes(), color = colors[1]) +
  labs(x = "", y = " Spotpris i DKK", 
       color = "") +
  scale_x_date(date_labels = "%Y", breaks = pretty(datesY, n = 6))  +
  p6
#pClean

# Histogram for priserne
pHist <- ggplot(data.frame(X1 = datesY, 
                           X2 = DK1$A),
                aes(x = X2)) +
  geom_histogram(binwidth = 20, color = "white", fill = colors[1]) + 
  stat_function(fun = 
                  function(x){dnorm(x = x, 
                                    mean = mean(DK1$A), 
                                    sd = sd(DK1$A))*length(DK1$A)*24.1
                  }, 
                color = colors[2]) +
  labs(x = "Spotpris i DKK", y = "") +
  p6
#pHist

# Plot af acf
pAcfA <- ggplot(data = data.frame(X1 = acf(DK1$A, plot = FALSE)$lag,
                                  X2 = acf(DK1$A, plot = FALSE)$acf), 
                aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0)) +
  geom_segment(aes(xend = X1, yend = 0)) +
  geom_hline(aes(yintercept = -ci()), 
             color = colors[1], linetype = "dotted") +
  geom_hline(aes(yintercept =  ci()), 
             color = colors[1], linetype = "dotted") +
  labs(x = "Lag", y = "ACF") +
  p6
#pAcfA

### ¤¤ Regressions plot ¤¤ ### ----------------------------------------------------------

# Plotter priser hvor det deterministiske er fjernet
pDecomposed <- ggplot(data = data.frame(X1 = datesY,
                                        X2 = DK1$Decomposed),
                      aes(x = X1, y = X2)) +
  geom_line(color = colors[1]) +
  scale_x_date(breaks = pretty(datesY, n = 6)) +
  labs(x = "", y = "Spotpris i DKK") + 
  p6
#pDecomposed

# Plotter spotpriser med den determinstiske model lagt ovenpå
pObsVEsc <-  ggplot(data.frame(X1 = datesY, 
                               X2 = DK1$A), 
                    aes(x = X1 , y = X2 , size = sz$l) ) +
  geom_line(aes(col = "Obs")) +
  geom_point(data = data.frame(X1 = datesY, 
                               X2 = DK1$EscMod), 
             aes(col = "Esc model", size = sz$p) )+
  scale_x_date(date_labels = "%Y", breaks = pretty(datesY, n = 6))  +
  labs(x = "", y = "Spotpris i DKK", title = "", color = "") +
  scale_color_manual(values = colors[1:2]) +
  p6 +
  theme(legend.position = c(0.13,0.9), legend.direction = "horizontal",
        legend.background = element_blank())
#pObsVEsc

# Plot af acf for decomposed
pAcfDecomposed <- ggplot(data = data.frame(X1 = acf(DK1$Decomposed, plot = FALSE)$lag,
                                           X2 = acf(DK1$Decomposed, plot = FALSE)$acf), 
                         aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0)) +
  geom_segment(aes(xend = X1, yend = 0)) +
  geom_hline(aes(yintercept = -ci()), 
             color = colors[1], linetype = "dotted") +
  geom_hline(aes(yintercept =  ci()), 
             color = colors[1], linetype = "dotted") +
  labs(x = "Lag", y = "ACF") +
  p6
#pAcfDecomposed


### ¤¤ Gemmer plots ¤¤ ### --------------------------------------------------------------

#dev.on()
#pAcfDecomposed
#dev.off()

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------



