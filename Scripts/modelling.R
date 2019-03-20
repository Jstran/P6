### ¤¤ Intro ¤¤ ### ---------------------------------------------------------------------

rm(list=ls())
load("./Workspaces/preliminary.Rdata")

### ¤¤ Pakker ¤¤ ### --------------------------------------------------------------------

library(stats)
library(ggplot2)
library(forecast)
library(astsa)

### ¤¤ Regression ¤¤ ### ----------------------------------------------------------------

# Regression på s model med kvadratisk led (t^2)
t     <- time(DK1$A)
s.lm <- lm(DK1$A ~ t + I(t^2) + 
             sin((2*pi/365.25)*t) + cos((2*pi/365.25)*t) + 
             sin((4*pi/365.25)*t) + cos((4*pi/365.25)*t) +
             sin((8*pi/365.25)*t) + cos((8*pi/365.25)*t) +
             DK1$sat + DK1$sun + DK1$hol)

s.pred  <- predict(s.lm)

DK1[[length(DK1)+1]] <- c(s.pred)
names(DK1)[[length(DK1)]] <- "s.pred"

DK1[[length(DK1)+1]] <- c(DK1$A - s.pred)
names(DK1)[[length(DK1)]] <- "D"

### ¤¤ Gemmer workspace ¤¤ ### ----------------------------------------------------------

save(t , s.lm, s.pred, DK1, 
     file = "./Workspaces/modelling.Rdata")

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------



