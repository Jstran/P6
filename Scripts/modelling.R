### ¤¤ Intro ¤¤ ### ---------------------------------------------------------------------

rm(list=ls())
load("./Workspaces/preliminary.Rdata")

### ¤¤ Pakker ¤¤ ### --------------------------------------------------------------------

library(stats)
library(ggplot2)
library(forecast)
library(astsa)

### ¤¤ Gemmer workspace ¤¤ ### ----------------------------------------------------------

save(n.obs, 
     file = "./Workspaces/modelling.Rdata")

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------



