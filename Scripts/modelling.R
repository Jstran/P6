### ¤¤ Intro ¤¤ ### ---------------------------------------------------------------------

rm(list=ls())
load("./Workspaces/preliminary.Rdata")

### ¤¤ Pakker ¤¤ ### --------------------------------------------------------------------

library(stats)
library(ggplot2)
library(forecast)
library(astsa)
library(timeDate) # Til skewness og kurtosis
library(MuMIn) # Til test af modeller

### ¤¤ Infotabeller om data ¤¤ ### ------------------------------------------------------

dat.wd  <- DK1$A[!as.logical(DK1$sat + DK1$sun + DK1$hol)]
dat.sat <- DK1$A[as.logical(DK1$sat)]
dat.sun <- DK1$A[as.logical(DK1$sun)]
dat.hol <- DK1$A[as.logical(DK1$hol)]
ls <- list(wd = dat.wd , sat = dat.sat , sun = dat.sun , hol = dat.hol , A = DK1$A)

df <- data.frame(mean = sapply(ls , mean)         ,
                 sd   = sapply(ls , sd)           ,
                 skew = sapply(ls , skewness)     ,
                 kurt = sapply(ls , kurtosis) + 3 ,
                 min  = sapply(ls , min)          ,
                 max  = sapply(ls , max)          ,
                 len  = sapply(ls , length)       )
rownames(df) <- c("Hverdage" , "Lørdage" , "Søndage" , "Helligdage" , "Alle Dage") ; df

rm("dat.wd" , "dat.sat" , "dat.sun" , "dat.hol" , "ls" , "df")

### ¤¤ Regression ¤¤ ### ----------------------------------------------------------------

# Regression på s model med kvadratisk led (t^2)
t     <- time(DK1$A)
s.lm <- lm(DK1$A ~ t + I(t^2) + 
             sin((2*pi/365.25)*t) + cos((2*pi/365.25)*t) + 
             sin((4*pi/365.25)*t) + cos((4*pi/365.25)*t) +
             sin((8*pi/365.25)*t) + cos((8*pi/365.25)*t) +
             sin((96*pi/365.25)*t) + cos((96*pi/365.25)*t) +
             DK1$sat + DK1$sun + DK1$hol)

s.pred  <- predict(s.lm)

DK1[[length(DK1)+1]] <- c(s.pred)
names(DK1)[[length(DK1)]] <- "s.pred"

DK1[[length(DK1)+1]] <- c(DK1$A - s.pred)
names(DK1)[[length(DK1)]] <- "D"

### ¤¤ AIC af forskellige modeller ¤¤ ### -----------------------------------------------

#M <- 2^15
#
#MSEf <- data.frame( X2014 = numeric(M) , X2015 = numeric(M) ,
#                    X2016 = numeric(M) , X2018 = numeric(M))
#
#glob.lm <- lm(DK1$A ~ t + I(t^2) + I(t^3) + I(t^4) +
#                      sin((2*pi/365.25)*t) + cos((2*pi/365.25)*t) + 
#                      sin((4*pi/365.25)*t) + cos((4*pi/365.25)*t) +
#                      sin((8*pi/365.25)*t) + cos((8*pi/365.25)*t) +
#                      sin((24*pi/365.25)*t) + cos((24*pi/365.25)*t) +
#                      DK1$sat + DK1$sun + DK1$hol , na.action = "na.fail")
#
#lm.combinations <- dredge(glob.lm , 
#                          subset = dc(sin((2*pi/365.25)*t)  , 
#                                      cos((2*pi/365.25)*t)  ,
#                                      sin((4*pi/365.25)*t)  , 
#                                      cos((4*pi/365.25)*t)  ,
#                                      sin((8*pi/365.25)*t)  , 
#                                      cos((8*pi/365.25)*t)  ,
#                                      sin((24*pi/365.25)*t) , 
#                                      cos((24*pi/365.25)*t) )) 
#
#ind <- as.integer(na.omit(row.names(lm.combinations[1:M,] ) ) )
#
#MSEf[ind,1] <- lm.combinations$AICc

### ¤¤ Gemmer workspace ¤¤ ### ----------------------------------------------------------

save(t , s.lm, s.pred, DK1, 
     file = "./Workspaces/modelling.Rdata")

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------

# Ronald alpha_0 mean reverision (på al data)
meanrev2 = lm(diff(DK1$D)~DK1$D[1:2190]-1);summary(meanrev2)
