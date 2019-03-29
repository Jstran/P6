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

dfdates <- seq(as.Date("2017/1/1"), by = "month", length.out = 24)
MSEf <- data.frame( X = numeric(24))
MSEf <- t(MSEf)
colnames(MSEf) <- as.character(dfdates)

glob.lm <- lm(DK1$A ~ t + I(t^2) + I(t^3) + I(t^4) +
                      sin((2*pi/365.25)*t) + cos((2*pi/365.25)*t) + 
                      sin((4*pi/365.25)*t) + cos((4*pi/365.25)*t) +
                      sin((8*pi/365.25)*t) + cos((8*pi/365.25)*t) +
                      sin((24*pi/365.25)*t) + cos((24*pi/365.25)*t) +
                      DK1$sat + DK1$sun + DK1$hol , na.action = "na.fail")

lm.combinations <- lapply(dredge(glob.lm , 
                                 evaluate = FALSE,
                                 subset = dc(sin((2*pi/365.25)*t)  , 
                                             cos((2*pi/365.25)*t)  ,
                                             sin((4*pi/365.25)*t)  , 
                                             cos((4*pi/365.25)*t)  ,
                                             sin((8*pi/365.25)*t)  , 
                                             cos((8*pi/365.25)*t)  ,
                                             sin((24*pi/365.25)*t) , 
                                             cos((24*pi/365.25)*t) )), eval)


### ¤¤ Gemmer workspace ¤¤ ### ----------------------------------------------------------

save(t , s.lm, s.pred, DK1, 
     file = "./Workspaces/modelling.Rdata")

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------

# Ronald alpha_0 mean reverision (på al data)
<<<<<<< HEAD
meanrev2 = lm(diff(DK1$D)~DK1$D[1:2190]-1);summary(meanrev2)
=======
meanrev2 = lm(diff(DK1$D)~DK1$D[1:2190]-1);summary(meanrev2)

mse <- numeric(1152)
for (i in 0:24) {
  glob.lm <- lm(DK1$A[1:(1461 + 24*i)] ~ t[1:(1461 + 24*i)] + I(t[1:(1461 + 24*i)]^2) + I(t[1:(1461 + 24*i)]^3) + I(t[1:(1461 + 24*i)]^4) +
                      sin((2*pi/365.25)*t[1:(1461 + 24*i)]) + cos((2*pi/365.25)*t[1:(1461 + 24*i)]) + 
                      sin((4*pi/365.25)*t[1:(1461 + 24*i)]) + cos((4*pi/365.25)*t[1:(1461 + 24*i)]) +
                      sin((8*pi/365.25)*t[1:(1461 + 24*i)]) + cos((8*pi/365.25)*t[1:(1461 + 24*i)]) +
                      sin((24*pi/365.25)*t[1:(1461 + 24*i)]) + cos((24*pi/365.25)*t[1:(1461 + 24*i)]) +
                      DK1$sat[1:(1461 + 24*i)] + DK1$sun[1:(1461 + 24*i)] + DK1$hol[1:(1461 + 24*i)] , na.action = "na.fail")

  lm.combinations <- lapply(dredge(glob.lm , 
                          evaluate = FALSE,
                          subset = dc(sin((2*pi/365.25)*t[1:1461 + 24*i])  , 
                                      cos((2*pi/365.25)*t[1:1461 + 24*i])  ,
                                      sin((4*pi/365.25)*t[1:1461 + 24*i])  , 
                                      cos((4*pi/365.25)*t[1:1461 + 24*i])  ,
                                      sin((8*pi/365.25)*t[1:1461 + 24*i])  , 
                                      cos((8*pi/365.25)*t[1:1461 + 24*i])  ,
                                      sin((24*pi/365.25)*t[1:1461 + 24*i]) , 
                                      cos((24*pi/365.25)*t[1:1461 + 24*i]) )), eval)
  print(i)
}
  for (l in 1:5) {
    mse[l] <- mse[l] + (DK1$A[1:(1461 + 24*i)] - predict(lm.combinations[[l]]))/length(1:(1461 + 24*i))
  }
  

  

>>>>>>> 17a3668e61546f251e54bf5f212ae36bc902b25e
