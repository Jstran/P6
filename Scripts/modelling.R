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
pc    <- pi/365.25 
s.lm <- lm(DK1$A ~ t + I(t^2) + 
             sin((2*pc)*t) + cos((2*pc)*t) + 
             sin((4*pc)*t) + cos((4*pc)*t) +
             sin((8*pc)*t) + cos((8*pc)*t) +
             sin((96*pc)*t) + cos((96*pc)*t) +
             DK1$sat + DK1$sun + DK1$hol)

s.pred  <- predict(s.lm)

DK1[[length(DK1)+1]] <- c(s.pred)
names(DK1)[[length(DK1)]] <- "s.pred"

DK1[[length(DK1)+1]] <- c(DK1$A - s.pred)
names(DK1)[[length(DK1)]] <- "D"

### ¤¤ AIC af forskellige modeller ¤¤ ### -----------------------------------------------

rmse <- numeric(576)
aic  <- numeric(576)
for (i in 0:730) {
  len <- c(1:(1461 + i))
  glob.lm <- lm(DK1$A[len] ~ t[len] + I(t[len]^2) + I(t[len]^3) +
                  sin((2*pc)*t[len]) + cos((2*pc)*t[len]) + 
                  sin((4*pc)*t[len]) + cos((4*pc)*t[len]) +
                  sin((8*pc)*t[len]) + cos((8*pc)*t[len]) +
                  sin((24*pc)*t[len]) + cos((24*pc)*t[len]) +
                  DK1$sat[len] + DK1$sun[len] + DK1$hol[len] , na.action = "na.fail")
  
  lm.combinations <- lapply(dredge(glob.lm , 
                                   evaluate = FALSE,
                                   subset = dc(sin(( 2*pc)*t[len])  , 
                                               cos(( 2*pc)*t[len])  ,
                                               sin(( 4*pc)*t[len])  , 
                                               cos(( 4*pc)*t[len])  ,
                                               sin(( 8*pc)*t[len])  , 
                                               cos(( 8*pc)*t[len])  ,
                                               sin((24*pc)*t[len]) , 
                                               cos((24*pc)*t[len]) ) ),
                            eval)
  aic <- aic + sapply(lm.combinations, AIC)
  
  for (l in 1:576) {
    pred.inter <- data.frame(t = 1461 + i + 1)
    rmse[l]    <- rmse[l] + sqrt((DK1$A[pred.inter$t] - predict(lm.combinations[[l]], newdata=pred.inter))^2)/length(pred.inter)
  }
  print(i)  
}

# Gennemsnitter
mse <- rmse/730
aic <- aic/730

lm.combinations[[which.min(aic)]]
lm.combinations[[which.min(rmse)]]


### ¤¤ Gemmer workspace ¤¤ ### ----------------------------------------------------------

save(t , s.lm, s.pred, DK1, 
     file = "./Workspaces/modelling.Rdata")

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------

# Ronald alpha_0 mean reverision (på al data)

meanrev2 = lm(diff(DK1$D)~DK1$D[1:2190]-1);summary(meanrev2)






