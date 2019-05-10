### ¤¤ Intro ¤¤ ### ---------------------------------------------------------------------

rm(list=ls())
load("./Workspaces/preliminary.Rdata")
load("./Workspaces/forecast.Rdata")

### ¤¤ Pakker ¤¤ ### --------------------------------------------------------------------

library(stats)
library(ggplot2)
library(forecast)
library(astsa)
library(timeDate) # Til skewness og kurtosis
library(MuMIn)    # Til test af modeller
library(tseries)
library(pracma)
library(zoo)
library(rms)
library(nlme)
library(psd)


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
t  <- time(DK1$A)
pc <- pi/365.25 

df <- data.frame(DK1 = DK1$A , t = t , sat = DK1$sat , sun = DK1$sun , hol = DK1$hol ,
                 sin2  = sin((2*pc)*t)  , cos2  = cos((2*pc)*t) ,
                 sin8  = sin((8*pc)*t)  , cos8  = cos((8*pc)*t) ,
                 sin24 = sin((24*pc)*t) , cos24 = cos((24*pc)*t) )

s.lm <- lm(DK1 ~ t + I(t^2) + 
                 sin2 + cos2 + sin8 + cos8 + sin24 + cos24 +
                 sat + sun + hol , data = df)

s.pred  <- predict(s.lm)

DK1[[length(DK1)+1]] <- c(s.pred)
names(DK1)[[length(DK1)]] <- "s.pred"

DK1[[length(DK1)+1]] <- c(s.lm$residuals)
names(DK1)[[length(DK1)]] <- "D"

# GLM for standard errors
A <- DK1$A
sat <- DK1$sat
sun <- DK1$sun
hol <- DK1$hol
s.gls <- gls(A ~ t + I(t^2) +
             sin((2*pi/365.25 )*t)  +  cos((2*pi/365.25 )*t) +
             sin((8*pi/365.25 )*t)  +  cos((8*pi/365.25 )*t) +
             sin((24*pi/365.25 )*t) + cos((24*pi/365.25 )*t) +
             sat + sun + hol, weights = varPower())

summary(s.gls)

# OOS data
t.oos  <- (length(DK1$A) + 1):(length(DK1$A) + length(OOS$A))
df.oos <- data.frame(t = t.oos, sat = OOS$sat, sun = OOS$sun, hol = OOS$hol, 
                     sin2  = sin((2*pc)*t.oos)  , cos2  = cos((2*pc)*t.oos) ,
                     sin8  = sin((8*pc)*t.oos)  , cos8  = cos((8*pc)*t.oos) ,
                     sin24 = sin((24*pc)*t.oos) , cos24 = cos((24*pc)*t.oos) )

s.pred.oos <- predict(s.lm, newdata = df.oos)

OOS[[length(OOS) + 1]] <- c(s.pred.oos)
names(OOS)[[length(OOS)]] <- "s.pred"

OOS[[length(OOS) + 1]] <- c(OOS$A - s.pred.oos)
names(OOS)[[length(OOS)]] <- "D"

rm("df", "df.oos", "A", "sat", "sun", "hol")

### ¤¤ Tabel med estimater og std. fejl i OLS og GLS ¤¤ ### -----------------------------

co.sd.lm <- scoef(s.lm)
sd.gls <- scoef(s.gls)$sdc

tbl <- data.frame(Estimates = as.numeric(co.sd.lm$coef),sdOLS = as.numeric(co.sd.lm$sdc), 
                  sdGLS = as.numeric(sd.gls) ) ; rownames(tbl) <- colnames(co.sd.lm$coef)
tbl

### ¤¤ ADF-test ¤¤ ### ------------------------------------------------------------------
DK1.A.filtered <- rollmedian(DK1$A , k = 5)

adf.test(DK1.A.filtered)

adf.test(DK1.A.filtered, k = 0)


DK1.D.filtered <- rollmedian(DK1$D , k = 5)

adf.test(DK1.D.filtered)

adf.test(DK1.D.filtered, k = 0)

### ¤¤ ARMAX ¤¤ ### ---------------------------------------------------------------------
# Kilde: https://onlinecourses.science.psu.edu/stat510/lesson/9/9.1
ccf(DK1$D, DK1$W)

# Nedenstående tyder på MA(3)
acf(DK1$W)
pacf(DK1$W)

w.arma <- auto.arima(DK1$W)
w.arma

w.arma.res <- w.arma$residuals

plot(w.arma.res)
dk1.d.prew <- residuals(Arima(DK1$D, model = w.arma))

ccf(w.arma$residuals, dk1.d.prew)

### ¤¤ Gemmer workspace ¤¤ ### ----------------------------------------------------------

save(t , s.lm, s.pred, DK1, OOS, w.arma.res, dk1.d.prew,
     file = "./Workspaces/modelling.Rdata")

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------

