### ¤¤ Intro ¤¤ ### ---------------------------------------------------------------------

rm(list=ls())
load("./Workspaces/preliminary.Rdata")

### ¤¤ Pakker ¤¤ ### --------------------------------------------------------------------

library(stats)
library(ggplot2)
library(forecast)
library(astsa)
library(timeDate) # Til skewness og kurtosis
library(MuMIn)    # Til test af modeller
library(tseries)


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

rm("df")

### ¤¤ Estimater for hele perioden i vores model ¤¤ ### ---------------------------------

tbl <- scoef(s.lm) ; tbl

### ¤¤ ADF-test ¤¤ ### ------------------------------------------------------------------
adf.test(DK1$D)

adf.test(DK1$D, k = 0)

### ¤¤ Gemmer workspace ¤¤ ### ----------------------------------------------------------

save(t , s.lm, s.pred, DK1, 
     file = "./Workspaces/modelling.Rdata")

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------


### ¤¤ Ronald ¤¤ ### --------------------------------------------------------------------
# Ronald alpha_0 mean reverision (på al data)
meanrev2 = lm(diff(DK1$D)~DK1$D[1:2190]-1);summary(meanrev2)

set.seed(1)
a_1 = as.numeric(-meanrev2$coefficients[1])
a_3 = 0.444
r_1= c();r_1[1]=0
r_2= c();r_2[1]=0
r_3= c();r_3[1]=0
X_t= c();X_t[1]=0

eps = rnorm(2000, mean = 0, sd = 1)
prob = runif(2000)
limit = 0.926
sigma_1 = 0.34
sigma_2 = 0.874
sigma_3 = 0.918
mu = 0.377
s=1
spikes = 0
state = c()
#r_1[i] = (1-a_1)*r_1[i-1] + sigma_1*eps[i]
#r_2[i] = r_2[i-1] + mu + sigma_2*eps[i]
#r_3[i] = (1-a_3)*r_3[i-1] + sigma_3*eps[i]

for (i in 2:2000) {
  if (s == 3) {
    X_t[i]=(1-a_1)*X_t[i-1] + sigma_1*eps[i]
    s = 1
    state = c(state,s)
  }
  if (s == 2) { 
    X_t[i] = (1-a_3)*X_t[i-1] + sigma_3*eps[i]
    s = 3
    state = c(state,s)
  }
  if (prob[i]>=limit && s == 1) {
    X_t[i] = X_t[i-1] + mu + sigma_2*eps[i]
    s = 2
    state = c(state,s)
    spikes = spikes + 1
  }
  if (prob[i]<limit && s == 1) {
    X_t[i]=(1-a_1)*X_t[i-1] + sigma_1*eps[i]
    state = c(state,s)
  }
}
print(spikes)
plot(state)
par(mfrow=c(1,2))
ts.plot(X_t)
ts.plot(DK1$D)
par(mfrow = c(1,1))

quantile(X_t)
quantile(DK1$D)
