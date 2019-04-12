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
library(pracma)
library(zoo)


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

DK1.D.filtered <- rollmedian(DK1$D , k = 5)

adf.test(DK1.D.filtered)

adf.test(DK1.D.filtered, k = 0)

### ¤¤ Gemmer workspace ¤¤ ### ----------------------------------------------------------

save(t , s.lm, s.pred, DK1, 
     file = "./Workspaces/modelling.Rdata")

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------


### ¤¤ Ronald ¤¤ ### --------------------------------------------------------------------
# Parameter estimering ----------------------------------------------------
# Hamilton, Regime-Switching Models, 2005, metode til MLE

DK1$D = as.numeric(DK1$D)
l = length(DK1$D)
f = function(theta){
  sigma_1 = theta[1];sigma_2=theta[2];sigma_3=theta[3];alpha_1=theta[4];alpha_3=theta[5];p = theta[6];mu_2=theta[7]
  like=c()
  xi= matrix(nrow=l,ncol=3); xi[1,]=1/3
  likesum = 0
  for (i in 1:(l-1)) {
    like[i]= p*xi[i,1]*dnorm(DK1$D[i+1], mean = (1-alpha_1)*DK1$D[i], sd = sigma_1) +
            (1-p)*xi[i,1]*dnorm(DK1$D[i+1], mean = (-mu_2 + DK1$D[i]), sd = sigma_2) +
            xi[i,2]*dnorm(DK1$D[i+1], mean = ((1-alpha_3)*DK1$D[i]), sd = sigma_3) +
            xi[i,3]*dnorm(DK1$D[i+1], mean = ((1-alpha_1)*DK1$D[i]), sd = sigma_1)
    xi[i+1,1] = (p*xi[i,1]*dnorm(DK1$D[i+1], mean = (1-alpha_1)*DK1$D[i], sd = sigma_1) +
                xi[i,3]*dnorm(DK1$D[i+1], mean = ((1-alpha_1)*DK1$D[i]), sd = sigma_1))/
                (like[i])
    xi[i+1,2] = ((1-p)*xi[i,1]*dnorm(DK1$D[i+1], mean = (-mu_2 + DK1$D[i]), sd = sigma_2))/like[i]
    xi[i+1,3] = xi[i,2]*dnorm(DK1$D[i+1], mean = ((1-alpha_3)*DK1$D[i]), sd = sigma_3)/like[i]
    likesum = likesum + log(like[i])
  }
  return(-likesum)
}
f(par)
par = c(10,50,10,0.5,0.5,0.9,5)
MRS = optim(par, f, lower = c(0,0,0,0,0,0.000001,-100), 
      upper = c(100,100,100,0.99999,0.99999,0.99999,100), method = "L-BFGS-B", control=list(trace=TRUE, maxit= 500))


# Simulering --------------------------------------------------------------
set.seed(1)
a_1 = MRS$par[4]
a_3 = MRS$par[5]
r_1= numeric(1)
r_2= numeric(1)
r_3= numeric(1)
X_t= numeric(1)

eps = rnorm(2191, mean = 0, sd = 1)
prob = runif(2191)
limit = MRS$par[6]
sigma_1 = MRS$par[1]
sigma_2 = MRS$par[2]
sigma_3 = MRS$par[3]
mu = MRS$par[7]
s=1
spikes = 0
state = c()
#r_1[i] = (1-a_1)*r_1[i-1] + sigma_1*eps[i]
#r_2[i] = r_2[i-1] + mu + sigma_2*eps[i]
#r_3[i] = (1-a_3)*r_3[i-1] + sigma_3*eps[i]

for (i in 2:2191) {
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
plot(state)
par(mfrow=c(2,1))
ts.plot(X_t)
ts.plot(DK1$D)

par(mfrow = c(1,1))

quantile(X_t)
quantile(DK1$D)
