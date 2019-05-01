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

### ¤¤ Estimater for hele perioden i vores model ¤¤ ### ---------------------------------

tbl <- scoef(s.lm) ; tbl

### ¤¤ ADF-test ¤¤ ### ------------------------------------------------------------------
DK1.A.filtered <- rollmedian(DK1$A , k = 5)

adf.test(DK1.A.filtered)

adf.test(DK1.A.filtered, k = 0)


DK1.D.filtered <- rollmedian(DK1$D , k = 5)

adf.test(DK1.D.filtered)

adf.test(DK1.D.filtered, k = 0)

### ¤¤ Gemmer workspace ¤¤ ### ----------------------------------------------------------

save(t , s.lm, s.pred, DK1, OOS,
     file = "./Workspaces/modelling.Rdata")

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------
plot(res.is, type = "l", main = "Residualer")
acf(res.is, main = "Residualer")
Box.test(res.is, type = "Ljung-Box") # H0 er, at tidsrækken er hvid støj.


# Parameter estimering ----------------------------------------------------
# Hamilton, Regime-Switching Models, 2005, metode til MLE

#DK1$D = as.numeric(DK1$D)
#l = length(DK1$D)
#f = function(theta){
#  sigma_1 = theta[1];sigma_2=theta[2];sigma_3=theta[3];alpha_1=theta[4];alpha_3=theta[5];p = theta[6];mu_2=theta[7]
#  like=c()
#  xi= matrix(nrow=l,ncol=3); xi[1,]=1/3
#  likesum = 0
#  for (i in 1:(l-1)) {
#    like[i]=   p *xi[i,1]*dnorm(DK1$D[i+1],  mean =  (1-alpha_1)*DK1$D[i] , sd = sigma_1) +
#            (1-p)*xi[i,1]*dnorm(DK1$D[i+1],  mean = (-mu_2 + DK1$D[i])    , sd = sigma_2) +
#                  xi[i,2]*dnorm(DK1$D[i+1],  mean = ((1-alpha_3)*DK1$D[i]), sd = sigma_3) +
#                  xi[i,3]*dnorm(DK1$D[i+1],  mean = ((1-alpha_1)*DK1$D[i]), sd = sigma_1)

#    xi[i+1,1] =(p*xi[i,1]*dnorm(DK1$D[i+1],  mean =  (1-alpha_1)*DK1$D[i] , sd = sigma_1) +
#                  xi[i,3]*dnorm(DK1$D[i+1],  mean = ((1-alpha_1)*DK1$D[i]), sd = sigma_1))/(like[i])

#    xi[i+1,2] =((1-p)*xi[i,1]*dnorm(DK1$D[i+1], mean = (-mu_2 + DK1$D[i]), sd = sigma_2))/like[i]

#    xi[i+1,3] =   xi[i,2]*dnorm(DK1$D[i+1], mean = ((1-alpha_3)*DK1$D[i]), sd = sigma_3)/like[i]
#    likesum = likesum + log(like[i])
#  }
#  return(-likesum)
#}
#par = c(10,50,10,0.5,0.5,0.9,5)
#f(par)
#MRS = optim(par, f, lower = c(0,0,0,0,0,0.000001,-100), 
#      upper = c(100,100,100,0.99999,0.99999,0.99999,100), method = "L-BFGS-B", control=list(trace=TRUE, maxit= 500))


# Simulering --------------------------------------------------------------
#set.seed(10)
#a_1 = MRS$par[4]
#a_3 = MRS$par[5]
#r_1= numeric(1)
#r_2= numeric(1)
#r_3= numeric(1)
#X_t= numeric(1)

#eps = rnorm(2191, mean = 0, sd = 1)
#prob = runif(2191)
#limit = MRS$par[6]
#sigma_1 = MRS$par[1]
#sigma_2 = MRS$par[2]
#sigma_3 = MRS$par[3]
#mu = MRS$par[7]
#s=1
#spikes = 0
#state = c()
#r_1[i] = (1-a_1)*r_1[i-1] + sigma_1*eps[i]
#r_2[i] = r_2[i-1] + mu + sigma_2*eps[i]
#r_3[i] = (1-a_3)*r_3[i-1] + sigma_3*eps[i]

#for (i in 2:100) {
#  if (s == 3) {
#    X_t[i]=(1-a_1)*X_t[i-1] + sigma_1*eps[i]
#    s = 1
#    state = c(state,s)
#  }
#  if (s == 2) { 
#    X_t[i] = (1-a_3)*X_t[i-1] + sigma_3*eps[i]
#    s = 3
#    state = c(state,s)
#  }
#  if (prob[i]>=limit && s == 1) {
#    X_t[i] = X_t[i-1] + mu + sigma_2*eps[i]
#    s = 2
#    state = c(state,s)
#    spikes = spikes + 1
#  }
#  if (prob[i]<limit && s == 1) {
#    X_t[i]=(1-a_1)*X_t[i-1] + sigma_1*eps[i]
#    state = c(state,s)
#  }
#}
#
#xt <- X_t
#xt[1] <- DK1$D[2191-100]
#df <- data.frame(t = seq(to = 191, length.out = 100), xt)

#plot(DK1$D[2000:2191], type = "l")
#lines(df, col = "red")

#plot(state)
#par(mfrow=c(2,1))
#ts.plot(X_t)
#ts.plot(DK1$D)

#par(mfrow = c(1,1))

#quantile(X_t)
#quantile(DK1$D)


# Model med mulighed for flere regime 2 og 3 i streg ----------------------
#DK1$D = as.numeric(DK1$D)
#l = length(DK1$D)
#f_2 = function(theta){
  
#  sigma_1 = theta[1];sigma_2=theta[2];sigma_3=theta[3];
#  alpha_1=theta[4];alpha_3=theta[5];p_11 = theta[6];p_22 = theta[7]; p_33 = theta[8];mu_2=theta[9]
#  like=c()
#  xi= matrix(nrow=l,ncol=3); xi[1,]=1/3
#  likesum = 0
#  for (i in 1:(l-1)) {
#    like[i]= p_11*xi[i,1]*dnorm(DK1$D[i+1], mean = (1-alpha_1)*DK1$D[i], sd = sigma_1) +
#      (1-p_11)*xi[i,1]*dnorm(DK1$D[i+1], mean = (-mu_2 + DK1$D[i]), sd = sigma_2) +
#      p_22*xi[i,2]*dnorm(DK1$D[i+1], mean = (-mu_2 + DK1$D[i]), sd = sigma_2) +
#      (1-p_22)*xi[i,2]*dnorm(DK1$D[i+1], mean = ((1-alpha_3)*DK1$D[i]), sd = sigma_3) +
#      (1-p_33)*xi[i,3]*dnorm(DK1$D[i+1], mean = ((1-alpha_1)*DK1$D[i]), sd = sigma_1) +
#      p_33*xi[i,3]*dnorm(DK1$D[i+1], mean = ((1-alpha_3)*DK1$D[i]), sd = sigma_3)
#    
#    
#    xi[i+1,1] = (p_11*xi[i,1]*dnorm(DK1$D[i+1], mean = (1-alpha_1)*DK1$D[i], sd = sigma_1) +
#                (1-p_33)*xi[i,3]*dnorm(DK1$D[i+1], mean = ((1-alpha_1)*DK1$D[i]), sd = sigma_1))/(like[i])
#    xi[i+1,2] = ((1-p_11)*xi[i,2]*dnorm(DK1$D[i+1], mean = (-mu_2 + DK1$D[i]), sd = sigma_2)+
#                p_22*xi[i,2]*dnorm(DK1$D[i+1], mean = (-mu_2 + DK1$D[i]), sd = sigma_2))/(like[i])
#    xi[i+1,3] = ((1-p_22)*xi[i,2]*dnorm(DK1$D[i+1], mean = ((1-alpha_3)*DK1$D[i]), sd = sigma_3)+
#                p_33*xi[i,3]*dnorm(DK1$D[i+1], mean = ((1-alpha_3)*DK1$D[i]), sd = sigma_3))/(like[i])
#    likesum = likesum + log(like[i])
#  }
#  return(-likesum)
#}
#upper = c(100,100,100,0.99999,0.99999,0.99999,0.99999,0.99999,20)
#lower = c(10,30,50,0.01,0.01,0.01,0.01,0.01,-20)
#par2 = c(30,80,70,0.1,0.1,0.5,0.5,0.5,5)
## par = sigma1,sigma2,sigma3,alpha_1,alpha_3,p_11,p_22,p_33,mu
#MRS2 = optim(par2, f_2, lower = lower, upper=upper, method = "L-BFGS-B", 
#             control=list(trace=TRUE, maxit= 500,
#                          parscale=c(0.5,0.5,0.5,0.05,0.05,0.001,0.001,0.001,0.1), factr = 1e-12))

#library(optimx)
#MRS2 = optimx(par2, f_2, lower = lower, upper=upper, method = "L-BFGS-B", 
#             control=list(trace=TRUE, maxit= 500,parscale=c(0.5,0.5,0.5,0.05,0.05,0.001,0.001,0.001,0.1)))
