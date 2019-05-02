rm(list=ls())
load("./Workspaces/modelling.Rdata")

set.seed(12345)

DK1$D  <- as.numeric(DK1$D) # Bare pga. R
OOS$D  <- as.numeric(OOS$D) # Bare pga. R
dat    <- c(DK1$D, OOS$D)
datW   <- c(as.numeric(DK1$W), as.numeric(OOS$W))
datW <- diff(log(datW))
DK1$W  <- as.numeric(diff(log(DK1$W)))
OOS$W  <- as.numeric(diff(log(OOS$W)))

start.oos <-  length(DK1$D) + 1 # Indeks hvor OOS starter
slut.oos  <- start.oos + length(OOS$D) - 1 # Indeks hvor OOS slutter
slut.is   <- start.oos - 1 # Sidste obs i IS

### ¤¤ MLE ¤¤ ### -----------------------------------------------------------------------
logLike <- function(theta){
  sigma1 <- theta[1]
  sigma2 <- theta[2]
  sigma3 <- theta[3]
  alpha1 <- theta[4]
  alpha3 <- theta[5]
  mu2    <- theta[6]
  beta   <- theta[7:8]
  
  eta <- numeric(3)
  like    <- c()
  x       <- numeric(3)
  x[1:3]  <- 1/3
  likesum <-  0
  for (i in 1:(slut.is - 1)) {
    x.temp <- x
    p <- exp(beta[1] + beta[2]*DK1$W[i])/(1+exp(beta[1] + beta[2]*DK1$W[i]))
    
    eta[1] <- dnorm(DK1$D[i+1], mean = (1-alpha1)*DK1$D[i], sd = sigma1)
    eta[2] <- dnorm(DK1$D[i+1], mean = (-mu2 + DK1$D[i]), sd = sigma2)
    eta[3] <- dnorm(DK1$D[i+1], mean = ((1-alpha3)*DK1$D[i]), sd = sigma3)
    
    like[i] <- p*x[1]*eta[1] + (1-p)*x[1]*eta[2] + x[2]*eta[3] + x[3]*eta[1]
    
    x[1] <- (p*x.temp[1]*eta[1] + x.temp[3]*eta[1])/like[i]
    
    x[2] <- (1-p)*x.temp[1]*eta[2]/like[i]
    
    x[3] <- x.temp[2]*eta[3]/like[i]
  }
  
  likesum <- sum(log(like))
  
  return(-likesum)
}

theta0 <- c(33,90,75,0.3,0.6,   4,   2,    1) # Startværdier for parametre til optim
lB     <- c( 0, 0, 0,0  ,0  ,-500,-100, -100) # Nedre grænse for parametre
uB     <- c(100,100,100,0.99999,0.99999,500,100,100) # Øvre grænse for parametre

MRS <- optim(theta0, logLike, lower = lB, upper = uB, method = "L-BFGS-B", # Finder MLE
             control=list(trace=TRUE, maxit= 500), hessian = TRUE)

sigma1 <- MRS$par[1] 
sigma2 <- MRS$par[2]
sigma3 <- MRS$par[3]
alpha1     <- MRS$par[4] 
alpha3     <- MRS$par[5]
mu2    <- MRS$par[6]
beta   <- MRS$par[7:8]


se <- sqrt(diag(solve(MRS$hessian))) # Standard error
AIC <- 2*(length(MRS$par) - MRS$value)

data.frame("alpha1" = c(a1, se[4]), "alpha3" = c(a3, se[5]), "mu2" = c(mu2, se[6]), 
           "sigma1" = c(sigma1, se[1]), "sigma2" = c(sigma2, se[2]), 
           "sigma3" = c(sigma3, se[3]), "beta1" = c(beta[1],se[7]),
           "beta2" = c(beta[2],se[8]))

xi <- numeric(3)
xi[1:3] <- 1/3

eta <- numeric(3)

x.pred.oos.c <- c() # Tom vektor til at indsætte de forecasted værdier for OOS
like <- c()
p <- c()

for (l in start.oos:slut.oos) {
#l <- start.oos + 1

  p[l-slut.is] <- (exp(beta[1] + beta[2]*datW[l-1])/
                  (1 + exp(beta[1] + beta[2]*datW[l-1])))
  
  eta[1] <- dnorm(dat[l], mean = (1-alpha1)*dat[l-1], sd = sigma1)
  eta[2] <- dnorm(dat[l], mean = (-mu2 + dat[l-1]), sd = sigma2)
  eta[3] <- dnorm(dat[l], mean = ((1-alpha3)*dat[l-1]), sd = sigma3)
  
  like[l-slut.is] <- p[l-slut.is]*xi[1]*eta[1] + (1-p[l-slut.is])*xi[1]*eta[2] + 
                     xi[2]*eta[3] + xi[3]*eta[1]
  
  xi.temp <- xi
  
  xi[1] <- (p[l-slut.is]*xi.temp[1]*eta[1] + xi.temp[3]*eta[1])/like[l-slut.is]
  
  xi[2] <- ((1-p[l-slut.is])*xi.temp[1]*eta[2])/like[l-slut.is]
  
  xi[3] <- xi.temp[2]*eta[3]/like[l-slut.is]
  
  x.pred.oos.c[l] <- xi[1]*(1 - alpha1)*dat[l-1] + xi[2]*(dat[l-1] + mu2) + 
                     xi[3]*(1 - alpha3)*dat[l-1]
}

plot(dat[2100:slut.oos], type = "l", main = "Uden sæson (OOS)")
lines(x.pred.oos.c[2100:slut.oos], col = "red")

plot(OOS$A, type = "l", main = "Med sæson (OOS)")
lines(x.pred.oos.c[start.oos:slut.oos] + OOS$s.pred, col = "red")

rmse.oos.c <- sqrt(1/(slut.oos - start.oos + 1) * sum((dat[start.oos:slut.oos] - 
                                                      x.pred.oos.c[start.oos:slut.oos])^2))
rmse.oos.c

mae.oos  <- mean(abs(dat[start.oos:slut.oos] - x.pred.oos.c[start.oos:slut.oos]))
mae.oos