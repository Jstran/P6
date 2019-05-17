### ¤¤ Preliminary ¤¤ ### ---------------------------------------------------------------
rm(list=ls())
load("./Workspaces/modelling.Rdata")

DK1$D  <- as.numeric(DK1$D) # Bare pga. R
OOS$D  <- as.numeric(OOS$D) # Bare pga. R
dat    <- c(DK1$D, OOS$D)
datW   <- c(as.numeric(DK1$W), as.numeric(OOS$W))
datW   <- diff(log(datW))
datW.pred <- c(DK1$W, OOS$WP)
datW.pred <- diff(log(datW.pred))

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
  beta   <- theta[7:9]
  
  eta <- numeric(3)
  like    <- c()
  xi      <- numeric(3)
  xi[1:3] <- 1/3
  likesum <-  0
  for (i in 2:(slut.is - 1)) {
    xi.temp <- xi
    p <- exp(beta[1] + beta[2]*datW.pred[i] + beta[3]*datW[i-1])/
      (1+exp(beta[1] + beta[2]*datW.pred[i] + beta[3]*datW[i-1]))
    
    eta[1] <- dnorm(DK1$D[i+1], mean = (1-alpha1)*DK1$D[i], sd = sigma1)
    eta[2] <- dnorm(DK1$D[i+1], mean = (-mu2 + DK1$D[i]), sd = sigma2)
    eta[3] <- dnorm(DK1$D[i+1], mean = ((1-alpha3)*DK1$D[i]), sd = sigma3)
    
    like[i] <- p*xi[1]*eta[1] + (1-p)*xi[1]*eta[2] + xi[2]*eta[3] + xi[3]*eta[1]
    
    xi[1] <- (p*xi.temp[1]*eta[1] + xi.temp[3]*eta[1])/like[i]
    
    xi[2] <- (1-p)*xi.temp[1]*eta[2]/like[i]
    
    xi[3] <- xi.temp[2]*eta[3]/like[i]
  }
  
  likesum <- sum(log(like), na.rm = TRUE)
  
  return(-likesum)
}

theta0 <- c(33,90,75,0.3,0.6,   4,   2,    1, 0.1) # Startværdier for parametre til optim
lB     <- c( 0, 0, 0,0  ,0  ,-500,-100, -100, -100) # Nedre grænse for parametre
uB     <- c(100,100,100,0.99999,0.99999,500,100,100, 100) # Øvre grænse for parametre

MRS <- optim(theta0, logLike, lower = lB, upper = uB, method = "L-BFGS-B", # Finder MLE
             control=list(trace=TRUE, maxit= 500), hessian = TRUE)

sigma1 <- MRS$par[1] 
sigma2 <- MRS$par[2]
sigma3 <- MRS$par[3]
alpha1 <- MRS$par[4] 
alpha3 <- MRS$par[5]
mu2    <- MRS$par[6]
beta   <- MRS$par[7:9]


se <- sqrt(diag(solve(MRS$hessian))) # Standard error
AIC <- 2*(length(MRS$par) - MRS$value)

data.frame("alpha1" = c(alpha1, se[4], MRS$par[4]/se[4]), 
           "alpha3" = c(alpha3, se[5], MRS$par[5]/se[5]), 
           "mu2"    = c(mu2,    se[6], MRS$par[6]/se[6]), 
           "sigma1" = c(sigma1, se[1], MRS$par[1]/se[1]), 
           "sigma2" = c(sigma2, se[2], MRS$par[2]/se[2]), 
           "sigma3" = c(sigma3, se[3], MRS$par[3]/se[3]), 
           "beta1"  = c(beta[1],se[7], MRS$par[7]/se[7]), 
           "beta2"  = c(beta[2],se[8], MRS$par[8]/se[8]),
           "beta3"  = c(beta[3],se[9], MRS$par[9]/se[9]))

### ¤¤ IS mm ¤¤ ### ---------------------------------------------------------------------
xi <- c()
xi[1:3] <- 1/3

eta <- numeric(3)

x.pred.is.c <- c() # Tom vektor til at indsætte de forecasted værdier for OOS
p.is <- c()

for (l in 3:slut.is) {
  p.is[l] <- (exp(beta[1] + beta[2]*datW.pred[l-1] + beta[3]*datW[l-2])/
             (1 + exp(beta[1] + beta[2]*datW.pred[l-1]+ beta[3]*datW[l-2])))
  
  x.pred.is.c[l] <- xi[1]*(1 - alpha1)*dat[l-1] + xi[2]*(dat[l-1] + mu2) + 
                    xi[3]*(1 - alpha3)*dat[l-1]
  
  eta[1] <- dnorm(dat[l], mean = (1-alpha1)*dat[l-1], sd = sigma1)
  eta[2] <- dnorm(dat[l], mean = (-mu2 + dat[l-1]), sd = sigma2)
  eta[3] <- dnorm(dat[l], mean = ((1-alpha3)*dat[l-1]), sd = sigma3)
  
  like <- p.is[l]*xi[1]*eta[1] + (1-p.is[l])*xi[1]*eta[2] + xi[2]*eta[3] + xi[3]*eta[1]
  xi.temp <- xi
  
  xi[1] <- (p.is[l]*xi.temp[1]*eta[1] + xi.temp[3]*eta[1])/like
  
  xi[2] <- ((1-p.is[l])*xi.temp[1]*eta[2])/like
  
  xi[3] <- xi.temp[2]*eta[3]/like
}
x.pred.is.c <- x.pred.is.c[-c(1,2)]

plot(dat[3:slut.is], type = "l", main = "Uden sæson (OOS)")
lines(x.pred.is.c[1:slut.is], col = "red")


rmse.is.c <- sqrt(1/slut.is * sum((dat[3:slut.is] - x.pred.is.c)^2))
rmse.is.c

mae.is  <- mean(abs(dat[3:slut.is] - x.pred.is.c))
mae.is

### ¤¤ OOS mm ¤¤ ### --------------------------------------------------------------------
logLike.oos <- function(theta){
  sigma1 <- theta[1]
  sigma2 <- theta[2]
  sigma3 <- theta[3]
  alpha1 <- theta[4]
  alpha3 <- theta[5]
  mu2    <- theta[6]
  beta   <- theta[7:9]
  
  eta <- numeric(3)
  like    <- c()
  xi      <- numeric(3)
  xi[1:3] <- 1/3
  likesum <-  0
  for (i in (1+l-slut.is):(l-1)) {
    p <- exp(beta[1] + beta[2]*datW.pred[i-1]+ beta[3]*datW[i-2])/
      (1+exp(beta[1] + beta[2]*datW.pred[i-1]+ beta[3]*datW[i-2]))
    
    xi.temp <- xi
    
    eta[1] <- dnorm(dat[i+1], mean = (1-alpha1)*dat[i],   sd = sigma1)
    eta[2] <- dnorm(dat[i+1], mean = (-mu2 + dat[i]),     sd = sigma2)
    eta[3] <- dnorm(dat[i+1], mean = ((1-alpha3)*dat[i]), sd = sigma3)
    
    like[i] <- p*xi[1]*eta[1] + (1-p)*xi[1]*eta[2]+ xi[2]*eta[3] + xi[3]*eta[1]
    
    xi[1] <- (p*xi.temp[1]*eta[1] + xi.temp[3]*eta[1])/like[i]
    
    xi[2] <- ((1-p)*xi.temp[1]*eta[2])/like[i]
    
    xi[3] <- xi.temp[2]*eta[3]/like[i]
  }
  
  likesum <- sum(log(like), na.rm = TRUE)
  
  return(-likesum)
}

eta <- numeric(3)

x.pred.oos.c <- c() # Tom vektor til at indsætte de forecasted værdier for OOS
pred.inter.c <- c()
p <- c()

for (l in start.oos:slut.oos) {
  
  theta0 <- c(sigma1, sigma2, sigma2, alpha1, alpha3, mu2, beta[1], beta[2], beta[3])
  
  MRS <- optim(theta0, logLike.oos, lower = lB, upper = uB, method = "L-BFGS-B",
               control = list(trace = TRUE, maxit = 500), hessian = FALSE)
  
  sigma1 <- MRS$par[1] 
  sigma2 <- MRS$par[2]
  sigma3 <- MRS$par[3]
  alpha1 <- MRS$par[4] 
  alpha3 <- MRS$par[5]
  mu2    <- MRS$par[6]
  beta   <- MRS$par[7:9]

  p[l-slut.is] <- (exp(beta[1] + beta[2]*datW.pred[l-1]+ beta[3]*datW[l-2])/
                  (1 + exp(beta[1] + beta[2]*datW.pred[l-1]+ beta[3]*datW[l-2])))
  
  x.pred.oos.c[l] <- xi[1]*(1 - alpha1)*dat[l-1] + xi[2]*(dat[l-1] + mu2) + 
                     xi[3]*(1 - alpha3)*dat[l-1]
  pred.inter.c[l] <- xi[1]*sigma1 + xi[2]*sigma2 + xi[3]*sigma3
  
  eta[1] <- dnorm(dat[l], mean = (1-alpha1)*dat[l-1], sd = sigma1)
  eta[2] <- dnorm(dat[l], mean = (-mu2 + dat[l-1]), sd = sigma2)
  eta[3] <- dnorm(dat[l], mean = ((1-alpha3)*dat[l-1]), sd = sigma3)
  
  like <- p[l-slut.is]*xi[1]*eta[1] + (1-p[l-slut.is])*xi[1]*eta[2] + 
          xi[2]*eta[3] + xi[3]*eta[1]
  
  xi.temp <- xi
  
  xi[1] <- (p[l-slut.is]*xi.temp[1]*eta[1] + xi.temp[3]*eta[1])/like
  
  xi[2] <- ((1-p[l-slut.is])*xi.temp[1]*eta[2])/like

  xi[3] <- xi.temp[2]*eta[3]/like
  
  print(l-slut.is)
}

plot(dat[2100:slut.oos], type = "l", main = "Uden sæson (IS)")
lines(x.pred.oos.c[2100:slut.oos], col = "red")

plot(OOS$A, type = "l", main = "Med sæson (OOS)")
lines(x.pred.oos.c[start.oos:slut.oos] + OOS$s.pred, col = "red")

rmse.oos.c <- sqrt(1/(slut.oos-start.oos+1)*sum((dat[start.oos:slut.oos] - 
                                                 x.pred.oos.c[start.oos:slut.oos])^2))
rmse.oos.c

mae.oos  <- mean(abs(dat[start.oos:slut.oos] - x.pred.oos.c[start.oos:slut.oos]))
mae.oos

### ¤¤ Pred.inter mm ¤¤ ### -------------------------------------------------------------
pred.inter.c <- 1.96 * pred.inter.c

res.oos.c <- OOS$D - x.pred.oos.c[start.oos:slut.oos]

res.is.c <- DK1$D[3:slut.is] - x.pred.is.c

ee### ¤¤ Gemmer workspace ¤¤ ### ----------------------------------------------------------
save(x.pred.is.c, x.pred.oos.c, rmse.is.c, rmse.oos.c, pred.inter.c, res.is.c, res.oos.c,
     file = "./Workspaces/forecastModC2.Rdata")
