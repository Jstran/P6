### ¤¤ Intro ¤¤ ### ---------------------------------------------------------------------
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
  
  like    <- c()
  x       <- numeric(3)
  x[1:3]  <- 1/3
  likesum <-  0
  for (i in 1:(slut.is - 1)) {
    x.temp <- x
    p <- exp(beta[1] + beta[2]*DK1$W[i])/(1+exp(beta[1] + beta[2]*DK1$W[i]))
    
    like[i] <- p*x[1]*dnorm(DK1$D[i+1], mean = (1-alpha1)*DK1$D[i], sd = sigma1) +
      (1-p)*x[1]*dnorm(DK1$D[i+1], mean = (-mu2 + DK1$D[i]), sd = sigma2) +
      x[2]*dnorm(DK1$D[i+1], mean = ((1-alpha3)*DK1$D[i]), sd = sigma3) +
      x[3]*dnorm(DK1$D[i+1], mean = ((1-alpha1)*DK1$D[i]), sd = sigma1)
    
    x[1] <- (p*x.temp[1]*dnorm(DK1$D[i+1], mean = (1-alpha1)*DK1$D[i], sd = sigma1) +
               x.temp[3]*dnorm(DK1$D[i+1], mean = ((1-alpha1)*DK1$D[i]), sd = sigma1))/like[i]
    
    x[2] <- ((1-p)*x.temp[1]*dnorm(DK1$D[i+1], mean = (-mu2 + DK1$D[i]), sd = sigma2))/like[i]
    
    x[3] <- x.temp[2]*dnorm(DK1$D[i+1], mean = ((1-alpha3)*DK1$D[i]), sd = sigma3)/like[i]
  }
  
  likesum <- sum(log(like))
  
  return(-likesum)
}

theta0 <- c(10,50,10,0.5,0.5,   5,   5,    5) # Startværdier for parametre til optim
lB     <- c( 0, 0, 0,0  ,0  ,-500,-100, -100) # Nedre grænse for parametre
uB     <- c(100,100,100,0.99999,0.99999,500,100,100) # Øvre grænse for parametre

MRS <- optim(theta0, logLike, lower = lB, upper = uB, method = "L-BFGS-B", # Finder MLE
             control=list(trace=TRUE, maxit= 500), hessian = TRUE)

sigma1 <- MRS$par[1] 
sigma2 <- MRS$par[2]
sigma3 <- MRS$par[3]
a1     <- MRS$par[4] 
a3     <- MRS$par[5]
mu2    <- MRS$par[6]
beta   <- MRS$par[7:8]


se <- sqrt(diag(solve(MRS$hessian))) # Standard error
AIC <- 2*(length(MRS$par) - MRS$value)

data.frame("alpha1" = c(a1, se[4]), "alpha3" = c(a3, se[5]), "mu2" = c(mu2, se[6]), 
           "sigma1" = c(sigma1, se[1]), "sigma2" = c(sigma2, se[2]), 
           "sigma3" = c(sigma3, se[3]), "beta1" = c(beta[1],se[7]),
           "beta2" = c(beta[2],se[8]))
### ¤¤ In-sample ¤¤ ### -----------------------------------------------------------------
prob.is <- runif(length(dat)) # Tilfældige for skift mellem regimer

x.pred.is.c <- c() # Tom vektor til at indsætte de forecasted værdier ofr OOS

s.is <- 1 # Start regime
spike.count.is <- 0 # Tæller antal spikes for OOS

state.is <- c() # Giver hvilket stadie vi befinder os i for OOS


# OOS forecast
for (l in 2:(slut.is)) {
  p <- (exp(beta[1] + beta[2]*DK1$W[l-1])/(1 + exp(beta[1] + beta[2]*DK1$W[l-1])))
  if (s.is == 3) {
    x.pred.is.c[l]=(1 - a1)*dat[l-1] 
    s.is = 1
    state.is[l] = s.is
  }
  else if (s.is == 2) { 
    x.pred.is.c[l] = (1 - a3)*dat[l-1] 
    s.is = 3
    state.is[l] = s.is
  }
  else if (prob.is[l] >= p && s.is == 1) {
    x.pred.is.c[l] = dat[l-1] + mu2 
    s.is = 2
    state.is[l] = s.is
    spike.count.is <- spike.count.is + 1
  }
  else {
    x.pred.is.c[l] <- (1 - a1)*dat[l-1] 
    state.is[l] = s.is
  }
}

plot(DK1$D[2:slut.is], type = "l", main = "Uden sæson (IS)")
lines(x.pred.is.c[2:slut.is], col = "red")

rmse.is.c <- sqrt(1/(2190) * sum((dat[2:slut.is] - x.pred.is.c[2:slut.is])^2));rmse.is.c
mae.is.c  <- 1/(2190) * sum(abs(dat[2:slut.is] - x.pred.is.c[2:slut.is]));mae.is.c

### ¤¤ Out-of-sample ¤¤ ### -------------------------------------------------------------
prob.oos <- runif(length(dat)) # Tilfældige for skift mellem regimer

x.pred.oos.c <- c() # Tom vektor til at indsætte de forecasted værdier ofr OOS

s.oos <- 1 # Start regime
spike.count.oos <- 0 # Tæller antal spikes for OOS

state.oos <- c() # Giver hvilket stadie vi befinder os i for OOS
p <- c()

# OOS forecast
for (l in start.oos:slut.oos) {
  p[l-slut.is] <- (exp(beta[1] + beta[2]*datW[l-1])/(1 + exp(beta[1] + beta[2]*datW[l-1])))
  if (s.oos == 3) {
    x.pred.oos.c[l]=(1 - a1)*dat[l-1] 
    s.oos = 1
    state.oos[l] = s.oos
  }
  else if (s.oos == 2) { 
    x.pred.oos.c[l] = (1 - a3)*dat[l-1] 
    s.oos = 3
    state.oos[l] = s.oos
  }
  else if (prob.oos[l] >= p && s.oos == 1) {
    x.pred.oos.c[l] = dat[l-1] + mu2 
    s.oos = 2
    state.oos[l] = s.oos
    spike.count.oos <- spike.count.oos + 1
  }
  else {
    x.pred.oos.c[l] <- (1 - a1)*dat[l-1] 
    state.oos[l] = s.oos
  }
}

plot(dat[2100:slut.oos], type = "l", main = "Uden sæson (OOS)")
lines(x.pred.oos.c[2100:slut.oos], col = "red")

plot(OOS$A, type = "l", main = "Med sæson (OOS)")
lines(x.pred.oos.c[start.oos:slut.oos] + OOS$s.pred, col = "red")

rmse.oos.c <- sqrt(1/(slut.oos - start.oos + 1) * sum((dat[start.oos:slut.oos] - 
                                                     x.pred.oos.c[start.oos:slut.oos])^2));
rmse.oos.c
mae.oos  <- 1/(slut.oos - start.oos + 1) * sum(abs(dat[start.oos:slut.oos] - 
                                                   x.pred.oos.c[start.oos:slut.oos]));
mae.oos

spike.count.oos
p

### ¤¤ Pred.inter mm ¤¤ ### -------------------------------------------

pred.inter.c <- 1.96 * sqrt(var(dat[start.oos:slut.oos] - x.pred.oos.c[start.oos:slut.oos]))

res.oos.c <- OOS$D - x.pred.oos.c[start.oos:slut.oos]

res.is.c <- DK1$D[2:slut.is] - x.pred.is.c[2:slut.is]

save(x.pred.is.c, x.pred.oos.c, rmse.is.c, rmse.oos.c, pred.inter.c, res.is.c, res.oos.c,
     file = "./Workspaces/forecastModC.Rdata")
