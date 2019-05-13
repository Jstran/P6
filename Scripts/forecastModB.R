### ¤¤ Preliminary ¤¤ ### ---------------------------------------------------------------
rm(list=ls())
load("./Workspaces/modelling.Rdata")

DK1$D  <- as.numeric(DK1$D) # Bare pga. R
OOS$D  <- as.numeric(OOS$D) # Bare pga. R
dat    <- c(DK1$D, OOS$D)
datW   <- c(as.numeric(DK1$W), as.numeric(OOS$W))
datW <- diff(log(datW))

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
  p      <- theta[7]
  omega01 <- theta[8]
  omega02 <- theta[9]
  omega03 <- theta[10]
  omega11 <-  theta[11]
  omega12 <-  theta[12]
  
  eta <- numeric(3)
  like    <- c()
  xi       <- numeric(3)
  xi[1:3]  <- 1/3
  likesum <-  0
  for (i in 2:(slut.is - 1)) {
    xi.temp <- xi

    eta[1] <- dnorm(DK1$D[i+1], mean = ((1-alpha1)*DK1$D[i]+omega01*datW[i] + 
                                        omega11*datW[i-1]), sd = sigma1)
    eta[2] <- dnorm(DK1$D[i+1], mean = (-mu2 + DK1$D[i]+omega02*datW[i] + 
                                        omega12*datW[i-1]), sd = sigma2)
    eta[3] <- dnorm(DK1$D[i+1], mean = ((1-alpha3)*DK1$D[i]+omega03*datW[i]), 
                    sd = sigma3)
    
    like[i] <- p*xi[1]*eta[1] + (1-p)*xi[1]*eta[2] + xi[2]*eta[3] + xi[3]*eta[1]
    
    xi[1] <- (p*xi.temp[1]*eta[1] + xi.temp[3]*eta[1])/like[i]
    
    xi[2] <- (1-p)*xi.temp[1]*eta[2]/like[i]
    
    xi[3] <- xi.temp[2]*eta[3]/like[i]
  }
  
  likesum <- sum(log(like), na.rm = TRUE)
  
  return(-likesum)
}

theta0 <- c(28,65,55,0.2,0.4,   11,   0.9,    -21, -72, -64, -4, -23) 
lB     <- c( 0, 0, 0,0  ,0  ,-500,0.00001, -1000, -1000, -1000, -1000, -1000) 
uB     <- c(100,100,100,0.99999,0.99999,500,0.99999,100, 100, 100, 100, 100) 

MRS <- optim(theta0, logLike, lower = lB, upper = uB, method = "L-BFGS-B", # Finder MLE
             control=list(trace=TRUE, maxit= 500), hessian = TRUE)

sigma1 <- MRS$par[1] 
sigma2 <- MRS$par[2]
sigma3 <- MRS$par[3]
alpha1 <- MRS$par[4] 
alpha3 <- MRS$par[5]
mu2    <- MRS$par[6]
p      <- MRS$par[7]
omega01 <- MRS$par[8]
omega02 <- MRS$par[9]
omega03 <- MRS$par[10]
omega11 <- MRS$par[11]
omega12 <- MRS$par[12]


se <- sqrt(diag(solve(MRS$hessian))) # Standard error
AIC <- 2*(length(MRS$par) - MRS$value)
AIC

data.frame("alpha1" = c(alpha1, se[4], alpha1/se[4]), 
           "alpha3" = c(alpha3, se[5], alpha3/se[5]), 
           "mu2" = c(mu2, se[6], mu2/se[6]), 
           "sigma1" = c(sigma1, se[1], sigma1/se[1]), 
           "sigma2" = c(sigma2, se[2], sigma2/se[2]), 
           "sigma3" = c(sigma3, se[3], sigma3/se[3]), 
           "p" = c(p,se[7], p/se[7]), 
           "omega01" = c(omega01,se[8], omega01/se[8]),
           "omega02" = c(omega02,se[9], omega02/se[9]), 
           "omega03" = c(omega03,se[10], omega03/se[10]), 
           "omega11" = c(omega11,se[11], omega11/se[11]), 
           "omega12" = c(omega12,se[12], omega12/se[12]))

### ¤¤ IS mm ¤¤ ### ---------------------------------------------------------------------
xi <- c()
xi[1:3] <- 1/3

eta <- numeric(3)

x.pred.is.b <- c() # Tom vektor til at indsætte de forecasted værdier for OOS

for (l in 2:slut.is) {
  x.pred.is.b[l] <- xi[1]*(1 - alpha1)*dat[l-1] + xi[2]*(dat[l-1] + mu2) + 
                    xi[3]*(1 - alpha3)*dat[l-1]
  
  eta[1] <- dnorm(dat[l], mean = ((1-alpha1)*dat[l-1]+omega01*datW[l-1] + 
                                  omega11*datW[l-1]), sd = sigma1)
  eta[2] <- dnorm(dat[l], mean = (-mu2 + dat[l-1]+omega02*datW[l-1]+ 
                                  omega12*datW[l-1]), sd = sigma2)
  eta[3] <- dnorm(dat[l], mean = ((1-alpha3)*dat[l-1]+omega03*datW[l-1]), sd = sigma3)
  
  like <- p*xi[1]*eta[1] + (1-p)*xi[1]*eta[2] + xi[2]*eta[3] + xi[3]*eta[1]
  xi.temp <- xi
  
  xi[1] <- (p*xi.temp[1]*eta[1] + xi.temp[3]*eta[1])/like
  
  xi[2] <- ((1-p)*xi.temp[1]*eta[2])/like
  
  xi[3] <- xi.temp[2]*eta[3]/like
}
x.pred.is.b <- x.pred.is.b[-1]

plot(dat[2:slut.is], type = "l", main = "Uden sæson (OOS)")
lines(x.pred.is.b[1:slut.is], col = "red")


rmse.is.b <- sqrt(1/slut.is * sum((dat[2:slut.is] - x.pred.is.b)^2))
rmse.is.b

mae.is  <- mean(abs(dat[2:slut.is] - x.pred.is.b))
mae.is

### ¤¤ OOS mm ¤¤ ### --------------------------------------------------------------------
logLike.oos <- function(theta){
  sigma1 <- MRS$par[1] 
  sigma2 <- MRS$par[2]
  sigma3 <- MRS$par[3]
  alpha1 <- MRS$par[4] 
  alpha3 <- MRS$par[5]
  mu2    <- MRS$par[6]
  p      <- MRS$par[7]
  omega01 <- MRS$par[8]
  omega02 <- MRS$par[9]
  omega03 <- MRS$par[10]
  omega11 <- MRS$par[11]
  omega12 <- MRS$par[12]
  
  eta <- numeric(3)
  like    <- c()
  xi      <- numeric(3)
  xi[1:3] <- 1/3
  likesum <-  0
  for (i in (1+l-slut.is):(l-1)) {
    xi.temp <- xi
    
    eta[1] <- dnorm(dat[l], mean = ((1-alpha1)*dat[l-1]+omega01*datW[l-1] + 
                                    omega11*datW[l-1]), sd = sigma1)
    eta[2] <- dnorm(dat[l], mean = (-mu2 + dat[l-1]+omega02*datW[l-1]+ 
                                    omega12*datW[l-1]), sd = sigma2)
    eta[3] <- dnorm(dat[l], mean = ((1-alpha3)*dat[l-1]+omega03*datW[l-1]), sd = sigma3)
    
    like[i] <- p*xi[1]*eta[1] + (1-p)*xi[1]*eta[2]+ xi[2]*eta[3] + xi[3]*eta[1]
    
    xi[1] <- (p*xi.temp[1]*eta[1] + eta[1])/like[i]
    
    xi[2] <- ((1-p)*xi.temp[1]*eta[2])/like[i]
    
    xi[3] <- xi.temp[2]*eta[3]/like[i]
  }
  
  likesum <- sum(log(like), na.rm = TRUE)
  
  return(-likesum)
}

eta <- numeric(3)

x.pred.oos.b <- c() # Tom vektor til at indsætte de forecasted værdier for OOS
pred.inter.b <- c()

for (l in start.oos:slut.oos) {
  
  theta0 <- c(sigma1, 
              sigma2, 
              sigma2, 
              alpha1, 
              alpha3, 
              mu2, 
              p, 
              omega01, 
              omega02, 
              omega03, 
              omega11, 
              omega12)
  
  MRS <- optim(theta0, logLike.oos, lower = lB, upper = uB, method = "L-BFGS-B",
               control = list(trace = TRUE, maxit = 500), hessian = FALSE)
  
  sigma1 <- MRS$par[1] 
  sigma2 <- MRS$par[2]
  sigma3 <- MRS$par[3]
  alpha1 <- MRS$par[4] 
  alpha3 <- MRS$par[5]
  mu2    <- MRS$par[6]
  p      <- MRS$par[7]
  omega01 <- MRS$par[8]
  omega02 <- MRS$par[9]
  omega03 <- MRS$par[10]
  omega11 <- MRS$par[11]
  omega12 <- MRS$par[12]
  
  
  x.pred.oos.b[l] <- xi[1]*(1 - alpha1)*dat[l-1] + xi[2]*(dat[l-1] + mu2) + 
                     xi[3]*(1 - alpha3)*dat[l-1]
  pred.inter.b[l] <- xi[1]*sigma1 + xi[2]*sigma2 + xi[3]*sigma3
  
  eta[1] <- dnorm(dat[l], mean = ((1-alpha1)*dat[l-1]+omega01*datW[l-1] + 
                                  omega11*datW[l-1]), sd = sigma1)
  eta[2] <- dnorm(dat[l], mean = (-mu2 + dat[l-1]+omega02*datW[l-1]+ 
                                  omega12*datW[l-1]), sd = sigma2)
  eta[3] <- dnorm(dat[l], mean = ((1-alpha3)*dat[l-1]+omega03*datW[l-1]), sd = sigma3)
  
  like <- p*xi[1]*eta[1] + (1-p)*xi[1]*eta[2] + 
          xi[2]*eta[3] + xi[3]*eta[1]
  
  xi.temp <- xi
  
  xi[1] <- (p*xi.temp[1]*eta[1] + xi.temp[3]*eta[1])/like
  
  xi[2] <- ((1-p)*xi.temp[1]*eta[2])/like
  
  xi[3] <- xi.temp[2]*eta[3]/like
  
  print(l-slut.is)
}

plot(dat[2100:slut.oos], type = "l", main = "Uden sæson (IS)")
lines(x.pred.oos.b[2100:slut.oos], col = "red")

plot(OOS$A, type = "l", main = "Med sæson (OOS)")
lines(x.pred.oos.b[start.oos:slut.oos] + OOS$s.pred, col = "red")

rmse.oos.b <- sqrt(1/(slut.oos-start.oos+1)*sum((dat[start.oos:slut.oos] - 
                                                 x.pred.oos.b[start.oos:slut.oos])^2))
rmse.oos.b

mae.oos  <- mean(abs(dat[start.oos:slut.oos] - x.pred.oos.b[start.oos:slut.oos]))
mae.oos

### ¤¤ Pred.inter mm ¤¤ ### -------------------------------------------------------------
pred.inter.b <- 1.96 * pred.inter.b

res.oos.b <- OOS$D - x.pred.oos.b[start.oos:slut.oos]

res.is.b <- DK1$D[2:slut.is] - x.pred.is.b

### ¤¤ Gemmer workspace ¤¤ ### ----------------------------------------------------------
save(x.pred.is.b, x.pred.oos.b, rmse.is.b, rmse.oos.b, pred.inter.b, res.is.b, res.oos.b,
     file = "./Workspaces/forecastModB.Rdata")
