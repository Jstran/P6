rm(list=ls())
load("./Workspaces/modelling.Rdata")

set.seed(101296)
logLike <- function(theta){
  sigma1 <- theta[1]
  sigma2 <- theta[2]
  sigma3 <- theta[3]
  alpha1 <- theta[4]
  alpha3 <- theta[5]
  p      <- theta[6]
  mu2    <- theta[7]
  
  like    <- c()
  x       <- numeric(3)
  x[1:3]  <- 1/3
  likesum <-  0
  for (i in 1:(j - 1)) {
    x.temp <- x
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

DK1$D  <- as.numeric(DK1$D) # Bare pga. R
OOS$D  <- as.numeric(OOS$D) # Bare pga. R

dat <- c(DK1$D, OOS$D)

x.pred <- c() # Tom vektor til at indsætte de forecasted værdier ofr OOS

theta0 <- c(10,50,10,0.5,0.5,0.9,5) # Startværdier for parametre til optim
lB     <- c(0,0,0,0,0,0.000001,-100) # Nedre grænse for parametre
uB     <- c(100,100,100,0.99999,0.99999,0.99999,100) # Øvre grænse for parametre


prob <- runif(length(dat)) # Tilfældige for skift mellem regimer

s <- 1 # Start regime
spike.count <- 0 # Tæller antal spikes for OOS

state <- c() # Giver hvilket stadie vi befinder os i for OOS

start.oos <-  length(DK1$D) + 1 # Indeks hvor OOS starter
slut.oos  <- start.oos + length(OOS$D) - 1 # Indeks hvor OOS slutter

j <- start.oos - 1 # Sidste obs i IS

MRS <- optim(theta0, logLike, lower = lB, upper = uB, method = "L-BFGS-B", # Finder MLE
             control=list(trace=TRUE, maxit= 500))

#  sigma1 <- MRS$par[1]
#  sigma2 <- MRS$par[2]
#  sigma3 <- MRS$par[3]
a1     <- MRS$par[4] 
a3     <- MRS$par[5]
p      <- MRS$par[6]
mu2    <- MRS$par[7]


# OOS forecast
for (l in start.oos:slut.oos) {
  
  if (s == 3) {
    x.pred[l]=(1 - a1)*dat[l-1] 
    s = 1
    state[l] = s
  }
  else if (s == 2) { 
    x.pred[l] = (1 - a3)*dat[l-1] 
    s = 3
    state[l] = s
  }
  else if (prob[l-start.oos + 1] >= p && s == 1) {
    x.pred[l] = dat[l-1] + mu2 
    s = 2
    state[l] = s
    spike.count <- spike.count + 1
  }
  else {
    x.pred[l] <- (1 - a1)*dat[l-1] 
    state[l] = s
  }
}

x.pred.df <- data.frame(t = start.oos:slut.oos, x.pred = x.pred[start.oos:slut.oos])


plot(dat[2000:slut.oos], type = "l", main = "Uden sason")
lines(x.pred[2000:slut.oos], col = "red")

plot(OOS$A, type = "l", main = "Med sason")
lines(x.pred[start.oos:slut.oos] + OOS$s.pred, col = "red")

rmse <- sqrt(1/(slut.oos - start.oos + 1) * sum((dat[start.oos:slut.oos] - 
                                                 x.pred[start.oos:slut.oos])^2))

pred.inter <- 1.96 * sqrt(var(dat[start.oos:slut.oos] - x.pred[start.oos:slut.oos]))

save(x.pred, rmse, a1, a3, p, mu2, pred.inter,
          file = "./Workspaces/forecast.Rdata")
