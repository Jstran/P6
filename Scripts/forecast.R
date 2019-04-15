rm(list=ls())
load("./Workspaces/modelling.Rdata")


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
  for (i in 1:(l - 1)) {
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

DK1$D  <- as.numeric(DK1$D)

n.pred <- 2
x.pred = c()

theta0 <-  c(10,50,10,0.5,0.5,0.9,5)
lB     <- c(0,0,0,0,0,0.000001,-100)
uB     <- c(100,100,100,0.99999,0.99999,0.99999,100)

set.seed(290697)
prob <- runif(n.pred + 1)

s <- 1
spike.count <- 0
state <- c()
start <- (2191-n.pred)
for (l in start:2191) {
  
  MRS <- optim(theta0, logLike, lower = lB, upper = uB, method = "L-BFGS-B", 
               control=list(trace=TRUE, maxit= 500))
  
#  sigma1 <- MRS$par[1]
#  sigma2 <- MRS$par[2]
#  sigma3 <- MRS$par[3]
  a1     <- MRS$par[4]
  a3     <- MRS$par[5]
  p      <- MRS$par[6]
  mu2    <- MRS$par[7]
  
  if (s == 3) {
    x.pred[l]=(1 - a1)*DK1$D[l-1] 
    s = 1
    state = c(state,s)
  }
  else if (s == 2) { 
    x.pred[l] = (1 - a3)*DK1$D[l-1] 
    s = 3
    state = c(state,s)
  }
  else if (prob[l-start+1] >= p && s == 1) {
    x.pred[l] = DK1$D[l-1] + mu2 
    s = 2
    state <- c(state,s)
    spikes <- spike.count + 1
  }
  else {
    x.pred[l] <- (1 - a1)*DK1$D[l-1] 
    state <- c(state,s)
  }
  print(l)
}

#pred.df <- data.frame(t = c(81:100), x.pred = x.pred)

plot(DK1$D[2000:2191], type = "l")
lines(x.pred[2000:2191], col = "red")
