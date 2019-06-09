### ¤¤ Preliminary ¤¤ ### ---------------------------------------------------------------
rm(list=ls())
load("./Workspaces/modelling.Rdata")


library(ggplot2)
# FArver brugt i plots
colors <- c("royalblue4" ,
            "firebrick4" ,
            "darkorchid4",
            "chartreuse4",
            "black",
            "grey70")
# Linje/punkt tykkelser brugt i plots
sz <- list(l = I(0.2) , p = I(0.1))

# Grå farve der matcher projektet
myGray <- rgb(245/255, 245/255, 245/255)

# Tema til plots
p.th <- list(theme(panel.background = element_rect(fill = myGray, colour = myGray,
                                                   size = 2, linetype = "solid"),
                   panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                   colour = "white"), 
                   panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                   colour = "white") ))
# Konfidensinterval
ci <- function(n = numeric(2191)){
  qnorm((1 + 0.95)/2)/sqrt(length(n))
}


# Mindre ændringer i dataet
DK1$D  <- as.numeric(DK1$D) 
OOS$D  <- as.numeric(OOS$D) 
dat    <- c(DK1$D, OOS$D)

start.oos <-  length(DK1$D) + 1 # Indeks hvor OOS starter
slut.oos  <- start.oos + length(OOS$D) - 1 # Indeks hvor OOS slutter
slut.is   <- start.oos - 1 # Sidste obs i IS

### ¤¤ MLE ¤¤ ### -----------------------------------------------------------------------
logLike <- function(theta){
  
  alpha11 <- theta[1]
  alpha12 <- theta[2]
  alpha13 <- theta[3]
  alpha14 <- theta[4]
  alpha15 <- theta[5]
  alpha16 <- theta[6]
  alpha17 <- theta[7]
  lambda17 <- theta[8]
  alpha31 <- theta[9]
  alpha32 <- theta[10]
  alpha33 <- theta[11]
  alpha34 <- theta[12]
  alpha35 <- theta[13]
  alpha36 <- theta[14]
  alpha37 <- theta[15]
  lambda37 <- theta[16]
  mu2     <- theta[17]
  sigma1  <- theta[18]
  sigma2  <- theta[19]
  sigma3  <- theta[20]
  p       <- theta[21]
  alpha21 <- theta[22]
  alpha22 <- theta[23]
  alpha23 <- theta[24]
  alpha24 <- theta[25]
  alpha25 <- theta[26]
  alpha26 <- theta[27]
  alpha27 <- theta[28]
  lambda27 <- theta[29]

  
  eta <- numeric(3)
  
  like    <- c()
  
  xi      <- numeric(3)
  xi[1:3] <- 1/3
  
  likesum <-  0
  
  for (l in 8:slut.is) {
    xi.temp <- xi
    
    eta[1] <- dnorm(dat[l], mean = (((1-alpha11)*dat[l-1]  + 
                                      alpha12*dat[l-2] + 
                                      alpha13*dat[l-3] +
                                      alpha14*dat[l-4] +
                                      alpha15*dat[l-5] +
                                      alpha16*dat[l-6] +
                                      alpha17*dat[l-7] )*1/(1-lambda17*dat[l-7])) , 
                    sd = sigma1)
    
    eta[2] <- dnorm(dat[l], mean = ((mu2 + alpha21*dat[l-1]  + 
                                      alpha22*dat[l-2] + 
                                      alpha23*dat[l-3] +
                                      alpha24*dat[l-4] +
                                      alpha25*dat[l-5] +
                                      alpha26*dat[l-6] +
                                      alpha27*dat[l-7] +
                                      alpha28*dat[l-8])**1/(1-lambda27*dat[l-7])), sd = sigma2)
    
    eta[3] <- dnorm(dat[l], mean = (((1-alpha31)*dat[l-1]  + 
                                      alpha32*dat[l-2] + 
                                      alpha33*dat[l-3] +
                                      alpha34*dat[l-4] +
                                      alpha35*dat[l-5] +
                                      alpha36*dat[l-6] +
                                      alpha37*dat[l-7])*1/(1-lambda37*dat[l-7])), sd = sigma3)
    
    like[l] <- p*xi[1]*eta[1] + (1-p)*xi[1]*eta[2]+ xi[2]*eta[3] + xi[3]*eta[1]
    
    xi[1] <- (p*xi.temp[1]*eta[1] + xi.temp[3]*eta[1])/like[l]
    
    xi[2] <- ((1-p)*xi.temp[1]*eta[2])/like[l]
    
    xi[3] <- xi.temp[2]*eta[3]/like[l]
  }
  
  likesum <- sum(log(like), na.rm = TRUE)
  
  return(-likesum)
}

alpha110 <- c(0.3,0,200)
alpha120 <- c(0.1,-10,200)
alpha130 <- c(0.1,-10,200)
alpha140 <- c(0.1,-10,200)
alpha150 <- c(0.1,-10,200)
alpha160 <- c(0.1,-10,200)
alpha170 <- c(0.1,-10,200)
lambda170 <-c(0.5,-10,10)
alpha310 <- c(0.6,0,200)
alpha320 <- c(0.1,-10,200)
alpha330 <- c(0.1,-10,200)
alpha340 <- c(0.1,-10,200)
alpha350 <- c(0.1,-10,200)
alpha360 <- c(0.1,-10,200)
alpha370 <- c(0.1,-10,200)
lambda370 <-c(0.5,-10,10)
mu20     <- c(7,-200,200)
sigma10  <- c(33,0,200)
sigma20  <- c(90,0,200)
sigma30  <- c(75,0,200)
p0       <- c(0.9,0.001)
alpha210 <- c(0.1,-10,200)
alpha220 <- c(0.1,-10,200)
alpha230 <- c(0.1,-10,200)
alpha240 <- c(0.1,-10,200)
alpha250 <- c(0.1,-10,200)
alpha260 <- c(0.1,-10,200)
alpha270 <- c(0.1,-10,200)
lambda270 <- c(0.1,-10,200)


theta0 <- c(alpha110[1], 
            alpha120[1], 
            alpha130[1], 
            alpha140[1], 
            alpha150[1], 
            alpha160[1], 
            alpha170[1], 
            lambda170[1],
            alpha310[1], 
            alpha320[1], 
            alpha330[1], 
            alpha340[1], 
            alpha350[1], 
            alpha360[1], 
            alpha370[1], 
            lambda370[1],
            mu20[1], 
            sigma10[1], 
            sigma20[1], 
            sigma30[1], 
            p0[1],
            alpha210[1],
            alpha220[1],
            alpha230[1],
            alpha240[1],
            alpha250[1],
            alpha260[1],
            alpha270[1],
            lambda270[1]) # Startværdier for parametre til optim

lB     <- c(alpha110[2], 
            alpha120[2], 
            alpha130[2], 
            alpha140[2], 
            alpha150[2], 
            alpha160[2], 
            alpha170[2], 
            lambda170[2],
            alpha310[2], 
            alpha320[2], 
            alpha330[2], 
            alpha340[2], 
            alpha350[2], 
            alpha360[2], 
            alpha370[2], 
            lambda370[2],
            mu20[2], 
            sigma10[2], 
            sigma20[2], 
            sigma30[2], 
            p0[2],
            alpha210[2],
            alpha220[2],
            alpha230[2],
            alpha240[2],
            alpha250[2],
            alpha260[2],
            alpha270[2],
            lambda270[2]) 

uB     <- c(alpha110[3], 
            alpha120[3], 
            alpha130[3], 
            alpha140[3], 
            alpha150[3], 
            alpha160[3], 
            alpha170[3], 
            lambda170[3],
            alpha310[3], 
            alpha320[3], 
            alpha330[3], 
            alpha340[3], 
            alpha350[3], 
            alpha360[3], 
            alpha370[3], 
            lambda170[3],
            mu20[3], 
            sigma10[3], 
            sigma20[3], 
            sigma30[3], 
            p0[3],
            alpha210[3],
            alpha220[3],
            alpha230[3],
            alpha240[3],
            alpha250[3],
            alpha260[3],
            alpha270[3],
            lambda270[3]) 

MRS <- optim(theta0, logLike, lower = lB, upper = uB, method = "L-BFGS-B", 
             control=list(trace=TRUE, maxit= 2500), hessian = TRUE)

alpha11 <- MRS$par[1]
alpha12 <- MRS$par[2]
alpha13 <- MRS$par[3]
alpha14 <- MRS$par[4]
alpha15 <- MRS$par[5]
alpha16 <- MRS$par[6]
alpha17 <- MRS$par[7]
lambda17 <- MRS$par[8]
alpha31 <- MRS$par[9]
alpha32 <- MRS$par[10]
alpha33 <- MRS$par[11]
alpha34 <- MRS$par[12]
alpha35 <- MRS$par[13]
alpha36 <- MRS$par[14]
alpha37 <- MRS$par[15]
lambda37 <- MRS$par[16]
mu2     <- MRS$par[17]  
sigma1  <- MRS$par[18]
sigma2  <- MRS$par[19]
sigma3  <- MRS$par[20]
p       <- MRS$par[21]
alpha21 <- MRS$par[22]
alpha22 <- MRS$par[23]
alpha23 <- MRS$par[24]
alpha24 <- MRS$par[25]
alpha25 <- MRS$par[26]
alpha26 <- MRS$par[27]
alpha27 <- MRS$par[28]
lambda27 <- MRS$par[29]


se <- sqrt(diag(solve(MRS$hessian))) # Standard error
AIC <- 2*(length(MRS$par) - MRS$value)
AIC

data.frame("alpha11" = c(MRS$par[1],  se[1],  MRS$par[1]/se[1]),
           "alpha12" = c(MRS$par[2],  se[2],  MRS$par[2]/se[2]),
           "alpha13" = c(MRS$par[3],  se[3],  MRS$par[3]/se[3]),
           "alpha14" = c(MRS$par[4],  se[4],  MRS$par[4]/se[4]),
           "alpha15" = c(MRS$par[5],  se[5],  MRS$par[5]/se[5]),
           "alpha16" = c(MRS$par[6],  se[6],  MRS$par[6]/se[6]),
           "alpha17" = c(MRS$par[7],  se[7],  MRS$par[7]/se[7]),
           "alpha31" = c(MRS$par[8],  se[8],  MRS$par[8]/se[8]),
           "alpha32" = c(MRS$par[9],  se[9],  MRS$par[9]/se[9]),
           "alpha33" = c(MRS$par[10], se[10], MRS$par[10]/se[10]),
           "alpha34" = c(MRS$par[11], se[11], MRS$par[11]/se[11]),
           "alpha35" = c(MRS$par[12], se[12], MRS$par[12]/se[12]),
           "alpha36" = c(MRS$par[13], se[13], MRS$par[13]/se[13]),
           "alpha37" = c(MRS$par[14], se[14], MRS$par[14]/se[14]),
           "mu2"     = c(MRS$par[15], se[15], MRS$par[15]/se[15]),
           "sigma1"  = c(MRS$par[16], se[16], MRS$par[16]/se[16]),
           "sigma2"  = c(MRS$par[17], se[17], MRS$par[17]/se[17]),
           "sigma3"  = c(MRS$par[18], se[18], MRS$par[18]/se[18]),
           "p"       = c(MRS$par[19], se[19], MRS$par[19]/se[19]))



### ¤¤ IS mm ¤¤ ### ---------------------------------------------------------------------
xi <- numeric(3)
xi[1:3] <- 1/3

eta <- numeric(3)

x.pred.is.a <- c() # Tom vektor til at indsætte de forecasted værdier for OOS

for (l in 8:slut.is) {
  x.pred.is.a[l] <- xi[1]*((1-alpha11)*dat[l-1]  + 
                              alpha12*dat[l-2] + 
                              alpha13*dat[l-3] +
                              alpha14*dat[l-4] +
                              alpha15*dat[l-5] +
                              alpha16*dat[l-6] +
                              alpha17*dat[l-7] )*1/(1-lambda17*dat[l-7]) +
                    xi[2]*(dat[l-1] + mu2) + 
                    xi[3]*((1-alpha31)*dat[l-1]  + 
                           alpha33*dat[l-3] +
                           alpha32*dat[l-2] + 
                           alpha34*dat[l-4] +
                           alpha35*dat[l-5] +
                           alpha36*dat[l-6] +
                           alpha37*dat[l-7])*1/(1-lambda37*dat[l-7])
  
  xi.temp <- xi
  
  eta[1] <- dnorm(dat[l], mean = (((1-alpha11)*dat[l-1]  + 
                                     alpha12*dat[l-2] + 
                                     alpha13*dat[l-3] +
                                     alpha14*dat[l-4] +
                                     alpha15*dat[l-5] +
                                     alpha16*dat[l-6] +
                                     alpha17*dat[l-7] )*1/(1-lambda17*dat[l-7])) , 
                  sd = sigma1)
  
  eta[2] <- dnorm(dat[l], mean = (mu2 + dat[l-1]), sd = sigma2)
  
  eta[3] <- dnorm(dat[l], mean = (((1-alpha31)*dat[l-1]  + 
                                     alpha32*dat[l-2] + 
                                     alpha33*dat[l-3] +
                                     alpha34*dat[l-4] +
                                     alpha35*dat[l-5] +
                                     alpha36*dat[l-6] +
                                     alpha37*dat[l-7])*1/(1-lambda37*dat[l-7])), sd = sigma3)
  
  like <- p*xi[1]*eta[1] + (1-p)*xi[1]*eta[2]+ xi[2]*eta[3] + xi[3]*eta[1]
  
  xi[1] <- (p*xi.temp[1]*eta[1] + xi.temp[3]*eta[1])/like
  
  xi[2] <- ((1-p)*xi.temp[1]*eta[2])/like
  
  xi[3] <- xi.temp[2]*eta[3]/like
}
x.pred.is.a <- x.pred.is.a[-c(1:7)]

plot(dat[8:slut.is], type = "l", main = "Uden sæson (OOS)")
lines(x.pred.is.a[1:slut.is], col = "red")


rmse.is.a <- sqrt(1/slut.is * sum((dat[8:slut.is] - x.pred.is.a)^2))
rmse.is.a

mae.is  <- mean(abs(dat[8:slut.is] - x.pred.is.a))
mae.is

### ¤¤ OOS mm ¤¤ ### --------------------------------------------------------------------
# 
# logLike.oos <- function(theta){
#   
#   alpha11 <- theta[1]
#   alpha12 <- theta[2]
#   alpha13 <- theta[3]
#   alpha14 <- theta[4]
#   alpha15 <- theta[5]
#   alpha16 <- theta[6]
#   alpha17 <- theta[7]
#   alpha31 <- theta[8]
#   alpha32 <- theta[9]
#   alpha33 <- theta[10]
#   alpha34 <- theta[11]
#   alpha35 <- theta[12]
#   alpha36 <- theta[13]
#   alpha37 <- theta[14]
#   mu2     <- theta[15]  
#   sigma1  <- theta[16]
#   sigma2  <- theta[17]
#   sigma3  <- theta[18]
#   p       <- theta[19]
#   
#   eta <- numeric(3)
#   
#   like    <- c()
#   
#   xi      <- numeric(3)
#   xi[1:3] <- 1/3
#   
#   likesum <-  0
#   
#   for (l in 8:slut.is) {
#     xi.temp <- xi
#     
#     eta[1] <- dnorm(dat[l], mean = ((1-alpha11)*dat[l-1]  + 
#                                       alpha12*dat[l-2] + 
#                                       alpha13*dat[l-3] +
#                                       alpha14*dat[l-4] +
#                                       alpha15*dat[l-5] +
#                                       alpha16*dat[l-6] +
#                                       alpha17*dat[l-7]), 
#                     sd = sigma1)
#     
#     eta[2] <- dnorm(dat[l], mean = (-mu2 + dat[l-1]), sd = sigma2)
#     
#     eta[3] <- dnorm(dat[l], mean = ((1-alpha31)*dat[l-1]  + 
#                                       alpha32*dat[l-2] + 
#                                       alpha33*dat[l-3] +
#                                       alpha34*dat[l-4] +
#                                       alpha35*dat[l-5] +
#                                       alpha36*dat[l-6] +
#                                       alpha37*dat[l-7]), sd = sigma3)
#     
#     like[l] <- p*xi[1]*eta[1] + (1-p)*xi[1]*eta[2]+ xi[2]*eta[3] + xi[3]*eta[1]
#     
#     xi[1] <- (p*xi.temp[1]*eta[1] + xi.temp[3]*eta[1])/like[l]
#     
#     xi[2] <- ((1-p)*xi.temp[1]*eta[2])/like[l]
#     
#     xi[3] <- xi.temp[2]*eta[3]/like[l]
#   }
#   
#   likesum <- sum(log(like), na.rm = TRUE)
#   
#   return(-likesum)
# }
# 
# eta <- numeric(3)
# 
# x.pred.oos.a <- c() # Tom vektor til at indsætte de forecasted værdier for OOS
# pred.inter.a <- c()
# 
# 
# for (l in start.oos:slut.oos) {
#   
#   theta0 <- c(alpha11, alpha12, alpha13, alpha14, alpha15, alpha16, alpha17,
#               alpha31, alpha32, alpha33, alpha34, alpha35, alpha36, alpha37,
#               mu2, sigma1, sigma2, sigma3, p)
#   
#   MRS <- optim(theta0, logLike.oos, lower = lB, upper = uB, method = "L-BFGS-B",
#                control = list(trace = FALSE, maxit = 500), hessian = FALSE)
#   
#   alpha11 <- MRS$par[1]
#   alpha12 <- MRS$par[2]
#   alpha13 <- MRS$par[3]
#   alpha14 <- MRS$par[4]
#   alpha15 <- MRS$par[5]
#   alpha16 <- MRS$par[6]
#   alpha17 <- MRS$par[7]
#   alpha31 <- MRS$par[8]
#   alpha32 <- MRS$par[9]
#   alpha33 <- MRS$par[10]
#   alpha34 <- MRS$par[11]
#   alpha35 <- MRS$par[12]
#   alpha36 <- MRS$par[13]
#   alpha37 <- MRS$par[14]
#   mu2     <- MRS$par[15]  
#   sigma1  <- MRS$par[16]
#   sigma2  <- MRS$par[17]
#   sigma3  <- MRS$par[18]
#   p       <- MRS$par[19]
#   
#   x.pred.oos.a[l] <- xi[1]*((1-alpha11)*dat[l-1]  + 
#                               alpha12*dat[l-2] + 
#                               alpha13*dat[l-3] +
#                               alpha14*dat[l-4] +
#                               alpha15*dat[l-5] +
#                               alpha16*dat[l-6] +
#                               alpha17*dat[l-7]) +
#     xi[2]*(dat[l-1] + mu2) + 
#     xi[3]*((1-alpha31)*dat[l-1]  + 
#              alpha32*dat[l-2] + 
#              alpha33*dat[l-3] +
#              alpha34*dat[l-4] +
#              alpha35*dat[l-5] +
#              alpha36*dat[l-6] +
#              alpha37*dat[l-7])
#   
#   xi.temp <- xi
#   
#   eta[1] <- dnorm(dat[l], mean = ((1-alpha11)*dat[l-1]  + 
#                                     alpha12*dat[l-2] + 
#                                     alpha13*dat[l-3] +
#                                     alpha14*dat[l-4] +
#                                     alpha15*dat[l-5] +
#                                     alpha16*dat[l-6] +
#                                     alpha17*dat[l-7]), 
#                   sd = sigma1)
#   eta[2] <- dnorm(dat[l], mean = (-mu2 + dat[l-1]), sd = sigma2)
#   eta[3] <- dnorm(dat[l], mean = ((1-alpha31)*dat[l-1] + (alpha32)*dat[l-2]), sd = sigma3)
#   
#   like <- p*xi[1]*eta[1] + (1-p)*xi[1]*eta[2]+ xi[2]*eta[3] + xi[3]*eta[1]
#   
#   xi[1] <- (p*xi.temp[1]*eta[1] + xi.temp[3]*eta[1])/like
#   
#   xi[2] <- ((1-p)*xi.temp[1]*eta[2])/like
#   
#   xi[3] <- xi.temp[2]*eta[3]/like
#   
#   print(l-slut.is)
# }
# 
# plot(dat[2100:slut.oos], type = "l", main = "Uden sæson (OOS)")
# lines(x.pred.oos.a[2100:slut.oos], col = "red")
# 
# plot(OOS$A, type = "l", main = "Med sæson (OOS)")
# lines(x.pred.oos.a[start.oos:slut.oos] + OOS$s.pred, col = "red")
# 
# rmse.oos.a <- sqrt(1/(slut.oos-start.oos+1)*sum((dat[start.oos:slut.oos] - 
#                                                    x.pred.oos.a[start.oos:slut.oos])^2))
# rmse.oos.a
# 
# mae.oos  <- mean(abs(dat[start.oos:slut.oos] - x.pred.oos.a[start.oos:slut.oos]))
# mae.oos
### ¤¤ Pred.inter mm ¤¤ ### -------------------------------------------------------------
pred.inter.a <- 1.96 * pred.inter.a

res.oos.a <- OOS$D - x.pred.oos.a[start.oos:slut.oos]

res.is.a <- DK1$D[8:slut.is] - x.pred.is.a

### ¤¤ Visual mm ¤¤ ### -----------------------------------------------------------------

# Histogram for residualer Model A
p.hist.res.a <- ggplot(data.frame(X2 = res.is.a),
                       aes(x = X2)) +
  geom_histogram(binwidth = 20, color = "white", fill = colors[1]) + 
  stat_function(fun = function(x){dnorm(x = x, 
                                        mean = mean(res.is.a), 
                                        sd = sd(res.is.a))*length(res.is.a)*24.5}, 
                color = colors[2], size = sz$l) +
  labs(x = "Residualer", y = "Tæthed") +
  scale_x_continuous() +
  p.th
p.hist.res.a

# Q-Q plot af sæsonrensede priser
std.res.a <- (res.is.a - mean(res.is.a))/sqrt(var(res.is.a))
y <- quantile(std.res.a, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
quantiles <- qqnorm(std.res.a)

p.qq.res.a <- ggplot(data.frame(x = quantiles$x, y = quantiles$y), aes(x = x, y = y))+
  geom_point(col = colors[1], size = sz$p) +
  geom_abline(slope = slope, intercept = int, linetype = "dashed", col = colors[2],
              size = sz$l) + 
  labs(x = "Standard normal teoretisk fraktil", y = "Standardiserede residualer") +
  theme(legend.position="none") +
  p.th
p.qq.res.a    

# ACF for residauler Model A
p.acf.res.a <- ggplot(data.frame(X1 = acf(res.is.a, lag.max = 2190 , plot = FALSE)$lag[2:31],
                                 X2 = acf(res.is.a, lag.max = 2190 , plot = FALSE)$acf[2:31]), 
                      aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0, size = sz$l)) +
  geom_segment(aes(xend = X1, yend = 0), color = colors[1]) +
  geom_hline(aes(yintercept = -ci(res.is.a)), 
             color = colors[2], linetype = "dotted") +
  geom_hline(aes(yintercept =  ci(res.is.a)), 
             color = colors[2], linetype = "dotted") +
  labs(x = "Lag", y = "ACF") +
  p.th
p.acf.res.a

# Ljung-Box residualer Model A
lag.max <- 30
which.lag <- 1:lag.max
p.vals <- numeric(lag.max)
for (l in 1:lag.max){
  p.vals[l] <- Box.test(res.is.a , lag = l , type = "Ljung-Box")$p.value
}
p.lbox.res.a <- ggplot(data.frame(X1 = which.lag,
                                  X2 = p.vals), 
                       aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0, size = sz$l)) +
  geom_point(aes(), color = colors[1]) +
  geom_hline(aes(yintercept =  0.05 ), 
             color = colors[2], linetype = "dotted") +
  labs(x = "Lag", y = "P-værdi") +
  coord_cartesian(ylim=c(0,1)) +
  p.th
p.lbox.res.a
