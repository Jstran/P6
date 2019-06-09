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
  alpha18 <- theta[8]
  alpha31 <- theta[9]
  alpha32 <- theta[10]
  alpha33 <- theta[11]
  alpha34 <- theta[12]
  alpha35 <- theta[13]
  alpha36 <- theta[14]
  alpha37 <- theta[15]
  alpha38 <- theta[16]
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
  alpha28 <- theta[29]
  
  eta <- numeric(3)
  
  like    <- c()
  
  xi      <- numeric(3)
  xi[1:3] <- 1/3
  
  likesum <-  0
  
  for (l in 9:slut.is) {
    xi.temp <- xi
    
    eta[1] <- dnorm(dat[l], mean = ((1-alpha11)*dat[l-1]  + 
                                      alpha12*dat[l-2] + 
                                      alpha13*dat[l-3] +
                                      alpha14*dat[l-4] +
                                      alpha15*dat[l-5] +
                                      alpha16*dat[l-6] +
                                      alpha17*dat[l-7] +
                                      alpha18*dat[l-8]), 
                    sd = sigma1)
    
    eta[2] <- dnorm(dat[l], mean = (mu2 + alpha21*dat[l-1]  + 
                      alpha22*dat[l-2] + 
                      alpha23*dat[l-3] +
                      alpha24*dat[l-4] +
                      alpha25*dat[l-5] +
                      alpha26*dat[l-6] +
                      alpha27*dat[l-7] +
                      alpha28*dat[l-8]), sd = sigma2)
    
    eta[3] <- dnorm(dat[l], mean = ((1-alpha31)*dat[l-1]  + 
                                      alpha32*dat[l-2] + 
                                      alpha33*dat[l-3] +
                                      alpha34*dat[l-4] +
                                      alpha35*dat[l-5] +
                                      alpha36*dat[l-6] +
                                      alpha37*dat[l-7] +
                                      alpha38*dat[l-8]), sd = sigma3)
    
    like[l] <- p*xi[1]*eta[1] + (1-p)*xi[1]*eta[2]+ xi[2]*eta[3] + xi[3]*eta[1]
    
    xi[1] <- (p*xi.temp[1]*eta[1] + xi.temp[3]*eta[1])/like[l]
    
    xi[2] <- ((1-p)*xi.temp[1]*eta[2])/like[l]
    
    xi[3] <- xi.temp[2]*eta[3]/like[l]
  }
  
  likesum <- sum(log(like), na.rm = TRUE)
  
  return(-likesum)
}

alpha11 <- c(0.3,0,200)
alpha12 <- c(0.1,-10,200)
alpha13 <- c(0.1,-10,200)
alpha14 <- c(0.1,-10,200)
alpha15 <- c(0.1,-10,200)
alpha16 <- c(0.1,-10,200)
alpha17 <- c(0.1,-10,200)
alpha18 <- c(0.1,-10,200)
alpha31 <- c(0.6,0,200)
alpha32 <- c(0.1,-10,200)
alpha33 <- c(0.1,-10,200)
alpha34 <- c(0.1,-10,200)
alpha35 <- c(0.1,-10,200)
alpha36 <- c(0.1,-10,200)
alpha37 <- c(0.1,-10,200)
alpha38 <- c(0.1,-10,200)
mu2     <- c(7,-200,200)
sigma1  <- c(33,0,200)
sigma2  <- c(90,0,200)
sigma3  <- c(75,0,200)
p       <- c(0.9,0.001)
alpha21 <- c(0.1,-10,200)
alpha22 <- c(0.1,-10,200)
alpha23 <- c(0.1,-10,200)
alpha24 <- c(0.1,-10,200)
alpha25 <- c(0.1,-10,200)
alpha26 <- c(0.1,-10,200)
alpha27 <- c(0.1,-10,200)
alpha28 <- c(0.1,-10,200)


theta0 <- c(alpha11[1], 
            alpha12[1], 
            alpha13[1], 
            alpha14[1], 
            alpha15[1], 
            alpha16[1], 
            alpha17[1], 
            alpha18[1],
            alpha31[1], 
            alpha32[1], 
            alpha33[1], 
            alpha34[1], 
            alpha35[1], 
            alpha36[1], 
            alpha37[1], 
            alpha38[1],
            mu2[1], 
            sigma1[1], 
            sigma2[1], 
            sigma3[1], 
            p[1],
            alpha21[1], 
            alpha22[1], 
            alpha23[1], 
            alpha24[1], 
            alpha25[1], 
            alpha26[1], 
            alpha27[1], 
            alpha28[1])

lB     <- c(alpha11[2], 
            alpha12[2], 
            alpha13[2], 
            alpha14[2], 
            alpha15[2], 
            alpha16[2], 
            alpha17[2], 
            alpha18[2],
            alpha31[2], 
            alpha32[2], 
            alpha33[2], 
            alpha34[2], 
            alpha35[2], 
            alpha36[2], 
            alpha37[2], 
            alpha38[2],
            mu2[2], 
            sigma1[2], 
            sigma2[2], 
            sigma3[2], 
            p[2],
            alpha21[2], 
            alpha22[2], 
            alpha23[2], 
            alpha24[2], 
            alpha25[2], 
            alpha26[2], 
            alpha27[2], 
            alpha28[2]) 

uB     <- c(alpha11[3], 
            alpha12[3], 
            alpha13[3], 
            alpha14[3], 
            alpha15[3], 
            alpha16[3], 
            alpha17[3], 
            alpha18[3],
            alpha31[3], 
            alpha32[3], 
            alpha33[3], 
            alpha34[3], 
            alpha35[3], 
            alpha36[3], 
            alpha37[3], 
            alpha38[3],
            mu2[3], 
            sigma1[3], 
            sigma2[3], 
            sigma3[3], 
            p[3],
            alpha21[3], 
            alpha22[3], 
            alpha23[3], 
            alpha24[3], 
            alpha25[3], 
            alpha26[3], 
            alpha27[3], 
            alpha28[3]) 

MRS <- optim(theta0, logLike, lower = lB, upper = uB, method = "L-BFGS-B", 
             control=list(trace=TRUE, maxit= 2500), hessian = TRUE)

alpha11 <- MRS$par[1]
alpha12 <- MRS$par[2]
alpha13 <- MRS$par[3]
alpha14 <- MRS$par[4]
alpha15 <- MRS$par[5]
alpha16 <- MRS$par[6]
alpha17 <- MRS$par[7]
alpha18 <- MRS$par[8]
alpha31 <- MRS$par[9]
alpha32 <- MRS$par[10]
alpha33 <- MRS$par[11]
alpha34 <- MRS$par[12]
alpha35 <- MRS$par[13]
alpha36 <- MRS$par[14]
alpha37 <- MRS$par[15]
alpha38 <- MRS$par[16]
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
alpha28 <- MRS$par[29]

se <- sqrt(diag(solve(MRS$hessian))) # Standard error
AIC.ar8 <- 2*(length(MRS$par) - MRS$value)
AIC.ar8

est.ar8 <- data.frame("alpha11" = c(MRS$par[1],  se[1],  MRS$par[1]/se[1]), 
           "alpha12" = c(MRS$par[2],  se[2],  MRS$par[2]/se[2]), 
           "alpha13" = c(MRS$par[3],  se[3],  MRS$par[3]/se[3]), 
           "alpha14" = c(MRS$par[4],  se[4],  MRS$par[4]/se[4]), 
           "alpha15" = c(MRS$par[5],  se[5],  MRS$par[5]/se[5]), 
           "alpha16" = c(MRS$par[6],  se[6],  MRS$par[6]/se[6]), 
           "alpha17" = c(MRS$par[7],  se[7],  MRS$par[7]/se[7]), 
           "alpha18" = c(MRS$par[8],  se[8],  MRS$par[8]/se[8]), 
           "alpha31" = c(MRS$par[9],  se[9],  MRS$par[9]/se[9]), 
           "alpha32" = c(MRS$par[10], se[10], MRS$par[10]/se[10]),
           "alpha33" = c(MRS$par[11], se[11], MRS$par[11]/se[11]),
           "alpha34" = c(MRS$par[12], se[12], MRS$par[12]/se[12]),
           "alpha35" = c(MRS$par[13], se[13], MRS$par[13]/se[13]),
           "alpha36" = c(MRS$par[14], se[14], MRS$par[14]/se[14]),
           "alpha37" = c(MRS$par[15], se[15], MRS$par[15]/se[15]),
           "alpha38" = c(MRS$par[16], se[16], MRS$par[16]/se[16]),
           "mu2"     = c(MRS$par[17], se[17], MRS$par[17]/se[17]),
           "sigma1"  = c(MRS$par[18], se[18], MRS$par[18]/se[18]),
           "sigma2"  = c(MRS$par[19], se[19], MRS$par[19]/se[19]),
           "sigma3"  = c(MRS$par[20], se[20], MRS$par[20]/se[20]),
           "p"       = c(MRS$par[21], se[21], MRS$par[21]/se[21]),
           "alpha21" = c(MRS$par[22], se[22], MRS$par[22]/se[22]),
           "alpha22" = c(MRS$par[23], se[23], MRS$par[23]/se[23]),
           "alpha23" = c(MRS$par[24], se[24], MRS$par[24]/se[24]),
           "alpha24" = c(MRS$par[25], se[25], MRS$par[25]/se[25]),
           "alpha25" = c(MRS$par[26], se[26], MRS$par[26]/se[26]),
           "alpha26" = c(MRS$par[27], se[27], MRS$par[27]/se[27]),
           "alpha27" = c(MRS$par[28], se[28], MRS$par[28]/se[28]),
           "alpha28" = c(MRS$par[29], se[29], MRS$par[29]/se[29]))


### ¤¤ IS mm ¤¤ ### ---------------------------------------------------------------------
xi <- numeric(3)
xi[1:3] <- 1/3

eta <- numeric(3)

x.pred.is.ar8 <- c() # Tom vektor til at indsætte de forecasted værdier for OOS

for (l in 9:slut.is) {
  x.pred.is.ar8[l] <- xi[1]*((1-alpha11)*dat[l-1]  + 
                             alpha12*dat[l-2] + 
                             alpha13*dat[l-3] +
                             alpha14*dat[l-4] +
                             alpha15*dat[l-5] +
                             alpha16*dat[l-6] +
                             alpha17*dat[l-7] +
                             alpha18*dat[l-8]) +
    xi[2]*(mu2 + alpha21*dat[l-1]  + 
             alpha22*dat[l-2] + 
             alpha23*dat[l-3] +
             alpha24*dat[l-4] +
             alpha25*dat[l-5] +
             alpha26*dat[l-6] +
             alpha27*dat[l-7] +
             alpha28*dat[l-8]) + 
    xi[3]*((1-alpha31)*dat[l-1]  + 
             alpha32*dat[l-2] + 
             alpha33*dat[l-3] +
             alpha34*dat[l-4] +
             alpha35*dat[l-5] +
             alpha36*dat[l-6] +
             alpha37*dat[l-7] +
             alpha38*dat[l-8])
  
  xi.temp <- xi
  
  eta[1] <- dnorm(dat[l], mean = ((1-alpha11)*dat[l-1]  + 
                                    alpha12*dat[l-2] + 
                                    alpha13*dat[l-3] +
                                    alpha14*dat[l-4] +
                                    alpha15*dat[l-5] +
                                    alpha16*dat[l-6] +
                                    alpha17*dat[l-7] +
                                    alpha18*dat[l-8]), 
                  sd = sigma1)
  
  eta[2] <- dnorm(dat[l], mean = (mu2 + alpha21*dat[l-1]  + 
                                    alpha22*dat[l-2] + 
                                    alpha23*dat[l-3] +
                                    alpha24*dat[l-4] +
                                    alpha25*dat[l-5] +
                                    alpha26*dat[l-6] +
                                    alpha27*dat[l-7] +
                                    alpha28*dat[l-8]), sd = sigma2)
  
  eta[3] <- dnorm(dat[l], mean = ((1-alpha31)*dat[l-1]  + 
                                    alpha32*dat[l-2] + 
                                    alpha33*dat[l-3] +
                                    alpha34*dat[l-4] +
                                    alpha35*dat[l-5] +
                                    alpha36*dat[l-6] +
                                    alpha37*dat[l-7] +
                                    alpha38*dat[l-8]), sd = sigma3)
  
  like <- p*xi[1]*eta[1] + (1-p)*xi[1]*eta[2]+ xi[2]*eta[3] + xi[3]*eta[1]
  
  xi[1] <- (p*xi.temp[1]*eta[1] + xi.temp[3]*eta[1])/like
  
  xi[2] <- ((1-p)*xi.temp[1]*eta[2])/like
  
  xi[3] <- xi.temp[2]*eta[3]/like
}
x.pred.is.ar8 <- x.pred.is.ar8[-c(1:8)]

plot(dat[8:slut.is], type = "l", main = "Uden sæson (OOS)")
lines(x.pred.is.ar8[1:slut.is], col = "red")


rmse.is.ar8 <- sqrt(1/slut.is * sum((dat[9:slut.is] - x.pred.is.ar8)^2))
rmse.is.ar8

mae.is  <- mean(abs(dat[9:slut.is] - x.pred.is.a))
mae.is

### ¤¤ OOS mm ¤¤ ### --------------------------------------------------------------------

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
# pred.inter.a <- 1.96 * pred.inter.a

# res.oos.a <- OOS$D - x.pred.oos.a[start.oos:slut.oos]

res.is.ar8 <- DK1$D[9:slut.is] - x.pred.is.ar8

### ¤¤ Visual mm ¤¤ ### -----------------------------------------------------------------

# Histogram for residualer Model A
ggplot(data.frame(X2 = res.is.ar8),
                       aes(x = X2)) +
  geom_histogram(binwidth = 20, color = "white", fill = colors[1]) + 
  stat_function(fun = function(x){dnorm(x = x, 
                                        mean = mean(res.is.ar8), 
                                        sd = sd(res.is.ar8))*length(res.is.ar8)*24.5}, 
                color = colors[2], size = sz$l) +
  labs(x = "Residualer", y = "Tæthed") +
  scale_x_continuous() +
  p.th


# Q-Q plot af sæsonrensede priser
std.res.ar8 <- (res.is.ar8 - mean(res.is.ar8))/sqrt(var(res.is.ar8))
y <- quantile(std.res.ar8, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
quantiles <- qqnorm(std.res.ar8)

ggplot(data.frame(x = quantiles$x, y = quantiles$y), aes(x = x, y = y))+
  geom_point(col = colors[1], size = sz$p) +
  geom_abline(slope = slope, intercept = int, linetype = "dashed", col = colors[2],
              size = sz$l) + 
  labs(x = "Standard normal teoretisk fraktil", y = "Standardiserede residualer") +
  theme(legend.position="none") +
  p.th
  

# ACF for residauler Model A
ggplot(data.frame(X1 = acf(res.is.ar8, lag.max = 2190 , plot = FALSE)$lag[2:31],
                                 X2 = acf(res.is.ar8, lag.max = 2190 , plot = FALSE)$acf[2:31]), 
                      aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0, size = sz$l)) +
  geom_segment(aes(xend = X1, yend = 0), color = colors[1]) +
  geom_hline(aes(yintercept = -ci(res.is.a)), 
             color = colors[2], linetype = "dotted") +
  geom_hline(aes(yintercept =  ci(res.is.a)), 
             color = colors[2], linetype = "dotted") +
  labs(x = "Lag", y = "ACF") +
  p.th


# Ljung-Box residualer Model A
lag.max <- 30
which.lag <- 1:lag.max
p.vals <- numeric(lag.max)
for (l in 1:lag.max){
  p.vals[l] <- Box.test(res.is.ar8 , lag = l , type = "Ljung-Box")$p.value
}
ggplot(data.frame(X1 = which.lag,
                                  X2 = p.vals), 
                       aes(x = X1, y = X2)) +
  geom_hline(aes(yintercept =  0, size = sz$l)) +
  geom_point(aes(), color = colors[1]) +
  geom_hline(aes(yintercept =  0.05 ), 
             color = colors[2], linetype = "dotted") +
  labs(x = "Lag", y = "P-værdi") +
  coord_cartesian(ylim=c(0,1)) +
  p.th


### ¤¤ Gemmer workspace ¤¤ ### ----------------------------------------------------------
save(x.pred.is.ar8, rmse.is.ar8, res.is.ar8,
     est.ar8, AIC.ar8,
     file = "./Workspaces/forecastModAr8.Rdata")
