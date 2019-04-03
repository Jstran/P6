rm(list=ls())
load("./Workspaces/preliminary.Rdata")
load("./Workspaces/modelling.Rdata")
library(MuMIn)

pc    <- pi/365.25

start.time <- Sys.time()

rmse <- numeric(5184)
aic  <- numeric(5184)

df <- data.frame(DK1 = DK1$A , t = t , sat = DK1$sat , sun = DK1$sun , hol = DK1$hol ,
                 sin2  = sin((2*pc)*t)  , cos2  = cos((2*pc)*t) ,
                 sin4  = sin((4*pc)*t)  , cos4  = cos((4*pc)*t) ,
                 sin8  = sin((8*pc)*t)  , cos8  = cos((8*pc)*t) ,
                 sin24 = sin((24*pc)*t) , cos24 = cos((24*pc)*t) )

for (i in 0:729) {
  dfsub <- subset(df, t <= 1461 + i)
  glob.lm <- lm(DK1 ~ t + I(t^2) + I(t^3) +
                      sin2 + cos2 + sin4  + cos4  + 
                      sin8 + cos8 + sin24 + cos24 +
                      sat  + sun  + hol , 
                      na.action = "na.fail" , data = dfsub )
  
  lm.combinations <- sapply(dredge(glob.lm , 
                                   evaluate = FALSE , 
                                   subset = c( dc(sin2 , cos2) , dc(sin4 , cos4) ,
                                               dc(sin8 , cos8) , dc(sin24 , cos24) ) ) , 
                            eval)
  
  aic  <- aic + sapply(lm.combinations, AIC)
  for (l in 1:5184) {
    rmse[l] <- rmse[l] + sqrt((dfsub$DK1 - predict(lm.combinations[[l]], newdata = dfsub ) )^2)
  }
  print(i)  
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Gennemsnitter
rmse1 <- rmse/730
aic1 <- aic/730

lm.combinations[[which.min(aic1)]]
lm.combinations[[which.min(rmse1)]]

save(rmse1, aic1, 
     file = "./Workspaces/nested.Rdata")


### ¤¤ Det Vilde Vesten ¤¤ ### ----------------------------------------------------------
x  <- 1:10
y  <- rnorm(10)
z  <- rnorm(10)
y2 <- rnorm(10)
z2 <- rnorm(10)
df <- data.frame(x = x , y = y , y2 =  y2 , z = z , z2 = z2)
glob.lm.test <- lm(x ~ y + cos(2 * y2) + z + z2 , na.action = "na.fail" , data = df)
test <- getAllTerms(glob.lm.test)


lm.combinations.test <- dredge(glob.lm.test , evaluate = FALSE , subset = (y & (cos(2 * y2)) ) | (z & z2) )

lm.combinations.test <- sapply(dredge(glob.lm.test , evaluate = FALSE , subset = (y & y2) | (z & z2) ) , eval )

lm.combinations.test
lm.combinations.test[[4]]


# 