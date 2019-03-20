### ¤¤ Intro ¤¤ ### ---------------------------------------------------------------------

rm(list=ls())

### ¤¤ Pakker ¤¤ ### --------------------------------------------------------------------

library(stats)
library(ggplot2)
library(lubridate)
library(forecast)
library(astsa)

### ¤¤ Funktioner ¤¤ ### --------------------------------------------

# Omregning til kende koefficienter af s(t) 
scoef <- function(mod){
  coef <- as.numeric(mod$coefficients)
  
  # Koefficienter til årlig periode
  c1 <- sqrt(coef[4]^2 + coef[5]^2)
  c2 <- atan(coef[5]/coef[4]) * 365.25/(2*pi)
  
  # Koefficienter til halvårlig periode
  c3 <- sqrt(coef[6]^2 + coef[7]^2)
  c4 <- atan(coef[7]/coef[6]) * 365.25/(4*pi)
  
  # Koefficienter til kvartal periode
  c5 <- sqrt(coef[8]^2 + coef[9]^2)
  c6 <- atan(coef[9]/coef[8]) * 365.25/(8*pi) 
  
  # Dataframe med alt info
  df <- data.frame(b0 = coef[1] , b1 = coef[2] , b2 = coef[3] , c1 = c1 , c2 = c2 ,
                   c3 = c3 , c4 = c4 , c5 = c5, c6 = c6, 
                   d1 = coef[10] , d2 = coef[11] , d3 = coef[12]) 
  return(df)
}

# Konfidensinterval
ci <- function(n = numeric(2191)){
  qnorm((1 + 0.95)/2)/sqrt(length(n))
}

### ¤¤ Farver + tema til brug i plots ¤¤ ### -------------------------------------------

# Farver til grafer
colors <- c("royalblue4" ,
            "firebrick4" ,
            "darkorchid4",
            "chartreuse4",
            "black")

# Grå farve der matcher projektet
myGray <- rgb(245/255, 245/255, 245/255)

# Tema til plots
p6 <- theme(panel.background = element_rect(fill = myGray, colour = myGray,
                                  size = 2, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
            )

### ¤¤ Indlæsning af data ¤¤ ### --------------------------------------------------------

# Indlaeser csv filerne som "dat20xx".
path <- file.path("./Data")
years <- list.files(path,pattern =".csv",full.names = 1)
years.names <- seq(2013, length.out = length(years))

for (l in 1:length(years.names)) {
  assign(paste("dat", years.names[l], sep = ""), read.csv2(years[l], skip = 2))
}
oos <- dat2019[,8]

### ¤¤ Data frame og lister med data ¤¤ ### ---------------------------------------------

# Laver datoer.
dates <- seq(ymd("2013-01-01"), ymd("2018-12-31"), by="days")

# Totale antal observationer
n.obs <- length(dates)

# Lørdag dummy, 01-01-13 er en tirsdag
sat <- numeric(n.obs)
sat[seq(5, n.obs, by = 7)] <- 1

# Søndag dummy, 01-01-13 er en tirsdag
sun <- numeric(n.obs)
sun[seq(6, n.obs, by = 7)] <- 1

# Danske helligdage
hol <- numeric(n.obs)
hol[c(1,    83,   87,   88,   90,   91,   116,  129,  139,  140,  359,  360,  # 2013
      366,  468,  472,  473,  475,  476,  501,  514,  524,  525,  724,  725,  # 2014
      731,  818,  822,  823,  825,  826,  851,  864,  874,  875,  1089, 1090, # 2015
      1096, 1175, 1179, 1180, 1182, 1183, 1208, 1221, 1231, 1232, 1455, 1456, # 2016
      1462, 1560, 1564, 1565, 1567, 1568, 1593, 1606, 1616, 1617, 1820, 1821, # 2017
      1827, 1910, 1914, 1915, 1917, 1918, 1943, 1956, 1966, 1967, 2185, 2186  # 2018
      )] <- 1

# Fjerner alle lørdage der også er helligdage
for (i in 1:2191) {
  if (sat[i] == 1 && sat[i] == hol[i]){
    sat[i] <- 0
  } 
}

# Fjerner alle søndage der også er helligdage
for (i in 1:2191) {
  if (sun[i] == 1 && sun[i] == hol[i]){
    sun[i] <- 0
  } 
}


# Samler hvert år samt alle år sammen for DK1 i en liste
fq <- 1
DK1 <- list(A   = ts(c(dat2013[,8], 
                       dat2014[,8],
                       dat2015[,8],
                       dat2016[,8],
                       dat2017[,8],
                       dat2018[,8]) , frequency = fq),
            sat = sat,
            sun = sun,
            hol = hol)

# Fjerner 07-06-13 og erstatter med gennesnit af dagen før og efter. (Spike-dagen)
DK1$A[which.max(DK1$A)] <- mean(c((DK1$A[which.max(DK1$A)-1]), 
                                  (DK1$A[which.max(DK1$A)+1])))

# Fjerner midlertidige variable
rm("dat2013","dat2014","dat2015","dat2016","dat2017","dat2018","dat2019","fq","i","l",
   "myGray","path","sat","sun","hol","years","years.names")

### ¤¤ Regression ¤¤ ### ----------------------------------------------------------------

# Regression på Escribano model med kvadratisk led (t^2)
t     <- time(DK1$A)
lm.s <- lm(DK1$A ~ t + I(t^2) + 
              sin((2*pi/365.25)*t) + cos((2*pi/365.25)*t) + 
              sin((4*pi/365.25)*t) + cos((4*pi/365.25)*t) +
              sin((8*pi/365.25)*t) + cos((8*pi/365.25)*t) +
              DK1$sat + DK1$sun + DK1$hol)

s.pred  <- predict(lm.s)

DK1[[length(DK1)+1]] <- c(s.pred)
names(DK1)[[length(DK1)]] <- "s.pred"

DK1[[length(DK1)+1]] <- c(DK1$A - s.pred)
names(DK1)[[length(DK1)]] <- "D"

### ¤¤ Gemmer workspace ¤¤ ### ----------------------------------------------------------

save(DK1, p6, colors, dates, n.obs, oos, s.pred, t, ci, scoef,
     file = "./Workspaces/preliminary.Rdata")

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------

