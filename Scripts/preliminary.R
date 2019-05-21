### ¤¤ Intro ¤¤ ### ---------------------------------------------------------------------

rm(list=ls())

### ¤¤ Pakker ¤¤ ### --------------------------------------------------------------------

library(stats)
library(ggplot2)
library(lubridate)
library(forecast)
library(astsa)
library(tseries)

### ¤¤ Funktioner ¤¤ ### --------------------------------------------

# Omregning til kende koefficienter af s(t) 
scoef <- function(mod){
  coef   <- as.numeric(mod$coefficients)
  cm     <- vcov(mod)
  # Koefficienter til årlig periode
  c1  <- sqrt(coef[4]^2 + coef[5]^2)
  c2  <- atan(coef[5]/coef[4]) * 365.25/(2*pi)
  
  # Koefficienter til halvårlig periode
  c3  <- sqrt(coef[6]^2 + coef[7]^2)
  c4  <- atan(coef[7]/coef[6]) * 365.25/(8*pi)
  
  # Koefficienter til kvartal periode
  c5  <- sqrt(coef[8]^2 + coef[9]^2)
  c6  <- atan(coef[9]/coef[8]) * 365.25/(24*pi) 
  
  # Standardafvigelser til årlig periode
  sdc1 <- sqrt( 1/(coef[4]^2 + coef[5]^2)*(coef[4]^2 * cm[4,4] + coef[5]^2*cm[5,5] + coef[4]*coef[5] * 2*cm[4,5]) )
  sdc2 <- sqrt( (365.25/( 2*pi*(coef[4]^2 + coef[5]^2) ) )^2 * ( coef[5]^2 * cm[4,4] + coef[4]^2 * cm[5,5] - 2*coef[4]*coef[5]*cm[4,5])  )
  
  # Standardafvigelser til halvårlig periode
  sdc3 <- sqrt( 1/(coef[6]^2 + coef[7]^2)*(coef[6]^2 * cm[6,6] + coef[7]^2*cm[7,7] + coef[6]*coef[7] * 2*cm[6,7]) )
  sdc4 <- sqrt( (365.25/( 2*pi*(coef[6]^2 + coef[7]^2) ) )^2 * ( coef[7]^2 * cm[6,6] + coef[6]^2 * cm[7,7] - 2*coef[6]*coef[7]*cm[6,7])  )
  
  # Standardafvigelser til kvartal periode
  sdc5 <- sqrt( 1/(coef[8]^2 + coef[9]^2)*(coef[8]^2 * cm[8,8] + coef[9]^2*cm[9,9] + coef[8]*coef[9] * 2*cm[8,9]) )
  sdc6 <- sqrt( (365.25/( 2*pi*(coef[8]^2 + coef[9]^2) ) )^2 * ( coef[9]^2 * cm[8,8] + coef[8]^2 * cm[9,9] - 2*coef[8]*coef[9]*cm[8,9])  )
  
  # Dataframe med alt info
  ls <- list( coef = data.frame(b0 = coef[1] , b1 = coef[2] , b2 = coef[3] , c1 = c1 , c2 = c2 ,
                                c3 = c3 , c4 = c4 , c5 = c5, c6 = c6, 
                                d1 = coef[10] , d2 = coef[11] , d3 = coef[12]) ,
              sdc  = data.frame(b0 = sqrt(cm[1,1]) , b1 = sqrt(cm[2,2]) , b2 = sqrt(cm[3,3]) , 
                                c1 = sdc1 , c2 = sdc2 , c3 = sdc3 , c4 = sdc4 , c5 = sdc5 , c6 = sdc6 ,
                                d1 = sqrt(cm[10,10]) , d2 = sqrt(cm[11,11]) , d3 = sqrt(cm[12,12])  ) )
  return(ls)
}

# Konfidensinterval
ci <- function(n = numeric(2191)){
  qnorm((1 + 0.95)/2)/sqrt(length(n))
}

### ¤¤ Indlæsning af data ¤¤ ### --------------------------------------------------------

# Indlaeser csv filerne som "dat20xx".
path        <- file.path("./Data/Spotpriser")
years       <- list.files(path,pattern =".csv",full.names = 1)
years.names <- seq(2013, length.out = length(years))

for (l in 1:length(years.names)) {
  assign(paste("dat", years.names[l], sep = ""), read.csv2(years[l], skip = 2))
}

# Indlaeser csv filerne som "wind20xx".
path.win        <- file.path("./Data/Vind")
years.win       <- list.files(path.win ,pattern =".csv",full.names = 1)
years.names.win <- seq(2013, length.out = length(years.win))

for (l in 1:length(years.names.win)) {
  assign(paste("wind", years.names.win[l], sep = ""), read.csv2(years.win[l], skip = 2))
}

# Indlaeser csv filerne som "windProg20xx".
path.win.prog        <- file.path("./Data/Vind/Prognose")
years.win.prog       <- list.files(path.win.prog ,pattern =".csv",full.names = 1)
years.names.win.prog <- seq(2019, length.out = length(years.win.prog))

for (l in 1:length(years.names.win.prog)) {
  assign(paste("wind.prog", years.names.win.prog[l], sep = ""), read.csv2(years.win.prog[l], 
                                                                     skip = 2))
}

# Indlaeser csv filerne som "con20xx".
# path.con        <- file.path("./Data/Consumption")
# years.con       <- list.files(path.con ,pattern =".csv",full.names = 1)
# years.names.con <- seq(2013, length.out = length(years.con))
# 
# for (l in 1:length(years.names.con)) {
#   assign(paste("con", years.names.win[l], sep = ""), read.csv2(years.con[l], skip = 2))
# }

### ¤¤ Data frame og lister med data ¤¤ ### ---------------------------------------------

# Laver datoer.
dates  <- seq(ymd("2013-01-01"), ymd("2018-12-31"), by="days")
dates2 <- seq(ymd("2019-01-01"), ymd("2019-03-31"), by="days")
dates.all <- seq(ymd("2013-01-01"), ymd("2019-03-31"), by="days")

# Totale antal observationer
n.obs <- length(dates)
n.obs.oos <- length(dates2)

# Lørdag dummy, 01-01-13 er en tirsdag
sat <- numeric(n.obs)
sat[seq(5, n.obs, by = 7)] <- 1

sat.oos <- numeric(n.obs.oos)
sat.oos[seq(5, n.obs.oos, by = 7)] <- 1

# Søndag dummy, 01-01-13 er en tirsdag
sun <- numeric(n.obs)
sun[seq(6, n.obs, by = 7)] <- 1

sun.oos <- numeric(n.obs.oos)
sat.oos[seq(6, n.obs.oos, by = 7)] <- 1

# Danske helligdage
hol <- numeric(n.obs)
hol[c(1,    83,   87,   88,   90,   91,   116,  129,  139,  140,  359,  360,  # 2013
      366,  468,  472,  473,  475,  476,  501,  514,  524,  525,  724,  725,  # 2014
      731,  818,  822,  823,  825,  826,  851,  864,  874,  875,  1089, 1090, # 2015
      1096, 1175, 1179, 1180, 1182, 1183, 1208, 1221, 1231, 1232, 1455, 1456, # 2016
      1462, 1560, 1564, 1565, 1567, 1568, 1593, 1606, 1616, 1617, 1820, 1821, # 2017
      1827, 1910, 1914, 1915, 1917, 1918, 1943, 1956, 1966, 1967, 2185, 2186  # 2018
      )] <- 1

hol.oos <- numeric(n.obs.oos)
hol.oos[c(1)] <- 1

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

# Gør out-of-sample vindprognose til daglige forecast
wind.prog2019[2139,3] <- mean(wind.prog2019[2138,3],wind.prog2019[2140,3]) # Fjerner Na
wind.prog2019Daily <- c()
for (j in 0:(length(wind.prog2019[,2])/24 -1)) {
  wind.prog2019Daily[j+1] <- sum(wind.prog2019[((j*24 + 1):((j+1)*24)),3])
}


# Samler hvert år samt alle år sammen for DK1 i en liste
fq <- 1
DK1 <- list(A   = ts(c(dat2013[,8], 
                       dat2014[,8],
                       dat2015[,8],
                       dat2016[,8],
                       dat2017[,8],
                       dat2018[,8]),  frequency = fq),
            W   = ts(c(wind2013[,2],
                       wind2014[,2],
                       wind2015[,2],
                       wind2016[,2],
                       wind2017[,2],
                       wind2018[,2]), frequency = fq),
            oos = ts(dat2019[,8],     frequency = fq),
            sat = sat,
            sun = sun,
            hol = hol,
            Raw = ts(c(dat2013[,8], 
                      dat2014[,8],
                      dat2015[,8],
                      dat2016[,8],
                      dat2017[,8],
                      dat2018[,8]),   frequency = fq))

OOS <- list(A   = ts(dat2019[,8], frequency = fq),
            W   = wind2019[1:90,2],
            WP  = wind.prog2019Daily,
            sat = sat.oos,
            sun = sun.oos,
            hol = hol.oos)

# Fjerner 07-06-13 og erstatter med gennesnit af dagen før og efter. (Spike-dagen)
DK1$A[which.max(DK1$A)] <- mean(c((DK1$A[which.max(DK1$A)-1]), 
                                  (DK1$A[which.max(DK1$A)+1])))

# Fjerner midlertidige variable
rm("dat2013","dat2014","dat2015","dat2016","dat2017","dat2018","dat2019","fq","i","l",
   "path","sat","sun","hol","years","years.names", "wind2013" , "wind2014", 
   "wind2015", "wind2016", "wind2017", "wind2018", "years.names.win" , "years.win", 
   "path.win", "sun.oos", "sat.oos", "hol.oos", "wind2019", "wind2019Daily",
   "years.win.prog", "wind.prog2019", "wind.prog2019Daily", "years.names.win.prog", "j")

### ¤¤ Farver + tema til brug i plots ¤¤ ### -------------------------------------------

# Farver til grafer
colors <- c("royalblue4" ,
            "firebrick4" ,
            "darkorchid4",
            "chartreuse4",
            "black",
            "grey70")

# Grå farve der matcher projektet
myGray <- rgb(245/255, 245/255, 245/255)

# Tema til plots

  # Til år
p.Y <- list(theme(panel.background = element_rect(fill = myGray, colour = myGray,
                                  size = 2, linetype = "solid"),
                  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
                  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white") ), 
                  scale_y_continuous(), 
                  scale_x_date(date_labels = "%Y", breaks = pretty(dates, n = 6)))
  # 
p.th <- list(theme(panel.background = element_rect(fill = myGray, colour = myGray,
                                                  size = 2, linetype = "solid"),
                  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                  colour = "white"), 
                  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                  colour = "white") ))
 

# Størrelse af linjer og punkter i plots
sz <- list(l = I(0.2) , p = I(0.1))

### ¤¤ Gemmer workspace ¤¤ ### ----------------------------------------------------------

save(DK1, p.Y, p.th, colors, dates, dates2, n.obs, ci, scoef, sz, OOS, dates.all,
     file = "./Workspaces/preliminary.Rdata")

### ¤¤ Det vilde vesten ¤¤ ### ----------------------------------------------------------


