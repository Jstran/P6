library(stats)
library(lubridate)
library(ggplot2)
# DK1 : Jylland og Fyn, DK2: Sjælland , DK : Det hele 

### Indlæsning af data --------------------
# Indlaeser csv filerne som "dat20xx".
path <- file.path("./Data")
years <- list.files(path,pattern =".csv",full.names = 1)
years_names <- seq(2013, length.out = length(years))

for (l in 1:length(years_names)) {
  assign(paste("dat", years_names[l], sep = ""), read.csv2(years[l], skip = 2))
}

# Fjerner skudaar i 2016.
dat2016LY <- dat2016
dat2016 <- dat2016[-60,] 


### Data frame og lister med data --------------------
# Laver datoer.
datesY <- seq(ymd("2013-01-01"), ymd("2018-12-31"), by="days")
dates<-format(datesY, format="%d-%m")[1:365]

# Samler hvert aar for DK2 i en data frame.
DK2f <- data.frame(numeric(365))
row.names(DK2f) <- dates
l <- 1
for (i in 2013:2018) {
  DK2f[,l] <- get(paste("dat", i, sep=""))[,9]
  names(DK2f)[l] <- i
  l <- l + 1
}

# Samler alle aar for DK2 i samme data frama.
dfDK2 <- data.frame(DK2 = numeric(2191), sat = numeric(2191), sun = numeric(2191))
row.names(dfDK2) <- datesY
dfDK2[,1] <- c(DK2f[,1], 
           DK2f[,2],
           DK2f[,3],
           dat2016LY[,9],
           DK2f[,5],
           DK2f[,6])

# Lørdag dummy, 01-01-13 er en tirsdag
sat <- numeric(nrow(dfDK2))
sat[seq(5, 2191, by = 7)] <- 1
dfDK2[,2] <- sat

# Søndag dummy, 01-01-13 er en tirsdag
sun <- numeric(nrow(dfDK2))
sun[seq(6, 2191, by = 7)] <- 1
dfDK2[,3] <- sun

# Samler hvert aar samt alle år sammen for DK1 i en liste
fq <- 1
DK2 <- list(Y13 = ts(DK2f[,1], frequency = fq),
            Y14 = ts(DK2f[,2], frequency = fq), 
            Y15 = ts(DK2f[,3], frequency = fq), 
            Y16 = ts(DK2f[,4], frequency = fq), 
            Y17 = ts(DK2f[,5], frequency = fq), 
            Y18 = ts(DK2f[,6], frequency = fq),
            YAll = ts(dfDK2[,1],   frequency = fq),
            sat = sat,
            sun = sun)


# Samler hvert aar for DK1 i en data frame.
DK1f <- data.frame(DK1 = numeric(365))
row.names(DK1f) <- dates
l <- 1
for (i in 2013:2018) {
  DK1f[,l] <- get(paste("dat", i, sep=""))[,8]
  names(DK1f)[l] <- i
  l <- l + 1
}

# Data frama der indeholder alle år for DK1
dfDK1 <- data.frame(DK1 = numeric(2191), sat = numeric(2191), sun = numeric(2191))
row.names(dfDK1) <- datesY
dfDK1[,1] <- c(DK1f[,1], 
           DK1f[,2],
           DK1f[,3],
           dat2016LY[,8],
           DK1f[,5],
           DK1f[,6])

# Lørdag dummy, 01-01-13 er en tirsdag
sat <- numeric(nrow(dfDK1))
sat[seq(5, 2191, by = 7)] <- 1
dfDK1[,2] <- sat

# Søndag dummy, 01-01-13 er en tirsdag
sun <- numeric(nrow(dfDK1))
sun[seq(6, 2191, by = 7)] <- 1
dfDK1[,3] <- sun

# Samler hvert aar samt alle år sammen for DK1 i en liste
fq <- 1
DK1 <- list(Y13 = ts(DK1f[,1], frequency = fq),
            Y14 = ts(DK1f[,2], frequency = fq), 
            Y15 = ts(DK1f[,3], frequency = fq), 
            Y16 = ts(DK1f[,4], frequency = fq), 
            Y17 = ts(DK1f[,5], frequency = fq), 
            Y18 = ts(DK1f[,6], frequency = fq),
            YAll = ts(dfDK1[,1],   frequency = fq),
            sat = sat,
            sun = sun)

### Leg med data --------------------

# Mean, sd og andre gode sager
apply(DK2, 2, mean)
apply(DK1, 2, mean)

apply(DK2f, 2, sd)

# Nogle plots.
plot(DK2$Y14)
lines(DK1$Y14, col = "red")

par(mfrow = c(2,2))
acf(DK2$Y16)
acf(DK2$Y17)

pacf(DK2$Y16)
pacf(DK2$Y16)

plot(decompose(DK2$Y14))


# Scatter plot med lag.
par(mfrow = c(2,1))
plot(DK2$Y14, lag(DK2$Y14,2),main = "2014", xlab = "x_t", ylab = "x_{t-1}")
plot(DK2$Y15, lag(DK2$Y15,2),main = "2015", xlab = "x_t", ylab = "x_{t-1}")


diffDK2Y14 <- diff(DK2$Y14, differences = 1)

plot(diffDK2Y14 ,panel.first = grid(col = "white",lty = 1))
plot(DK2$Y14)
par(mfrow = c(1,1))



### ggplot --------------------
colors <- c("royalblue4" ,
            "firebrick4" ,
            "darkorchid4",
            "chartreuse4",
            "black")

pY14 <- ggplot(data.frame(X1 = time(DK2$Y14) , 
                       X2 = DK2$Y14) , 
            aes(x = X1 , y = X2))+
  geom_line()+
  geom_smooth()
pY14



pDK1vDK2Y14 <-  ggplot(data.frame(X1 = time(DK2$Y14), 
                         X2 = DK2$Y14), 
              aes(x = X1 , y = X2))+
  geom_line(aes(col = "DK2Y14")) +
  geom_line(data = data.frame(X1 = time(DK1$Y14), 
                              X2 = DK1$Y14), 
            aes(col = "DK1Y14")) +
  labs(x = "Tid", y = "Pris i DKK", title = "DK1 vs. DK2 Y2014", color = "")+
  scale_color_manual(values = colors[1:2])

pDK1vDK2Y14

pDK1vDK2Yall <-  ggplot(data.frame(X1 = datesY, 
                                   X2 = DK2$YAll), 
                        aes(x = X1 , y = X2)) +
  geom_line(aes(col = "DK2")) +
  geom_line(data = data.frame(X1 = datesY, 
                              X2 = DK1$YAll), 
            aes(col = "DK1"))+
  labs(x = "Tid", y = "Spotpris i DKK", title = "DK1 vs. DK2 2013-2018: Spotpriser", color = "") +
  scale_color_manual(values = colors[1:2]) + 
  ylim(-100,600)

pDK1vDK2Yall

### Regression --------------------
t <- time(DK2$YAll)

# DK1 regression på Escribano model  
modEscDK1 <- nls(dfDK1[,1] ~ B0 + BT * t + C1 * sin((C2 + t) * 2*pi/365.25) + C3 * sin((C4 + t) * 4*pi/365.25) 
                 + D1 * DK1$sat + D2 * DK1$sun,
                 start = c(B0 = 1, BT = 1, C1 = 1, C2 = 1, C3 = 1, C4 = 1, D1 = 1, D2 = 1))

modEscCoefDK1 <- coefficients(modEscDK1)

EscModDK1 <- invisible(modEscCoefDK1[1] + modEscCoefDK1[2] * t + modEscCoefDK1[3] * sin((modEscCoefDK1[4] + t) * 2*pi/365.25) 
                     + modEscCoefDK1[5] * sin((modEscCoefDK1[6] + t) * 4*pi/365.25) + modEscCoefDK1[7] * DK2$sat + modEscCoefDK1[8] * DK2$sun)

DK1[[length(DK1)+1]] <- c(EscModDK1)
names(DK1)[[length(DK1)]] <- "EscMod"

DK1[[length(DK1)+1]] <- c(DK1$YAll - EscModDK1)
names(DK1)[[length(DK1)]] <- "Decomposed"


# DK2 regression på Escribano model  
modEscDK2 <- nls(dfDK2[,1] ~ B0 + BT * t + C1 * sin((C2 + t) * 2*pi/365.25) + C3 * sin((C4 + t) * 4*pi/365.25) 
                          + D1 * DK2$sat + D2 * DK2$sun,
             start = c(B0 = 1, BT = 1, C1 = 1, C2 = 1, C3 = 1, C4 = 1, D1 = 1, D2 = 1))

modEscCoefDK2 <- coefficients(modEscDK2)

EscModDK2 <- invisible(modEscCoefDK2[1] + modEscCoefDK2[2] * t + modEscCoefDK2[3] * sin((modEscCoefDK2[4] + t) * 2*pi/365.25) 
           + modEscCoefDK2[5] * sin((modEscCoefDK2[6] + t) * 4*pi/365.25) + modEscCoefDK2[7] * DK2$sat + modEscCoefDK2[8] * DK2$sun)

DK2[[length(DK2)+1]] <- c(EscModDK2)
names(DK2)[[length(DK2)]] <- "EscMod"


# Plots
pEscModSDK1 <- ggplot(data.frame(X1 = datesY, 
                              X2 = DK1$EscMod), 
                   aes(x = X1 , y = X2)) +
  geom_point(colour = colors[1], size = 0.7) +
  labs(x = "Tid", y = "DKK", title = "Escribano model DK1", color = "")
pEscModSDK1

pObsVEscDK1 <-  ggplot(data.frame(X1 = datesY, 
                                         X2 = DK1$EscMod), 
                              aes(x = X1 , y = X2)) +
  geom_point(aes(col = "Escribano model")) +
  geom_line(data = data.frame(X1 = datesY, 
                              X2 = DK1$YAll), 
            aes(col = "Raw"))+
  labs(x = "Tid", y = "DKK", title = "DK1: Observationer vs. Escribane", color = "") +
  scale_color_manual(values = colors[1:2]) +
  ylim(-200,600)

pObsVEscDK1


pRawVDecDK1 <-  ggplot(data.frame(X1 = datesY, 
                                  X2 = DK1$Decomposed), 
                       aes(x = X1 , y = X2)) +
  geom_line(aes(col = "Decomposed")) +
  geom_line(data = data.frame(X1 = datesY, 
                              X2 = DK1$YAll), 
            aes(col = "Raw"))+
  labs(x = "Tid", y = "DKK", title = "DK1: Rå vs. Decomposed", color = "") +
  scale_color_manual(values = colors[1:2]) +
  ylim(-200,600)

pRawVDecDK1
