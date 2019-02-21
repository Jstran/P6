library(stats)
library(lubridate)
library(ggplot2)
# DK1 : Jylland og Fyn, DK2: Sjælland , DK : Det hele 

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

# Laver datoer.
datesY <- seq(ymd("2013-01-01"), ymd("2018-12-31"), by="days")
dates<-format(datesY, format="%d-%m")[1:365]

# Samler hvert aar for DK2 i en data frame og liste.
DK2f <- data.frame(numeric(365))
row.names(DK2f) <- dates
l <- 1
for (i in 2013:2018) {
  DK2f[,l] <- get(paste("dat", i, sep=""))[,9]
  names(DK2f)[l] <- i
  l <- l + 1
}

fq <- 7
DK2 <- list(Y13 = ts(DK2f[,1], frequency = fq),
            Y14 = ts(DK2f[,2], frequency = fq), 
            Y15 = ts(DK2f[,3], frequency = fq), 
            Y16 = ts(DK2f[,4], frequency = fq), 
            Y17 = ts(DK2f[,5], frequency = fq), 
            Y18 = ts(DK2f[,6], frequency = fq))

# Data frama der indeholder alle år for DK2
dfDK2 <- data.frame(numeric(2191))
row.names(dfDK2) <- datesY
dfDK2 <- c(DK2f[,1], 
           DK2f[,2],
           DK2f[,3],
           dat2016LY[,9],
           DK2f[,5],
           DK2f[,6])



# Samler hvert aar for DK1 i en data frame og liste.
DK1f <- data.frame(numeric(365))
row.names(DK1f) <- dates
l <- 1
for (i in 2013:2018) {
  DK1f[,l] <- get(paste("dat", i, sep=""))[,8]
  names(DK1f)[l] <- i
  l <- l + 1
}

fq <- 7
DK1 <- list(Y13 = ts(DK1f[,1], frequency = fq),
            Y14 = ts(DK1f[,2], frequency = fq), 
            Y15 = ts(DK1f[,3], frequency = fq), 
            Y16 = ts(DK1f[,4], frequency = fq), 
            Y17 = ts(DK1f[,5], frequency = fq), 
            Y18 = ts(DK1f[,6], frequency = fq))

# Data frama der indeholder alle år for DK1
dfDK1 <- data.frame(numeric(2191))
row.names(dfDK1) <- datesY
dfDK1 <- c(DK1f[,1], 
           DK1f[,2],
           DK1f[,3],
           dat2016LY[,8],
           DK1f[,5],
           DK1f[,6])


# Mean, sd og andre gode sager
apply(DK2f, 2, mean)
apply(DK1f, 2, mean)

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

# ggplot
p <- ggplot(data.frame(X1 = time(DK2$Y14) , 
                       X2 = DK2$Y14) , 
                       aes(x = X1 , y = X2))+
                       geom_line()+
                       geom_smooth(); p
