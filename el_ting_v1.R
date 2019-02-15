library(stats)
library(lubridate)

setwd("~/Desktop/P6/P6/Data")

# Indl??ser csv filerne som "dat20xx".
path <- file.path("~/Desktop/P6/Data")
years <- list.files(path,pattern =".csv")
years_names <- seq(2014, length.out = length(years))

for (l in 1:length(years_names)) {
  assign(paste("dat", years_names[l], sep = ""), read.csv2(years[l], skip = 2))
}

# Fjerner den skud??r i 2016.
dat2016 <- dat2016[-60,] 

# Laver datoer.
dates_temp <- seq(ymd("1000-01-01"), ymd("1000-12-31"), by="days")
dates<-format(dates_temp, format="%d-%m")

# Samler hvert ??r for DK2 i ??n data frame.
DK2 <- data.frame(numeric(365))
row.names(DK2) <- dates
l <- 1
for (i in 2014:2018) {
  DK2[,l] <- get(paste("dat", i, sep=""))[,9]
  names(DK2)[l] <- i
  l <- l + 1
}

# Nogle plots.
plot(DK2[,1], type = "l")
lines(DK2[,3], col = "red")

acf(DK2[,4])
acf(DK2[,3])

pacf(DK2[,4])
pacf(DK2[,3])


# Laver DK2ts som er en tids serie.
DK2ts <- ts(DK2, frequency = 7)

plot(decompose(DK2ts[,3]))


# Scatetr plot med lag.
n <- nrow(DK2)
plot(DK2[1:(n-1),4], DK2[2:n,4], main = "2017", xlab = "x_t", ylab = "x_{t-1}")

plot(DK2ts[,2], lag(DK2ts[,2],2),main = "2015", xlab = "x_t", ylab = "x_{t-1}")



