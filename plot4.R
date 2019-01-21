library(chron)
library(dplyr)

## Download the file from online location every time
## Alternatively could store in a local directory 
## However downloading from the internet makes it easier for peer reviewers
## to run the code without any adjustments (e.g. specification of local file path)
## in case they wish to verify or reproduce the output of this code

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
temp <- tempfile()  ##temporary file
zip.file <- download.file(url,temp)
data.filename <- "household_power_consumption.txt"

## Create data.frame here
hh.power.cons <- read.table(unz(temp, data.filename), sep=";", header=TRUE, stringsAsFactors=FALSE )
unlink(temp) ##delete temporary file

## Transform the Date column into a proper date
## and the time column into a proper time of day (using the chron package)
hh.power.cons$Date <- as.Date(hh.power.cons$Date, format="%d/%m/%Y")
hh.power.cons$Time <- times(hh.power.cons$Time)

## Transform the remaining columns into numeric, 
## accounting for the "?" character as NA
num.cols <- ncol(hh.power.cons)
hh.power.cons.num <- hh.power.cons[, 3:num.cols]
hh.power.cons.num[hh.power.cons.num == "?"] <- NA
hh.power.cons[, 3:num.cols] <- sapply(hh.power.cons.num, as.numeric)
remove(hh.power.cons.num)

## subset to the desired dates using the filter function in dplyr
## (could have achieved the same result using the subset(.) function but I prefer to use dplyr)
hh.power.cons.on.dates <- filter(hh.power.cons, Date=="2007-02-01" | Date=="2007-02-02")

## Combine Date and Time columns into a single DateTime column using the chron package 
hh.power.cons.on.dates$datetime <- chron(format(hh.power.cons.on.dates$Date, "%Y-%m-%d"), 
                                         as.character(hh.power.cons.on.dates$Time), 
                                         format=c(dates="y-m-d", time="h:m:s"))


## Set up 2x2 subplot grid
par(mfrow=c(2, 2))

## x-axis tickers common to the 4 plots
x.ticks <- seq(hh.power.cons.on.dates$datetime[[1]], tail(hh.power.cons.on.dates$datetime,1)+0.1, by=1)
x.tick.labels <- weekdays(x.ticks)

## First add the figure from plot2
y.title <- "Global Active Power (kilowatts)"
with(hh.power.cons.on.dates, plot(datetime, Global_active_power, type = "l", xaxt="n", xlab="", ylab=y.title))
axis(1, at=x.ticks, labels=x.tick.labels)

## Then the plot of voltage vs. datetime
y.title <- "Voltage"
with(hh.power.cons.on.dates, plot(datetime, Voltage, type = "l", xaxt="n", ylab=y.title))
axis(1, at=x.ticks, labels=x.tick.labels)

## Then the figure from plot 3
y.title <- "Energy sub metering"
with(hh.power.cons.on.dates, { 
    plot(datetime, Sub_metering_1, type="l", xaxt="n", xlab="", ylab=y.title, col="black", lty="solid", lwd=1)
    lines(datetime, Sub_metering_2, col="red", lty="solid", lwd=1)
    lines(datetime, Sub_metering_3, col="blue", lty="solid", lwd=1)
})
axis(1, at=x.ticks, labels=x.tick.labels)
legend("top", col=c("black", "red", "blue"), lty=rep("solid", 3), lwd=rep(1, 3), 
       bty = "n", legend=names(hh.power.cons.on.dates[7:9]), xjust=1)

## Then the plot of global reactive power vs. datetime
with(hh.power.cons.on.dates, plot(datetime, Global_reactive_power, type = "l", xaxt="n", yaxt="n"))
axis(1, at=x.ticks, labels=x.tick.labels)
axis(2, at=seq(0,0.5, by=0.1), labels=seq(0,0.5, by=0.1))

## Finally, save to png file, forcing the width and height to the required pixels although these appear
## to be defaults anyway
dev.copy(png, file="plot4.png", width=480, height=480)
dev.off()