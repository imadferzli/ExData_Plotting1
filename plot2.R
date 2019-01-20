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
hh.power.cons.on.dates$DateTime <- chron(format(hh.power.cons.on.dates$Date, "%Y-%m-%d"), 
                                         as.character(hh.power.cons.on.dates$Time), 
                                         format=c(dates="y-m-d", time="h:m:s"))

## Create the line plot with the required customizations
## First create the plot with no x-axis ticks or labels
y.title <- "Global Active Power (kilowatts)"
with(hh.power.cons.on.dates, plot(DateTime, Global_active_power, type = "l", xaxt="n", xlab="", ylab=y.title))
## Then create the series of ticks 1 day apart from one another
## adding 0.1 at the end to display the beginning of a third day (Saturday)
x.ticks <- seq(hh.power.cons.on.dates$DateTime[[1]], tail(hh.power.cons.on.dates$DateTime,1)+0.1, by=1)
## Labels will be the weekdays corresponding to these ticks
x.tick.labels <- weekdays(x.ticks)
axis(1, at=x.ticks, labels=x.tick.labels)

## Finally, save to png file, forcing the width and height to the required pixels although these appear
## to be defaults anyway
dev.copy(png, file="plot2.png", width=480, height=480)
dev.off()
