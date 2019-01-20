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

## Create the histogram plot with the required customizations
title <- "Global Active Power"
x.title <- "Global Active Power (kilowatts)"
with(hh.power.cons.on.dates, hist(Global_active_power, col="red", main=title, xlab=x.title))

## Finally, save to png file, forcing the width and height to the required pixels although these appear
## to be defaults anyway
dev.copy(png, file="plot1.png", width=480, height=480)
dev.off()
