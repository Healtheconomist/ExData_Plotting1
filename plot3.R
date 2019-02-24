library(data.table)
library(lubridate)
library(dplyr)

## Load dataset using table function defining the variable classes and NAs 
## To peer reveiwer, i am unable to source directly from online source due to restricted user permissions
## and will therefore source directly from HDD instead.

temp <- read.table("household_power_consumption.txt", 
                 header=TRUE, 
                 sep=";", 
                 na.strings = "?", 
                 colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

#Loading data into a working table
t <- temp 

#Transforming the date character variable to the dates class
t$Date <- as.Date(t$Date, "%d/%m/%Y") 

## Filter data set from Feb. 1, 2007 to Feb. 2, 2007 as per instruction from the excercice
t <- subset(t,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

## Removeiing incomplete observations 
t <- t[complete.cases(t),]

## Combine Date and Time column to prepare for a POSIXct containing both time and date 
dateTime <- paste(t$Date, t$Time)

## Name the vector
dateTime <- setNames(dateTime, "DateTime")

## Remove Date and Time column to simplify data handling
t <- t[ ,!(names(t) %in% c("Date","Time"))]

## Add DateTime column to the data table
t <- cbind(dateTime, t)

## Format dateTime Column to POSIXvt class
t$dateTime <- as.POSIXct(dateTime)


## Plot 3
dev.new(width=3,height=9) #Opens a new device to gain more control over the graphical output
plot(t$Sub_metering_1 ~ t$dateTime, type = "l")
with(t, {
        plot(Sub_metering_1 ~ dateTime, type = "l", ylab = "Global Active Power (Kilowatts)", xlab = "")
        lines(Sub_metering_2~dateTime,col="red")
        lines(Sub_metering_3~ dateTime, col = "blue")
        })
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

## saving the graph to a png file and closing device
dev.copy(png,"plot3.png", width=480, height=480)
dev.off()


