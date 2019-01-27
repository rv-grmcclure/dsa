library(tidyverse)

##READ IN AND CLEAN DATA

setwd("/Users/grmcclure/OneDrive - Red Ventures/Documents/Projects/ExData_Plotting1/")
raw_data <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?")

##get only the date range we care about
data <- subset(raw_data, Date %in% c("1/2/2007","2/2/2007"))
##clean the date column
data$Date <- as.Date(data$Date, "%d/%m/%Y")
##add datetime
datetime <- paste(as.Date(data$Date), data$Time)
data$Datetime <- as.POSIXct(datetime)

##PRODUCE GRAPH
with(data, {
  plot(Sub_metering_1~Datetime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~Datetime,col='Red')
  lines(Sub_metering_3~Datetime,col='Blue')
})

legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, 
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

## Save file, close device
dev.copy(png,"plot3.png", width=480, height=480)
dev.off()
