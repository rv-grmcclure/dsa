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
plot(data$Global_active_power~data$Datetime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

## Save file, close device
dev.copy(png,"plot2.png", width=480, height=480)
dev.off()
