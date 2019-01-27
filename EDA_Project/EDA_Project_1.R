library(tidyverse)
setwd()
#read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#1) Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission from 
#all sources for each of the years 1999, 2002, 2005, and 2008.
groupedNEI <- NEI %>% group_by(year) %>% summarize(totalEmissions = sum(Emissions))

barplot(groupedNEI$totalEmissions, names = groupedNEI$year,xlab = "Year", 
        ylab = "Total Emissions",main = "Total Emissions by Year")

#yes, they have decreased

## Save file, close device
dev.copy(png,"number1.png", width=480, height=480)
dev.off()