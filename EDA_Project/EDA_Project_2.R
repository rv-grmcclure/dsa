library(tidyverse)
setwd()
#read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#2) Have total emissions from PM2.5 decreased in the Baltimore City, 
#Maryland (fips == "24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.
baltimoreGroupedNEI <- NEI[NEI$fips=="24510",] %>% group_by(year) %>% summarize(totalEmissions = sum(Emissions))
barplot(baltimoreGroupedNEI$totalEmissions, names = baltimoreGroupedNEI$year,xlab = "Year", ylab = "Total Emissions",main = "Total Emissions by Year")

#overall have decreased, but not every year like they did overall

## Save file, close device
dev.copy(png,"number2.png", width=480, height=480)
dev.off()
