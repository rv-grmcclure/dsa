library(tidyverse)
setwd()
#read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#3) Of the four types of sources indicated by 
#type(point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

baltimoreGroupedNEI <- NEI[NEI$fips=="24510",] %>% group_by(year,type) %>% 
  summarize(totalEmissions = sum(Emissions))

ggplot(baltimoreGroupedNEI,aes(factor(year),totalEmissions,fill=type)) + 
  geom_bar(stat="identity", position = "dodge") +
  labs(x="Year", y="Total Emissions") + 
  labs(title="Emissions in Baltimore by year and by type")

#non road, non point, and on-road all decreased while point increased
  
## Save file, close device
dev.copy(png,"number3.png", width=480, height=480)
dev.off()
