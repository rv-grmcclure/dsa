library(tidyverse)
setwd()
#read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#5) How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

SCCmotor <- SCC[grepl("vehicle", SCC$SCC.Level.Two, ignore.case = T),]
NEI_balt_motor <- NEI[NEI$SCC %in% SCCmotor$SCC & NEI$fips == "24510",]
balt_motor <- NEI_balt_motor %>% group_by(year) %>% summarize(totalEmissions = sum(Emissions))

ggplot(balt_motor,aes(factor(year),totalEmissions)) + 
  geom_bar(stat="identity") +
  labs(x="Year", y="Total Emissions") + 
  labs(title="Baltimore Motor Vehicle Emissions")

#motor vehicle emissions have decreased in Baltimore

## Save file, close device
dev.copy(png,"number5.png", width=480, height=480)
dev.off()

