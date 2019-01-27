library(tidyverse)
setwd()
#read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#6) Compare emissions from motor vehicle sources in Baltimore City with emissions 
#from motor vehicle sources in Los Angeles County, California 
#(fips=="06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?

SCCmotor <- SCC[grepl("vehicle", SCC$SCC.Level.Two, ignore.case = T),]
NEI_baltAndLA_motor <- NEI[NEI$SCC %in% SCCmotor$SCC & NEI$fips %in% c("24510","06037"),]
baltAndLA_motor <- NEI_baltAndLA_motor %>% 
  mutate(city = case_when(fips == "24510" ~"Baltimore",fips == "06037" ~ "LA")) %>%
  group_by(year,city) %>% summarize(totalEmissions = sum(Emissions))

ggplot(baltAndLA_motor,aes(factor(year),totalEmissions,fill=city)) + 
  geom_bar(stat="identity", position = "dodge") +
  labs(x="Year", y="Total Emissions") + 
  labs(title="Baltimore vs LA Motor Vehicle Emissions")

#Baltimore is nothing compared to LA in terms of motor vehicle emissions, and it descreases as LA increases

## Save file, close device
dev.copy(png,"number6.png", width=480, height=480)
dev.off()

