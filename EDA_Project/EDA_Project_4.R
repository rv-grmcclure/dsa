library(tidyverse)
setwd()
#read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#4) Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

SCCcoal <- SCC[grepl("coal", SCC$Short.Name, ignore.case = T),]
NEIcoal <- NEI[NEI$SCC %in% SCCcoal$SCC,]
coal <- NEIcoal %>% group_by(year) %>% summarize(totalEmissions = sum(Emissions))
  
ggplot(coal,aes(factor(year),totalEmissions)) + 
  geom_bar(stat="identity") +
  labs(x="Year", y="Total Emissions") + 
  labs(title="Coal Emissions by year")

#Coal emissions have decreased significantly between 1999 and 2008

## Save file, close device
dev.copy(png,"number4.png", width=480, height=480)
dev.off()