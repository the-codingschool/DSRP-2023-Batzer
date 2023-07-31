data <- read.csv("data/carbonmonitor-eu_datas_2023-07-19.csv")
summary.data.frame(data)

library(tidyr)
library(parsnip)
library(rsample)
library(yardstick)
library(dplyr)
library(rsample)
library(xgboost)
library(ranger)
library(Metrics)
library(reshape2)

#need to make this into dataframe first
carbondata <- as.data.frame(data)
head(carbondata)

#CHECK TO MAKE SURE IF 0 MEANS NO DATA

#filter the data so that theres no 0 values
zerodata <- filter(data,value == 0) #view all of the 0 values 
#the main countries that have 0 values are austria,luxembourg, cyprus and lithuania

#idea: find all of the sectors that have zero values and find the average like what i did for austria
# and then any zero data in the carbon data to the average
#could try using mutate with an ifelse statement

#Austria
austriadata <- filter(carbondata,country == "Austria")
austriazerodata <- filter(carbondata,country == "Austria", value == 0) #0s in power and industry
 
#find the averages of austria power and industry
austriadata <- filter(data,country == "Austria")
austriapower <- filter(austriadata,sector == "Power")
austriaindustry <- filter(austriadata,sector == "Industry")

austriapowermean <- mean(austriapower$value)
austriaindustrymean <- mean(austriaindustry$value)


austriazerodata <- filter(austriadata,value == 0)

carbondata$value[carbondata$value == 0  & carbondata$country == "Austria" & carbondata$sector== "Power"]<- austriapowermean
carbondata$value[carbondata$value == 0  &  carbondata$country == "Austria" & carbondata$sector== "Industry"]<-austriaindustrymean

#Latvia
#power and industry

latviapower <- filter(carbondata,country == "Latvia",sector == "Power")
latviaindustry <- filter(carbondata,country=="Latvia", sector == "Industry")

latviapowermean <- mean(latviapower$value)
latviaindustrymean <- mean(latviaindustry$value)

carbondata$value[carbondata$value == 0 & carbondata$country == "Latvia" & carbondata$sector == "Power"] <- latviapowermean
carbondata$value[carbondata$value == 0 & carbondata$country == "Latvia" & carbondata$sector == "Industry"] <- latviaindustrymean

#Luxembourg
luxzerodata <- filter(data,country == "Luxembourg", value == 0)
# luxembourg seems to have all 0 values in domestic aviation
#FIND OUT WHEN LUXEMBOURG STOPed having lockdown and only change those after that

luxdomesticaviation <- filter(data,country == "Luxembourg",sector == "Domestic Aviation")
luxdomesticaviationmean <- mean(luxdomesticaviation$value)


#Cyprus
cyrpuszerodata <- filter(data,country == "Cyprus", value == 0)
#all zeros in domestic aviation like luxembourg
#most in 2019 though so not caused by lockdown

cyprusdomesticaviation <- filter(carbondata,country == "Cyprus",sector == "Domestic Aviation")
cyprusdomesticaviationmean <- mean(cyprusdomesticaviation$value)

carbondata$value[carbondata$value== 0 & carbondata$country == "Cyprus" & carbondata$sector == "Domestic Aviation"] <- cyprusdomesticaviationmean

#Lithuania
lithuaniazerodata <- filter(data,country == "Lithuania",value==0)
#all zeros in domestic aviation
lithuaniadomesticaviation <- filter(carbondata,country == "Lithuania",sector == "Domestic Aviation")
lithuaniaaviationmean <- mean(lithuaniadomesticaviation$value)

carbondata$value[carbondata$value == 0 & carbondata$country == "Lithuania" & carbondata$sector == "Domestic Aviation"] <- lithuaniaaviationmean

#Hungary
hungaryzerodata <- filter(data,country == "Hungary", value == 0)
#all zeros in domestic aviation for 2019

hungarydomesticaviation <- filter(carbondata,country=="Hungary",sector == "Domestic Aviation")
hungarydomesticaviationmean <- mean(hungarydomesticaviation$value)

carbondata$value[carbondata$value == 0 & carbondata$country == "Hungary" & carbondata$sector == "Domestic Aviation"] <- hungarydomesticaviationmean

#ask professor batzer about zero values
# he said that for luxembourg it could be 0 values
# but otherwise treat 0s as missing values

#can do stuff like anova testing and chi testing to try to find trends
# among regions or countries
#and then can do background research to see why these trends are happening

zeeerodata <- filter(carbondata,value==0)

populationdata <- worldbank_population_bycountry_data

populationordered <- arrange(populationdata,year,population)
populationordered

population2022 <- filter(populationdata,year == "2022")
population2022
# some of the values are wrong so im going to correct them using the world bank website
population2022$population[population2022$country == "Croatia"] <- 3854000
population2022$population[population2022$country == "Cyprus"] <- 1251488
population2022$population[population2022$country == "Czech Republic"] <- 10526073
population2022$population[population2022$country == "Denmark"] <- 5903037
population2022$population[population2022$country == "Greece"] <- 10566531
population2022$population[population2022$country == "Hungary"] <- 9683505
population2022$population[population2022$country == "Ireland"] <- 5086988
population2022$population[population2022$country == "Italy"] <- 58856847
population2022$population[population2022$country == "Latvia"] <- 1883379
population2022$population[population2022$country == "Lithuania"] <- 2833000
population2022$population[population2022$country == "Luxembourg"] <- 650774
population2022$population[population2022$country == "Malta"] <- 523417
population2022$population[population2022$country == "Netherlands"] <- 17703090
population2022$population[population2022$country == "Poland"] <- 37561599
population2022$population[population2022$country == "Portugal"] <- 10379007
population2022$population[population2022$country == "Romania"] <- 18956666
population2022$population[population2022$country == "Slovakia"] <- 5431752
population2022$population[population2022$country == "Slovenia"] <- 2108732
population2022$population[population2022$country == "Spain"] <- 47615034
population2022$population[population2022$country == "Sweden"] <- 10486941
population2022$population[population2022$country == "United Kingdom"] <- 66971411
population2022$population[population2022$country == "EU27 & UK"] <- 447956050 + 66971411
populationalphabetical <- arrange(population2022,country)

# testing
?pivot_longer
?pivot_wider
#Spreading the sectors out 
sectordata = pivot_wider(carbondata, 
                          names_from = sector, 
                          values_from = value)

pivottest = pivot_longer(sectordata,cols = c("Power", "Industry", "Ground Transport", "Residential",
                         "Domestic Aviation", "International Aviation"), names_to = "Sector",values_to = "count")

# idea: make a summary dataset for total values, make a summary dataset for power values
# divide the average power value column by the total column, mutate it into a new column and then order that
test <- group_by(sectordata,country)                     
