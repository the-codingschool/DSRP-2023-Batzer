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

nonadata <- filter(data,!is.na)

#CHECK TO MAKE SURE IF 0 MEANS NO DATA

#filter the data so that theres no 0 values
zerodata <- filter(data,value == 0) #view all of the 0 values 
#the main countries that have 0 values are austria,luxembourg, cyprus and lithuania

#idea: find all of the sectors that have zero values and find the average like what i did for austria
# and then any zero data in the carbon data to the average
#could try using mutate with an ifelse statement

#Austria
austriazerodata <- filter(data,country == "Austria", value == 0) #0s in power and industry

#find the averages of austria power and industry
austriadata <- filter(data,country == "Austria")
austriapower <- filter(austriadata,sector == "Power")
austriaindustry <- filter(austriadata,sector == "Industry")

austriapowermean <- mean(austriapower$value)
austriaindustrymean <- mean(austriaindustry$value)

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




#Lithuania



#Hungary


#ask professor batzer about zero values


francedata <- filter(data,country == "France")
germanydata <- filter(data,country == "Germany")
ukdata <- filter(data,country == "United Kingdom")
eudata <- filter(data,country != "United Kingdom")
spaindata <- filter(data,country == "Spain")
italydata <- filter(data, country == "Italy")

westerneurope <- filter(data,country %in% c("United Kingdom","France","Germany","Belgium","Belgium",
                                            "Netherlands","Luxembourg","Austria"))
northerneurope <- filter(data,country %in% c("Denmark","Sweden","Finland","Estonia","Latvia","Lithuania"))

southerneurope <- filter(data, country %in% c("Spain","Portugal","Italy","Greece","Malta","Cyprus"))

easterneurope <- filter(data,country %in% c("Poland","Czech Republic","Slovakia","Slovenia",
                                            "Hungary","Croatia","Romania","Bulgaria"))

#seperate into various regions and countries so they can be compared amongst each other and also 
#so it will be easier to visualize and perform machine learning on them

unique(data$sector)
francedata
germanydata
mean(germanydata$value)
mean(francedata$value)

head(data)
#need to make this into dataframe first
carbondata <- as.data.frame(data)
head(carbondata)
