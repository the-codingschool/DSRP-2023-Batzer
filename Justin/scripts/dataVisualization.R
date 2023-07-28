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
library(ggplot2)

ggplot(austriadata,aes(x = sector,color=value))+
  labs(title= "Emission by sector")+
  geom_histogram(stat="count",binwidth = 15)



summarize(carbondata,mean = mean(value,na.rm = T),
          count = n(),
          .by =c(country,sector))

carbonsummary <- summarize(carbondata,mean = mean(value),
          .by = c(country,sector))
meanvalues <- arrange(carbonsummary,mean)
#Things I noticed:
# domestic aviation is by far the lowest emitting sector. even domestic aviation for every country is below the means for the other sectors
# this could be because europe has many small countries and that public transportation is common
#in terms of individual countries germany has the most emissions by far, especially in power
#poland has a large amount of power emissions compared to its other sectors though
#while germany has high power emissions, the next 2 most populous countries in this dataset (france and the uk) have drastically lower power emissions
# there must be some difference between the energy policies of these countries
sectorsummary <- summarize(carbondata,mean = mean(value),
                           .by = sector)
sectorsummary
#when looking at the sectors overall, power and ground transport are the highest emitting, followed by industry and residential
#both domestic and international aviation have barely any emissions in comparison

ggplot(germanydata,aes(x=sector,y=value))+
  geom_boxplot()

ggplot(francedata,aes(x=sector,y=value))+
  geom_boxplot()

ggplot(westerneurope,aes(x=country,y=value))+
  geom_jitter()
#germany highest emitter by far, luxembourg lowest emitter by far
# scale goes up by 0.3s up to 1.2

ggplot(easterneurope,aes(x=country,y=value))+
  geom_jitter()
# poland highest emitter by car, croatia and slovenia lowest emitters
#scale goes up by 0.1s up to 0.5

ggplot(northerneurope,aes(x=country,y=value))+
  geom_jitter()
#finland is the highest emitter but its more even then the other two, the scale is smaller though
# scale goes up by 0.025s, up to 0.1

ggplot(southerneurope,aes(x=country,y=value))+
  geom_jitter()
#italy is the highest emitter followed by spain
#scale goes up by 0.1 up to 0.45

#western europe has the most emissions by a long shot while northern europe has the least emissions 


ggplot(westerneurope,aes(x=timestamp,y=value))+
  geom_point()

ggplot(easterneurope,aes(x=timestamp,y=value))+
  geom_point()

ggplot(southerneurope,aes(x=timestamp,y=value))+
  geom_point()

ggplot(northerneurope,aes(x=timestamp,y=value))+
  geom_point()

#testing some bar plots

ggplot(westerneurope,aes(x=sector,y=value))+
  geom_bar(stat = "summary")
#apart from the aviations the other 4 sectors here seem to be relatively even

ggplot(easterneurope,aes(x=sector,y=value))+
  geom_bar(stat = "summary")
# power is by far the highest emitting sector with ground transport industry and residential all around the same

ggplot(southerneurope,aes(x=sector,y=value))+
  geom_bar(stat = "summary")
# ground transport is the highest emitting followed by power, industry and residential

ggplot(northerneurope,aes(x=sector,y=value))+
  geom_bar(stat = "summary")
#ground transport is highest followed by power and industry. residential here though is lower and almost the same as
#international aviation

#things i've noticed:
#1. power is the most dominant emitter in eastern europe, could be due to more fossil fuel usage? 
#2. northern europe has proportionatly the least residential emissions vs the other regions even though its very cold
# and needs heating
#3. for western europe other than aviation the other 4 sectors seem to be pretty even

#testing individual countries

ggplot(francedata,aes(x=sector,y=value))+
  geom_bar(stat = "summary")
# france has a lot of emissions in ground transport
#not as much in industry and power though

ggplot(germanydata,aes(x=sector,y=value))+
  geom_bar(stat = "summary")
# highest emitting sector by far is power

ggplot(spaindata,aes(x=sector,y=value))+
  geom_bar(stat = "summary")
# most emissiosn in ground transport then industry then power then residential

ggplot(ukdata,aes(x=sector,y=value))+
  geom_bar(stat = "summary")
# highest emitting sector by a lot is ground transport

ggplot(polanddata,aes(x=sector,y=value))+
  geom_bar(stat = "summary")
# power is highest emitting sector by a long shot

ggplot(spaindata,aes(x=sector,y=value))+
  geom_bar(stat = "summary")
# ground transport and industry are the highest

ggplot(italydata,aes(x=sector,y=value))+
  geom_bar(stat = "summary")
# both ground transport and power are very high

# testing using violin plots
ggplot(westerneurope,aes(x=sector,y=value))+
  geom_violin()

#notes form what professor batzer said
# can form datasets of countries who rely more on fossil fuels
# vs ones who dont
# like france and uk vs germany and nordic countries
# try to do statistical tests to see the relationship
# could try tos how like "oh if you rely more on fossil fuels
# your emmisions will look more like this"

powerdata <- filter(carbondata,sector == "Power")
powersummary <- summarize(powerdata,mean = mean(value),
                           .by = country)
poweralphabetical <- arrange(powersummary,country)
powervalues <- arrange(powersummary,desc(mean))

#idea: use the population data and then mutate this for power per capita
# so its adjusted per person
powermutated <- mutate(poweralphabetical,emissionsperperson = mean/populationalphabetical$population)
powermutated <- arrange(powermutated,desc(emissionsperperson))
# highest emissions per person at the top
# countries like estonia, czech republic and poland at the top
# meanwhile france, lithuania, sweden and luxembourg at bottom
# france is at the very bottom despite having a large population

# run git pull origin main before merging