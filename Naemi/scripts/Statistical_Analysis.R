## loading in libraries ####
#install.packages("gghighlight")
library(gghighlight)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(Metrics)

## reading data ####
data <- read.csv("Data/carbonmonitor-eu_datas_2023-07-19.csv")
pop <- read.csv("Data/worldbank_population_bycountry - data.csv")
View(pop)
View(data)
?setwd()

## Wrangling data ####

#year column
data <- data %>%
  mutate(year = case_when(
    endsWith(date, "19") ~ 2019,
    endsWith(date, "20") ~ 2020,
    endsWith(date, "21") ~ 2021,
    endsWith(date, "22") ~ 2022,
    endsWith(date, "23") ~ 2023))

View(data)

#month column
data <- separate(data,
         date,
         into = c("date", "month"),
         sep = "/") # what is seperated
View(data)

# replacing zeroes with mean 
data$value <- replace(data$value, data$value == 0, mean(data$value))
?replace()

# merge #

data <- merge(data, pop)
View(data)

#separate countries by strict and not strict policies ####

#29 countries https://www.bbc.com/news/world-52103747 <- a chart of asia and europe's lockdown severity
# politico.eu/article/europes-coronavirus-lockdown-measures-compared/ <-  more detailed
#EU27 are just the countries in european union (ignore)

#national lockdown: France, Germany, UK, Russia, Italy, Spain, Belgium, Switzerland, Croatia, Austria
#                   Romania, Greece, Netherlands, Estonia, Denmark, Lithuania, Monaco, Luxembourg, Ireland, 
#                   Czech Republic, Portugal, Poland, Slovenia, Slovakia, Serbia, Bulgaria, Cyprus

#Less strict lockdown: Finland, Sweden, Norway, Iceland, Latvia, Ukraine, Hungary, Bosnia, Malta 

## adding another column for lockdown severity ####
data <- data %>%
  mutate(lock_severe = case_when(
    startsWith(country, "France") ~ "national lockdown",
    startsWith(country, "Germany") ~ "national lockdown",
    startsWith(country, "United Kingdom") ~ "national lockdown",
    startsWith(country, "Russia") ~ "national lockdown",
    startsWith(country, "Italy") ~ "national lockdown",
    startsWith(country, "Spain") ~ "national lockdown",
    startsWith(country, "Belgium") ~ "national lockdown",
    startsWith(country, "Switzerland") ~ "national lockdown",
    startsWith(country, "Croatia") ~ "national lockdown",
    startsWith(country, "Austria") ~ "national lockdown",
    startsWith(country, "Romania") ~ "national lockdown",
    startsWith(country, "Greece") ~ "national lockdown",
    startsWith(country, "Netherlands") ~ "national lockdown",
    startsWith(country, "Estonia") ~ "national lockdown",
    startsWith(country, "Denmark") ~ "national lockdown",
    startsWith(country, "Lithuania") ~ "national lockdown",
    startsWith(country, "Monaco") ~ "national lockdown",
    startsWith(country, "Luxembourg") ~ "national lockdown",
    startsWith(country, "Ireland") ~ "national lockdown",
    startsWith(country, "Czech Republic") ~ "national lockdown",
    startsWith(country, "Portugal") ~ "national lockdown",
    startsWith(country, "Poland") ~ "national lockdown",
    startsWith(country, "Slovenia") ~ "national lockdown",
    startsWith(country, "Slovakia") ~ "national lockdown",
    startsWith(country, "Serbia") ~ "national lockdown",
    startsWith(country, "Bulgaria") ~ "national lockdown",
    startsWith(country, "Cyprus") ~ "national lockdown",
    startsWith(country, "Finland") ~ "Localised and below",
    startsWith(country, "Sweden") ~ "Localised and below",
    startsWith(country, "Norway") ~ "Localised and below",
    startsWith(country, "Iceland") ~ "Localised and below",
    startsWith(country, "Latvia") ~ "Localised and below",
    startsWith(country, "Ukraine") ~ "Localised and below",
    startsWith(country, "Hungary") ~ "Localised and below",
    startsWith(country, "Bosnia") ~ "Localised and below",
    startsWith(country, "Malta") ~ "Localised and below"
    ))
View(data)

## More plotting using highlight and facet wrap ####
# 2020
twenty <- filter(data, year == "2020")
ggplot(data = twenty, aes(x = month, y = value)) +
  geom_point(color = "red") +
  gghighlight(sector == "Power")


ggplot(data = twenty, aes(x = month, y = value, color = sector)) +
  geom_line() +
  facet_wrap(~sector) +
  theme_minimal() +
  ggtitle("Month vs Value in 2020")

ggplot(data = twenty, aes(x = month, y = value, color = sector)) +
  geom_point() +
  facet_wrap(~sector) +
  theme_minimal() +
  ggtitle("Month vs Value in 2020")

# 2019
nineteen <- filter(data, year == "2019")
nineteen

ggplot(data = nineteen, aes(x = month, y = value, color = sector)) +
  geom_line() +
  facet_wrap(~sector) +
  theme_minimal() +
  ggtitle("Month vs Value in 2019")

## Hypothesis testing ####
# null hypothesis: countries with strict lockdown have the same emission mean than those with lighter ones
# alternate hypothesis: countries with strict lockdown have a lower emission mean than those with lighter ones

# t test#
strict <- data %>% filter(lock_severe == "national lockdown") 
strict

light <- data %>% filter(lock_severe == "Localised and below")
light

?t.test()
t.test(strict$value, light$value, paired = F) #p-val 2.2e-16 < 0.5 significantly different?

## Hypothesis testing ####
# null hypothesis: countries with strict lockdown have the same emission mean from power sector than those with lighter ones
# alternate hypothesis: countries with strict lockdown have a lower emission mean from power sector than those with lighter ones

power <- filter(data, sector == "Power")

strictpow <- power %>% filter(lock_severe == "national lockdown") 
strictpow 

lightpow <- power %>% filter(lock_severe == "Localised and below")
lightpow

t.test(strictpow$value, lightpow$value, paired = F) #p val = 2.2e-16

## more plotting ####
twenty <- filter(twenty, !is.na(lock_severe))

ggplot(data = twenty, aes(x = lock_severe, y = value)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  stat_summary(fun.data = 'mean_se',
               geom = 'errorbar',
               width = 0.2)
  ggtitle("Strict vs. Lighter??")

power <- filter(twenty, !is.na(lock_severe))
  ggplot(data = power, aes(x = lock_severe, y = value)) +
    geom_bar(stat = "summary",
             fun = "mean") +
    stat_summary(fun.data = 'mean_se',
                 geom = 'errorbar',
                 width = 0.2)
  ggtitle("Strict vs. Lighter for power??")
  
uk <- filter(data, country == "United Kingdom")
uk <- filter(uk, sector == "Power")
  
ggplot(data = uk, aes(x = month, y = value)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  ggtitle("Uk's power usage")

ggplot(data = twenty, aes(x = month, y = value)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  ggtitle("Power 2020")


## difference in 2019 and 2020 #### 

twentydif <- mean(twenty$value)
twentydif # 0.09147855

nineteendif <- mean(nineteen$value)
nineteendif # 0.1027264

dif <- nineteendif - twentydif
dif # 0.0112479

#plot ###
difference <- filter(data, year == '2019'|year == '2020')
View(difference)

ggplot(data = difference, aes(x = year, y = value, fill = year)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  ggtitle("Difference between 2019 and 2020") 
 # annotate("text", label = "difference = 0.0112479",
       #   size = 10, 
       #  x = "2020",
       #   y = 0.09)


# divide value by pop

pop <- filter(pop, year == "2020")
pop <- filter(pop, !country == "EU27 & UK")
pop <- pop %>%
  mutate(lock_severe = case_when(
    startsWith(country, "France") ~ "national lockdown",
    startsWith(country, "Germany") ~ "national lockdown",
    startsWith(country, "United Kingdom") ~ "national lockdown",
    startsWith(country, "Russia") ~ "national lockdown",
    startsWith(country, "Italy") ~ "national lockdown",
    startsWith(country, "Spain") ~ "national lockdown",
    startsWith(country, "Belgium") ~ "national lockdown",
    startsWith(country, "Switzerland") ~ "national lockdown",
    startsWith(country, "Croatia") ~ "national lockdown",
    startsWith(country, "Austria") ~ "national lockdown",
    startsWith(country, "Romania") ~ "national lockdown",
    startsWith(country, "Greece") ~ "national lockdown",
    startsWith(country, "Netherlands") ~ "national lockdown",
    startsWith(country, "Estonia") ~ "national lockdown",
    startsWith(country, "Denmark") ~ "national lockdown",
    startsWith(country, "Lithuania") ~ "national lockdown",
    startsWith(country, "Monaco") ~ "national lockdown",
    startsWith(country, "Luxembourg") ~ "national lockdown",
    startsWith(country, "Ireland") ~ "national lockdown",
    startsWith(country, "Czech Republic") ~ "national lockdown",
    startsWith(country, "Portugal") ~ "national lockdown",
    startsWith(country, "Poland") ~ "national lockdown",
    startsWith(country, "Slovenia") ~ "national lockdown",
    startsWith(country, "Slovakia") ~ "national lockdown",
    startsWith(country, "Serbia") ~ "national lockdown",
    startsWith(country, "Bulgaria") ~ "national lockdown",
    startsWith(country, "Cyprus") ~ "national lockdown",
    startsWith(country, "Finland") ~ "Localised and below",
    startsWith(country, "Sweden") ~ "Localised and below",
    startsWith(country, "Norway") ~ "Localised and below",
    startsWith(country, "Iceland") ~ "Localised and below",
    startsWith(country, "Latvia") ~ "Localised and below",
    startsWith(country, "Ukraine") ~ "Localised and below",
    startsWith(country, "Hungary") ~ "Localised and below",
    startsWith(country, "Bosnia") ~ "Localised and below",
    startsWith(country, "Malta") ~ "Localised and below"
  ))
View(pop)

sval <- filter(twenty, lock_severe == "national lockdown" )
sval <- filter(sval, sector == "Power")
svalue <- sum(sval$value)
svalue # 776.6971
spop <- filter(pop, lock_severe == "national lockdown")
strictpop <- sum(spop$population)
strictpop # 490827187

#mean of value for strict pops
strictpopulation <- svalue/strictpop
strictpopulation #1.582425e-06

lval <-  filter(twenty, lock_severe == "Localised and below")
lval <- filter(lval, sector == "Power")
lvalue <- sum(lval$value)
lvalue # 29.78524
lpop <- filter(pop, lock_severe == "Localised and below")
lightpop <- sum(lpop$population)
lightpop # 23946362

#mean of value for light pops
lightpopulation <- lvalue/lightpop
lightpopulation # 1.243832e-06

#conclusion
#Countries with stricter lockdowns have bigger pop
#countries with lighter lockdowns have smaller pop

#data frame
strictlightdif <- data.frame(lockdown = c("light", "strict"),
                             value = c(1.243832e-06, 1.582425e-06)
                             )
strictlightdif

ggplot(data = strictlightdif, aes(x = lockdown, y = value, fill = lockdown)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  ggtitle("Strict vs. Lighter")

## Finding the difference from 2019 to 2020 ####

#light
lnine <-  filter(nineteen, lock_severe == "Localised and below")
lnine <- filter(lnine, sector == "Power")
lnineval <- sum(lnine$value)
lnineval # 35.39567

#strict
snine <- filter(nineteen, lock_severe == "national lockdown" )
snine <- filter(snine, sector == "Power")
snineval <- sum(snine$value)
snineval # 899.6906


ninetotwenty <- data.frame(change = c("strictNine", "strictTwenty", "lightNine", "lightTwenty"),
                           value = c(899.6906, 776.6971, 35.39567, 29.78524))

ninetotwenty
ggplot(data = ninetotwenty, aes(x = change, y = value, fill = change)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  ggtitle("Strict vs. Lighter through 2019 - 2020")








