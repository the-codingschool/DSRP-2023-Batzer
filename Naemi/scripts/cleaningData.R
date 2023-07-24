## loading in libraries ####
#install.packages("lubridate")
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
## reading data ####
data <- read.csv("Data/carbonmonitor-eu_datas_2023-07-19.csv")
View(data)

#getting rid of timestamp value
data <- select(data, -timestamp)
data <- filter(data, (value != 0))
View(data)

## add a column for year ####
?separate()
#data <- separate(data,
                 #date,
                # into = c("date", "year"),
                # sep = "/2")
#View(data) 

#or
data <- data %>%
  mutate(year = case_when(
    endsWith(date, "19") ~ 2019,
    endsWith(date, "20") ~ 2020,
    endsWith(date, "21") ~ 2021,
    endsWith(date, "22") ~ 2022,
    endsWith(date, "23") ~ 2023))

View(data)

## exploring data ####
mean(data$value) # 0.09992181
median(data$value) # 0.01533485

max(data$value) #4.02748
min(data$value) # 2.58245e-12
max(data$value) - min(data$value) #range: 4.02748

var(data$value) # variation: 0.1101863

#outliers
lower <- mean(data$value) - 3*sd(data$value)
lower # -0.895908
upper <- mean(data$value) + 3*sd(data$value)
upper # 1.095752

#categorical variables: 
#sector: power, industry, Ground Transport, Residential, Domestic Aviation, International Aviation

## plotting ####
# year/value
ggplot(data = data, aes(x = year, y = value, fill = year)) +
  geom_bar(stat = "summary",
           fun = "mean") #value ofc drops in 2020

ggplot(data = data, aes(x = year, y = value, color = sector)) +
  geom_bar(stat = "summary",
           fun = "mean")

ggplot(data = data, aes(x = date, y = value, color = sector)) +
  geom_point() #super messy can't tell

#power
#power <- filter(data, sector == "Power")
#ggplot(data = power, aes(x = date, y = value)) +
  #geom_line(stat = "summary",
          #  fun = "mean")
