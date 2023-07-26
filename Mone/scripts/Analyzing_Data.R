# Reading in Data 
Eu_co2 = read.csv("Data/carbonmonitor-eu_datas_2023-07-19.csv")
population = read.csv("Data/worldbank_population_bycountry - data.csv")

View(Eu_co2)
View(population)

## Load in packages ####
library(lubridate)
library(tidyr)
library(janitor)
library(dplyr)
library(ggplot2)
library(ranger)
library(reshape2)
library(yardstick)
library(Metrics)
library(rsample)

###########################################################################

# reading in Numeric Data ####
# Adding Year column 
Eu_co2 <- mutate(Eu_co2, year = format(as.POSIXct(Eu_co2$timestamp, 
                                                  origin = "1970-01-02)"), "%Y"))

## Making numeric data 
carbon_all_numeric = pivot_wider(Eu_co2, 
                                 names_from = sector,
                                 values_from = value)

# removing 2020 & 2023 from the data set 
carbon_all_numeric = carbon_all_numeric |>
  filter(year != c("2020", "2023"))

# turning year & integer into a numeric variable 
carbon_all_numeric = carbon_all_numeric |>
  mutate(year = as.integer(as.factor(year))) |>
  mutate(country = as.integer(as.factor(country))) |>
  # adding total emission data 
  mutate(
    total_em = Power + Industry + `Ground Transport`+ Residential +
      `Domestic Aviation` + `International Aviation`
  )

# removing data & time stamp from data set 
carbon_all_numeric = carbon_all_numeric |>
  select(-c(date:timestamp))


########################################################################### 
# Prepping Data sets ####

#Regression Data set
# 75% of data into a training set 
reg_split = initial_split(carbon_all_numeric, prop = .75)


