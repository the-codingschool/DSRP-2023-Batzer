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
library(parsnip)

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
  filter(year != c(2, 5))

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


### total year dataset ##### 

# total emissions year 
total_em = c(sum(filter(Eu_co2, year == "2019")$value),
             sum(filter(Eu_co2, year == "2021")$value),
             sum(filter(Eu_co2, year == "2022")$value))

#total power emissions 
tot_power = c(sum(filter(filter(Eu_co2, year == "2019"), sector == "Power")$value),
              sum(filter(filter(Eu_co2, year == "2021"), sector == "Power")$value),
              sum(filter(filter(Eu_co2, year == "2022"), sector == "Power")$value))

#total Industry emissions 
tot_Industry = c(sum(filter(filter(Eu_co2, year == "2019"), sector == "Industry")$value),
              sum(filter(filter(Eu_co2, year == "2021"), sector == "Industry")$value),
              sum(filter(filter(Eu_co2, year == "2022"), sector == "Industry")$value))

#total GT emissions 
tot_GT = c(sum(filter(filter(Eu_co2, year == "2019"), sector == "Ground Transport")$value),
                 sum(filter(filter(Eu_co2, year == "2021"), sector == "Ground Transport")$value),
                 sum(filter(filter(Eu_co2, year == "2022"), sector == "Ground Transport")$value))

#total Residential emissions 
tot_res = c(sum(filter(filter(Eu_co2, year == "2019"), sector == "Residential")$value),
           sum(filter(filter(Eu_co2, year == "2021"), sector == "Residential")$value),
           sum(filter(filter(Eu_co2, year == "2022"), sector == "Residential")$value))

#total Domestic Aviation emissions 
tot_dom = c(sum(filter(filter(Eu_co2, year == "2019"), sector == "Domestic Aviation")$value),
            sum(filter(filter(Eu_co2, year == "2021"), sector == "Domestic Aviation")$value),
            sum(filter(filter(Eu_co2, year == "2022"), sector == "Domestic Aviation")$value))

#total International Aviation emissions 
tot_int = c(sum(filter(filter(Eu_co2, year == "2019"), sector == "International Aviation")$value),
            sum(filter(filter(Eu_co2, year == "2021"), sector == "International Aviation")$value),
            sum(filter(filter(Eu_co2, year == "2022"), sector == "International Aviation")$value))


# all 3 years data                  
year = c("2019", "2021", "2022")

total_emissions = data.frame(year, total_em, tot_power, tot_Industry, tot_GT, tot_res, tot_dom, tot_int)

#renaming values 
total_emissions = total_emissions |> 
  rename(emissions = total_em) |>
  rename(power = tot_power) |>
  rename(Industry = tot_Industry) |>
  rename(`Ground Transportation` = tot_GT) |>
  rename(Residential = tot_res) |>
  rename(`Domestic Aviation` = tot_dom) |>
  rename(`International Aviation` = tot_int)

# converting year to numeric  
all_numeric_tot_em = total_emissions |>
  mutate(year = as.integer(as.factor(year)))
  

########################################################################### 
# Prepping Data sets ####

#Regression Data set 1 
# 75% of data into a training set 
reg_split = initial_split(all_numeric_tot_em, prop = .75)

# make training & testing data set 
reg_train = training(reg_split)
reg_test = testing(reg_split)

#Regression Data set 2 (aka carbon all numeric )
# 75% of data into a training set 
reg_split_2 = initial_split(carbon_all_numeric, prop = .75)

# make training & testing data set 
reg_train_2 = training(reg_split_2)
reg_test_2 = testing(reg_split_2)

## Linear Regression ####
#Data set 1 
lm_fit = linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |> 
  fit(emissions ~ year + power + Industry + ., data = reg_train)

lm_fit
summary(lm_fit)

# Data set 2
lm_fit_2 = linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |> 
  fit(total_em ~ year + Power + Industry + ., data = reg_train_2)

lm_fit_2
summary(lm_fit_2)


## Boosted Tree ####
boost_reg_fit = boost_tree() |>
  set_engine("xgboost") |> 
  set_mode("regression") |> 
  fit(Sepal.Length ~ ., data = reg_train)


### Evaluation 
