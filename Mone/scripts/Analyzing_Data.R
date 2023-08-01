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

predlm_fit_2
summary(lm_fit_2)


## Boosted Tree ####
boost_reg_fit = boost_tree() |>
  set_engine("xgboost") |> 
  set_mode("regression") |> 
  fit(emissions ~ year + power + Industry + ., data = reg_train)


# data set 2 
boost_reg_fit_2 = boost_tree() |>
  set_engine("xgboost") |> 
  set_mode("regression") |> 
  fit(total_em ~ year + Power + Industry + ., data = reg_train_2)



### Evaluation 
reg_results = reg_test # Data set 1 
reg_results_2 = reg_test_2 # Data set 2 


# accuracy calculations for data set 1 
reg_results$lm_pred = predict(lm_fit, reg_test)$.pred
reg_results$boost_pred = predict(boost_reg_fit, reg_test)$.pred

yardstick::mae(reg_results, emissions, lm_pred)
yardstick::mae(reg_results, emissions, boost_pred)

yardstick::rmse(reg_results, emissions, lm_pred)
yardstick::rmse(reg_results, emissions, boost_pred)

# accuracy for data set 2 
reg_results_2$lm_pred = predict(lm_fit_2, reg_test_2)$.pred
reg_results_2$boost_pred = predict(boost_reg_fit_2, reg_test_2)$.pred

yardstick::mae(reg_results_2, total_em, lm_pred)
yardstick::mae(reg_results_2, total_em, boost_pred)

yardstick::rmse(reg_results_2, total_em, lm_pred_2)
yardstick::rmse(reg_results_2, total_em, boost_pred_2)


### ALTERNATIVE PREDICTION MODEL 



### Graphing Results ####

# making the table longer 
data_set_2_results = reg_results_2 |>
  pivot_longer(
    cols = c(`total_em`, `lm_pred`, `boost_pred`),
    names_to = "data_type", 
    values_to = "values"
  )

# scatter ploting the data 
# DATA SET 2 - LINEAR REG 
ggplot(data = reg_results_2, aes( x = reg_results_2$total_em, y = reg_results_2$lm_pred)) + 
  geom_point() + 
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  labs(
    title = "Linear Regression accuracy",
    x = "Acutal Total Emissions",
    y = "Linear Regression predicted Emissions"
  ) +
  theme_minimal()

#DATA SET 2 - BOOSTED TREEE 
ggplot(data = reg_results_2, aes (x = reg_results_2$total_em, y = reg_results_2$boost_pred))+
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) + 
  theme_minimal() +
  labs(
    title = "Boosted Tree accuracy",
    x = "Acutal Total Emissions",
    y = "Boosted Tree predicted Emissions"
  )


### mae & rmse  comparisons (change the axis a bit )

model_type = c("linear reg", "boosted tree")
Data_set_1_mae = c( 300, 293)
Data_set_2_mae = c(4.02e-15, 0.0127 ) 
Data_set_1_rmse = c( 300, 293)
Data_set_2_rmse = c(6.90e-15, 0.0313 )
model_comparison = data.frame(model_type, Data_set_1_mae, Data_set_2_mae, Data_set_1_rmse, Data_set_2_rmse)



### Try to predict based on:  lm(emissions ~ month_of_year + year)
#### predict(linear_model, newdf = [all_months, years out to 2030])
## Do maybe a statiscal test best on the number of confidence to get hte likelhood 
# WHAT IS THE PROBILITY THAT WE ARE GOING TO BE OVER THE GOAL, BASED ON THE STATISCAL NOISE