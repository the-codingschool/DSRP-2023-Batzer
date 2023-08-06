# Reading in Data 
Eu_co2 = read.csv("Data/carbonmonitor-eu_datas_2023-07-19.csv")

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
library(readr)

###########################################################################

# Creating Numeric Data set & Preping the dataset ####

carbon_numeric = Eu_co2 |>
  # Adding year column
  mutate(Eu_co2, year = format(as.POSIXct(Eu_co2$timestamp, 
                                          origin = "1970-01-02)"), "%Y"))|> 
  #Removing 2020 & 2023 + Removing Uk and Eu27 & UK 
  filter(
    country != "EU27 & UK",
    country != "United Kingdom",
    year != "2023",
    year != "2020"
  ) |>
  #expanding the sectors out 
  pivot_wider(
    names_from = sector,
    values_from = value)

# Removing country, date & year 
carbon_numeric = carbon_numeric |>
  select(-c(country, date, year))
           
# Grouping the data by the timestamp & adding the sectors in that are combined
carbon_numeric = carbon_numeric |>
  group_by(timestamp) |>
  summarise(across(c(Power, Industry, `Ground Transport`, `Residential`, `Domestic Aviation`, `International Aviation`), sum))

# Converting everything to numeric 
carbon_numeric = carbon_numeric |>
  mutate(timestamp = as.numeric(timestamp))

########################################################################### 
# Prepping Data sets ####
reg_split = initial_split(carbon_numeric, prop = .75)

# making training & testing data set 
reg_train = training(reg_split)
reg_test = testing(reg_split)

## Linear Regression ####
lm_fit = linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |> 
  fit(timestamp ~ Power + Industry + ., data = reg_train)

lm_fit
summary(lm_fit)

# Boosted Trees!! 
boosted_reg_fit = boost_tree() |>
  set_engine("xgboost") |>
  set_mode("regression") |> 
  fit(timestamp ~ Power + Industry + ., data = reg_train)

boosted_reg_fit$fit$evaluation_log # lowest value = 10818680


### Evaluation ####
reg_results = reg_test

## accuracy calculations 
reg_results$lm_pred = predict(lm_fit, reg_test)$.pred
reg_results$boost_pred = predict(boosted_reg_fit, reg_test)$.pred

yardstick::mae(reg_results, timestamp, lm_pred)
yardstick::mae(reg_results, timestamp, boost_pred)

yardstick::rmse(reg_results, timestamp, lm_pred) #37471716.
yardstick::rmse(reg_results, timestamp, boost_pred) #15373917. Winner Winner Chicken Dinner

########################################################################### 

### Predictions #####
newtime = data.frame(timestamp = 1666739800)
newtime$prediction = predict(boosted_reg_fit, new_data = newtime)

########################################################################### 
## Misc (mostly grpahs ) ####

# 2019 - 2023 
carbon = Eu_co2 |>
  pivot_wider(
    names_from = sector,
    values_from = value) |>
  filter(country != "EU27 & UK" )|>
  select(-c(country, date))


carbon = carbon |>
  group_by(timestamp) |>
  summarise(across(c(Power, Industry, `Ground Transport`, `Residential`, `Domestic Aviation`, `International Aviation`), sum)) |>
  mutate(
    total_em = Power + Industry + `Ground Transport`+ Residential +
      `Domestic Aviation` + `International Aviation`
  ) 
  
ggplot(data = carbon, aes(x = carbon$timestamp, y = carbon$total_em)) +
  geom_point()+
  geom_smooth( color = "#6aa84fff" , se=FALSE) +
  labs(
    title = "Total Emissions for Everyday in the EU (2019 - 2022)",
    x = "Seconds Since 1970/01/02",
    y = "Total Emissions / MtCO2"
  ) +
  theme_minimal()
  
           
  


# adding total_em value for graphing purposes 
carbon_numeric_3 = carbon_numeric_3 |>
  mutate(
    total_em = Power + Industry + `Ground Transport`+ Residential +
      `Domestic Aviation` + `International Aviation`
  )

# graphing total_em 
ggplot(data = carbon_numeric_3, aes(x = carbon_numeric_3$timestamp, y = carbon_numeric_3$total_em)) +
  geom_point()+
  geom_smooth( color = "#6aa84fff" , se=FALSE, method = lm) +
  labs(
    title = "Total Emissions for Everyday in the EU (2019 - 2022)",
    x = "Seconds Since 1970/01/02",
    y = "Total Emissions / MtCO2"
  ) +
  theme_minimal()
         

#Boosted Tree predicitons 
ggplot(data = reg_results, aes( x = lm_pred ,  y = reg_results$timestamp)) +
  geom_point()+
  geom_smooth(color = "#6aa84fff") + 
  labs(
    title = "Linear Model Predictions v. Timestamps",
    x = "Linear Regression Predicitons",
    y = "Actual Timestamp"
  
  ) +
  theme_minimal()

          
ggplot(data = reg_results, aes( x = boost_pred ,  y = reg_results$timestamp)) +
  geom_point()+
  geom_smooth(color = "#6aa84fff") + 
  labs(
    title = "Boosted Tree Predictions v. Timestamps",
    x = "Boosted Tree Predicitons",
    y = "Actual Timestamp"
    
  )  + 
  theme_minimal()
