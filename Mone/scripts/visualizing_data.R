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

###########################################################################
# Changining class type ###
Eu_co2$timestamp = as.numeric(Eu_co2$timestamp)

Eu_co2$timestamp = as.POSIXct(Eu_co2$timestamp, origin = 1970-01-01)

# adding year coloumn 
Eu_co2 <- mutate(Eu_co2, year = format(as.POSIXct(Eu_co2$timestamp, origin = "1970-01-02)"), "%Y"))

View(Eu_co2)
###########################################################################
## creating  plot for lightning talk ####


#(Separate) plots 4 date 

# 2019 emission plot 

neo_2019 = Eu_co2 |>
  filter(timestamp < 1577836801)

ggplot( data = neo_2019, aes( x = sector, y = value, fill = sector)) + 
  geom_bar(stat = "sum", na.rm = FALSE) + 
  labs(
    title = "Total EU + UK Emissions (2019)",
    y = "Emissions (MT CO2)",
    x = "Sectors"
  ) + 
  theme_minimal()


# 2020 emissions plot 

neo_2020 = Eu_co2 |>
  filter(between(timestamp, 1577836801, 1609372801))

ggplot( data = neo_2020, aes( x = sector, y = value, fill = sector)) + 
  geom_bar(stat = "sum", na.rm = FALSE) + 
  labs(
    title = "Total EU + UK Emissions (2020)",
    y = "Emissions (MT CO2)",
    x = "Sectors"
  ) +
  theme_minimal()


# 2021

neo_2021 = Eu_co2 |>
  filter(between(timestamp, 1609372801, 1640908801))

ggplot( data = neo_2021, aes( x = sector, y = value, fill = sector)) + 
  geom_bar(stat = "sum", na.rm = FALSE) + 
  labs(
    title = "Total EU + UK Emissions (2021)",
    y = "Emissions (MT CO2)",
    x = "Sectors"
  ) + 
  theme_minimal()

#2022 emission plot

neo_2022 = Eu_co2 |>
  filter(between(timestamp, 1640908801, 1672444801))

ggplot( data = neo_2022, aes( x = sector, y = value, fill = sector)) + 
  geom_bar(stat = "sum", na.rm = FALSE) + 
  labs(
    title = "Total EU + UK Emissions (2022)",
    y = "Emissions (MT CO2)",
    x = "Sectors"
  ) + 
  theme_minimal()

# 2023 emission plot (DATA VERY SMALL IN COMPARISON)
neo_2023 = Eu_co2 |>
  filter(timestamp > 1672444801)

ggplot( data = neo_2023, aes( x = sector, y = value, fill = sector)) + 
  geom_bar(stat = "sum", na.rm = FALSE) + 
  labs(
    title = "Total EU + UK Emissions (2023)",
    y = "Emissions (MT CO2)",
    x = "Sectors"
  ) + 
  theme_minimal()

## Emission plot comparing all years 

# removing the UK from the data set 
no_uk_carbon = Eu_co2 |> 
  filter(country != "United Kingdom") |>
  filter(country != "EU27 & UK") |>
  mutate (year = format(as.POSIXct(Eu_co2$timestamp, origin = "1970-01-02)"), "%Y"))

ggplot( data = no_uk_carbon, aes( x = year, y = value, fill = year)) + 
  geom_bar(stat = "sum", na.rm = FALSE) + 
  labs(
    title = "Total European Union Emissions per year (2019 - 2023)",
    y = "Emissions (MT CO2)",
    x = "Year"
  ) + 
  theme_minimal()


ggplot(data = )

# Total emissions data ####
carbon = Eu_co2 |>
  pivot_wider(
    names_from = sector, 
    values_from = value
  ) |>
  mutate(
    total_em = Power + Industry + `Ground Transport`+ Residential +
                   `Domestic Aviation` + `International Aviation`
  )


### Population Data #### 

country_pop = pivot_wider(
  population,
  names_from = country,
  values_from = population
)


no_uk_pop = population |>
  filter(country != c("United Kingdom", "EU27 & UK"))

# ploting the population data in bar plot 
ggplot( data = no_uk_pop, aes( x = year, y = population, fill = year)) + 
  geom_bar(stat = "sum", na.rm = FALSE) + 
  labs(
    title = "Total European Union Population per year (2019 - 2023)",
    y = "Population",
    x = "Year"
  ) + 
  ylim(7.5e+08, 1.0e+09) # work on getting this to work!!  + 
  theme_minimal()
  
# ploting the population data in density plot 
  ggplot(no_uk_pop, aes( x = year)) +
    geom_density() +
    labs(
      title = "Total European Union Population per year (2019 - 2023)",
      y = "Population",
      x = "Year"
    ) +  
    theme_minimal()
  

## Making numeric data 
  carbon_all_numeric = pivot_wider(Eu_co2, 
                                   names_from = sector,
                                   values_from = value)
  
# turning year & integer into a numeric variable 
carbon_all_numeric = carbon_all_numeric |>
  mutate(year = as.integer(as.factor(year))) |>
  mutate(country = as.integer(as.factor(country))) |>
  # adding total emission data 
  mutate(
    total_em = Power + Industry + `Ground Transport`+ Residential +
      `Domestic Aviation` + `International Aviation`
  )

# removing data & timestamp from data set 
carbon_all_numeric = carbon_all_numeric |>
  select(-c(date:timestamp))

# calculating correlations 
carbon_corrs = cor(carbon_all_numeric) |>
  melt()|>
  as.data.frame() 

# plotting correlations 
ggplot(carbon_corrs, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "green", high = "blue", mid = "white", midpoint = 0) +
  theme_minimal()

## Calculating 1990 emissions ####
# 2019 is 25% less than 1990 

total_em_2019 = sum(neo_2019$value)
Goal_2030 = total_em_2019 - (total_em_2019 *.3) # 2030 target is in the 4573.9435 ballpark! 

######## use rbind() to combine 2 different data frames 

### use code below to make a data frame all at once
test = data.frame(mae = c(mae(acutal, predicted), mae(acutal, predicted2)),
                  rmse = c(rmse(acutal, predicted), rmse (actual, predicted2)),
                  modelType = c("randForest", "boostTree"))



### year v total emissions 

ggplot(carbon, aes( x = carbon$timestamp, y = carbon$total_em)) + 
  geom_point()+
  geom_smooth(method = lm, color = "lightblue", fill = "lightpink", se=TRUE)
  
