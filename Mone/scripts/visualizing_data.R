# Reading in Data 
Eu_co2 = read.csv("Data/carbonmonitor-eu_datas_2023-07-19.csv")

View(Eu_co2)

## Load in packages ####
library(lubridate)
library(tidyr)
library(janitor)
library(dplyr)
library(ggplot2)

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
  filter(country != "United Kingdom")

ggplot( data = no_uk_carbon, aes( x = year, y = value, fill = year)) + 
  geom_bar(stat = "sum", na.rm = FALSE) + 
  labs(
    title = "Total European Union Emissions per year (2019 - 2023)",
    y = "Emissions (MT CO2)",
    x = "Year"
  ) + 
  theme_minimal()
