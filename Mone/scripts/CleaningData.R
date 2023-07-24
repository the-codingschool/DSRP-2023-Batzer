# little note! 
#EU27 & UK = every country in the EU + the UK data 
###########################################################################
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
#info about data set ####
names(Eu_co2)
class(Eu_co2$)

# what are the values in sector 
unique(Eu_co2$sector)

#Unique Value in Country: 
unique(Eu_co2$country)

###########################################################################
# reorganizing the data ####
?pivot_wider
#Spreading the sectors out 
tidy_carbon = pivot_wider(Eu_co2, 
                          names_from = sector, 
                          values_from = value)
# making it look pretty! 
tidy_carbon = tidy_carbon |> 
  clean_names("snake")


# total emission for that given day 
tidy_carbon = mutate(tidy_carbon, total_em = sum(power, industry, ground_transport, residential, domestic_aviation, international_aviation))

View(tidy_carbon)

# turning the date into a factor 
tidy_carbon$date = as.character(tidy_carbon$date)

class(Eu_co2$date)

# turning timestamp into a numeric class
 tidy_carbon$timestamp = as.numeric(tidy_carbon$timestamp)
 Eu_co2$timestamp = as.numeric(Eu_co2$timestamp)
 class(tidy_carbon$timestamp)

###########################################################################
##Country Data set #### 

#Germany
germany_data = tidy_carbon |>
  filter(country == "Germany")
View(germany_data)

#United Kingdom 
uk_data = tidy_carbon |>
  filter(country == "United Kingdom")
View(uk_data)

#France
france_data = tidy_carbon |>
  filter(country == "France")
View(france_data)

###########################################################################
## Year Data sets ####

# 2019 Data set 
data_2019 = tidy_carbon |>
  filter(timestamp <= 1577836800)

View(data_2019)

# 2020 Data set 
data_2020 = tidy_carbon |> 
  filter(between(timestamp, 1577836801, 1609372801))

View(data_2020)

# 2021 Data set 
data_2021 = tidy_carbon |> 
  filter(between(timestamp, 1609372801, 1640908801 ))

View(data_2021)

# 2022 Data set 
data_2022 = tidy_carbon |> 
  filter(between(timestamp, 1640908801, 1672444801 ))

View(data_2022)


# 2023 Data set 
data_2023 = tidy_carbon |>
  filter(timestamp > 1672444801)

View(data_2023)

############################################################################
# Plotting & Visualizing Data ####

# Tile plot 

all_numeric_carbon = Eu_co2 |> 
  pivot_wider(
    names_from = sector,
    values_from = value
  ) |>
  mutate(
    country = as.integer(Eu_co2$country)
  ) |> 
  select(
    
  )
  



