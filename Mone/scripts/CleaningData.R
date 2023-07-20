# little note! 
#EU27 & UK = every country in the EU + the UK data 

# Reading in Data 
Eu_co2 = read.csv("Data/carbonmonitor-eu_datas_2023-07-19.csv")

View(Eu_co2)

## Load in packages 
library(tidyr)
library(janitor)
library(dplyr)
library(ggplot2)

#info about data set ####
names(Eu_co2)
class(Eu_co2$)

# what are the values in sector 
unique(Eu_co2$sector)

#Unique Value in Country: 
unique(Eu_co2$country)


# removing the 0 emissions data from the data set ####


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
tidy_carbon$date = as.factor(tidy_carbon$date)

class(tidy_carbon$date)

##Country Data set #### 

#Germany
germany_data = tidy_carbon |>
  filter(country == "Germany")
View(germany_data)

#United Kingdom 
uk_data = tidy_carbon |>
  filter(country == "United Kingdom")
View(uk_data)

#Germany
germany_data = tidy_carbon |>
  filter(country == "Germany")
View(germany_data)

# Plotting the data ####


                        
