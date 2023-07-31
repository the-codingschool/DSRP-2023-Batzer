#making data for every country to use later

eudata <- filter(carbondata,country != "United Kingdom")
austriadata <- filter(carbondata,country == "Austria")
belgiumdata <- filter(carbondata,country == "Begium")
croatiadata <- filter(carbondata,country == "Croatia")
czechdata <- filter(carbondata,country == "Czech Republic")
denmarkdata <- filter(carbondata,country == "Denmark")
estoniadata <- filter(carbondata,country == "Estonia")
finlanddata <- filter(carbondata,country == "Finland")
francedata <- filter(carbondata,country == "France")
germanydata <- filter(carbondata,country == "Germany")
greecedata <- filter(carbondata,country == "Greece")
hungarydata <- filter(carbondata,country == "Hungary")
irelanddata <- filter(carbondata,country == "Ireland")
italydata <- filter(carbondata,country == "Italy")
latviadata <- filter(carbondata,country == "Latvia")
lithuaniadata <- filter(carbondata,country == "Lithuania")
luxembourgdata <- filter(carbondata, country == "Luxembourg")
maltadata <- filter(carbondata, country == "Malta")
netherlandsdata <- filter(carbondata, country == "Netherlands")
polanddata <- filter(carbondata, country == "Poland")
portugaldata <- filter(carbondata,country == "Portugal")
romaniadata <- filter(carbondata,country == "Romania")
slovakiadata <- filter(carbondata,country == "Slovakia")
sloveniadata <- filter(carbondata, country == "Slovenia")
spaindata <- filter(carbondata, country == "Spain")
swedendata <- filter(carbondata,country == "Sweden")
ukdata <- filter(carbondata, country == "United Kingdom")


westerneurope <- filter(carbondata,country %in% c("United Kingdom","France","Germany","Belgium","Belgium",
                                                  "Netherlands","Luxembourg","Austria"))
northerneurope <- filter(carbondata,country %in% c("Denmark","Sweden","Finland","Estonia","Latvia","Lithuania"))

southerneurope <- filter(carbondata, country %in% c("Spain","Portugal","Italy","Greece","Malta","Cyprus"))

easterneurope <- filter(carbondata,country %in% c("Poland","Czech Republic","Slovakia","Slovenia",
                                                  "Hungary","Croatia","Romania","Bulgaria"))

#seperate into various regions and countries so they can be compared amongst each other and also 
#so it will be easier to visualize and perform machine learning on them