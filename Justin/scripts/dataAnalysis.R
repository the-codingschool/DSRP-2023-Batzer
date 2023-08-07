#notes fromm what professor batzer said
# can form datasets of countries who rely more on fossil fuels
# vs ones who dont
# like france and uk vs germany and nordic countries
# try to do statistical tests to see the relationship
# could try tos how like "oh if you rely more on fossil fuels
# your emmisions will look more like this"

# run git pull origin main before merging

powerdata <- filter(carbondata,sector == "Power")
powersummary <- summarize(powerdata,mean = mean(value),
                          .by = country)
poweralphabetical <- arrange(powersummary,country)
powervalues <- arrange(powersummary,desc(mean))

#idea: use the population data and then mutate this for power per capita
# so its adjusted per person
powermutated <- mutate(poweralphabetical,emissionsperperson = mean/populationalphabetical$population)
powermutated <- arrange(powermutated,desc(emissionsperperson))
# highest emissions per person at the top
# countries like estonia, czech republic and poland at the top
# meanwhile france, lithuania, sweden and luxembourg at bottom
# france is at the very bottom despite having a large population

# top 5 biggest power emitters per capita: estonia, czech republic, poland, bulgaria, germany
# top 5 lowest power emitters per capita: france, lithuania, sweden, luxembourg, latvia

powerarranged <- arrange(powersummary,mean)
powerarranged
# top 5 biggest power emitters on average: germany, poland, italy, uk, czech republic
# top 5 lowest power emitters on average: luxembourg, malta, latvia, lithuania, cyprus

#have some sub datasets on big power emitters (those who use a lot of fossil fuels overall and/or per person) and on 
# small power emitters (countries who barely admit any fossil fuels overall and/or per person)
# and then can do some tests on the relationship between power emissions and everything else

highemissionsdataset <- filter(powermutated,emissionsperperson > 4.00e-09)
highemissionsdataset
lowemissionsdataset <- filter(powermutated, emissionsperperson < 4.00e-09)
lowemissionsdataset

highemissionscountries <- filter(carbondata,country %in% highemissionsdataset$country)
highemissionscountries
lowemissionscountries <- filter(carbondata, country %in% lowemissionsdataset$country)
lowemissionscountries

highpoweremissions <- filter(highemissionscountries, sector == "Power")
highpoweremissions
lowpoweremissions <- filter(lowemissionscountries, sector == "Power")
lowpoweremissions

## divided the dataset into half based on emissions per capita
# then i filtered the original dataset based off of which group that country was in
# then made a sub dataset for only power

# decided to use the cutoff of 4.00e-09 since its an easy place to slice the countries in half and theres 
# a noticable difference in the emissions per person between the two datasets
highmean <- mean(highemissionsdataset$emissionsperperson)
lowmean <- mean(lowemissionsdataset$emissionsperperson)
highmean
lowmean

# try correlation stuff

# null hypothesis: the average emissions for the two is the same
# alternate hypothesis: average emissions for the two is different
t.test(highemissionscountries$value,lowemissionscountries$value, alternative = "two.sided")
# mean of x is 0.1635, mean of y is 0.0403
# p value is <2.2e-16, rejects null hypothesis
# countries who emit more fossil fuels emit more overall

# test to see if this is true among other sectors
highnonpoweremissions <- filter(highemissionscountries,sector != "Power")
lownonpoweremissions <- filter(lowemissionscountries, sector != "Power")

t.test(highnonpoweremissions$value,lownonpoweremissions$value, alternative = "two.sided")
# mean of x is 0.135, mean of y is 0.0399, p value <2.23-16

highanovaresults <- aov(value~sector,highemissionscountries)
summary(highanovaresults)
TukeyHSD(highanovaresults)
# power has a high difference with the aviations but very little 
# with industry, residential and ground transport
# industry residential and ground transport all have low differences with each other
# but high differences with power
# international and domestic aviation have an extremly low difference

lowanovaresults <- aov(value~sector,lowemissionscountries)
summary(lowanovaresults)
TukeyHSD(lowanovaresults)
# for low power emitting countries power has a low difference with every other sector
# all of the sectors have a low difference with each other

# theres an interesting difference between the tukeyhsd for the highemissions dataset and the lowemission sone

# could make the sectors into their own columns and then to chi squared tests on them
# this would make it easier to compare between sectors
sectordata = pivot_wider(carbondata, names_from = sector, values_from = value)

highsectordata <- filter(sectordata,country %in% highemissionsdataset$country)
lowsectordata <- filter(sectordata,country %in% lowemissionsdataset$country)

# try some t tests and anova tests with the sector data
# can use t tests to compare the average value between the two datasets
#see the differences in the other sectors between countries who emit more power per person than those who dont

# null hypothesis for all of these is that the average of all the values in the sector in both datasets is the same
# alternate hypothesis for all of these is that the average is different

t.test(highsectordata$Power,lowsectordata$Power, alternative = "two.sided")
# x mean is 0.302, y mean is 0.042

t.test(highsectordata$Industry,lowsectordata$Industry, alternative = "two.sided")
# p value < 2.2e-16 
# mean of x is .190, mean of y is 0.053
# high sector data has noticeably more industry emissions
# power has a correlation with industry

t.test(highsectordata$`Ground Transport`,lowsectordata$`Ground Transport`, alternative = "two.sided")
# mean of x is 0.254, mean of y is 0.081
# p value < 2.2e-16
# power has a correlation with ground transport

t.test(highsectordata$Residential,lowsectordata$Residential, alternative = "two.sided")
# mean of x is 0.194, mean of y is 0.050
# p value <2.2e-16
# power has a correlation with residential

t.test(highsectordata$`Domestic Aviation`,lowsectordata$`Domestic Aviation`, alternative = "two.sided")
# p value <2.2e-16
# mean of x is 0.002, mean of y is 0.001
# power does not have a correlation with domestic aviation

t.test(highsectordata$`International Aviation`,lowsectordata$`International Aviation`, alternative = "two.sided")
# mean of x is 0.037, mean of y is 0.013
# p value <2.2e-16
# very very small correlation but not noticeable

##### What I noticed: Power has a large impact on industry, ground transport and residential but not the aviations####

## trying anova testing
sectoranovaresults <- aov(Power~country,sectordata)
summary(sectoranovaresults)
TukeyHSD(sectoranovaresults)

highsectoranovaresults <- aov(Power~country,highsectordata)
summary(highsectoranovaresults)
TukeyHSD(highsectoranovaresults)

# anova tests dont work well with the sector data because it needs a categorical variable and the only one
# in sector data is country

#### Important note: Ignore from here up to where I start working on coor tables####
# I tried doing chi squared testing but my data was too big for it
# im still leaving this here though to show what i tried to do

# I want to test if power and the other industries are independent or dependent
# I will do a chi squared test with power and the other sectors 
# i will do this for both high and low emitting countries to see if theres a difference

t <- table(highsectordata$Power,highsectordata$Industry)
highchisq_results <- chisq.test(t)
highchisq_results$p.value
highchisq_results$residuals

# correlation table was too big and my computer couldnt run it

# the dataset might be too big for chi squared testing so we can test on a training dataset

library(rsample)
set.seed(72723)
highsplit <- initial_split(highsectordata,prop = .10)
hightrain <- training(highsplit)
hightest <- testing(highsplit)

lowsplit<- initial_split(lowsectordata,prop = .10)
lowtrain <- training(lowsplit)
lowtest <- testing(lowsplit)

highindustrytable <- table(hightrain$Power,hightrain$Industry)
highchisq_results <- chisq.test(t)
highchisq_results$p.value
highchisq_results$residuals
# even when making the training data only 10% of the dataset my computer still couldnt run it
# i dont know if the dataset is too big or if my computer sucks (probably both) but i wont be able to do chi squared testing
#### end of chi squared testing part ####

# looking at the correlations for both datasets

numerichighsector <- select(highsectordata, -c(country,date,timestamp))
cor(numerichighsector)

highsectorcoors <- numerichighsector |>
  cor()|>
  melt()|>
  as.data.frame()

ggplot(highsectorcoors,aes(x= Var1, y = Var2,fill=value))+
  geom_tile()+
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                       midpoint = 0.85)+
  labs(title= "Correlation between sectors for high emitting countries (per person)")+ 
  theme(axis.text.x = element_text(angle = 90))

# low sector
numericlowsector <- select(lowsectordata, -c(country,date,timestamp))
cor(numericlowsector)

lowsectorcoors <- numericlowsector |>
  cor()|>
  melt()|>
  as.data.frame()

ggplot(lowsectorcoors,aes(x= Var1, y = Var2,fill=value))+
  geom_tile()+
  scale_fill_gradient2(low = "red", high = "blue",mid = "white",
                       midpoint = 0.85)+
  labs(title = "Correlation between sectors for low emitting countries (per person)")+
  theme(axis.text.x = element_text(angle = 90))
# note: compared to the high dataset, the correlations for the low dataset
# are a lot more spread out


# idea: make another dataset that has each sector as a column and then do machine learning with that
# also do what batzer said about percentage of emissions from power

# can be helpful to normalize data before putting it into a model

# from what professor batzer said: try to look into what fraction of per person emissions is from power
# try to narrow down topic
# dont underestimate the amount of time for statistical testing
# could maybe do background research on some countries energy policies like for france
# no matter what field you study, especially scientific research, its all data science in some capacity
# a lot of science is becoming statistical science

# idea for topic: How  does the amount of fossil fuel emissions affect the emissions of other sectors

#### Notes so far #####
## all the models and statistical testing seems to indicate that power has
# the most correlation with industry, ground transportation and residential

francetotalemissions <- sum(francedata$value)
francepowerdata <- filter(francedata,sector == "Power")
francepowersum <- sum(francepowerdata$value)
francepowerpercentage <- (francepowersum/francetotalemissions)*100
francepowerpercentage
# about 10% of frances emissions comes from power

germanytotalemissions <- sum(germanydata$value)
germanypowerdata <- filter(germanydata,sector == "Power")
germanypowersum <- sum(germanypowerdata$value)
germanypowerpercentage <- (germanypowersum/germanytotalemissions)*100
germanypowerpercentage
# power makes up 35% of germanys emissions, much more than france

#lets look at estonia, the highest emitter per person
estoniatotalemissions <- sum(estoniadata$value)
estoniapowerdata <- filter(estoniadata,sector == "Power")
estoniapowersum <- sum(estoniapowerdata$value)
estoniapowerpercentage <- (estoniapowersum/estoniatotalemissions)*100
estoniapowerpercentage
# Power makes up 66% of estonias emissions

# look at poland, the third highest emitter per person
polandtotalemissions <- sum(polanddata$value)
polandpowerdata <- filter(polanddata,sector == "Power")
polandpowersum <- sum(polandpowerdata$value)
polandpowerpercentage <- (polandpowersum/polandtotalemissions)*100
polandpowerpercentage
# 44% of polands emissions come from power

#what about sweden
swedentotalemissions <- sum(swedendata$value)
swedenpowerdata <- filter(swedendata,sector == "Power")
swedenpowersum <- sum(swedenpowerdata$value)
swedenpowerpercentage <- (swedenpowersum/swedentotalemissions)*100
swedenpowerpercentage
# only 15% of swedens emissions come from power
# the trend is that countries that emit more per person have 
# a higher percentage of their emissions come from power

# use sectordata to calculate percent of power emissions

# after that try the same thing as before with the t tests and stuff to see 
# amount of per capita power emissions vs the percentage of total emissions that power makes up

## important note: redo the stuff above because i did without the corrected values
# probably wont make much of a difference but still

#notes from google q and a
# more useful to learn about the broad set of technology
# that exists rather than something specific
# having the ability to learn and push yourself to learn is important

powersummary <- summarize(carbondata,sum = sum(value),
                           .by = c(country,sector))
powersummary <- filter(powersummary,sector == "Power")
totalsummary <- summarize(carbondata,sum=sum(value),
                          .by = country)
totalsummary <- mutate(totalsummary,powerpercent =
                         (powersummary$sum/totalsummary$sum)*100)
percentorganized <- arrange(totalsummary,desc(powerpercent))

highpercent <- filter(percentorganized,powerpercent > 20.00)
lowpercent <- filter(percentorganized,powerpercent < 20.00)

highpercentcountries <- filter(sectordata,country %in% highpercent$country)
lowpercentcountries <- filter(sectordata,country %in% lowpercent$country)

# do some more t testing like i did before
t.test(highpercentcountries$Power,lowpercentcountries$Power,alternative = "two.sided")
# p value <2.23-16, mean of x is 0.24, mean of y is 0.048

t.test(highpercentcountries$Industry,lowpercentcountries$Industry,alternative = "two.sided")
# p value <2.2e-16
# mean of x is 0.15, mean of y is 0.06

t.test(highpercentcountries$Residential,lowpercentcountries$Residential,alternative = "two.sided")
# mean of x is 0.154, mean of y is 0.062
# p value <2.2e-16

t.test(highpercentcountries$`Ground Transport`,lowpercentcountries$`Ground Transport`,alternative = "two.sided")
# mean of x is 0.203, mean of y is 0.101
# p value <2.2e-16

t.test(highpercentcountries$`Domestic Aviation`,lowpercentcountries$`Domestic Aviation`,alternative = "two.sided")
# mean of x is 0.00199, mean of y is 0.00137
# p value of 2.2e-16

t.test(highpercentcountries$`International Aviation`,lowpercentcountries$`International Aviation`,alternative = "two.sided")
# mean of x is 0.030, mean of y is 0.0168
# p value <2.2e-16

#### Notes ####
## similar trends here as when i organized it per capita
## more power emissions lead to more industry, residential and ground transport emissions
# compared to the per capita t tests the high and low means are slightly closer to each other
#the two avaitions are consisently the least emitting and least affected by power
# international aviation is always drastically higher than domestic aviation
# likely due to international tourism to and from europe

# high percent
numerichighpercent <- select(highpercentcountries, -c(country,date,timestamp))
cor(numerichighpercent)

highpercentcoors <- numerichighpercent |>
  cor()|>
  melt()|>
  as.data.frame()

ggplot(highpercentcoors,aes(x= Var1, y = Var2,fill=value))+
  geom_tile()+
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                                              midpoint = 0.85)

# low percent
numericlowpercent <- select(lowpercentcountries, -c(country,date,timestamp))
cor(numericlowpercent)

lowpercentcoors <- numericlowpercent |>
  cor()|>
  melt()|>
  as.data.frame()

ggplot(lowpercentcoors,aes(x= Var1, y = Var2,fill=value))+
  geom_tile()+
  scale_fill_gradient2(low = "red", high = "blue",mid = "white",
                       midpoint = 0.9)
# in both the high percent and high sector datasets, the variables
# have more correlation with each other overall
# whereas in the lower ones there is less correlation between the variables


