library(shiny)
library(tidyverse)
library(stats)
library(tidyverse)
library(lubridate)
library(broom)
library(gpclib)
#library(readr)
library(rgdal)
library(maptools)
library(mapdata)
library(rgeos)
#library(mapproj)
#library(ggmap)

gpclibPermit()

#loadData for temp anomalies
monthly_temps <- read_csv("monthly_global_land_and_ocean_temperature_anomalies.csv")

#number each row so that they can be plotted in order by 1
monthly_temps <- mutate(monthly_temps, month_number = row_number())

#convert date strings/integers to objects of date class
monthly_temps <- mutate(monthly_temps, date = ymd(YearMonth, truncated=1))

#extract year and month from date
monthly_temps <- mutate(monthly_temps,
                        year=year(date),
                        month=month(date, label = TRUE))

#rename Value variable to temperature_anomaly, remove yearMonth and date
monthly_temps <- rename(monthly_temps, temperature_anomaly = Value)
monthly_temps <- select(monthly_temps, -c(YearMonth, date))

#give average temperature anomaly by year
yearly_temps <- aggregate( temperature_anomaly ~ year , monthly_temps , mean )

#load co2 emission data
co2data <- read_csv("carbon_dioxide_total_emissions.csv")

#drop all years that aren't on the other data set
co2data <- co2data[, -c(2:115)]
co2data <- co2data[, -c(132:133)]

mapco2data <- co2data

#load row names into a variable
n <- co2data$'CO2 emission total'

#flip data frame sideways (making years in a column) and reassign the colnames with old rownames
co2data <- as.data.frame(t(co2data[,-1]))
colnames(co2data) <- n

#make year variable
co2data$year <- factor(row.names(co2data))

#take co2 average in world, discounting NAs
co2data$worldAvg <- rowMeans(co2data[,1:275], na.rm=TRUE)

save(co2data, file = "co2data.RData")

#get rid of countries so we only have year
co2data <- co2data[, -c(0:275)]

#convert year to numeric
co2data$year <- as.numeric(as.character(co2data$year))

#plot 1
regresData <- inner_join(co2data, yearly_temps, by = "year")

temp <- data.frame(x=regresData$year,y=regresData$temperature_anomaly)
coAvg <- data.frame(x=regresData$year,y=regresData$worldAvg)


#plot 2
spiralData <- inner_join(co2data, monthly_temps, by = "year")


#plot 3
## Mapping Data

map <- map_data("world")

names(mapco2data)[1]<- "region"

# make region names lowercase in both files
map$region <- tolower(map$region)
mapco2data$region <- tolower(mapco2data$region)

# clean region names
mapco2data$region[mapco2data$region == "united states"] <- "usa"
mapco2data$region[mapco2data$region == "congo, dem. rep."] <- "democratic republic of the congo"
mapco2data$region[mapco2data$region == "congo, rep."] <- "republic of congo"
mapco2data$region[mapco2data$region == "cote d'ivoire"] <- "ivory coast"
mapco2data$region[mapco2data$region == "kyrgyz republic"] <- "kyrgyzstan"
mapco2data$region[mapco2data$region == "lao"] <- "laos"
mapco2data$region[mapco2data$region == "macedonia, fyr"] <- "macedonia"
mapco2data$region[mapco2data$region == "czechoslovakia"] <- "slovakia"
mapco2data$region[mapco2data$region == "united kingdom"] <- "uk"

# keep ussr row for merging into russia later
ussr_mapco2data <- filter(mapco2data, region == "ussr")
ussr_mapco2data <- gather(ussr_mapco2data, year, co2, "1880":"2009")
ussr_mapco2data$year <- as.numeric(ussr_mapco2data$year)
ussr_mapco2data <- filter(ussr_mapco2data, year < 1992)
ussr_mapco2data$region[ussr_mapco2data$region == "ussr"] <- "russia"

# join map and mapco2data
joinedMapData <- left_join(map, mapco2data, by = "region")

# reshape long
joinedMapData_long <- gather(joinedMapData, year, co2, "1880":"2009") #, factor_key=TRUE)
joinedMapData_long$year <- as.numeric(joinedMapData_long$year)

# merge in ussr data to russia
joinedMapData_long <- left_join(joinedMapData_long , ussr_mapco2data , by = c("region","year"))
joinedMapData_long <- mutate(joinedMapData_long , co2.x= ifelse(region == "russia" & is.na(co2.x), co2.y ,co2.x))
joinedMapData_long <- joinedMapData_long[, -9]
colnames(joinedMapData_long)[colnames(joinedMapData_long)=="co2.x"] <- "co2"
joinedMapData_long$year <-as.factor(joinedMapData_long$year)