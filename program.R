# install.packages(c("plyr", "reshape2", "dplyr", "lubridate","ggplot2","sf","rnatutalearth","rnaturalearthdata","tydyr"))
# install.packages(c("tidyr","rnaturalearth","tmap"))
library(data.table)
library(plyr)
library(reshape2)
library(dplyr)
library(lubridate)

# ##########3333333
library("ggplot2")
theme_set(theme_bw())
library("sf")
#
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyr)
library(dplyr)
library(tidyr)
library(dplyr)

# casesDT <- fread("time_series_covid19_confirmed_global.csv")
# deathsDT <- fread("time_series_covid19_deaths_global.csv")
confirmedData <- read.csv("time_series_covid19_confirmed_global.csv")
deathsData <- read.csv("time_series_covid19_deaths_global.csv")

#  Remove columns with names Province, State, Lat and Long.
confirmedDT <- setDT(confirmedData)[,c("Province.State","Lat","Long"):=NULL]
deathsDT <- setDT(deathsData)[,c("Province.State","Lat","Long"):=NULL]
# exit()
# DATA CLEANING: To create country level and global combined data
# Convert each data set from wide to long AND aggregate at country level
# confirmed <- confirmedDT %>% gather(key=date, value=confirmed ) %>% group_by(Country.Region, date)# %>% summarize(confirmed=sum(confirmed))
# deaths <- deathsDT %>% gather(key="date", value="deaths") %>% group_by("Country.Region", "date")# %>% summarize(deaths=sum(deaths))

# Convert data from wide to long format
# confirmed <- confirmedDT %>% gather(key="date", value="confirmed", -c(Country.Region)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))
# deaths <- deathsDT %>% gather(key="date", value="deaths", -c(Country.Region)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))
confirmedLong <- melt(confirmedDT, id.vars=c("Country.Region"), variable.name="date",value.name="confirmed")
deathsLong <- melt(deathsDT, id.vars=c("Country.Region"), variable.name="date",value.name="deaths")


# Rename variable Country.Region to Country.
setnames(confirmedLong, "Country.Region", "Country")
setnames(deathsLong, "Country.Region", "Country")

# Convert the variable date from character to a date object (check the
# mdy()function in R). In the initial datasets for example X1.22.20 refers to
# 22/1/2020.
# col <- c("date")
# casesLong[, (col):= lapply(.SD, mdy), .SDcols = col ]
# deathsLong[, (col):= lapply(.SD, mdy), .SDcols = col ]
# country$date <- country$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
confirmedLong$date <- confirmedLong$date %>% sub("X", "", .) %>% mdy()
deathsLong$date <- deathsLong$date %>% sub("X", "", .) %>% mdy()

# # Group by country and date
confirmedLongGrouped <- confirmedLong %>% group_by(Country, date)
deathsLongGrouped <- deathsLong %>% group_by(Country, date)
# These 2 are the previous implementation
# confirmedLong <- confirmedLong[, .(cases = sum(confirmed)),by = .( Country,date)]
# deathsLongGrouped <- deathsLong[, .(deaths = sum(deaths)),by = .(Country, date)]

# casesLongGrouped <- casesLong[, lapply(.SD,sum), by=.("date","Country")]
# deathsLong <- deathsLong[,,by = list("date","Country")]

# Merge the two datasets into one.
merged <- merge(confirmedLongGrouped, deathsLongGrouped)
country <- full_join(confirmedLongGrouped, deathsLongGrouped)

# Culumative data, so we find the latest date by country
# merged <- merged[,.SD[which.max(date)], by = Country][,date := NULL]
typeof(merged)
typeof(country)
total_per_country <- setDT(merged)[,.SD[which.max(date)], by = Country]
confirmedTotal <- total_per_country[,sum(confirmed)]
deathsTotal <- total_per_country[,sum(deaths)]


test <- merged[date == "2020-01-22"]
# Sort (again) by country and date
mer <- merged[order(c(Country, date))]
# Create two extra variables: confirmed.ind and deaths.inc with the
# daily confirmed cases and daily deaths respectively (check the lag() function
# in R).
m <- merged[, .(sum(confirmed),sum(deaths)), by = date]

confirmed.ind <- m
confirmed.ind$confirmed <- m$V1 - lag(m$V1, default = 0)
confirmed.ind$date <- m$date
deaths.inc <- m
deaths.inc$deaths <- m$V2 - lag(m$V2, default = 0)
deaths.inc$date <- m$date

merged <- merged %>% group_by(Country) %>% mutate(cumconfirmed=cumsum(confirmed), days = date - first(date) + 1)
world <- merged %>% group_by(date) %>% summarize(confirmed=sum(confirmed), deaths=sum(deaths)) %>% mutate(days = date - first(date) + 1)
# ############################
# start graphs
options(scipen=999)
dailyConfirmedPlot <- ggplot(confirmed.ind, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Date", y= "Cumulative Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

dailyConfirmedPlot

# Line graph of cases over time
# World confirmed
# ggplot(world, aes(x=days, y=confirmed)) + geom_count() +
#   theme_classic() +
#   labs(title = "Covid-19 Global Confirmed Cases", x= "Days", y= "Daily confirmed cases") +
#   theme(plot.title = element_text(hjust = 0.5))
# Ignore warning

# countrytotal <- country %>% group_by(Country) %>% summarize(cumconfirmed=sum(confirmed), cumdeaths=sum(deaths), cumrecovered=sum(recovered))

####### Start computing top countries by total cases
top_countries_by_total_cases <- total_per_country %>%
  group_by(Country) %>%
  summarize(total_cases = max(confirmed)) %>%
  top_n(10, total_cases)
# c <-

glimpse(top_countries_by_total_cases)
top_countries_full_history <- merged %>% filter(Country %in% top_countries_by_total_cases$Country)

# Using confirmed_cases_top7_outside_china, draw a line plot of
# cum_cases vs. date, grouped and colored by country
p <- ggplot(top_countries_full_history, aes(date, confirmed, color = Country, group = Country)) +
  geom_line() +
  ylab("Cumulative confirmed cases")

p
####### End computing top countries by total cases
#######
# Start graphs with season
# Given a date returns a season
getSeason <- function(DATES) {
    WS <- as.Date("2020-12-15", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2020-3-15",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2020-6-15",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2020-9-15",  format = "%Y-%m-%d") # Fall Equinox

    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(DATES, format="2020-%m-%d"))
    # d <- mdy(d)

    ifelse (d >= WS | d < SE, "Winter",
      ifelse (d >= SE & d < SS, "Spring",
        ifelse (d >= SS & d < FE, "Summer", "Fall")))
}



my.dates <- as.Date("2011-12-01", format = "%Y-%m-%d") + 0:60
head(getSeason(my.dates), 24)

# Create new variable: season
top_countries_full_history <- top_countries_full_history %>% mutate(season = getSeason(date))
confirmedCasesPerSeason <- ggplot(top_countries_full_history) +
  geom_line(aes(season,confirmed,color = season,group = season)) +
  ylab("Cumulative confirmed cases")

confirmedCasesPerSeason
# end graphs season
##########
# Basemap from package tmap
# install.packages("tmap")
exit()

library(tmap)
data(World)
class(World)

# Combine basemap data to covid data
countrytotal$Country[!country$Country %in% World$name]
list <- which(!merged$Country %in% World$name)
countrytotal$country <- as.character(countrytotal$Country.Region)
countrytotal$country[list] <-
  c("Andorra", "Antigua and Barbuda", "Bahrain",
    "Barbados", "Bosnia and Herz.", "Myanmar",
    "Cape Verde", "Central African Rep.", "Congo",
    "Dem. Rep. Congo", "Czech Rep.", "Diamond Princess",
    "Dominica", "Dominican Rep.", "Eq. Guinea",
    "Swaziland", "Grenada", "Holy See",
    "Korea", "Lao PDR", "Liechtenstein",
    "Maldives", "Malta", "Mauritius",
    "Monaco", "MS Zaandam", "Macedonia",
    "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines",
    "San Marino", "Sao Tome and Principe", "Seychelles",
    "Singapore", "S. Sudan", "Taiwan",
    "United States", "Palestine", "W. Sahara")
countrytotal$Country.Region[!countrytotal$country %in% World$name]
World$country <- World$name
worldmap <- left_join(World, countrytotal, by="country")
worldmap$cumconfirmed[is.na(worldmap$cumconfirmed)] <- 0

# Map
ggplot(data = worldmap) + geom_sf(aes(fill=cumconfirmed), color="black") +
  ggtitle("World Map of Confirmed Covid Cases",
          subtitle="Total Cases on April 20, 2020") +
  theme_bw()

exit()




# Final data: combine all three

# Date variable
# Fix date variable and convert from character to date
str(country) # check date character
country$date <- country$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
str(country) # check date Date
# Create new variable: number of days
country <- country %>% group_by(Country.Region) %>% mutate(cumconfirmed=cumsum(confirmed), days = date - first(date) + 1)

# Aggregate at world level
world <- country %>% group_by(date) %>% summarize(confirmed=sum(confirmed), cumconfirmed=sum(cumconfirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% mutate(days = date - first(date) + 1)
# Extract specific country: Italy
italy <- country %>% filter(Country.Region=="Italy")
summary(country)
by(country$confirmed, country$Country.Region, summary)
by(country$cumconfirmed, country$Country.Region, summary)
by(country$deaths, country$Country.Region, summary)
by(country$recovered, country$Country.Region, summary)
summary(world)
summary(italy)
exit()



# Name the variable with the cumulative confirmed cases as confirmed and
# the variable with the cumulative number of deaths as deaths.

# pre process for avoid sum overflow warning
# casesLong[, ("cases"):= lapply(.SD, as.numeric), .SDcols = "cases" ]
# deathsLong[, ("deaths"):= lapply(.SD, as.numeric), .SDcols = "deaths" ]
# //todo preprocess with date
# confirmed <-  cumsum(casesLong[, "cases"])
# deaths <- cumsum(deathsLong[, "deaths"])





col <- c("date")
# # asn0 <- casesLong["date" == "1/22/20" && "Country" == "Albania","date"]
casesLong[, (col):= lapply(.SD, mdy), .SDcols = col ]
deathsLong[, (col):= lapply(.SD, mdy), .SDcols = col ]
# # casesLong$date <- dmy(casesLong$date)
# # asn <- casesLong["date" == "1/22/20" && "Country" == "Albania","date"]



# Calculate counts (confirmed and deaths) for the whole world.

# deaths <- colSums(m[,deaths])



# lag
# confirmed <- colSums(casesLong[,"cases"])
# deaths <- colSums(deathsLong[,"deaths"])
# Convert datafrom wide to long
# names <- colnames(cases)
# n <- unlist((strsplit(names," ")))
# n <- laply((strsplit(names," ")),function(x) x)
# unlist
# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel",
# "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
# world <- ne_countries(scale = "medium", returnclass = "sf")
# class(world)
#
# ggplot(data = merged) +
#     geom_sf()


top_countries_by_total_deaths <- total_per_country %>%
  group_by(Country) %>%
  summarize(total_deaths = max(deaths)) %>%
  top_n(10, total_deaths)

top_countries_full_history_deaths <- merged %>% filter(Country %in% top_countries_by_total_deaths$Country)

# Using confirmed_cases_top7_outside_china, draw a line plot of
# cum_cases vs. date, grouped and colored by country
q <- ggplot(top_countries_full_history_deaths, aes(date, deaths, color = Country, group = Country)) +
  geom_line() +
  ylab("Cumulative confirmed cases")

q