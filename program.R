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


casesDT <- fread("time_series_covid19_confirmed_global.csv")
deathsDT <- fread("time_series_covid19_deaths_global.csv")

#  Remove columns with names Province, State, Lat and Long.
casesDT[,c("Province/State","Lat","Long"):=NULL]
deathsDT[,c("Province/State","Lat","Long"):=NULL]

# Convert data from wide to long format
casesLong <- melt(casesDT, id.vars=c("Country/Region"), variable.name="date",value.name="cases")
deathsLong <- melt(deathsDT, id.vars=c("Country/Region"), variable.name="date",value.name="deaths")

# Rename variable Country.Region to Country.
setnames(casesLong, "Country/Region", "Country")
setnames(deathsLong, "Country/Region", "Country")

# Name the variable with the cumulative confirmed cases as confirmed and
# the variable with the cumulative number of deaths as deaths.

# pre process for avoid sum overflow warning
# casesLong[, ("cases"):= lapply(.SD, as.numeric), .SDcols = "cases" ]
# deathsLong[, ("deaths"):= lapply(.SD, as.numeric), .SDcols = "deaths" ]
# //todo preprocess with date
# confirmed <-  cumsum(casesLong[, "cases"])
# deaths <- cumsum(deathsLong[, "deaths"])



# Convert the variable date from character to a date object (check the
# mdy()function in R). In the initial datasets for example X1.22.20 refers to
# 22/1/2020.

col <- c("date")
# # asn0 <- casesLong["date" == "1/22/20" && "Country" == "Albania","date"]
casesLong[, (col):= lapply(.SD, mdy), .SDcols = col ]
deathsLong[, (col):= lapply(.SD, mdy), .SDcols = col ]
# # casesLong$date <- dmy(casesLong$date)
# # asn <- casesLong["date" == "1/22/20" && "Country" == "Albania","date"]

# # Group by country and date
casesLongGrouped <- casesLong[, .(cases = sum(cases)),by = .( Country,date)]
deathsLongGrouped <- deathsLong[, .(deaths = sum(deaths)),by = .(Country, date)]
# casesLongGrouped <- casesLong[, lapply(.SD,sum), by=.("date","Country")]
# deathsLong <- deathsLong[,,by = list("date","Country")]

# Merge the two datasets into one.
merged <- merge(casesLongGrouped, deathsLongGrouped)

# Calculate counts (confirmed and deaths) for the whole world.
# Culumative data, so we find the latest date by country
# merged <- merged[,.SD[which.max(date)], by = Country][,date := NULL]
total_per_country <- merged[,.SD[which.max(date)], by = Country]
confirmed <- total_per_country[,sum(cases)]
deaths <- total_per_country[,sum(deaths)]
# deaths <- colSums(m[,deaths])

# Sort (again) by country and date
mer <- merged[order(c(Country, date))]

# Create two extra variables: confirmed.ind and deaths.inc with the
# daily confirmed cases and daily deaths respectively (check the lag() function
# in R).
m <- merged[, .(sum(cases),sum(deaths)), by = date]
confirmed.ind <- m$V1 - lag(m$V1, default = 0.00)
deaths.inc <- m$V2 - lag(m$V2, default = 0.00)
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
# top_countries_by_total_cases <- mer %>%
#   group_by(country) %>%
#   summarize(total_cases = max(cum_cases)) %>%
#   top_n(7, total_cases)