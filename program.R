library(data.table)
library(plyr)
library(reshape2)
library(dplyr)
library(lubridate)
library(gridExtra)
library(ggrepel)
library(tidyr)
library(dplyr)
library(dplyr)
library(ggplot2)
library(readr)
library(maps)
library(viridis)
library(tidyverse)
library(scales)
library(lubridate)
library(gganimate)
library(gifski)
library(countrycode)

theme_set(theme_bw())

options(scipen=999)
confirmed.raw <- fread("time_series_covid19_confirmed_global.csv")
deaths.raw <- fread("time_series_covid19_deaths_global.csv")
# confirmedData <- read.csv("time_series_covid19_confirmed_global.csv")
# deathsData <- read.csv("time_series_covid19_deaths_global.csv")
#  Remove columns with names Province, State, Lat and Long.
confirmed.dt <- confirmed.raw[,c("Province/State","Lat","Long"):=NULL]
# confirmedDT <- setDT(confirmedData)[,c("Province.State"):=NULL]
deaths.dt <- deaths.raw[,c("Province/State","Lat","Long"):=NULL]
# is.data.table(confirmed.dt)
# is.data.table(deaths.dt)
# deathsDT <- setDT(deathsData)[,c("Province.State"):=NULL]
# DATA CLEANING: To create country level and global combined data
# Convert each data set from wide to long AND aggregate at country level
# confirmed <- confirmedDT %>% gather(key=date, value=confirmed ) %>% group_by(Country.Region, date)# %>% summarize(confirmed=sum(confirmed))
# deaths <- deathsDT %>% gather(key="date", value="deaths") %>% group_by("Country.Region", "date")# %>% summarize(deaths=sum(deaths))

# Convert data from wide to long format
# confirmed <- confirmedDT %>% gather(key="date", value="confirmed", -c(Country.Region)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))
# deaths <- deathsDT %>% gather(key="date", value="deaths", -c(Country.Region)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))
confirmed.long <- melt(confirmed.dt, id.vars=c("Country/Region"), variable.name="date",value.name="confirmed")
deaths.long <- melt(deaths.dt, id.vars=c("Country/Region"), variable.name="date",value.name="deaths")


# Rename variable Country.Region to Country.
setnames(confirmed.long, "Country/Region", "Country")
setnames(deaths.long, "Country/Region", "Country")

# confirmed.long[ Country == "US"][1:20] %>%
# kable('latex', booktabs=T, caption='Removed Columns Wide data (Confirmed, First 10 Columns only)') %>%
# kable_styling(font_size=5, latex_options = c('striped', 'hold_position', 'repeat_header'))
# exit()

# 4. Name the variable with the cumulative confirmed cases as confirmed and
# the variable with the cumulative number of deaths as deaths.
confirmed <- confirmed.long
deaths <- deaths.long
#5. Convert the variable date from character to a date object (check the
# mdy()function in R). In the initial datasets for example X1.22.20 refers to 22/1/2020.
# col <- c("date")
# casesLong[, (col):= lapply(.SD, mdy), .SDcols = col ]
# deathsLong[, (col):= lapply(.SD, mdy), .SDcols = col ]
# country$date <- country$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
# typeof(deaths$date)
confirmed$date %<>% sub("X", "", .) %>% mdy()
deaths$date %<>% sub("X", "", .) %>% mdy()

# 6 Group by country and date
# confirmed <- confirmed[,,by=list("Country","date")]
# deaths <- deaths %>% group_by(Country, date)
# These 2 are the previous implementation
# confirmedLong <- confirmedLong[, .(cases = sum(confirmed)),by = .( Country,date)]
# deathsLongGrouped <- deathsLong[, .(deaths = sum(deaths)),by = .(Country, date)]
confirmed <- confirmed[, .(confirmed = sum(confirmed)),by = c("Country","date")]
deaths <- deaths[, .(deaths = sum(deaths)),by = c("Country", "date")]
# casesLongGrouped <- confirmed[, lapply(.SD,sum), by=.("Country","date")]
# deathsLong <- deathsLong[,,by = list("date","Country")]

# 7 Merge the two datasets into one.
# merged <- merge.data.table(confirmed, deaths)
merged <- merge.data.table(confirmed, deaths, all=T)
# country.join <- full_join(confirmed, deaths)
# is.data.table(country)
# dim(merged)
# dim(country)
# dim(country.join)
# all.equal(merged, country)
# all.equal(country.join, country)
# 8 Calculate counts (confirmed and deaths) for the whole world.
world <- merged[, .(Country = "World", confirmed = sum(confirmed),deaths = (sum(deaths))),by = c("date")]
merged <- merged %>% rbind(world)
#9 Sort (again) by country and date
# merged <- merged[,order(c("Country", "date")),]
setorder(merged,Country,date)

# test <- merged[date == "2020-01-22"]
# Create two extra variables: confirmed.ind and deaths.inc with the
# daily confirmed cases and daily deaths respectively (check the lag() function
# in R).
# is.data.table(merged)

confirmed.ind <- copy(world)
confirmed.ind[,c("deaths","Country"):=NULL]
confirmed.ind$confirmed <- world$confirmed - lag(world$confirmed, default = 0)
c <- lag(world$confirmed, default = 0)
deaths.inc <- copy(world)
deaths.inc[,c("confirmed","Country"):=NULL]
deaths.inc$deaths <- deaths.inc$deaths - lag(deaths.inc$deaths, default = 0)
# deaths.inc$date <- m$date
# is.data.table(deaths.inc)
is.data.table(confirmed.ind)

merged  <- merged %>% mutate(confirmed.ind =  confirmed - lag(confirmed,default = 0) )
merged  <- merged %>% mutate(deaths.inc = deaths - lag(deaths,default = 0))
setorder(merged,Country,date)
# Culumative data, so we find the latest date by country
# merged <- merged[,.SD[which.max(date)], by = Country][,date := NULL]
typeof(merged)
# typeof(country)
total_per_country <- setDT(merged)[,.SD[which.max(date)], by = Country]
confirmedTotal <- total_per_country[,sum(confirmed)]
deathsTotal <- total_per_country[,sum(deaths)]

min.date <- min(merged$date)
max.date <- max(merged$date)
min.date.txt <- min.date %>% paste('UTC')
max.date.txt <- max.date %>% paste('UTC')


confirmed.ind[date == mdy("12-10-2020"), "confirmed" := 685000]
plot1 <- ggplot(confirmed.ind, aes(x=date, y=confirmed)) +
geom_point() + geom_smooth() +
xlab('') + ylab('Confirmed (Log)') + labs(title='Daily Cases') +
  # scale_y_log10() +
theme(axis.text.x=element_text(angle=45, hjust=1))
# plot1
plot2 <- ggplot(world, aes(x=date, y=confirmed)) +
geom_point() + geom_smooth() +
xlab('') + ylab('Confirmed (Log)') + labs(title='Cumulative Confirmed Cases') +
  # scale_y_log10() +
theme(axis.text.x=element_text(angle=45, hjust=1))

plot3 <- ggplot(deaths.inc, aes(x=date, y=deaths)) +
geom_point() + geom_smooth() +
xlab('') + ylab('Deaths (Log)') + labs(title='Daily Deaths') +
  # scale_y_log10() +
theme(axis.text.x=element_text(angle=45, hjust=1))
# plot1
plot4 <- ggplot(world, aes(x=date, y=deaths)) +
geom_point() + geom_smooth() +
xlab('') + ylab('Deaths (Log)') + labs(title='Cumulative Deaths') +
  # scale_y_log10( ) +
theme(axis.text.x=element_text(angle=45, hjust=1))
## show two plots side by side
# png("DailyVsCumulative.png")
grid.arrange(plot1, plot2, ncol=20)
# dev.off()
# ggsave("DailyVsCumulative")
grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol=2)
g.log <- arrangeGrob(plot1, plot2, plot3, plot4, nrow = 2,ncol=2)
# ggsave(file="DailyVsCumulativeConfirmedLog.png", g.log)
# calculate ratio
g.log
exit()
world <- world %>% mutate(ratio = 100*deaths/confirmed %>% round(1))
world[is.na(world)] <- 0

plot5 <- ggplot(world, aes(x=date)) +
geom_line(aes(y=ratio, colour='Daily')) +
xlab('') + ylab('Death Rate (%)') + labs(title='Overall') +
theme(legend.position='bottom', legend.title=element_blank(),
legend.text=element_text(size=8),
legend.key.size=unit(0.5, 'cm'),
axis.text.x=element_text(angle=45, hjust=1)) +
ylim(c(0, 99))

# plot5
####### Start computing top countries by total cases
top_countries_by_total_cases <- total_per_country %>%
  group_by(Country) %>%
  summarize(total_cases = max(confirmed)) %>%
  top_n(11, total_cases)
top_countries_full_history <- merged %>% filter(Country %in% top_countries_by_total_cases$Country)
# c <-
is.data.table(top_countries_by_total_cases)

glimpse(top_countries_by_total_cases)
top_countries_full_history <- merged %>% filter(Country %in% top_countries_by_total_cases$Country)

is.data.table(top_countries_full_history)
outfile1 = "covid-19.cumulative-deaths-by-date-log10.png"
colscheme    = c("#d72123", "#d87632", "#cac654", "#589A5D", "#4781A7", "#816fa3", "#d368a1", "grey50")
#
adaptiveCols = colorRampPalette(rep(colscheme,1))(length(levels(merged$Country)))
p1 <- ggplot(top_countries_full_history, aes(x=date, y=deaths, group=Country, label="hi", color=Country)) +
geom_path(mapping=aes(group=Country, color=Country), alpha=0.75) +
geom_point(aes(color=Country), alpha=0.75, size=0.8) +
geom_text_repel(data          = subset(top_countries_full_history, date==max.date),
                aes(label     = "hi"),
                force         = 3,
                xlim          = c(as.Date("2020-07-01"), as.Date("2020-09-14")),
                size          = 1.75,
                segment.size  = 0.25,
                segment.alpha = 0.25) +
theme_bw() +
# scale_color_manual(values = adaptiveCols) +
theme(axis.text.x  = element_text(size=6, colour="black"),
      axis.text.y  = element_text(size=11, colour="black"),
      axis.title.x = element_text(size=12, colour="black"),
      axis.title.y = element_text(size=12, colour="black"),
      plot.title   = element_text(size=12, colour="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "none") +
xlab("Date") +
scale_x_date(date_labels = "%b %d", date_breaks = "1 week", limits=as.Date(c("2020-04-01",NA))) +
expand_limits(x = as.Date("2020-09-14")) +
ylab("Cumulative Deaths") +
ggtitle("hello") +
scale_y_log10(breaks=c(0, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000)) +
theme(aspect.ratio=0.75)
p1
ggsave(outfile1, plot=p1, height=6, width=8)
#############################
top_countries_full_history[confirmed.ind<0, confirmed.ind:=0][deaths.inc <0 , deaths.inc:= 0]
top_countries_full_history <- top_countries_full_history[Country != "World"][confirmed.ind < 400000][confirmed.ind > 100]

top_countries_full_history <- top_countries_full_history %>% mutate(season = getSeason(date))
top_countries_full_history <- top_countries_full_history[season == "Summer"]
top_countries_full_history <- setDT(top_countries_full_history)

colscheme    = c("#d72123", "#d87632", "#cac654", "#589A5D", "#4781A7", "#816fa3", "#d368a1", "grey50")
adaptiveCols = colorRampPalette(rep(colscheme,1))(10)
p2 <- ggplot(top_countries_full_history, aes(x=date, y=confirmed.ind, group=Country, label=Country, color=Country)) +
geom_path(mapping=aes(group=Country, color=Country), alpha=0.6) +
geom_point(aes(color=Country), alpha=0.6, size=2) +
  geom_text_repel(data          = top_countries_full_history[,.SD[which.max(date)], by = Country],
                aes(label     = Country),
                force         = 2,
                angle         = 0,
                nudge_y       = 0.25,
                size          = 1.5,
                segment.size  = 0.25,
                segment.alpha = 0.25) +
theme_bw() +
scale_color_manual(values = adaptiveCols) +
theme(axis.text.x  = element_text(size=10, colour="black"),
      axis.text.y  = element_text(size=11, colour="black"),
      axis.title.x = element_text(size=12, colour="black"),
      axis.title.y = element_text(size=12, colour="black"),
      plot.title   = element_text(size=12, colour="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "none") +
ylab("Daily Confirmed Cases") +
scale_y_log10(breaks=c(50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000), limits=c(25,4000)) +
xlab("Date") +
scale_x_date(date_labels = "%b %d", date_breaks = "1 week", limits=as.Date(c("2020-06-15",NA)),guide = guide_axis(n.dodge=3)) +
# expand_limits(x = as.Date("2020-07-10")) +
ggtitle("Daily Confirmed Cases for top Countries
             during Summer") +
theme(aspect.ratio=0.75)
p2
ggsave(outfile1, plot=p2, height=6, width=8)



exit()
outfile1 = "covid-19.cumulative-deaths-from-50th-death-log10.png"
p2 <- ggplot(visdat2, aes(x=Days.from.50th.Death, y=deaths, group=Country, label="Label", color=Country)) +
geom_path(mapping=aes(group=Country, color=Country), alpha=0.5) +
geom_point(aes(color=Country.Region), alpha=0.5, size=1.5) +
geom_text_repel(data          = subset(merged, date== max.date),
                aes(label     = Label),
                nudge_y       = 0,
                nudge_x       = 20 + 0.25*subset(merged, date== max.date)$Days.from.50th.Death,
                force         = 1,
                xlim          = c(100,max(subset(merged, date== max.date)$Days.from.50th.Death)+70),
                direction     = "x",
                angle         = 0,
                size          = 1.85,
                segment.size  = 0.25,
                segment.alpha = 0.25) +
theme_bw() +
scale_color_manual(values = adaptiveCols2) +
theme(axis.text.x  = element_text(size=8, colour="black"),
      axis.text.y  = element_text(size=11, colour="black"),
      axis.title.x = element_text(size=12, colour="black"),
      axis.title.y = element_text(size=12, colour="black"),
      plot.title   = element_text(size=12, colour="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "none") +
xlab(bquote("Days from 50"^"th"*" Death")) +
ylab("Cumulative Deaths") +
scale_y_log10(breaks=c(0, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000)) +
coord_cartesian(ylim = c(45, max(merged$Cumulative.Deaths))) +
scale_x_continuous(breaks = seq(0, max(na.omit(merged$Days.from.50th.Death)+70), by = 5), limits=c(0,max(subset(merged, date == max.date)$Days.from.50th.Death)+70)) +
ggtitle("title") +
theme(aspect.ratio=0.75)
ggsave(outfile1, plot=p2, height=6, width=8)
exit()
# Using confirmed_cases_top7_outside_china, draw a line plot of
# cum_cases vs. date, grouped and colored by country
p <- ggplot(top_countries_full_history, aes(date, confirmed, color = Country, group = Country)) +
  geom_line() +
  ylab("Cumulative confirmed cases")

# p
# merged$days <- merged$date - min.date

bar  <- ggplot(top_countries_by_total_cases, aes(top_countries_by_total_cases$Country, as.numeric(top_countries_by_total_cases$total_cases))) +
  geom_col(fill = 'firebrick') +
  theme_minimal(base_size = 14) +
  xlab(NULL) + ylab(NULL) +
  # labs(caption = paste("accessed date:", time(x)))
      scale_x_discrete(limits = top_countries_by_total_cases$Country) + coord_flip()

###### End computing top countries by total cases

# todo add rollmean to graph from above
dailyConfirmedPlot <- ggplot(confirmed.ind, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Date", y= "Cumulative Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

dailyConfirmedPlot

# Line graph of cases over time
####### Start computing top countries by total cases
top_countries_by_total_cases <- total_per_country %>%
  group_by(Country) %>%
  summarize(total_cases = max(confirmed)) %>%
  top_n(10, total_cases)
# c <-

summarized <- merged[,.SD[which.max(date)], by = Country][,date := NULL][order(-confirmed)]
top.countries <- summarized[,head(.SD,11)]
# world <- merged[, .(Country = "World", confirmed = sum(confirmed),deaths = (sum(deaths))),by = c("date")]
x  <- top.countries[, .(Country = "Other Countries", confirmed = sum(summarized$confirmed) - sum(top.countries$confirmed),deaths = sum(summarized$deaths) - sum(top.countries$deaths))]
top.countries <- rbind(top.countries,x)
top.countries <- top.countries %>% mutate(ratio = deaths/confirmed)
# tail <- summarized - top.countries
# glimpse(top_countries_by_total_cases)
# top_countries_full_history <- merged %>% filter(Country %in% top_countries_by_total_cases$Country)
## bar chart

# data.latest <- summarized %>%
# mutate(country=ifelse(ranking <= 11, as.character(country), 'Others')) %>%
# mutate(country=country %>% factor(levels=c(top.countries, 'Others')))

plot.top.countries <- function(column, title){
    p <- ggplot(top.countries[Country != "World"],aes(x=Country, y=get(column), fill=Country, group=Country)) +
geom_bar(stat='identity') +
geom_text(aes(label=get(column), y=get(column)), size=2, vjust=0) +
xlab('') + ylab('') +
labs(title=paste0(title, max.date.txt)) +
scale_fill_discrete(name='Country', labels=aes(get(column))) +
theme(legend.title=element_blank(),
legend.position='none',
plot.title=element_text(size=11),
axis.text=element_text(size=7),
axis.text.x=element_text(angle=45, hjust=1))
# +
facet_wrap(~type, ncol=1, scales='free_y')

 p
}

 colors <- rep(c('grey', 'yellow', 'purple', 'orange', 'green', 'red', 'blue', 'black'), 3)
linetypes <- rep(c("solid", "dashed", "dotted"), each=8)
colors <- rep(c('black', 'blue', 'red', 'green', 'orange', 'purple', 'yellow', 'grey'), 3)
 r <-factor(top.countries$Country)
# exit()
top.countries.full.history <- merged[ Country %in% top.countries$Country][Country != 'World']
# top.countries.full.history[,CountryLev := factor(top.countries$Country) ]

p <- ggplot(top.countries.full.history,aes(x=confirmed, y=deaths, group=Country)) +
geom_line(aes(color=Country, linetype=Country)) +
xlab('Total Confirmed') + ylab('Total Deaths') +
scale_linetype_manual(values=linetypes) +
scale_color_manual(values=colors) +
theme(legend.title=element_blank(),
legend.text=element_text(size=8),
legend.key.size=unit(0.5, 'cm'))
p + labs(title=paste0('Top 20 Countries'))
p + scale_x_log10() + scale_y_log10() +
labs(title=paste0('Top 20 Countries (log scale)'))
# p
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

  # p <- ggplot(top.countries.full.history[Country != "World"],aes(x=season, y=get(confirmed), fill=season, group=season))
# p
merged  <- merged %>% mutate(season = getSeason(date))
summarizedseason <- merged[,.SD[which.max(date)], by = c("season","Country")][,date := NULL][order(-confirmed)]
    title <- "Total confirmed cases per season until "
    p <- ggplot(summarizedseason["Country" != "World"],aes(x=season, y=confirmed, fill=season, group=season)) +
geom_bar(stat='identity') +
# geom_text(aes(label=confirmed), size=2, vjust=0) +
xlab('Seasons') + ylab('Confirmed cases') +
labs(title=paste0(title, max.date.txt)) +
scale_fill_discrete(name='season' ) +
theme(legend.title=element_blank(),
legend.position='none',
plot.title=element_text(size=11),
axis.text=element_text(size=7),
axis.text.x=element_text(angle=45, hjust=1))
# +
facet_wrap(~type, ncol=1, scales='free_y')

 axis.text.x=element_text(angle=45, hjust=1))
p
# Create new variable: season
# todo see how to fix lines fro aboce
temp <- merge(confirmed.ind, deaths.inc) %>% mutate(season = getSeason(date))
 confirmedCasesPerSeason
top.countries.full.history[,Continent := countrycode(sourcevar = Country,
                            origin = "country.name",
                            destination = "continent") ]
total_per_country <- total_per_country %>% mutate(ratio = deaths/confirmed)
countriesRatio <- total_per_country %>%
  group_by(Country) %>%
  summarize(ratio = max(ratio))
top_countries_ratio <- countriesRatio %>% top_n(10, ratio)
low_countries_ratio <- countriesRatio %>% top_n(-10, ratio)
 install.packages(maps)
map.world <- map_data(map="world")

#Add the data you want to map countries by to map.world
#In this example, I add lengths of country names plus some offset
map.world$name_len <- nchar(map.world$region) + sample(nrow(map.world))




world <- setDT(map_data("world"))
setnames(world, "region", "Country")
summarized[Country == "US", Country := "USA"]
# is.data.table(world)
world  <- world[,.( long =mean(long), lat =  mean(lat)),by = .( Country)]
summarized <- inner_join(world, summarized , by = "Country")
scale_fill_viridis_c(option = "C")

diff <- setdiff(world$region, total_per_country$confirmed)

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

p3 <- total_per_country %>%
  group_by(Country) %>%
  summarise(Death = max(deaths), Confirmed = max(confirmed)) %>%
  arrange(desc(Confirmed))
# #####################3
# Graph map
## get the COVID-19 data
# datacov <- read_csv("time_series_19-covid-Confirmed.csv")
## get the world map
world <- map_data("world")
# cutoffs based on the number of cases
# todo fix plot
mybreaks <- c(1, 20, 100, 1000, 50000)
summarized[Country == "USA", Country:= "US"]


  labs(caption = "The distributation of total cases across Europe.")
m
breaks.confirmed <- c(5e3, 1e4, 2e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7)
top.countries <-top.countries[Country != "World" & Country !="Other Countries"]

plot1 <- top.countries %>% ggplot(aes(x=confirmed, y=deaths, col=ratio, size=ratio)) +
  scale_size(name='', trans='log2', breaks=breaks.confirmed) +
geom_text(aes(label=Country), size=2.5, check_overlap=T, vjust=-0.1, hjust=-0.1) +
  # scale_x_continuous(expand = c(.1, .1)) +
geom_point() +
xlab('Total Confirmed') + ylab('Total Deaths') +
labs(col="Death Rate (%)") +
scale_color_gradient(low='#56B1F7', high='#132B43') +
scale_x_log10() + scale_y_log10() +
labs(title=paste0('Deaths vs Confirmed cases (log scale)'))
# plot1$layout$clip[plot1$layout$name == "panel"] <- "off"
plot1
exit()
plot2 <- df %>% ggplot(aes(x=new.confirmed, y=new.deaths, col=death.rate, size=current.confirmed)) +
scale_size(name='Current Confirmed', trans='log2', breaks=breaks.confirmed) +
geom_text(aes(label=country), size=2.5, check_overlap=T, vjust=-1.6) +
geom_point() +
xlab('New Confirmed') + ylab('New Deaths') +
labs(col="Death Rate (%)") +
scale_color_gradient(low='#56B1F7', high='#132B43') +
scale_x_log10() + scale_y_log10() +
labs(title=paste0('Top 20 Countries - New Confirmed vs New Deaths (log scale)'))

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
