library(tidyverse)
library(tscount)
# for Poisson and NB regression
library(MASS)
# for zero-inflation
library(pscl)

# load data from URL
dat <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/data.csv")

# make a histogram of cases in year 2020
dat %>% 
  filter(year == 2020) %>% 
  ggplot() +
  geom_histogram(aes(x = cases))

# GOAL: to filter the data set where countiesAndTerritories matches with the countries in the list below
countries <- "China, Denmark, Estonia, France, Germany, Italy, Malaysia, Philippines, Qatar, South_Korea, Sri_Lanka, Sweden, Taiwan, Thailand, United_Arab_Emirates, United_Kingdom, United_States_of_America, Vietnam"
list.countries <- str_split(countries, ", ")

# change format and data type of dateRep
dat$dateRep <- as.Date(dat$dateRep, format = "%d/%m/%Y")

# filter for obs with the countries above, for year 2020
selected.dat <- dat %>%
  filter(countriesAndTerritories %in% list.countries[[1]], year == 2020)

# make histogram of new filtered data
selected.dat %>% 
  ggplot() +
  geom_histogram(aes(x = cases))

selected.dat %>% 
  filter(deaths == 0) %>% 
  count()

# select only China
china <- selected.dat %>% filter(countriesAndTerritories == "China")

ts.china <- ts(china %>% select(dateRep, deaths))


# trying countreg

