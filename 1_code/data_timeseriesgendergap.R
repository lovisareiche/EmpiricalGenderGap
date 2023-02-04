# ------------
# Introduction
# ------------

## Compares timeseries expectations from different surveys

rm(list=ls())
NAME <- 'data_timeseriesgendergap' ## Name of the R file goes here (without the file extension!)
PROJECT <- 'EmpiricalGenderGap'
PROJECT_DIR <- 'D:/Lovisa/Studium/Oxford/Department of Economics/DPhil' ## Change this to the directory in which your project folder is located, make sure to avoid using single backslashes (i.e. \)!

# ------------
# Preamble
# ------------

## -------
## Imports
## -------
### All the library imports go here

library('tidyverse')
library(moments)
library(zoo) # for date vector
library(lattice) # for plotting
library("ggplot2")
library(xtable)
library(caroline)

## --------
## Settings
## --------
### Any settings go here

S <- c('BOP-HH','Michigan','FRBNY')
# BOP-HH, Michigan, FRBNY - all you have


## ---------------------
## Set working directory
## ---------------------
### The code below will traverse the path upwards until it finds the root folder of the project.

setwd(file.path(PROJECT_DIR, PROJECT))

## ----------------------------------
## Set  up pipeline folder if missing
## ----------------------------------
### The code below will automatically create a pipeline folder for this code file if it does not exist.

if (dir.exists(file.path('empirical', '2_pipeline'))){
  pipeline <- file.path('empirical', '2_pipeline', NAME)
} else {
  pipeline <- file.path('2_pipeline', NAME)
}

if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  for (folder in c('out', 'store', 'tmp')){
    dir.create(file.path(pipeline, folder))
  }
}


### The code below will automatically create an output folder for this code file if it does not exist.

if (!dir.exists(file.path('empirical', '3_output','results',NAME))) {
  dir.create(file.path('empirical', '3_output','results',NAME))
}


# ---------
# Functions
# ---------

# function for moving average
moving_average <- function(x, n = 6) {             # Create user-defined function
  stats::filter(x, rep(1 / n, n), sides = 2)
}


# ---------
# Main code
# ---------


## -- Load data from pipeline folder --

for(i in 1:length(S)){
  T <- read_csv(file.path('empirical', '2_pipeline', 'data_align','out',S[i], 'T.csv')) %>%
    mutate(survey = S[i], date = as.yearmon(paste(year, month), format = "%Y %m"))
  assign(paste("T_",S[i],sep = ""),T)
  if(i==1){
    F <- T
  }
  if(i>1){
    F <- rbind(F,T)
  }
}

rm(T)

# Create data in wide format

M <- group_by(F,date,survey,female) %>%
  summarise(meany = mean(y)) %>%
  ungroup %>%
  mutate(key = paste(survey,female,sep = "_")) %>%
  select(-female) %>%
  select(-survey) %>%
  spread(key = key, value = meany) %>%
  mutate(date = as.Date(format(date, "%Y-%m-%d"))) %>%
  # take difference: women - men
  mutate(Michigan = Michigan_1-Michigan_0, FRBNY = FRBNY_1-FRBNY_0, BOP = `BOP-HH_1`-`BOP-HH_0`) %>%
  mutate(Michigan_mov = moving_average(Michigan),FRBNY_mov = moving_average(FRBNY),BOP_mov = moving_average(BOP)) %>%
  select(date,Michigan,Michigan_mov,FRBNY,FRBNY_mov,BOP,BOP_mov)

# load also ECFIN data

T_ECFIN <- read_csv2(file.path('empirical', '0_data','manual','ECFIN','Q6.csv')) %>%
  mutate(date = as.Date(date), ECFIN = f_mean-m_mean, ECFIN_mov = moving_average(f_mean-m_mean)) %>%
  arrange(date) %>%
  select(date,ECFIN,ECFIN_mov)

M <- merge(M,T_ECFIN, by = "date", all = TRUE)

# load in food inflation data

T <- read_csv2(file.path('empirical', '0_data', 'external', 'OECD_CPI_data.csv')) %>%
  mutate(date = as.Date(as.yearmon(TIME, format = "%Y-%m")), cpi = as.numeric(Value))

germany_f <- filter(T,LOCATION == "DEU" & SUBJECT == "FOOD") %>%
  select(date,cpi) %>%
  rename(cpi_food_germany = cpi)
germany_t <- filter(T,LOCATION == "DEU" & SUBJECT == "TOT") %>%
  select(date,cpi) %>%
  rename(cpi_tot_germany = cpi)

us_f <- filter(T,LOCATION == "USA" & SUBJECT == "FOOD") %>%
  select(date,cpi) %>%
  rename(cpi_food_us = cpi)
us_t <- filter(T,LOCATION == "USA" & SUBJECT == "TOT") %>%
  select(date,cpi) %>%
  rename(cpi_tot_us = cpi)

euro_f <- filter(T,LOCATION == "EA19" & SUBJECT == "FOOD") %>%
  select(date,cpi) %>%
  rename(cpi_food_euro= cpi)
euro_t <- filter(T,LOCATION == "EA19" & SUBJECT == "TOT") %>%
  select(date,cpi) %>%
  rename(cpi_tot_euro= cpi)

# merge all together

D <- merge(M,germany_f, by = "date", all = TRUE) %>%
  merge(germany_t, by = "date", all = TRUE) %>%
  merge(us_f, by = "date", all = TRUE) %>%
  merge(us_t, by = "date", all = TRUE) %>%
  merge(euro_f, by = "date", all = TRUE) %>%
  merge(euro_t, by = "date", all = TRUE) %>%
  select(date,BOP,Michigan,FRBNY,ECFIN,BOP_mov,Michigan_mov,FRBNY_mov,ECFIN_mov,cpi_food_germany,cpi_food_us,cpi_food_euro,cpi_tot_germany,cpi_tot_us,cpi_tot_euro) %>%
  mutate(date = as.Date(format(date, "%Y-%m-%d")))

# save as text

write.delim(D, file = file.path(pipeline, 'out', 'D.txt'), sep = "\t")

# save as csv

write.csv(D, file = file.path(pipeline, 'out', 'D.csv'))