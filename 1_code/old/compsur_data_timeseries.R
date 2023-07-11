# ------------
# Introduction
# ------------

## Compares timeseries expectations from different surveys

rm(list=ls())
NAME <- 'data_timeseries' ## Name of the R file goes here (without the file extension!)
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

f <- 'compsur'


## ---------------------
## Set working directory
## ---------------------
### The code below will traverse the path upwards until it finds the root folder of the project.

setwd(file.path(PROJECT_DIR, PROJECT))

## ----------------------------------
## Set  up pipeline folder if missing
## ----------------------------------
### The code below will automatically create a pipeline folder for this code file if it does not exist.

if (dir.exists(file.path('empirical', '2_pipeline',f))){
  pipeline <- file.path('empirical', '2_pipeline',f, NAME)
} else {
  pipeline <- file.path('2_pipeline',f, NAME)
}

if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  for (folder in c('out', 'store', 'tmp')){
    dir.create(file.path(pipeline, folder))
  }
}


# ---------
# Functions
# ---------

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

M <- group_by(F,date,survey) %>%
  summarise(meany = mean(y)) %>%
  spread(key = survey, value = meany)

# load ECFIN Data

T_ECFIN <- read_csv2(file.path('empirical', '0_data','manual','ECFIN','Q6_Mean.csv')) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# load inflation data

germany <- read_csv2(file.path('empirical', '0_data', 'external','Germany - HICP - Overall index.csv')) %>%
  mutate(date = as.yearmon(date, format = "%Y %m"), germany_hicp = as.numeric(s1))

us <- read_csv(file.path('empirical', '0_data', 'external','US_CPI.csv')) %>%
  mutate(date = as.yearmon(TIME, format = "%Y-%m"), us_cpi = as.numeric(Value))

euro <- read_csv2(file.path('empirical', '0_data', 'external','Euroarea - HICP - overall index.csv')) %>%
  mutate(date = as.yearmon(date, format = "%Y %m")) %>%
  arrange(date)

# merge

D <- merge(M,germany, by = "date", all = TRUE) %>%
  merge(us, by = "date", all = TRUE) %>%
  merge(euro, by = "date", all = TRUE) %>%
  select(date,`BOP-HH`,Michigan,FRBNY,germany_hicp,us_cpi,euro_hicp) %>%
  mutate(date = as.Date(format(date, "%Y-%m-%d")))

D <- merge(D,T_ECFIN, by = "date", all = TRUE)

# save as text

write.delim(D, file = file.path(pipeline, 'out', 'D.txt'), sep = "\t")
