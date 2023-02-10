# ------------
# Introduction
# ------------

## Compares timeseries expectations from different surveys

rm(list=ls())
NAME <- 'code02_tsgendergap' ## Name of the R file goes here (without the file extension!)
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


### The code below will automatically create an output folder for this code file if it does not exist.

if (!dir.exists(file.path('empirical', '3_output','results',f,NAME))) {
  outline <- file.path('empirical', '3_output','results',f,NAME)
  dir.create(outline)
}


# ---------
# Functions
# ---------

# function for moving average
moving_average <- function(x, n = 3) {             # Create user-defined function
  stats::filter(x, rep(1 / n, n), sides = 2)
}


# ---------
# Main code
# ---------


## -- Load data from pipeline folder --

for(i in 1:length(S)){
  T <- read_csv(file.path('empirical', '2_pipeline',f, 'code01_align','out',S[i], 'T.csv')) %>%
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
  summarise(meany = mean(y), sdy = sd(y), p25y = quantile(y,probs = 0.25), p50y = median(y), p75y = quantile(y,probs = 0.75)) %>%
  ungroup %>%
  mutate(key = paste(survey,female,sep = "_")) %>%
  select(-female) %>%
  select(-survey) %>%
  mutate(date = as.Date(format(date, "%Y-%m-%d"))) 
  
Mmean <- M %>%
  select(-sdy,-p25y,-p50y,-p75y) %>%
  spread(key = key, value = meany) %>%
  # take difference: women - men
  mutate(msc_mean = Michigan_1-Michigan_0, sce_mean = FRBNY_1-FRBNY_0, bop_mean = `BOP-HH_1`-`BOP-HH_0`) %>%
  mutate(msc_mean_mov = moving_average(msc_mean),sce_mean_mov = moving_average(sce_mean),bop_mean_mov = moving_average(bop_mean)) %>%
  select(date,msc_mean,msc_mean_mov,sce_mean,sce_mean_mov,bop_mean,bop_mean_mov)

Msd <- M %>%
  select(-meany,-p25y,-p50y,-p75y) %>%
  spread(key = key, value = sdy) %>%
  # take difference: women - men
  mutate(msc_sd = Michigan_1-Michigan_0, sce_sd = FRBNY_1-FRBNY_0, bop_sd = `BOP-HH_1`-`BOP-HH_0`) %>%
  mutate(msc_sd_mov = moving_average(msc_sd),sce_sd_mov = moving_average(sce_sd),bop_sd_mov = moving_average(bop_sd)) %>%
  select(date,msc_sd,msc_sd_mov,sce_sd,sce_sd_mov,bop_sd,bop_sd_mov)

Mp25 <- M %>%
  select(-sdy,-meany,-p50y,-p75y) %>%
  spread(key = key, value = p25y) %>%
  # take difference: women - men
  mutate(msc_25 = Michigan_1-Michigan_0, sce_25 = FRBNY_1-FRBNY_0, bop_25 = `BOP-HH_1`-`BOP-HH_0`) %>%
  mutate(msc_25_mov = moving_average(msc_25),sce_25_mov = moving_average(sce_25),bop_25_mov = moving_average(bop_25)) %>%
  select(date,msc_25,msc_25_mov,sce_25,sce_25_mov,bop_25,bop_25_mov)

Mp50 <- M %>%
  select(-sdy,-p25y,-meany,-p75y) %>%
  spread(key = key, value = p50y) %>%
  # take difference: women - men
  mutate(msc_50 = Michigan_1-Michigan_0, sce_50 = FRBNY_1-FRBNY_0, bop_50 = `BOP-HH_1`-`BOP-HH_0`) %>%
  mutate(msc_50_mov = moving_average(msc_50),sce_50_mov = moving_average(sce_50),bop_50_mov = moving_average(bop_50)) %>%
  select(date,msc_50,msc_50_mov,sce_50,sce_50_mov,bop_50,bop_50_mov)

Mp75 <- M %>%
  select(-sdy,-p25y,-p50y,-meany) %>%
  spread(key = key, value = p75y) %>%
  # take difference: women - men
  mutate(msc_75 = Michigan_1-Michigan_0, sce_75 = FRBNY_1-FRBNY_0, bop_75 = `BOP-HH_1`-`BOP-HH_0`) %>%
  mutate(msc_75_mov = moving_average(msc_75),sce_75_mov = moving_average(sce_75),bop_75_mov = moving_average(bop_75)) %>%
  select(date,msc_75,msc_75_mov,sce_75,sce_75_mov,bop_75,bop_75_mov)

# load also ECFIN data

T_ECFIN <- read_csv2(file.path('empirical', '0_data','manual','ECFIN','Q61.csv')) 

Tmean <- T_ECFIN %>%
  mutate(date = as.Date(date), ecfin_mean = f_mean-m_mean) %>%
  arrange(date) %>%
  select(date,ecfin_mean,ecfin_mean_mov)

Tp25 <- T_ECFIN %>%
  mutate(date = as.Date(date), ecfin_25 = f_25-m_25) %>%
  arrange(date) %>%
  select(date,ecfin_25,ecfin_25_mov)

Tp50 <- T_ECFIN %>%
  mutate(date = as.Date(date), ecfin_50 = f_50-m_50) %>%
  arrange(date) %>%
  select(date,ecfin_50,ecfin_50_mov)

Tp75 <- T_ECFIN %>%
  mutate(date = as.Date(date), ecfin_75 = f_75-m_75) %>%
  arrange(date) %>%
  select(date,ecfin_75,ecfin_75_mov)

Mmean <- merge(Mmean,Tmean, by = "date", all = TRUE)
Mp25 <- merge(Mp25,Tp25, by = "date", all = TRUE)
Mp50 <- merge(Mp50,Tp50, by = "date", all = TRUE)
Mp75 <- merge(Mp75,Tp75, by = "date", all = TRUE)

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

M <- merge(Mmean,Msd, by = "date", all = TRUE) %>%
  merge(Mp25, by = "date", all = TRUE) %>%
  merge(Mp50, by = "date", all = TRUE) %>%
  merge(Mp75, by = "date", all = TRUE)

D <- merge(M,germany_f, by = "date", all = TRUE) %>%
  merge(germany_t, by = "date", all = TRUE) %>%
  merge(us_f, by = "date", all = TRUE) %>%
  merge(us_t, by = "date", all = TRUE) %>%
  merge(euro_f, by = "date", all = TRUE) %>%
  merge(euro_t, by = "date", all = TRUE) %>%
  mutate(date = as.Date(format(date, "%Y-%m-%d"))) %>%
  mutate(germany_cpigap = germany_f-germany_t, euro_cpigap = euro_f-euro_t, us_cpigap = us_f-us_t)

# save as text

write.delim(D, file = file.path(pipeline, 'out', 'D.txt'), sep = "\t")

# save as csv

write.csv(D, file = file.path(pipeline, 'out', 'D.csv'))