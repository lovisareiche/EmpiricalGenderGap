# ------------
# Introduction
# ------------

## Coverts different inflation expectation surveys in a common format

rm(list=ls())
NAME <- 'code01_align' ## Name of the R file goes here (without the file extension!)
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


## --------
## Settings
## --------
### Any settings go here

s <- 'Michigan'
# BOP-HH, Michigan, FRBNY

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

## Add subfolders

if (!dir.exists(file.path(pipeline,'out',s))) {
  dir.create(file.path(pipeline,'out',s))
}


# ---------
# Functions
# ---------

# ---------
# Main code
# ---------


## -- Load data from 0_data folder --

T <- read_csv(file.path('empirical', '0_data', 'manual',s, 'T.csv'))

if(s=='BOP-HH'){
  T <- mutate(T,single = as.numeric(non_single == 0)) %>%
    filter(abs(y) <= 95 & abs(female) <=1 & abs(single) <=1 & abs(eduschool) <= 6) %>%
    rename(region = eastgerman) %>%
    select(-non_single,-pinc)
}

if(s=='Michigan'){
  T <- mutate(T,female = SEX-1, single = as.numeric(NUMADT == 1), year = as.numeric(substr(YYYYMM, 1, 4)), month = as.numeric(substr(YYYYMM, 5, 6))) %>%
    rename(y = PX1, age = AGE, hhinc = INCOME, eduschool = EDUC, region = REGION) %>%
    filter(abs(y) <= 95 & !is.na(female) & !is.na(single) & !is.na(hhinc) & !is.na(eduschool) & !is.na(region) & !is.na(age) ) %>%
    select(female,single,age,eduschool,hhinc,region,y,year,month)
}

if(s=='FRBNY'){
  T <- mutate(T,female = as.numeric(Q33==1), single = Q38 - 1, year = as.numeric(substr(date, 1, 4)), month = as.numeric(substr(date, 5, 6))) %>%
    rename(y = Q8v2part2, age = Q32, eduschool = Q36, hhinc = Q47, regionCAT = `_REGION_CAT`) %>%
    filter(abs(y) <= 95 & !is.na(female) & !is.na(single) & !is.na(hhinc) & !is.na(eduschool) & !is.na(regionCAT) & !is.na(age) & abs(eduschool)<=8) %>%
    mutate(region = as.numeric(factor(regionCAT))) %>%
    select(female,single,age,eduschool,hhinc,region,y,year,month)
}

if(s=='BOE'){
  T <- mutate(T,female = as.numeric(Q33==1), single = Q38 - 1, year = as.numeric(substr(date, 1, 4)), month = as.numeric(substr(date, 5, 6))) %>%
    rename(y = Q8v2part2) %>%
    filter(abs(y) <= 95 & !is.na(female) & !is.na(single)) %>%
    select(female,single,y,year,month)
}


write_csv(T,file.path(pipeline,'out',s, 'T.csv')) 

