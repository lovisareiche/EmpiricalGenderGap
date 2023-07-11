# ------------
# Introduction
# ------------

## This file 

rm(list=ls())
NAME <- 'code01_summary' 
PROJECT <- 'EmpiricalGenderGap'
PROJECT_DIR <- 'D:/Lovisa/Studium/Oxford/Department of Economics/DPhil' ## Change this to the directory in which your project folder is located, make sure to avoid using single backslashes (i.e. \)!

# ------------
# Preamble
# ------------

## -------
## Imports
## -------
### All the library imports go here

#install.packages('plm')
library('plm')
library('tidyverse')
library('datawizard')
library(stargazer)
library(lubridate) # for ymd
library(xtable)

## --------
## Settings
## --------
### Any settings go here

f <- 'hhrole'


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

if (dir.exists(file.path('empirical', '3_output','results',f))){
  outline <- file.path('empirical', '3_output','results',f,NAME)
} else {
  outline <- file.path('3_output','results',f,NAME)
}

if (!dir.exists(outline)) {
  dir.create(outline)
}



# ---------
# Main code
# ---------

## -- Load data from pipeline folder --
T <- read_csv(file.path('empirical', '0_data', 'manual','BOP-HH', 'T_exp.csv')) %>%
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  pdata.frame( index=c( "id", "date" ) )

# summarise gender roles for full sample
all <- group_by(T,female) %>%
  summarise(shop_groceries = mean(shop_groceries), shop_major = mean(shop_major), prep_meals = mean(prep_meals), decide_finance = mean(decide_finance))

# and non single subset
non_singles <- group_by(filter(T,single==0),female) %>%
  summarise(shop_groceries = mean(shop_groceries), shop_major = mean(shop_major), prep_meals = mean(prep_meals), decide_finance = mean(decide_finance))

# combine
F <- rbind(all, non_singles)

# print
writeLines(capture.output(xtable(F)), 
           file.path(outline, 'code_gghhrole.tex'))



