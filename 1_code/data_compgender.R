# ------------
# Introduction
# ------------

## Create a table to compare empirical moments of demographics along gender dimension

rm(list=ls())
NAME <- 'data_compgender' ## Name of the R file goes here (without the file extension!)
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
library('xtable') # write final table in latex

## --------
## Settings
## --------
### Any settings go here


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


# ---------
# Main code
# ---------


## -- Load data from pipeline folder --

T <- read_csv(file.path('empirical', '2_pipeline', 'code03_compilepanel.m','out','base', 'T.csv')) 


## -- Crate table of means split by male and female

W <- T %>%
  group_by(female) %>%
  summarise(live_alone = mean(live_alone), shop_groceries = mean(shop_groceries), 
            shop_major= mean(shop_major), prep_meals = mean(prep_meals), 
            decide_finance = mean(decide_finance), pessimist = mean(pessimist), 
            prob_intqr = mean(prob_intqr), refresher = mean(refresher), 
            nround = mean(nround), f_nointerest = mean(f_nointerest), 
            f_easy = mean(f_easy)) %>%
  select(-female) %>%
  # tranpose
  t %>%
  # make dataframe
  as.data.frame %>%
  # change variable names
  rename("Male subsample" = "V1", "Female subsample" = "V2")
  

## -- Save output

writeLines(capture.output(xtable(W, 
                                 caption = "Comparing the male and female subsamples", 
                                 label = "compgender")),
           file.path('empirical', '3_output','results', NAME, 'code_compgender.tex'))


