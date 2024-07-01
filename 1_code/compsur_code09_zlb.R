# ------------
# Introduction
# ------------

## checks share of individuals with negative expectations

rm(list=ls())
NAME <- 'code09_zlb' ## Name of the R file goes here (without the file extension!)
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
library(moments) # to compute skew
library(zoo) # for date vector
library(xtable) # for latex conversion
library(stargazer) # for latex table
library('plm')
library('datawizard')
library(lubridate)
library(caret)

library(scatterplot3d) # to create scatterplot
library(rgl) # to save scatterplot

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

if (dir.exists(file.path('empirical', '3_output','results',f,NAME))){
  outline <- file.path('empirical', '3_output','results',f,NAME)
} else {
  outline <- file.path('empirical','3_output','results',f,NAME)
}

if (!dir.exists(file.path('empirical', '3_output','results',f,NAME))) {
  dir.create(outline)
}


# ---------
# Functions
# ---------

# ---------
# Main code
# ---------


## -- Load data from pipeline folder --

for(i in 1:length(S)){
  
  # load aligned data
  
  T <- read_csv(file.path('empirical', '2_pipeline',f, 'code01_align','out',S[i], 'T.csv')) %>%
    mutate(survey = S[i], date = ymd(paste0(year, "-", month, "-01"))) %>%
    pdata.frame( index=c( "id", "date" ) )
  
  #h <- hist(T$y, probability = TRUE, breaks = seq(from = min(T$y), to = max(T$y), by = 0.5), xlim=c(-20,50))
  
  # Compute the share of values below 0
  share_below_zero_f <- mean(T$y[T$female==1] < 0)
  assign(paste("share_below_zero_f_",S[i],sep = ""),share_below_zero_f)
  share_below_zero_m <- mean(T$y[T$female==0] < 0)
  assign(paste("share_below_zero_m_",S[i],sep = ""),share_below_zero_m)
  
}
  
  