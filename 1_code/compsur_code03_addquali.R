# ------------
# Introduction
# ------------

## Adds qualitative indicator to the timeseries

rm(list=ls())
NAME <- 'code03_addquali' ## Name of the R file goes here (without the file extension!)
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
library(zoo) # for date vector


## --------
## Settings
## --------
### Any settings go here

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

D <- read_csv(file.path('empirical', '2_pipeline', f,'code02_tsgendergap','out', 'D.csv')) %>%
  mutate(date = as.Date(date))

## -- Load qualitative data

## ECFIN
Qecfin <- read_csv2(file.path('empirical', '0_data', 'manual','ECFIN','Q60.csv')) %>%
  # need to divide by 100 as other surveys will be in probabilities
  mutate(date = as.Date(date), qecfin = as.numeric(gap)/100) %>%
  dplyr::select(date,qecfin)


## BOP 
# have to code balance statistic

Qbop <- read_csv(file.path('empirical', '2_pipeline', 'compsur','code01_align','out','BOP-HH', 'T.csv')) %>%
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  dplyr::select(quali,date,female) %>%
  group_by(date,female) %>%
  dplyr::summarise(q1 = sum(as.numeric(quali == 1))/length(quali),q2 = sum(as.numeric(quali == 2))/length(quali),q4 = sum(as.numeric(quali == 4))/length(quali),q5 = sum(as.numeric(quali == 5))/length(quali)) %>%
  ungroup %>%
  # compute balance statitsic
  mutate(b = -q1 -0.5*q2 + 0.5*q4 + q5) %>%
  dplyr::select(-q1,-q2,-q4,-q5) 

Qbop_f <- filter(Qbop,female == 1) %>%
  dplyr::select(-female) %>%
  dplyr::rename(b_f = b)
Qbop_m <- filter(Qbop,female == 0) %>%
  dplyr::select(-female) %>%
  dplyr::rename(b_m = b) 

# compute gap in main
Qbop <- merge(Qbop_f,Qbop_m, by = "date") %>%
  mutate(qbop = b_f - b_m, qbop_mov = moving_average(b_f - b_m))

dates <- seq(as.Date("2019-05-01"), by = "month", length.out = nrow(Qbop)) 
Qbop$date <- dates

Qbop <- dplyr::select(Qbop, qbop,qbop_mov, date)

## Michigan

Qmsc <- read_csv(file.path('empirical', '2_pipeline', 'compsur','code01_align','out','Michigan', 'T.csv')) %>%
  dplyr::rename(q_inflation = quali) %>%
  filter(abs(q_inflation) <= 5 & !is.na(female)) %>%
  dplyr::select(female,q_inflation,year,month) %>%
  # inefficient way to get right date format
  mutate(date = as.yearmon(paste(year, month), format = "%Y %m")) %>%
  mutate(date = as.Date(format(date, "%Y-%m-%d"))) %>%
  # continue as with bop
  dplyr::select(q_inflation,date,female) %>%
  group_by(date,female) %>%
  dplyr::summarise(q1 = sum(as.numeric(q_inflation == 1))/length(q_inflation),q5 = sum(as.numeric(q_inflation == 5))/length(q_inflation)) %>%
  ungroup %>%
  # compute balance statitsic, note michigan has 1 go up, eu has 1 go down
  mutate(b = q1 - q5) %>%
  dplyr::select(-q1,-q5) 

Qmsc_f <- filter(Qmsc,female == 1) %>%
  dplyr::select(-female) %>%
  dplyr::rename(b_f = b)
Qmsc_m <- filter(Qmsc,female == 0) %>%
  dplyr::select(-female) %>%
  dplyr::rename(b_m = b) 

# compute gap in main
Qmsc <- merge(Qmsc_f,Qmsc_m, by = "date") %>%
  mutate(qmsc = b_f - b_m, qmsc_mov = moving_average(b_f - b_m)) %>%
  dplyr::select(date,qmsc, qmsc_mov)

## SCE

Qsce <- read_csv(file.path('empirical', '2_pipeline', 'compsur','code01_align','out','FRBNY', 'T.csv'))%>%
  dplyr::rename(q_inflation = quali) %>%
  filter(!is.na(q_inflation) & !is.na(female) ) %>%
  dplyr::select(female,q_inflation,year,month) %>%
  # inefficient way to get right date format (continue as in msc)
  mutate(date = as.yearmon(paste(year, month), format = "%Y %m")) %>%
  mutate(date = as.Date(format(date, "%Y-%m-%d"))) %>%
  # continue as with bop
  dplyr::select(q_inflation,date,female) %>%
  group_by(date,female) %>%
  dplyr::summarise(q1 = sum(as.numeric(q_inflation == 1))/length(q_inflation),q5 = sum(as.numeric(q_inflation == 2))/length(q_inflation)) %>%
  ungroup %>%
  # compute balance statitsic, note sce has 1 go up, eu has 1 go down
  mutate(b = q1 - q5) %>%
  dplyr::select(-q1,-q5) 

Qsce_f <- filter(Qsce,female == 1) %>%
  dplyr::select(-female) %>%
  dplyr::rename(b_f = b)
Qsce_m <- filter(Qsce,female == 0) %>%
  dplyr::select(-female) %>%
  dplyr::rename(b_m = b) 

# compute gap in main
Qsce <- merge(Qsce_f,Qsce_m, by = "date") %>%
  mutate(qsce = b_f - b_m, qsce_mov = moving_average(b_f - b_m)) %>%
  dplyr::select(date,qsce, qsce_mov) 



## -- Merge all together

D <- merge(D,Qmsc, by = "date", all = TRUE) %>%
  merge(Qsce, by = "date", all = TRUE) %>%
  merge(Qecfin, by = "date", all = TRUE) %>%
  merge(Qbop, by = "date", all = TRUE) 

## -- Save

# save as text

write.delim(D, file = file.path(pipeline, 'out', 'D.txt'), sep = "\t")

# save as csv

write.csv(D, file = file.path(pipeline, 'out', 'D.csv'))
