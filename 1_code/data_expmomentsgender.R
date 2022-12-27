# ------------
# Introduction
# ------------

## Create a table to compare empirical moments of demographics along gender dimension

rm(list=ls())
NAME <- 'data_expmomentsgender' ## Name of the R file goes here (without the file extension!)
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

## -- All

all <- summarise(T,cat = 0, mean = mean(y), median = median(y), std = sd(y))

all_fem <- T[T$female == 1,] %>%
  summarise(cat = 0, mean = mean(y), median = median(y), std = sd(y))

all_mal <- T[T$female == 0,] %>%
  summarise(cat = 0, mean = mean(y), median = median(y), std = sd(y))

## -- Age grouping

# Matrix with mean median and std of expectations for age categories 
age <- mutate(T,age_44 = as.numeric(age<=44)) %>%
  mutate(age_45_64 = as.numeric(age > 44 & age <= 64)*2) %>%
  mutate(age_65 = as.numeric(age > 64)*3) %>%
  mutate(cat = age_44 + age_45_64 + age_65) %>%
  group_by(cat) %>%
  summarise(mean = mean(y), median = median(y), std = sd(y)) %>%
  ungroup 

# Matrix with mean median and std of expectations for age categories for female subsample
age_fem <- T[T$female == 1,] %>%
  mutate(age_44 = as.numeric(age<=44)) %>%
  mutate(age_45_64 = as.numeric(age > 44 & age <= 64)*2) %>%
  mutate(age_65 = as.numeric(age > 64)*3) %>%
  mutate(cat = age_44 + age_45_64 + age_65) %>%
  group_by(cat) %>%
  summarise(mean_fem = mean(y), median_fem = median(y), std_fem = sd(y)) %>%
  ungroup 

# Matrix with mean median and std of expectations for age categories for male subsample
age_mal <- T[T$female == 0,] %>%
  mutate(age_44 = as.numeric(age<=44)) %>%
  mutate(age_45_64 = as.numeric(age > 44 & age <= 64)*2) %>%
  mutate(age_65 = as.numeric(age > 64)*3) %>%
  mutate(cat = age_44 + age_45_64 + age_65) %>%
  group_by(cat) %>%
  summarise(mean_mal = mean(y), median_mal = median(y), std_mal = sd(y)) %>%
  ungroup 

## -- East West grouping

# Matrix with mean median and std of expectations for age categories 
east <- group_by(T,eastgerman) %>%
  summarise(mean = mean(y), median = median(y), std = sd(y)) %>%
  rename(cat = eastgerman) %>%
  ungroup 

# Matrix with mean median and std of expectations for age categories for female subsample
east_fem <- T[T$female == 1,] %>%
  group_by(eastgerman) %>%
  summarise(mean_fem = mean(y), median_fem = median(y), std_fem = sd(y)) %>%
  rename(cat = eastgerman) %>%
  ungroup 

# Matrix with mean median and std of expectations for age categories for male subsample
east_mal <- T[T$female == 0,] %>%
  group_by(eastgerman) %>%
  summarise(mean_mal = mean(y), median_mal = median(y), std_mal = sd(y)) %>%
  rename(cat = eastgerman) %>%
  ungroup 

## -- Income grouping

# Matrix with mean median and std of expectations for income categories 
inc <- mutate(T,inc_low = as.numeric(hhinc<=quantile(T$hhinc,0.3333))) %>%
  mutate(inc_mid = as.numeric(hhinc > quantile(T$hhinc,0.3333) & hhinc < quantile(T$hhinc,0.6666))*2) %>%
  mutate(inc_high = as.numeric(hhinc >= quantile(T$hhinc,0.6666))*3) %>%
  mutate(cat = inc_low + inc_mid + inc_high) %>%
  group_by(cat) %>%
  summarise(mean = mean(y), median = median(y), std = sd(y)) %>%
  ungroup 

# Matrix with mean median and std of expectations for age categories for female subsample
inc_fem <- T[T$female == 1,] %>%
  mutate(inc_low = as.numeric(hhinc<=quantile(T$hhinc,0.3333))) %>%
  mutate(inc_mid = as.numeric(hhinc > quantile(T$hhinc,0.3333) & hhinc < quantile(T$hhinc,0.6666))*2) %>%
  mutate(inc_high = as.numeric(hhinc >= quantile(T$hhinc,0.6666))*3) %>%
  mutate(cat = inc_low + inc_mid + inc_high) %>%
  group_by(cat) %>%
  summarise(mean_fem = mean(y), median_fem = median(y), std_fem = sd(y)) %>%
  ungroup  

# Matrix with mean median and std of expectations for age categories for male subsample
inc_mal <- T[T$female == 0,] %>%
  mutate(inc_low = as.numeric(hhinc<=quantile(T$hhinc,0.3333))) %>%
  mutate(inc_mid = as.numeric(hhinc > quantile(T$hhinc,0.3333) & hhinc < quantile(T$hhinc,0.6666))*2) %>%
  mutate(inc_high = as.numeric(hhinc >= quantile(T$hhinc,0.6666))*3) %>%
  mutate(cat = inc_low + inc_mid + inc_high) %>%
  group_by(cat) %>%
  summarise(mean_mal = mean(y), median_mal = median(y), std_mal = sd(y)) %>%
  ungroup 

## -- education grouping

# Matrix with mean median and std of expectations for education categories 
edu <- mutate(T,edu_low = as.numeric(eduschool<=quantile(T$eduschool,0.3333))) %>%
  mutate(edu_mid = as.numeric(eduschool > quantile(T$eduschool,0.3333) & eduschool < quantile(T$eduschool,0.6666))*2) %>%
  mutate(edu_high = as.numeric(eduschool >= quantile(T$eduschool,0.6666))*3) %>%
  mutate(cat = edu_low + edu_mid + edu_high) %>%
  group_by(cat) %>%
  summarise(mean = mean(y), median = median(y), std = sd(y)) %>%
  ungroup 

# Matrix with mean median and std of expectations for age categories for female subsample
edu_fem <- T[T$female == 1,] %>%
  mutate(edu_low = as.numeric(eduschool<=quantile(T$eduschool,0.3333))) %>%
  mutate(edu_mid = as.numeric(eduschool > quantile(T$eduschool,0.3333) & eduschool < quantile(T$eduschool,0.6666))*2) %>%
  mutate(edu_high = as.numeric(eduschool >= quantile(T$eduschool,0.6666))*3) %>%
  mutate(cat = edu_low + edu_mid + edu_high) %>%
  group_by(cat) %>%
  summarise(mean_fem = mean(y), median_fem = median(y), std_fem = sd(y)) %>%
  ungroup  

# Matrix with mean median and std of expectations for age categories for male subsample
edu_mal <- T[T$female == 0,] %>%
  mutate(edu_low = as.numeric(eduschool<=quantile(T$eduschool,0.3333))) %>%
  mutate(edu_mid = as.numeric(eduschool > quantile(T$eduschool,0.3333) & eduschool < quantile(T$eduschool,0.6666))*2) %>%
  mutate(edu_high = as.numeric(eduschool >= quantile(T$eduschool,0.6666))*3) %>%
  mutate(cat = edu_low + edu_mid + edu_high) %>%
  group_by(cat) %>%
  summarise(mean_mal = mean(y), median_mal = median(y), std_mal = sd(y)) %>%
  ungroup 


## -- Single grouping

# Matrix with mean median and std of expectations for single or nonsingle categories 
single <- group_by(T,live_alone) %>%
  summarise(mean = mean(y), median = median(y), std = sd(y)) %>%
  rename(cat = live_alone) %>%
  ungroup 

# Matrix with mean median and std of expectations for age categories for female subsample
single_fem <- T[T$female == 1,] %>%
  group_by(live_alone) %>%
  summarise(mean_fem = mean(y), median_fem = median(y), std_fem = sd(y)) %>%
  rename(cat = live_alone) %>%
  ungroup 

# Matrix with mean median and std of expectations for age categories for male subsample
single_mal <- T[T$female == 0,] %>%
  group_by(live_alone) %>%
  summarise(mean_mal = mean(y), median_mal = median(y), std_mal = sd(y)) %>%
  rename(cat = live_alone) %>%
  ungroup 

## -- Put table together

W <- rbind(all,age,east,edu,inc,single)
W_fem <- rbind(all_fem,age_fem,east_fem,edu_fem,inc_fem,single_fem) %>%
  select(-cat)
W_mal <- rbind(all_mal,age_mal,east_mal,edu_mal,inc_mal,single_mal) %>%
  select(-cat)

W <- cbind(W,W_fem,W_mal)

## -- Save output
writeLines(capture.output(xtable(W, 
                                caption = "Empirical moments of inflation expectations by demographics and gender", 
                                label = "tab:expmomentsgender")),
           file.path('empirical', '3_output','results', NAME, 'code_expmomentgender.tex'))
