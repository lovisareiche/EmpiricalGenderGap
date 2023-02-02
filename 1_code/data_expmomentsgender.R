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


# ---------
# Main code
# ---------



for(i in 1:length(S)){

  ## -- Load data from pipeline folder --
  T <- read_csv(file.path('empirical', '2_pipeline', 'data_align','out',S[i], 'T.csv')) %>%
    mutate(survey = S[i])
  assign(paste("T_",S[i],sep = ""),T)
  
  ## -- All
  
  all <- summarise(T,cat = 0, mean = mean(y), median = median(y), std = sd(y))
  
  all_fem <- T[T$female == 1,] %>%
    summarise(cat = 0, mean_fem = mean(y), median_fem = median(y), std_fem = sd(y))
  
  all_mal <- T[T$female == 0,] %>%
    summarise(cat = 0, mean_mal = mean(y), median_mal = median(y), std_mal = sd(y))
  
  ## -- Age grouping
  
  # Matrix with mean median and std of expectations for age categories 
  age <- mutate(T,age_low = as.numeric(age<=quantile(T$age,0.3333))) %>%
    mutate(age_mid = as.numeric(age > quantile(T$age,0.3333) & age < quantile(T$age,0.6666))*2) %>%
    mutate(age_high = as.numeric(age >= quantile(T$age,0.6666))*3) %>%
    mutate(cat = age_low + age_mid + age_high) %>%
    group_by(cat) %>%
    summarise(mean = mean(y), median = median(y), std = sd(y)) %>%
    ungroup 
  
  # Matrix with mean median and std of expectations for age categories for female subsample
  age_fem <- T[T$female == 1,] %>%
    mutate(age_low = as.numeric(age<=quantile(T$age,0.3333))) %>%
    mutate(age_mid = as.numeric(age > quantile(T$age,0.3333) & age < quantile(T$age,0.6666))*2) %>%
    mutate(age_high = as.numeric(age >= quantile(T$age,0.6666))*3) %>%
    mutate(cat = age_low + age_mid + age_high) %>%
    group_by(cat) %>%
    summarise(mean_fem = mean(y), median_fem = median(y), std_fem = sd(y)) %>%
    ungroup 
  
  # Matrix with mean median and std of expectations for age categories for male subsample
  age_mal <- T[T$female == 0,] %>%
    mutate(age_low = as.numeric(age<=quantile(T$age,0.3333))) %>%
    mutate(age_mid = as.numeric(age > quantile(T$age,0.3333) & age < quantile(T$age,0.6666))*2) %>%
    mutate(age_high = as.numeric(age >= quantile(T$age,0.6666))*3) %>%
    mutate(cat = age_low + age_mid + age_high) %>%
    group_by(cat) %>%
    summarise(mean_mal = mean(y), median_mal = median(y), std_mal = sd(y)) %>%
    ungroup 
  
  ## -- East West grouping
  
  # Matrix with mean median and std of expectations for age categories 
  region <- group_by(T,region) %>%
    summarise(mean = mean(y), median = median(y), std = sd(y)) %>%
    rename(cat = region) %>%
    ungroup 
  
  # Matrix with mean median and std of expectations for age categories for female subsample
  reg_fem <- T[T$female == 1,] %>%
    group_by(region) %>%
    summarise(mean_fem = mean(y), median_fem = median(y), std_fem = sd(y)) %>%
    rename(cat = region) %>%
    ungroup 
  
  # Matrix with mean median and std of expectations for age categories for male subsample
  reg_mal <- T[T$female == 0,] %>%
    group_by(region) %>%
    summarise(mean_mal = mean(y), median_mal = median(y), std_mal = sd(y)) %>%
    rename(cat = region) %>%
    ungroup 
  
  # if it is BOP-HH we need to insert two new rows to match the 4 category region in US
  
  if(S[i]=="BOP-HH"){
    region [ nrow(region) + 1 , ] <- NA
    reg_fem [ nrow(reg_fem) + 1 , ] <- NA
    reg_mal [ nrow(reg_mal) + 1 , ] <- NA
    region [ nrow(region) + 1 , ] <- NA
    reg_fem [ nrow(reg_fem) + 1 , ] <- NA
    reg_mal [ nrow(reg_mal) + 1 , ] <- NA
  }
  
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
  single <- group_by(T,single) %>%
    summarise(mean = mean(y), median = median(y), std = sd(y)) %>%
    rename(cat = single) %>%
    ungroup 
  
  # Matrix with mean median and std of expectations for age categories for female subsample
  single_fem <- T[T$female == 1,] %>%
    group_by(single) %>%
    summarise(mean_fem = mean(y), median_fem = median(y), std_fem = sd(y)) %>%
    rename(cat = single) %>%
    ungroup 
  
  # Matrix with mean median and std of expectations for age categories for male subsample
  single_mal <- T[T$female == 0,] %>%
    group_by(single) %>%
    summarise(mean_mal = mean(y), median_mal = median(y), std_mal = sd(y)) %>%
    rename(cat = single) %>%
    ungroup 
  
  ## -- Put table together
  
  W <- rbind(all,age,region,edu,inc,single)
  W_fem <- rbind(all_fem,age_fem,reg_fem,edu_fem,inc_fem,single_fem) 
  
  if(i!=1){
    W_fem <- select(W_fem,-cat)
  }
  
  W_mal <- rbind(all_mal,age_mal,reg_mal,edu_mal,inc_mal,single_mal) %>%
    select(-cat)
  
  # This adjust depending on how table should look (ie can include all or only male female split)
  W <- cbind(W_fem,W_mal) #%>%
  #  select(-std_fem,-std_mal)
  assign(paste("W_",S[i],sep = ""),W)
  
}

rm(T,W_fem,W_mal,all,age,region,edu,inc,single,all_fem,age_fem,reg_fem,edu_fem,inc_fem,single_fem,all_mal,age_mal,reg_mal,edu_mal,inc_mal,single_mal)


W <- cbind(`W_BOP-HH`,W_Michigan,W_FRBNY)



## -- Save output
writeLines(capture.output(xtable(W, 
                                caption = "Mean and Median Inflation Expectations by Demographics and Gender", 
                                label = "tab:expmomentsgender")),
           file.path('empirical', '3_output','results', NAME, 'code_expmomentgender.tex'))
