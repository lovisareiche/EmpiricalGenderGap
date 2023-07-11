# ------------
# Introduction
# ------------

## Compares distributions from financially literate and illiterate

rm(list=ls())
NAME <- 'code15_fintradroles' ## Name of the R file goes here (without the file extension!)
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
library(xtable)

## --------
## Settings
## --------
### Any settings go here

f <- 'bopreg'


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
  outline <- file.path('3_output','results',f,NAME)
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

load(file.path('empirical', '2_pipeline',f, 'code09_fitlit','out', 'T_fin.RData')) 
Tac <- read_csv(file.path('empirical', '2_pipeline',f, 'code03_compilepanel.m','out','base', 'T_fin.csv'))

# for this analysis we only focus on non single women
Tf <- filter(T, female == 1 & non_single == 1) 
Tfs <- filter(T, female == 1 & non_single == 0) 


boxplot(Tfs$lpred_subj)
bg <- boxplot(fin_lit_test ~ shop_groceries_nsing, filter(Tac, non_single==1, female ==1))
bg <- boxplot(lpred_subj ~ shop_groceries_nsing, Tf)
bm <- boxplot(lpred_subj ~ shop_major_nsing, Tf)  
bp <- boxplot(lpred_subj ~ prep_meals_nsing, Tf)  
bf <- boxplot(lpred_subj ~ decide_finance_nsing, Tf)  

Tflow <- filter(Tf, lpred_subj <= median(lpred_subj))
Tfhigh <- filter(Tf, lpred_subj > median(lpred_subj))


## Plot table with mean and statistical tests

test <- filter(Tac, non_single==1, female ==1,shop_groceries_nsing==1)
mean(test$fin_lit_test)



## groceires

F_groceries <- group_by(Tf,shop_groceries_nsing) %>%
  summarise(mn = round(mean(lpred_test),2), md = median(lpred_test), std = round(sd(lpred_test),2)) %>%
  # want only highest and lowest to make binary
  filter(shop_groceries_nsing==0 | shop_groceries_nsing==2)

# run wilcoxin and kolmogorov smirnoff tests

w <-  wilcox.test(Tf$lpred_subj[Tf$shop_groceries_nsing==0],Tf$lpred_subj[Tf$shop_groceries_nsing==2], data = T,alternative = c("less"))

t <-  t.test(Tf$lpred_subj[Tf$shop_groceries_nsing==0],Tf$lpred_subj[Tf$shop_groceries_nsing==2], data = T,alternative = c("less"))

ks <-  ks.test(Tf$lpred_subj[Tf$shop_groceries_nsing==0],Tf$lpred_subj[Tf$shop_groceries_nsing==2])

F_groceries$t <- round(rep(t$p.value,2),2)
F_groceries$w <- round(rep(w$p.value,2),2)
F_groceries$ks <- round(rep(ks$p.value,2),2)

# transpose
F_groceries <- t(F_groceries)

## major

F_major <- group_by(Tf,shop_major_nsing) %>%
  summarise(mn = round(mean(lpred_subj),2), md = median(lpred_subj), std = round(sd(lpred_subj),2)) %>%
  # want only highest and lowest to make binary
  filter(shop_major_nsing==0 | shop_major_nsing==2)

# run wilcoxin and kolmogorov smirnoff tests

w <-  wilcox.test(Tf$lpred_subj[Tf$shop_major_nsing==0],Tf$lpred_subj[Tf$shop_major_nsing==2], data = T,alternative = c("less"))

t <-  t.test(Tf$lpred_subj[Tf$shop_major_nsing==0],Tf$lpred_subj[Tf$shop_major_nsing==2], data = T,alternative = c("less"))

ks <-  ks.test(Tf$lpred_subj[Tf$shop_major_nsing==0],Tf$lpred_subj[Tf$shop_major_nsing==2])

F_major$t <- round(rep(t$p.value,2),2)
F_major$w <- round(rep(w$p.value,2),2)
F_major$ks <- round(rep(ks$p.value,2),2)

# transpose
F_major <- t(F_major)

## meals

F_meals <- group_by(Tf,prep_meals_nsing) %>%
  summarise(mn = round(mean(lpred_subj),2), md = median(lpred_subj), std = round(sd(lpred_subj),2)) %>%
  # want only highest and lowest to make binary
  filter(prep_meals_nsing==0 | prep_meals_nsing==2)

# run wilcoxin and kolmogorov smirnoff tests

w <-  wilcox.test(Tf$lpred_subj[Tf$prep_meals_nsing==0],Tf$lpred_subj[Tf$prep_meals_nsing==2], data = T,alternative = c("less"))

t <-  t.test(Tf$lpred_subj[Tf$prep_meals_nsing==0],Tf$lpred_subj[Tf$prep_meals_nsing==2], data = T,alternative = c("less"))

ks <-  ks.test(Tf$lpred_subj[Tf$prep_meals_nsing==0],Tf$lpred_subj[Tf$prep_meals_nsing==2])

F_meals$t <- round(rep(t$p.value,2),2)
F_meals$w <- round(rep(w$p.value,2),2)
F_meals$ks <- round(rep(ks$p.value,2),2)

# transpose
F_meals <- t(F_meals)

## finance

F_finance <- group_by(Tf,decide_finance_nsing) %>%
  summarise(mn = round(mean(lpred_subj),2), md = median(lpred_subj), std = round(sd(lpred_subj),2)) %>%
  # want only highest and lowest to make binary
  filter(decide_finance_nsing==0 | decide_finance_nsing==2)

# run wilcoxin and kolmogorov smirnoff tests

w <-  wilcox.test(Tf$lpred_subj[Tf$decide_finance_nsing==0],Tf$lpred_subj[Tf$decide_finance_nsing==2], data = T,alternative = c("less"))

t <-  t.test(Tf$lpred_subj[Tf$decide_finance_nsing==0],Tf$lpred_subj[Tf$decide_finance_nsing==2], data = T,alternative = c("less"))

ks <-  ks.test(Tf$lpred_subj[Tf$decide_finance_nsing==0],Tf$lpred_subj[Tf$decide_finance_nsing==2])

F_finance$t <- round(rep(t$p.value,2),2)
F_finance$w <- round(rep(w$p.value,2),2)
F_finance$ks <- round(rep(ks$p.value,2),2)

# transpose
F_finance <- t(F_finance)

## combine in one table

F <- cbind(F_groceries, F_major, F_meals, F_finance)

xtable(F)
