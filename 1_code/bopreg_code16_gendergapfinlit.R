# ------------
# Introduction
# ------------

## Compares distributions from financially literate and illiterate

rm(list=ls())
NAME <- 'code16_gendergapfinlit' ## Name of the R file goes here (without the file extension!)
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
library(plm)

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
  outline <- file.path('empirical','3_output','results',f,NAME)
  dir.create(outline)
}


# ---------
# Functions
# ---------

# ---------
# Main code
# ---------


## -- Load data from pipeline folder --

# financial literacy predictors
load(file.path('empirical', '2_pipeline',f, 'code09_fitlit','out', 'T_fin.RData')) 

# real fin lit sample
T_fin <- read_csv(file.path('empirical', '2_pipeline',f, 'code03_compilepanel.m','out','base', 'T_fin.csv')) %>%
  pdata.frame(index=c( "id", "wave" ))

### Full sample
###############


## compute means by gender

F1 <- group_by(T,female) %>%
  summarise(lpred_subj = round(mean(lpred_subj),2),lpred_test = round(mean(lpred_test),2),prob_intqr = round(mean(prob_intqr),2), refresher = round(mean(refresher),2),nround = round(mean(nround),2),f_nointerest = round(mean(f_nointerest),2),f_easy = round(mean(f_easy),2))

# run wilcoxin and kolmogorov smirnoff tests on subsamples

w <- data.frame()
t <- data.frame()
ks <- data.frame()
vars <- data.frame(T$lpred_subj,T$lpred_test,T$prob_intqr,T$refresher,T$nround,T$f_nointerest,T$f_easy)

for (i in 1:length(vars)){
w[i,1] <-  round(wilcox.test(vars[,i]~T$female)$p.value,2)
t[i,1] <-  round(t.test(vars[,i]~T$female)$p.value,2)
ks[i,1] <-  round(ks.test(vars[T$female == 0,i],vars[T$female == 1,i])$p.value,2)
}

# transpose
F1 <- data.frame(t(F1)) %>%
  dplyr::mutate(ttest = rbind(0,t), wtest = rbind(0,w), kstest = rbind(0,ks))


### Sep 21 and Jan 2022
####################

## compute means by gender

F2 <- group_by(T_fin,female) %>%
  summarise(fin_lit_subj = round(mean(fin_lit_subj),2),fin_lit_test = round(mean(fin_lit_test),2),prob_intqr = round(mean(prob_intqr),2), refresher = round(mean(refresher),2),nround = round(mean(nround),2),f_nointerest = round(mean(f_nointerest),2),f_easy = round(mean(f_easy),2))

# run wilcoxin and kolmogorov smirnoff tests on subsamples

w <- data.frame()
t <- data.frame()
ks <- data.frame()
vars <- data.frame(T_fin$fin_lit_subj,T_fin$fin_lit_test,T_fin$prob_intqr,T_fin$refresher,T_fin$nround,T_fin$f_nointerest,T_fin$f_easy)

for (i in 1:length(vars)){
  w[i,1] <-  round(wilcox.test(vars[,i]~T_fin$female)$p.value,2)
  t[i,1] <-  round(t.test(vars[,i]~T_fin$female)$p.value,2)
  ks[i,1] <-  round(ks.test(vars[T_fin$female == 0,i],vars[T_fin$female == 1,i])$p.value,2)
}

# transpose
F2 <- data.frame(t(F2)) %>%
  dplyr::mutate(ttest = rbind(0,t), wtest = rbind(0,w), kstest = rbind(0,ks))

# combine

W <- rbind(F1,F2)

# Save output

writeLines(capture.output(xtable(W, 
                                 caption = "Comparing the male and female subsamples", 
                                 label = "ggfinlit")),
           file.path(outline, 'gendergapfinlit.tex'))