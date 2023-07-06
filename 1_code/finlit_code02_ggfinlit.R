# ------------
# Introduction
# ------------

## This file computes a table comparing the financial literacy scores for men and women


rm(list=ls())
NAME <- 'code02_ggfinlit' ## Name of the R file goes here (without the file extension!)
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
f <- 'finlit'
S <- c('BOP-HH','FRBNY')



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

for(s in 1:length(S)){
  if (!dir.exists(file.path(pipeline,'out',S[s]))) {
    dir.create(file.path(pipeline,'out',S[s]))
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
# Main code
# ---------

for(s in 1:length(S)){
  
  
  ## Load data from pipeline folder --
  T <- read_csv(file.path('empirical', '2_pipeline',f, 'code01_fitlit','out',S[s], 'T.csv')) %>%
    pdata.frame(index=c( "id", "date" ))
  assign(paste("T_",S[s],sep = ""),T)
  
  ## compute means by gender
  
  if(S[s]=='BOP-HH'){
  F <- group_by(T,female) %>%
    summarise(fin_lit_test = round(mean(fin_lit_test, na.rm = TRUE),2),lfinpred = round(mean(lfinpred),2),intqr = round(mean(intqr),2), refresher = round(mean(refresher),2),round = round(mean(round),2),qinterest = round(mean(qinterest),2),qeasy = round(mean(qeasy),2))
  }
  if(S[s]=='FRBNY'){
  F <- group_by(T,female) %>%
      summarise(fin_lit_test = round(mean(fin_lit_test, na.rm = TRUE),2),lfinpred = round(mean(lfinpred),2),intqr = round(mean(intqr),2), refresher = round(mean(refresher),2),round = round(mean(round),2),qinterest = round(mean(qinterest),2))
  }
  # run wilcoxin and kolmogorov smirnoff tests on subsamples
  
  w <- data.frame()
  t <- data.frame()
  ks <- data.frame()
  
  if(S[s]=='BOP-HH'){
  vars <- data.frame(T$fin_lit_test,T$lfinpred,T$intqr,T$refresher,T$round,T$qinterest,T$qeasy)
  }
  if(S[s]=='FRBNY'){
    vars <- data.frame(T$fin_lit_test,T$lfinpred,T$intqr,T$refresher,T$round,T$qinterest)
  }
  for (i in 1:length(vars)){
    w[i,1] <-  round(wilcox.test(vars[,i]~T$female, na.rm = TRUE)$p.value,2)
    t[i,1] <-  round(t.test(vars[,i]~T$female, na.rm = TRUE)$p.value,2)
    ks[i,1] <-  round(ks.test(vars[T$female == 0,i],vars[T$female == 1,i], na.rm = TRUE)$p.value,2)
  }
  
  # transpose
  F <- data.frame(t(F)) %>%
    dplyr::mutate(ttest = rbind(0,t), wtest = rbind(0,w), kstest = rbind(0,ks))
  assign(paste("F_",S[s],sep = ""),F)
  
}
 
# Add an empty row to F_FRBNY
F_FRBNY <- rbind(F_FRBNY, rep(NA, ncol(F_FRBNY)))

# Combine the tables into a single dataframe
merged <- cbind(`F_BOP-HH`, F_FRBNY)

# Save output
  
writeLines(capture.output(xtable(merged, 
            caption = "Comparing the male and female subsamples", 
            label = "tab:ggfinlit")),
            file.path('empirical',outline, 'code_ggfinlit.tex'))
  
