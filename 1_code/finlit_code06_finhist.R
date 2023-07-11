# ------------
# Introduction
# ------------

## This file shows how higher financial literacy increases inflexp 

rm(list=ls())
NAME <- 'code06_finhist' ## Name of the R file goes here (without the file extension!)
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
library(caroline)



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

if (dir.exists(file.path('empirical', '3_output','results',f))){
  outline <- file.path('empirical', '3_output','results',f,NAME)
} else {
  outline <- file.path('3_output','results',f,NAME)
}

if (!dir.exists(file.path('empirical', '3_output','results',f,NAME))) {
  dir.create(outline)
}


for(s in 1:length(S)){
  if (!dir.exists(file.path(outline,S[s]))) {
    dir.create(file.path(outline,S[s]))
  }
}


# ---------
# Main code
# ---------

for(s in 1:length(S)){
  
  
  ## Load data from pipeline folder 
  
  T <- read_csv(file.path('empirical', '2_pipeline',f, 'code01_fitlit','out',S[s], 'T.csv')) %>%
    pdata.frame(index=c( "id", "date" ))
  assign(paste("T_",S[s],sep = ""),T)
  
  ### Histogram
  
  ## -- Separate inflation expectations
  
  predfem <- T$lfinpred[T["female"]==1]
  predmen <- T$lfinpred[T["female"]==0]
  testfem <- T$fin_lit_test[T["female"]==1]
  testmen <- T$fin_lit_test[T["female"]==0]
  
  ## --- Draw histogram
  
  # predicted
  
  # create a vector of histogram breaks
  x <- seq(0,1,by = 0.02)
  
  hpredfem <- hist(predfem, breaks = x, freq = FALSE,
             col = alpha('#238a8DFF',0.8),
             xlim = c(0,1))
  hpredmen <- hist(predmen, breaks = x, freq = FALSE,
             col = alpha('#FDE725FF',0.7), 
             xlim = c(0,1), add = TRUE)
  
  
  # Save numbers in csv
  Hpred <- cbind(hpredfem$mids,hpredfem$counts,hpredfem$density,hpredmen$counts,hpredmen$density)
  write.delim(Hpred, file = file.path(pipeline, 'out', paste('Hpred_',S[s],'.txt',sep = "")), sep = "\t")
  
  # actual

  # create a vector of histogram breaks
  x <- seq(0,3,by = 0.02)
  
  htestfem <- hist(testfem, breaks = x, freq = FALSE,
                   col = alpha('#238a8DFF',0.8),
                   xlim = c(0,1))
  htestmen <- hist(testmen, breaks = x, freq = FALSE,
                   col = alpha('#FDE725FF',0.7), 
                   xlim = c(0,1), add = TRUE)
  
  
  ## --- Save numbers in csv
  Htest <- cbind(htestfem$mids,htestfem$counts,htestfem$density,htestmen$counts,htestmen$density)
  write.delim(Htest, file = file.path(pipeline, 'out', paste('Htest_',S[s],'.txt',sep = "")), sep = "\t")
  
  
  
}