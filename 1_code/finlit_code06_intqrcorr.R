# ------------
# Introduction
# ------------

## This file shows how higher financial literacy is correlated with intqr

rm(list=ls())
NAME <- 'code07_intqrcorr' 
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
    dplyr::select(intqr,lfinpred) %>%
    na.omit()
  assign(paste("T_",S[s],sep = ""),T)
  
  # Binscatter
  
  # Define the number of bins
  num_bins <- 10
  
  # Create a new dataframe with binned means
  binned_data <- T %>%
    dplyr::rename(x = lfinpred, y = intqr) %>%
    #filter(x >= 0 & x <= 1 & female ==0) %>%
    mutate(x_bin = cut(x, breaks = num_bins)) %>%
    group_by(x_bin) %>%
    summarize(mean_x = mean(x), mean_y = mean(y)) %>%
    ungroup()
  assign(paste("bd_",S[s],sep = ""),binned_data)
  
  
  write.delim(T, file = file.path(pipeline, 'out', paste('T_',S[s],'.txt',sep = "")), sep = "\t")
  write.delim(binned_data, file = file.path(pipeline, 'out', paste('bd_',S[s],'.txt',sep = "")), sep = "\t")
  
  # Correlation
  
  c <- cor(T$intqr,T$lfinpred)
  assign(paste("c_",S[s],sep = ""),c)
  
  ct <- cor.test(T$intqr,T$lfinpred,
           alternative = "two.sided",
           method = "pearson",
           exact = NULL, conf.level = 0.95, continuity = FALSE)
}



