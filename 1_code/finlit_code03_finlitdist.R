# ------------
# Introduction
# ------------

## This file shows how higher financial literacy increases inflexp 

rm(list=ls())
NAME <- 'code03_finlitdist' ## Name of the R file goes here (without the file extension!)
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
  
  
  ## Load data from pipeline folder 
  
  T <- read_csv(file.path('empirical', '2_pipeline',f, 'code01_fitlit','out',S[s], 'T.csv')) %>%
    pdata.frame(index=c( "id", "date" ))
  assign(paste("T_",S[s],sep = ""),T)
  
  
  # create binary vector for test and predicted score
  T <- mutate(T,lfinpred = as.numeric(lfinpred>=median(lfinpred)), fin_lit_test = as.numeric(fin_lit_test>=median(fin_lit_test, na.rm = TRUE)))
  
  Tsub <- filter(T, y >= quantile(y,0.05) & y <= quantile(y,0.95))
  
  ## Overlaid Kernel Density Plot
  
  ylowpred <- T$y[T["lfinpred"]==0]
  yhighpred <- T$y[T["lfinpred"]==1]
  ylowtest <- T$y[!is.na(T$fin_lit_test) & T$fin_lit_test==0]
  yhightest <- T$y[!is.na(T$fin_lit_test) & T$fin_lit_test==1]
  
  # predicted
  #plot first kernel density plot
  kd1 <- density(ylowpred[ylowpred>=-10 & ylowpred<=50],bw = "nrd0", adjust = 2)
  plot(kd1, col='blue', lwd=2)
  
  #plot second kernel density plot
  kd2 <- density(yhighpred[yhighpred>=-10 & yhighpred<=50],bw = "nrd0", adjust = 2)
  lines(kd2, col='red', lwd=2)
  
  ## --- Save numbers in csv
  
  K <- cbind(kd1$x,kd1$y,kd2$x,kd2$y)
  write.delim(K, file = file.path(pipeline, 'out',S[s], 'K_pred.txt'), sep = "\t")
  assign(paste("K_pred_",S[s],sep = ""),K)
  
  # test
  #plot first kernel density plot
  kd1 <- density(ylowtest[ylowtest>=-10 & ylowtest<=50],bw = "nrd0", adjust = 2)
  plot(kd1, col='blue', lwd=2)
  
  #plot second kernel density plot
  kd2 <- density(yhightest[yhightest>=-10 & yhightest<=50],bw = "nrd0", adjust = 2)
  lines(kd2, col='red', lwd=2)
  
  ## --- Save numbers in csv
  
  K <- cbind(kd1$x,kd1$y,kd2$x,kd2$y)
  write.delim(K, file = file.path(pipeline, 'out',S[s], 'K_test.txt'), sep = "\t")
  assign(paste("K_test_",S[s],sep = ""),K)
}
  
  