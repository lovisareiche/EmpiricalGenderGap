# ------------
# Introduction
# ------------

## Compares distributions from different surveys

rm(list=ls())
NAME <- 'code08_histogramming' ## Name of the R file goes here (without the file extension!)
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
library("viridis")  # colour
library(plyr)
library(caroline)

## --------
## Settings
## --------
### Any settings go here

S <- c('BOP-HH','Michigan','FRBNY')
# BOP-HH, Michigan, FRBNY - all you have
f <- 'compsur'
s <- "female"


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
  T <- read_csv(file.path('empirical', '2_pipeline',f, 'code01_align','out',S[i], 'T.csv')) %>%
    mutate(survey = S[i]) %>%
    filter(y >= -10 & y<=50)
  assign(paste("T_",S[i],sep = ""),T)
  if(i==1){
    F <- T
  }
  if(i>1){
    F <- rbind(F,T)
  }
  
  ## -- Separate inflation expectations by s
  
  y1 <- T$y[T[s]==0]
  y2 <- T$y[T[s]==1]
  
  ## --- Draw histogram
  
  # create a vector of histogram breaks
  x <- seq(-10-0.25,50+0.25,by = 0.5)
  
  
  h1 <- hist(y1, breaks = x, freq = FALSE,
             col = alpha('#238a8DFF',0.8), main = paste("Histogram of Inflation Expectations:" , s, sep = " "),
             xlim = c(-10,30), xlab = "point estimate of inflation in 12 months")
  h2 <- hist(y2, breaks = x, freq = FALSE,
             col = alpha('#FDE725FF',0.7), 
             xlim = c(-10,30), add = TRUE)

  
  ## --- Save numbers in csv
  
  H <- cbind(h1$mids,h1$counts,h1$density,h2$counts,h2$density)
  assign(paste("H_",S[i],sep = ""),H)
  write.delim(H, file = file.path(pipeline, 'out', paste('H_',S[i],s,'.txt',sep = "")), sep = "\t")

  ## ---- Overlaid Kernel Density Plot
  
  #plot first kernel density plot
  kd1 <- density(y1,bw = "nrd0", adjust = 3)
  plot(kd1, col='blue', lwd=2)
  
  #plot second kernel density plot
  kd2 <- density(y2,bw = "nrd0", adjust = 3)
  lines(kd2, col='red', lwd=2)
  
  ## --- Save numbers in csv
  
  K <- cbind(kd1$x,kd1$y,kd2$x,kd2$y)
  assign(paste("K_",S[i],sep = ""),K)
  write.delim(K, file = file.path(pipeline, 'out', paste('K_',S[i],s,'.txt',sep = "")), sep = "\t")
  
  
  
  }

rm(T)
