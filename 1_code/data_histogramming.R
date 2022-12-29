# ------------
# Introduction
# ------------

## This file 

rm(list=ls())
NAME <- 'data_histogramming' ## Name of the R file goes here (without the file extension!)
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

## --------
## Settings
## --------
### Any settings go here

s <- "female"
# run "female", "pessimist", "subj_finilliterate","hhcluster"

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

if (!dir.exists(file.path('empirical', '3_output','results',NAME,s))) {
  dir.create(file.path('empirical', '3_output','results',NAME,s))
}




# ---------
# Main code
# ---------

## -- Load data from pipeline folder --

load(file.path('empirical', '2_pipeline', 'code09_fitlit','out', 'T.RData'))
hhcluster <- read_csv(file.path('empirical', '2_pipeline', 'cluster.m','out', 'hhcluster.csv'))
T["hhcluster"] <- hhcluster
T["pessimist"] <- as.numeric(T$pessimist>=3)
T["subj_finilliterate"] <- as.numeric(T$pred_subj_bin==0)

## -- Separate inflation expectations by s

y1 <- T$y[T[s]==0]
y2 <- T$y[T[s]==1]

## --- Draw histogram

jpeg(file.path('empirical','3_output','results', NAME,s,paste("histogram_",s,".jpg", sep = "")), width = 1000, height = 700)

hist(y1, breaks = c(min(y1),-10,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,20,30,max(y1)), 
     col = alpha('#238a8DFF',0.8), main = paste("Histogram of Inflation Expectations:" , s, sep = " "),
     xlim = c(-10,30))
hist(y2, breaks = c(min(y2),-10,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,20,30,max(y2)), 
     col = alpha('#FDE725FF',0.7), 
     xlim = c(-10,30), add = TRUE)
legend("topleft", c(paste("not",s, sep = " "),s), fill = c('#238a8DFF','#FDE725FF'))
dev.off()
