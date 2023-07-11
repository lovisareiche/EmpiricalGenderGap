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
library(plyr)
library(caroline)

## --------
## Settings
## --------
### Any settings go here

f <- 'bopreg'
s <- "test_finilliterate"


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
# Main code
# ---------

## -- Load data from pipeline folder --

load(file.path('empirical', '2_pipeline',f, 'code09_fitlit','out', 'T_fin.RData'))

T <- T %>%
  filter(y >= -10 & y<=50)

T["pessimist"] <- as.numeric(T$pessimist>=3)
T["subj_finilliterate"] <- as.numeric(T$lpred_subj < median(T$lpred_subj))
T["test_finilliterate"] <- as.numeric(T$lpred_test< median(T$lpred_test))

## -- Separate inflation expectations by s

y1 <- T$y[T[s]==0]
y2 <- T$y[T[s]==1]

## --- Draw histogram

# create a vector of histogram breaks
# create a vector of histogram breaks
x <- seq(-10-0.25,50+0.25,by = 0.5)


jpeg(file.path('empirical','3_output','results', NAME,paste("histogram_",s,".jpg", sep = "")), width = 1000, height = 700)

h1 <- hist(y1, breaks = x, freq = FALSE,
     col = alpha('#238a8DFF',0.8), main = paste("Histogram of Inflation Expectations:" , s, sep = " "),
     xlim = c(-10,30), xlab = "point estimate of inflation in 12 months")
h2 <- hist(y2, breaks = x, freq = FALSE,
     col = alpha('#FDE725FF',0.7), 
     xlim = c(-10,30), add = TRUE)
legend("topleft", c(paste("not",s, sep = " "),s), fill = c('#238a8DFF','#FDE725FF'))
dev.off()

## --- Save numbers in csv

## --- Save numbers in csv

H <- cbind(h1$mids,h1$counts,h1$density,h2$counts,h2$density)
write.delim(H, file = file.path(pipeline, 'out', paste('H_',s,'.txt',sep = "")), sep = "\t")

