# ------------
# Introduction
# ------------

## This file computes the regressions for the cleaned unbalanced dataset

rm(list=ls())
NAME <- 'data_hicptimeseries' ## Name of the R file goes here (without the file extension!)
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
library(lubridate) # for working with dates
library(ggplot2)  # for creating graphs
library(scales)   # to access breaks/formatting functions
library(gridExtra) # for arranging plots
library("reshape2")                          # Load reshape2 package

## --------
## Settings
## --------
### Any settings go here



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


## -- Load data from 0_data folder --

food <- read_csv2(file.path('empirical', '0_data', 'external','Germany - HICP - Food and Non-alcoholic beverages.csv')) %>%
  mutate(date = as.Date(date), s1 = as.numeric(s1))
overall <- read_csv2(file.path('empirical', '0_data', 'external','Germany - HICP - Overall index.csv')) %>%
  mutate(date = as.Date(date), s1 = as.numeric(s1))

df <- data.frame(date = food$date, hicp = overall$s1, hicp_food = food$s1)
df_long <- melt(df, id.vars = "date")    # Reshaping data to long format

jpeg(file.path('empirical','3_output','results', NAME,"hicptimeseries.jpg"), width = 1000, height = 700)


#create time series plot
p <- ggplot(df_long, aes(x=date, y = value, col = variable)) +
  geom_line(na.rm=TRUE)  +
# set x and y labels
  xlab("Date") + ylab("HICP (Annual Percentage Change)") +
# Set minor breaks at survey start and end date  
  scale_x_date(minor_breaks = as.Date(c("2021-02-01","2022-01-01"))) +
# Hiding legend title
  theme(legend.title = element_blank()) +
# For placing the legend position as top
 theme(legend.position = "top")

# highlight sample period
p + geom_rect(
  aes(
    xmin = as.Date('2021-02-01'),
    xmax = as.Date('2022-01-01'),
    ymin = Inf,
    ymax = -Inf
  ),
  fill = NA,
  color = "yellow",
  alpha = 0.5,
  size = 1
) +
# add trendline
  geom_smooth(method=lm, se=FALSE)



dev.off()

