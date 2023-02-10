# ------------
# Introduction
# ------------

## Compares distributions from different surveys

rm(list=ls())
NAME <- 'code02_ggsingle' ## Name of the R file goes here (without the file extension!)
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
library(moments) # to compute skew
library(zoo) # for date vector
library(xtable) # for latex conversion
library(stargazer) # for latex table

## --------
## Settings
## --------
### Any settings go here

S <- c('BOP-HH','Michigan','FRBNY')
# BOP-HH, Michigan, FRBNY - all you have
f <- 'compsur'


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

for(i in 1:length(S)){
  
  # load aligned data
  
  T <- read_csv(file.path('empirical', '2_pipeline',f, 'code01_align','out',S[i], 'T.csv')) %>%
    mutate(survey = S[i], date = as.yearmon(paste(year, month), format = "%Y %m"))
  assign(paste("T_",S[i],sep = ""),T)
  
  # do t test and wolcoxon rank test
  
  Ts <- filter(T,single == 1)
  tgaps = t.test(Ts$y[Ts$female == 1],Ts$y[Ts$female == 0])
  assign(paste("tgaps_",S[i],sep = ""),tgaps)
  
  wgaps = wilcox.test(Ts$y[Ts$female == 1],Ts$y[Ts$female == 0])
  assign(paste("wgaps_",S[i],sep = ""),wgaps)
  
  Tn <- filter(T,single == 0)
  tgapn = t.test(Tn$y[Tn$female == 1],Tn$y[Tn$female == 0])
  assign(paste("tgapn_",S[i],sep = ""),tgapn)
  
  wgapn = wilcox.test(Tn$y[Tn$female == 1],Tn$y[Tn$female == 0])
  assign(paste("wgapn_",S[i],sep = ""),wgapn)
  
  # do multivariate analysis
  
  xnames <- setdiff(colnames(T),'date') %>%
    setdiff('y') %>%
    setdiff('month') %>%
    setdiff('year') %>%
    setdiff('survey') %>%
    setdiff('single')

  eq <- as.formula(paste('y ~ factor(date) +', paste(xnames, collapse='+')))
  n <- lm( eq, data=filter(T,single ==0) )
  assign(paste("n_",S[i],sep = ""),n)
  s <- lm( eq, data=filter(T,single ==1) )
  assign(paste("s_",S[i],sep = ""),s)
  
}

# write output

# settings for stargazer
title <- "Multivariate: The gender gap for singles and non-singles"
omit <- c("factor")
omit.labels <- c("Time dummies")
label <- "tab:ggsinglemulti"
dep.var.labels <- "Inflation expectation, 12 months ahead, point estimate"

# in which order
desiredOrder <- c("Constant","female","age","eduschool","hhinc",
                  "region")

writeLines(capture.output(stargazer(`n_BOP-HH`,`s_BOP-HH`,n_FRBNY,s_FRBNY,n_Michigan,s_Michigan,
                                    title = title, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels)), 
           file.path(outline, 'code_ggsinglemulti.tex'))
