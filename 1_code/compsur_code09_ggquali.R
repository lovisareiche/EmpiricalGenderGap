# ------------
# Introduction
# ------------

## Runs timeseries regression to understand if food prices really drive gender gap

rm(list=ls())
NAME <- 'code09_ggquali' ## Name of the R file goes here (without the file extension!)
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
library(stargazer)
library('plm')
library(zoo) # for date vector
library('datawizard')

## --------
## Settings
## --------
### Any settings go here

f <- 'compsur'
S <- c('BOP-HH','Michigan','FRBNY')
# BOP-HH, Michigan, FRBNY - all you have



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
    mutate(survey = S[i], date = as.yearmon(paste(year, month), format = "%Y %m")) %>%
    pdata.frame( index=c( "id", "date" ) )
  assign(paste("T_",S[i],sep = ""),T)
  
  # do t test and wilcoxon rank test
  
  tgap = t.test(T$quali[T$female == 1],T$quali[T$female == 0])
  assign(paste("tgap_",S[i],sep = ""),tgap)
  
  wgap = wilcox.test(T$quali[T$female == 1],T$quali[T$female == 0])
  assign(paste("wgap_",S[i],sep = ""),wgap)
  
  # do multivariate analysis
  
  xnames <- setdiff(colnames(T),'date') %>%
    setdiff(c('y','quali')) %>%
    setdiff('month') %>%
    setdiff('year') %>%
    setdiff('survey') %>%
    setdiff('id') 
  
  T_mean <- degroup(
    T,
    c("age","hhinc"),
    "id",
    center = "mean",
    suffix_demean = "_within",
    suffix_groupmean = "_between",
    add_attributes = TRUE,
    verbose = TRUE
  )
  
  T_c <- cbind(T,T_mean) %>%
    pdata.frame(index=c( "id", "date" ) )
  
  eq <- as.formula(paste('quali ~ factor(date) +', paste(xnames, collapse='+'), '+ age_between + hhinc_between'))
  
  m <- plm( eq, data=T_c, effect = "individual", model = "pooling" )
  assign(paste("m_",S[i],sep = ""),m)
  
}

# write output

# settings for stargazer
title <- "Multivariate: The gender gap for singles and non-singles"
omit <- c("factor","between")
omit.labels <- c("Year dummies","Between effects")
label <- "tab:ggsinglemulti"
dep.var.labels <- "Inflation expectation, 12 months ahead, point estimate"

# in which order
desiredOrder <- c("Constant","female","age","eduschool","hhinc",
                  "region")

writeLines(capture.output(stargazer(`m_BOP-HH`,m_FRBNY,m_Michigan,
                                    title = title, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels)), 
           file.path(outline, 'code_ggsinglemulti.tex'))
