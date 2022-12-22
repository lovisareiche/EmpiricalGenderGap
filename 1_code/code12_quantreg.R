# ------------
# Introduction
# ------------

## This file runs a quantile regression

rm(list=ls())
NAME <- 'code12_quantreg' ## Name of the R file goes here (without the file extension!)
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
library('datawizard')
library(stargazer)
library(quantreg)



## --------
## Settings
## --------
### Any settings go here

l <- 'level'

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

if (!dir.exists(file.path('empirical', '3_output','results',NAME,l))) {
  dir.create(file.path('empirical', '3_output','results',NAME,l))
}



# ---------
# Main code
# ---------

## -- Load data from pipeline folder --

T <- read_csv(file.path('empirical', '2_pipeline', 'code03_compilepanel.m','out','base', 'T.csv')) %>%
  pdata.frame( index=c( "id", "wave" ) )

waves <- colnames(T) %>%
  str_subset("w\\d")
xnames <- setdiff(colnames(T),waves) %>%
  setdiff('id') %>%
  setdiff('wave') %>%
  setdiff('y') %>%
  setdiff('full_time') %>%
  setdiff('full_time_fem')
xtinames <- c("eduschool","citysize","female","eastgerman","east1989","leave","homemaker","civil_servant","entrepreneur","eduschool_fem","citysize_fem","female","eastgerman_fem","east1989_fem","leave_fem","homemaker_fem","civil_servant_fem","entrepreneur_fem")
xtvnames <- setdiff(xnames,xtinames)

## -- define dependent variables

# in log
if (l == 'log') {
  T$y = log(T$y-min(T$y)+1)
}
# without log it's just y

## -- Use degrouped estimator

T_mean <- degroup(
  T,
  xtvnames,
  "id",
  center = "mean",
  suffix_demean = "_within",
  suffix_groupmean = "_between",
  add_attributes = TRUE,
  verbose = TRUE
)

T_c <- cbind(T,T_mean) %>%
  pdata.frame(index=c( "id", "wave" ) )


## -- Run quantile regression

# this uses the normal standrad baseline
f <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))


# this uses the latest regression as in code 09 regresswithfin
# f <- as.formula(paste('y ~','factor(wave) +', paste(xnames,collapse='+'),'+', paste(paste(xtvnames,"_between",sep = ""), collapse='+')))

# specify tau as which percentile we want to look at

m02 <- rq(f, data = T_c, tau = 0.2)
m04 <- rq(f, data = T_c, tau = 0.4)
m06 <- rq(f, data = T_c, tau = 0.6)
m08 <- rq(f, data = T_c, tau = 0.8)


# --- Write output

# settings for stargazer
notes <- "The full set of estimators included can be found in the appendix."
omit <- c("wave","q\\_|exp","si\\_","between")
omit.labels <- c("Time dummies","Macro qualitative","Shop intent","Between effects")
title <- "Quantile regression"
label <- "tab:quantreg"
dep.var.labels <- "Inflation expectation, 12 months ahead, point estimate"
column.labels <- c("Bottom 20%", "Bottom 40%", "Bottom 60%", "Bottom 80%")

# in which order
desiredOrder <- c("Constant","female","live_alone","shop_groceries","shop_major",
                  "prep_meals","decide_finance","pessimist","prob_intqr","refresher",
                  "nround","f_nointerest","f_easy","eduschool","eduwork","hhchildren",
                  "hhinc","pinc","age","citysize","eastgerman","east1989","part_time",
                  "unemployed","retired")

writeLines(capture.output(stargazer(m02, m04, m06, m08, 
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, column.labels = column.labels,
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels)), 
           file.path('empirical', '3_output','results', NAME,l, 'code_quantreg.tex'))


