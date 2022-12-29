# ------------
# Introduction
# ------------

## This file 

rm(list=ls())
NAME <- 'code11_regresswithfin' ## Name of the R file goes here (without the file extension!)
PROJECT <- 'EmpiricalGenderGap'
PROJECT_DIR <- 'D:/Lovisa/Studium/Oxford/Department of Economics/DPhil' ## Change this to the directory in which your project folder is located, make sure to avoid using single backslashes (i.e. \)!

# ------------
# Preamble
# ------------

## -------
## Imports
## -------
### All the library imports go here

#install.packages('plm')
library('plm')
library('tidyverse')
library('datawizard')
library(stargazer)

## --------
## Settings
## --------
### Any settings go here

l <- "level"

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

load(file.path('empirical', '2_pipeline', 'code09_fitlit','out', 'T.RData'))
hhcluster <- read_csv(file.path('empirical', '2_pipeline', 'cluster.m','out', 'hhcluster.csv'))
T["hhcluster"] <- hhcluster

waves <- colnames(T) %>%
  str_subset("w\\d")

fincon <- c('prob_intqr','nround','refresher','f_nointerest','f_easy')
hhroles <- c('shop_groceries_nsing','shop_major_nsing','prep_meals_nsing','decide_finance_nsing')
xnames <- setdiff(colnames(T),waves) %>%
  setdiff('id') %>%
  setdiff('wave') %>%
  setdiff('y') %>%
  setdiff(c('full_time','full_time_fem')) %>%
  # need to also remove all financial confidence variables
  setdiff(fincon) %>%
  # need to remove hhroles
  setdiff(hhroles) %>%
  # need to remove also binary variable and only leave in categorical
  setdiff(c('pred_subj_bin','pred_test_bin'))
xtinames <- c("eduschool","citysize","female","eastgerman","east1989","leave","homemaker","civil_servant","entrepreneur","eduschool_fem","citysize_fem","female","eastgerman_fem","east1989_fem","leave_fem","homemaker_fem","civil_servant_fem","entrepreneur_fem")
xtvnames <- setdiff(c(xnames,fincon,hhroles),xtinames)

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


## -- Run different specifications


# baseline 
f <- as.formula(paste('y ~','factor(wave) +', paste(fincon, collapse='+'),'+', 
                      paste(hhroles, collapse='+'),'+', paste(setdiff(xnames,c("pred_subj","pred_test","hhcluster")), collapse='+'),'+',
                      paste(paste(setdiff(xtvnames,c("pred_subj","pred_test","hhcluster")),"_between",sep = ""), collapse='+')))
y.base <- plm( f, data=T_c, effect = "individual", model = "pooling")


# replace fincon
f <- as.formula(paste('y ~','factor(wave) +', 
                      paste(hhroles, collapse='+'),'+', paste(setdiff(xnames,c("hhcluster")), collapse='+'),'+',
                      paste(paste(setdiff(xtvnames,c("hhcluster")),"_between",sep = ""), collapse='+')))
y.finlit <- plm( f, data=T_c, effect = "individual", model = "pooling")

# replace hhroles
f <- as.formula(paste('y ~','factor(wave) +', paste(fincon, collapse='+'),'+', 
                      paste(setdiff(xnames,c("pred_subj","pred_test")), collapse='+'),'+',
                      paste(paste(setdiff(xtvnames,c("pred_subj","pred_test")),"_between",sep = ""), collapse='+')))
y.hhcluster <- plm( f, data=T_c, effect = "individual", model = "pooling")

# replace both 
f <- as.formula(paste('y ~','factor(wave) +', paste(xnames,collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
y.both <- plm( f, data=T_c, effect = "individual", model = "pooling")

# interactions
f <- as.formula(paste('y ~','factor(wave) +', 'female:pessimist + female:pred_subj + female:hhcluster + pessimist:pred_subj + pessimist:hhcluster + pred_subj:hhcluster +',
                      paste(xnames,collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
y.int <- plm( f, data=T_c, effect = "individual", model = "pooling")


# --- Write output

# settings for stargazer
notes <- "The full set of estimators included can be found in the appendix."
omit <- c("wave","q\\_|exp","si\\_","between")
omit.labels <- c("Time dummies","Macro qualitative","Shop intent","Between effects")
title <- "Collapsing variables"
label <- "tab:finlithhcluster"
dep.var.labels <- "Inflation expectation, 12 months ahead, point estimate"

# which variables are removed
rmvars <- c(str_subset(names(coef(y.int)),"wave"),str_subset(names(coef(y.int)),"q\\_|exp"),str_subset(names(coef(y.int)),"si\\_"),str_subset(names(coef(y.int)),"between"), c("eduschool","eduwork","hhchildren","hhinc","pinc","age","citysize","eastgerman","east1989","part_time","unemployed","retired"))
# which are staying
stayvars <- setdiff(names(coef(y.int)),rmvars)
# in which order
desiredOrder <- c("Constant","female","pessimist","pred_subj","pred_test",
                  "prob_intqr","refresher","nround","f_nointerest","f_easy","hhcluster",
                  "non_single","shop_groceries_nsing","shop_major_nsing","prep_meals_nsing",
                  "decide_finance_nsing")

writeLines(capture.output(stargazer(y.base, y.finlit, y.hhcluster, y.both, y.int, 
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels)), 
           file.path('empirical', '3_output','results', NAME,l, 'code_finlithhcluster.tex'))
