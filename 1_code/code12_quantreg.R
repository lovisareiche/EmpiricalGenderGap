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
library('plm')




## --------
## Settings
## --------
### Any settings go here

l <- 'log'
t <- 'demo_only'
# sub <- 'subj_finilliterate'
sub <- ''

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

if (!dir.exists(file.path('empirical', '3_output','results',NAME,t))) {
  dir.create(file.path('empirical', '3_output','results',NAME,t))
}

if (!dir.exists(file.path('empirical', '3_output','results',NAME,t,l))) {
  dir.create(file.path('empirical', '3_output','results',NAME,t,l))
}


# ---------
# Main code
# ---------

## -- Load data from pipeline folder --

D <- read_csv(file.path('empirical', '2_pipeline', 'code03_compilepanel.m','out',t, 'T.csv')) %>%
  pdata.frame( index=c( "id", "wave" ) )

if(sub != ''){
  load(file.path('empirical', '2_pipeline', 'code09_fitlit','out', 'T.RData'))
  D$subj_finilliterate <- as.numeric(T$pred_subj_bin==0)
}

rm(T)
T <- D
rm(D)


waves <- colnames(T) %>%
  str_subset("w\\d")
xnames <- setdiff(colnames(T),waves) %>%
  setdiff('id') %>%
  setdiff('wave') %>%
  setdiff('y') %>%
  setdiff(c('full_time','full_time_fem')) %>%
  setdiff(c('subj_finilliterate'))
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

## -- Use subsample if specified

if(sub != ''){
  T0 <- T_c[T_c[sub] == 0,]
  T1 <- T_c[T_c[sub] == 1,]
}

## -- Run quantile regression

# this uses the normal standard baseline
f <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))

# specify tau as which percentile we want to look at

if(sub != ''){
  
  rqs_sub0 <- rq(f, data = T0, tau = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
  rqs_sub1 <- rq(f, data = T1, tau = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
  rqs0_sub0 <- rq(y ~ 1, data = T0, tau = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
  rqs0_sub1 <- rq(y ~ 1, data = T1, tau = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
  
  rho <- function(u,tau=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))u*(tau - (u < 0))
  R2_sub0 <- 1 - rqs_sub0$rho/rqs0_sub0$rho
  R2_sub1 <- 1 - rqs_sub1$rho/rqs0_sub1$rho
  
  s.rqs_sub0 <- summary.rqs(rqs_sub0)
  s.rqs_sub1 <- summary.rqs(rqs_sub1)
  
  jpeg(file.path('empirical','3_output','results', NAME,t,l,paste("quantreg_female_",sub,"0.jpg")), width = 1000, height = 700)
  plot.summary.rqs(s.rqs_sub0, parm = "female", ols = FALSE)
  dev.off()
  jpeg(file.path('empirical','3_output','results', NAME,t,l,paste("quantreg_female_",sub,"1.jpg")), width = 1000, height = 700)
  plot.summary.rqs(s.rqs_sub1, parm = "female", ols = FALSE)
  dev.off()
}


m02 <- rq(f, data = T_c, tau = 0.2)
m04 <- rq(f, data = T_c, tau = 0.4)
m06 <- rq(f, data = T_c, tau = 0.6)
m08 <- rq(f, data = T_c, tau = 0.8)

rqs <- rq(f, data = T_c, tau = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
rqs0 <- rq(y ~ 1, data = T_c, tau = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))

rho <- function(u,tau=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))u*(tau - (u < 0))
R2 <- 1 - rqs$rho/rqs0$rho

s.rqs <- summary.rqs(rqs)

jpeg(file.path('empirical','3_output','results', NAME,t,l,"quantreg_female.jpg"), width = 1000, height = 700)
plot.summary.rqs(s.rqs, parm = "female", ols = FALSE, ylim = c(-0.005,0.02))
dev.off()

jpeg(file.path('empirical','3_output','results', NAME,t,l,"quantreg_fincon.jpg"), width = 1000, height = 700)
plot.summary.rqs(s.rqs, parm = c("prob_intqr","nround","f_nointerest","f_easy"), ols = FALSE)
dev.off()


# --- Write output

# settings for stargazer
notes <- "The full set of estimators included can be found in the appendix."
omit <- c("wave","q\\_|exp","si\\_","between")
omit.labels <- c("Time dummies","Macro qualitative","Shop intent","Between effects")
title <- "Quantile regression"
label <- "tab:quantreg"
dep.var.labels <- "Inflation expectation, 12 months ahead, point estimate"
column.labels <- c("Bottom 20%", "Bottom 40%", "Bottom 60%", "Bottom 80%")
r2 <- c("Pseudo $R^2$",round(R2[2], digits = 2),round(R2[4], digits = 2),round(R2[6], digits = 2),round(R2[8], digits = 2))
add.lines <- list(r2)

# in which order
desiredOrder <- c("Constant","female","non_single","shop_groceries_nsing","shop_major_nsing",
                  "prep_meals_nsing","decide_finance_nsing","pessimist","prob_intqr","refresher",
                  "nround","f_nointerest","f_easy","eduschool","eduwork","hhchildren",
                  "hhinc","pinc","age","citysize","eastgerman","east1989","part_time",
                  "unemployed","retired")

writeLines(capture.output(stargazer(m02, m04, m06, m08, 
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, column.labels = column.labels,
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels,
                                    add.lines = add.lines)), 
           file.path('empirical', '3_output','results', NAME,t,l, 'code_quantreg.tex'))


