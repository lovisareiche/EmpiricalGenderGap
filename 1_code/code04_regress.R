# ------------
# Introduction
# ------------

## This file computes the regressions for the cleaned unbalanced dataset

rm(list=ls())
NAME <- 'code04_regress' ## Name of the R file goes here (without the file extension!)
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

## --------
## Settings
## --------
### Any settings go here

t <- 'no_shopintent'
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

if (!dir.exists(file.path(pipeline,'out',t))) {
  dir.create(file.path(pipeline,'out',t))
}

if (!dir.exists(file.path(pipeline,'out',t,l))) {
  dir.create(file.path(pipeline,'out',t,l))
}

# ---------
# Main code
# ---------

## -- Load data from pipeline folder --

T <- read_csv(file.path('empirical', '2_pipeline', 'code03_compilepanel.m','out',t, 'T.csv'))

waves <- colnames(T) %>%
  str_subset("w\\d")
xnames <- setdiff(colnames(T),waves) %>%
  setdiff('id') %>%
  setdiff('wave') %>%
  setdiff('y') %>%
  setdiff('full_time') %>%
  setdiff('full_time_fem')
xendonames <- c("pessimist","q_unemployment","q_rent","q_lending","q_inflation","q_property","q_fuel","q_tax","exphp_point","expint_sav","si_major","si_clothing","si_entz","si_mobility","si_services","si_holiday","si_reserves","f_nointerest","shop_groceries","shop_major","hhchildren","age","citysize","eastgerman","live_alone","unemployed","retired","civil_servant","decide_finance","leave","refresher","prob_md","east1989","pessimist_fem","q_unemployment_fem","q_rent_fem","q_lending_fem","q_inflation_fem","q_property_fem","q_fuel_fem","q_tax_fem","exphp_point_fem","expint_sav_fem","si_major_fem","si_clothing_fem","si_entz_fem","si_mobility_fem","si_services_fem","si_holiday_fem","si_reserves_fem","f_nointerest_fem","shop_groceries_fem","shop_major_fem","hhchildren_fem","age_fem","citysize_fem","eastgerman_fem","live_alone_fem","unemployed_fem","retired_fem","civil_servant_fem","decide_finance_fem","leave_fem","refresher_fem","prob_md_fem","east1989_fem")
xexonames <- setdiff(xnames,xendonames)
xtinames <- c("eduschool","citysize","female","eastgerman","east1989","leave","homemaker","civil_servant","entrepreneur","eduschool_fem","citysize_fem","female","eastgerman_fem","east1989_fem","leave_fem","homemaker_fem","civil_servant_fem","entrepreneur_fem")
xtvnames <- setdiff(xnames,xtinames)
T <- pdata.frame( T, index=c( "id", "wave" ) )

## -- define dependent variables

# in log
if (l == 'log') {
  T$y = log(T$y-min(T$y)+1)
}
# without log it's just y


## -- run OLS

## ols without time fixed effects

# assumption: observations are iid draws from a target population
# problem: ignores time invariant individual effects and/or individual-invariant time effects
# such that the conditional prob density of yit cond. on xit differs between cross-sectional units

f <- as.formula(paste('y ~', paste(xnames, collapse='+')))
y.OLS <- lm( f, data=T )


## least squares dummy variable estimation

# add time specific fixed effects (assume no individual-specific effects)
# add individual specific fixed effects (assume no time-specific effects)
# add both
# consistency relies only on usual exogeneity 

f <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+')))
y.LSDVt <- lm( f, data=T )
y.PO <- plm( f, data=T, effect = "individual", model = "pooling")


## --- Variable Coefficients Model

# allow coefficients to vary between individuals or time periods

f <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+')))
y.VCMt <- pvcm( f, data=T, effect = "time")

## --- Fixed effects

f <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+')))
y.FEt <- plm( f, data=T, effect = "individual",model = "within" )



## --- Between Estimator

# OLS on time averaged equation
# assumption for consistency: unobserved individual effects must be uncorrelated with explanatory 
# variables (note that ind. random effects is more efficient)

f <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+')))
y.B <- plm( f, data=T, effect = "individual", model = "between")


## --- Random effects estimator

# assumption: individual and/or time effects are uncorrelated with the covariates

f <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+')))
y.RE <- plm( f, data=T, effect = "time", model = "random", random.method = "walhus")
f <- as.formula(paste('y ~','factor(wave) +', paste(setdiff(xnames,c("female","east1989")), collapse='+')))
y.RE_subset <- plm( f, data=T, effect = "time", model = "random", random.method = "walhus")

## --- Feasible GLS Estimator

# Uses two-step procedure to arrive at unrestricted covariance structure robust against group-wise 
# correlation and heteroscedasticity
# Assumes no group-wise heteroscedasticity and cross-sectional dependence

f <- as.formula(paste('y ~', paste(xnames, collapse='+','+ factor(wave)')))
y.FGLS <- pggls( f, data=T, model = "pooling")

## --- Hausman Taylor type estimators

# Assume there are endogenous and exogenous time-constant and time-varying variables

# xnames contains all variables
# xexonames only contains exogenous variables

f <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+'), '|','factor(wave) +', paste(xexonames, collapse='+')))
y.HT  <- pht( f, data=T, model = "ht")

f <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+'), '|','factor(wave) +', paste(xexonames, collapse='+'), '|', paste(setdiff(xnames,xexonames), collapse='+')))
y.HTbaltagi  <- plm( f, data=T, random.method = "ht", model = "random", inst.method = "baltagi")
#y.HTam  <- plm( f, data=T, random.method = "ht", model = "random", inst.method = "am")


# Include time varying as averages to control 

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

f <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+'),'+',paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
y.LSDVt_control <- plm( f, data=T_c, effect = "individual", model = "pooling")
y.LSDVt_control2 <- lm( f, data=T_c)

# --- Variable importance

# contribution of variable
caret::varImp(y.LSDVt_control2)

# check multicollinearity
car::vif(y.LSDVt_control2)

## -- Save data to pipeline folder -- 

save(T, y.B, y.FEt, y.FGLS, y.HT, y.HTbaltagi, y.LSDVt, y.LSDVt_control, y.OLS, y.PO, y.RE, y.RE_subset, y.VCMt , file = file.path(pipeline, 'out',t,l, 'T.RData'))

# ----------
# Leftovers
# ----------
## Here you leave any code snippets or temporary code that you don't need but don't want to delete just yet


# f <- as.formula(paste('y ~', paste(xnames, collapse='+'),'+ factor(id)'))
# y.LSDVi <- lm( f, data=T )

# f <- as.formula(paste('y ~', paste(xnames, collapse='+'),'+ factor(wave) + factor(id)'))
# y.LSDVit <- lm( f, data=T )


# This treatment follows Wooldridge

# Assume time-constant variables uncorrelated with unobserved effect but time varying variables are 
# possibly correlated with time-invariant unobserved effect

# Use fixed effects to obtain coefficient on the time-varying explanatory variables
# betait <- yFEi$coefficients

# identify time invariant vars
# tivars <- setdiff(xnames,names(yFEi$coefficients))
# tvvars <- names(yFEi$coefficients)

# compute coefficients
# N <- nrow(T)
# z <- as.matrix(T[tivars])
# zprime <- t(z)

# denominator
# s <- vector(length = N)
#for (i in 1:N) {
#  s[i] <- zprime[,i] %*% z[i,]
#}

# denominator <- 1/N * sum(s)

# numerator

#f <- as.formula(paste('y ~', paste(tvvars, collapse='+')))
#yBus  <- plm( f, data=T, effect = "individual", model = "between")

#s <- matrix(nrow = length(tivars), ncol = N)
#for (i in 1:N) {
#  s[i] <- zprime[,i] * yBus$residuals[T[i,"id"]]
#}
#numerator <- 1/N * rowSums(s, na.rm = TRUE)

#gammai <- numerator/denominator

# still need to figure out standard errors

