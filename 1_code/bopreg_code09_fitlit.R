# ------------
# Introduction
# ------------

## This file fits a financial literacy test score and subjective measure by 
## regressing within selected sample and
## predicting out of sample


rm(list=ls())
NAME <- 'code09_fitlit' ## Name of the R file goes here (without the file extension!)
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
library(plm)
library('datawizard') # for degrouping
library(psychTools) # for writing latex
library(stargazer) # for writing regression tables
library(pscl) # for McFadden R2
library(caret) # for variable importance
library(InformationValue) # for optimal cutoff
library(rms) # for ordered logit


## --------
## Settings
## --------
### Any settings go here
f <- 'bopreg'


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

T_fin <- read_csv(file.path('empirical', '2_pipeline',f, 'code03_compilepanel.m','out','base', 'T_fin.csv')) %>%
  pdata.frame(index=c( "id", "wave" )) %>%
  # create binary vars
  mutate(fin_lit_subj_bin=as.numeric(fin_lit_subj >=2)) %>%
  mutate(fin_lit_test_bin=as.numeric(fin_lit_test >=2))

T <- read_csv(file.path('empirical', '2_pipeline',f, 'code03_compilepanel.m','out','base', 'T.csv')) %>%
  pdata.frame(index=c( "id", "wave" ))


finlitnames <- c("prob_intqr", "nround", "refresher", "f_easy", "f_nointerest")
waves_fin <- colnames(T_fin) %>%
  str_subset("w\\d")
waves <- colnames(T) %>%
  str_subset("w\\d")

xnames <- setdiff(colnames(T),waves) %>%
  setdiff('id') %>%
  setdiff('wave') %>%
  setdiff('y') %>%
  setdiff(c('full_time','full_time_fem'))

# Section to include time averages
# xnames <- setdiff(colnames(T),waves) %>%
#   setdiff('id') %>%
#   setdiff('wave') %>%
#   setdiff('y') %>%
#   setdiff('full_time') 
# xtinames <- c("eduschool","citysize","female","eastgerman","east1989","leave","homemaker","civil_servant","entrepreneur","eduschool_fem","citysize_fem","female","eastgerman_fem","east1989_fem","leave_fem","homemaker_fem","civil_servant_fem","entrepreneur_fem")
# xtvnames <- setdiff(xnames,xtinames)
#
# # Include time varying as averages to control 
# 
# T_mean <- degroup(
#   T,
#   xtvnames,
#   "id",
#   center = "mean",
#   suffix_demean = "_within",
#   suffix_groupmean = "_between",
#   add_attributes = TRUE,
#   verbose = TRUE
# )
# 
# T_mean_fin <- degroup(
#   T_fin,
#   xtvnames,
#   "id",
#   center = "mean",
#   suffix_demean = "_within",
#   suffix_groupmean = "_between",
#   add_attributes = TRUE,
#   verbose = TRUE
# )
# 
# T <- cbind(T,T_mean) %>%
#   pdata.frame(index=c( "id", "wave" ) )
# 
# T_fin <- cbind(T_fin,T_mean_fin) %>%
#   pdata.frame(index=c( "id", "wave" ) )


## -- Regress actual measures to apply out of sample


f_subj <- as.formula(paste('fin_lit_subj ~ ', paste(xnames, collapse='+'),'+',
                           #paste(paste(finlitnames,"_between",sep = ""), collapse='+'),'+',
                           paste(waves_fin[1:length(waves_fin)-1], collapse='+')))
f_test <- as.formula(paste('fin_lit_test ~ ', paste(xnames, collapse='+'),'+',
                           #paste(paste(finlitnames,"_between",sep = ""), collapse='+'),'+',
                           paste(waves_fin[1:length(waves_fin)-1], collapse='+')))

f_subj_bin <- as.formula(paste('fin_lit_subj_bin ~ ', paste(xnames, collapse='+'),'+',
                           #paste(paste(finlitnames,"_between",sep = ""), collapse='+'),'+',
                           paste(waves_fin[1:length(waves_fin)-1], collapse='+')))
f_test_bin <- as.formula(paste('fin_lit_test_bin ~ ', paste(xnames, collapse='+'),'+',
                           #paste(paste(finlitnames,"_between",sep = ""), collapse='+'),'+',
                           paste(waves_fin[1:length(waves_fin)-1], collapse='+')))

options(datadist='ddist')
ddist<- datadist(finlitnames)
ddist<- datadist(waves_fin)
ddist<- datadist(paste(finlitnames,"_between",sep = ""))

lsubj <- lrm(f_subj, data= T_fin)
ltest <- lrm(f_test, data= T_fin)
lsubj_bin <- glm(f_subj_bin, family="binomial", data=T_fin)
ltest_bin <- glm(f_test_bin, family="binomial", data=T_fin)

# --- Assessing model fit for logistic

# needs to be added manually in the stargazer file
mfr2_subj <- pscl::pR2(lsubj_bin)["McFadden"]
mfr2_test <- pscl::pR2(ltest_bin)["McFadden"]

#calculate predicted value for each individual in full dataset
T$pred_subj <- predict(lsubj, T)
T$pred_test <- predict(ltest, T)

# needs a bit more procedure for logistic
# find optimal cutoff probability to use to maximize accuracy
optsubj <- optimalCutoff(T_fin$fin_lit_subj_bin, fitted.values(lsubj_bin))[1]
opttest <- optimalCutoff(T_fin$fin_lit_test_bin, fitted.values(ltest_bin))[1]

# predict first and then create binary variable, 1 if exceeds cutoff
T <- mutate(T, pred_subj_bin = predict(lsubj_bin, T, type ="response")) %>%
  mutate(pred_subj_bin = as.numeric(pred_subj_bin >= optsubj)) %>%
  mutate(pred_test_bin = predict(ltest_bin, T, type = "response")) %>%
  mutate(pred_test_bin = as.numeric(pred_test_bin >= opttest))

# calculate sensitivity
sensubj <- sensitivity(T_fin$fin_lit_subj_bin, fitted.values(lsubj_bin))
sentest <- sensitivity(T_fin$fin_lit_test_bin, fitted.values(ltest_bin))

#calculate specificity
spesubj <- specificity(T_fin$fin_lit_subj_bin, fitted.values(lsubj_bin))
spetest <- specificity(T_fin$fin_lit_test_bin, fitted.values(ltest_bin))


#calculate total misclassification error rate
misesubj <- misClassError(T_fin$fin_lit_subj_bin, fitted.values(lsubj_bin), threshold=optsubj)
misetest <- misClassError(T_fin$fin_lit_test_bin, fitted.values(ltest_bin), threshold=opttest)


# --- Comparing models


# settings for stargazer
title <- "Explaining financial literacy through financial confidence variables"
label <- "tab:fitlit"
dep.var.labels <- c("Subjective financial literacy","Financial literacy test score")
omit <- c("w\\d","_between")
omit.labels <- c("Time dummies","Between effects")
column.labels <- c("Ordered logit","Logistic","Ordered logit","Logistic")



writeLines(capture.output(stargazer(lsubj, lsubj_bin, ltest, ltest_bin,
                                    title = title, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    column.labels = column.labels, model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE)), 
           file.path(outline,'code_fitlit.tex'))


## -- Save T with new predicted variables
save(T, file = file.path(pipeline, 'out', 'T.RData'))


## Alternative way to show results

# --- Variable importance

# contribution of variable
tvimp <- caret::varImp(ltest_bin)
myvalues <- tvimp$Overall
mynames <- rownames(tvimp)
tvimp <- setNames(myvalues, mynames)

svimp <- caret::varImp(lsubj_bin)
myvalues <- svimp$Overall
mynames <- rownames(svimp)
svimp <- setNames(myvalues, mynames)

# check multicollinearity

tvif <- car::vif(ltest_bin)

svif <- car::vif(lsubj_bin)

# --- Comparing models

# create "models" that show variable importance (absolute value of z startstic) and VIF (variance inflation factor)
tvimpm <- ltest_bin
tvimpm$coefficients <- tvimp
tvifm <- ltest_bin
tvifm$coefficients <- tvif

svimpm <- lsubj_bin
svimpm$coefficients <- svimp
svifm <- lsubj_bin
svifm$coefficients <- svif

# settings for stargazer
omit <- c("q\\_|exp","si\\_","w\\d","_between")
omit.labels <- c("Qualitative Macro","Shop intent","Time dummies","Between effects")
title <- "Predicting Financial Literacy"
label <- "tab:predfinlit"
dep.var.labels <- c("","","","","Subjective financial literacy","Financial literacy test score")
column.labels <- c("Absolute z score", "Variance Inflation Factor","Regression Coefficients")
column.separate <- c(2,2,2)
se <- c(NA, NA, NA, NA, NULL, NULL)
tstat <- c(NA, NA, NA, NA, NULL, NULL)
p <- c(NA, NA, NA, NA, NULL,NULL)
r2 <- c("Mc Fadden $R^2$","","","","",round(mfr2_test, digits = 2),round(mfr2_subj, digits = 2))
mise <- c("Missclassification Error Rate","","","","",round(misetest, digits = 2),round(misesubj, digits = 2))
sen <- c("Sensitivity","","","","",round(sentest, digits = 2),round(sensubj, digits = 2))
spe <- c("Specificity","","","","",round(spetest, digits = 2),round(spesubj, digits = 2))
add.lines <- list(r2,mise,sen,spe)

# in which order
desiredOrder <- c("Constant","shop_groceries_nsing","shop_major_nsing","prep_meals_nsing",
                  "decide_finance_nsing","pessimist","prob_intqr","refresher",
                  "nround","f_nointerest", "f_easy","eduschool","eduwork",
                  "hhchildren","hhinc","pinc","age","citysize",
                  "eastgerman","east1989","part_time","unemployed","retired")

writeLines(capture.output(stargazer(tvimpm, svimpm,tvifm, svifm,ltest_bin, lsubj_bin,
                                    title = title, label = label ,
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE,
                                    p = p, se = se, t = tstat,
                                    add.lines = add.lines,
                                    column.labels = column.labels, column.separate = column.separate
)), 
file.path(outline, 'code_fitlit_varimp.tex'))
