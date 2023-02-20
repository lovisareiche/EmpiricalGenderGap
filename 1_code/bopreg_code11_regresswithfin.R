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
f <- 'bopreg'
source <- 'code09_fitlit'
  # 'code09_fitlit'
# or 'code03_compilepanel.m'


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

## Add subfolders
if (!dir.exists(file.path(outline,l))) {
  dir.create(file.path(outline,l))
}




# ---------
# Main code
# ---------

## -- Load data from pipeline folder --

if(source == 'code09_fitlit'){
  load(file.path('empirical', '2_pipeline', f,source,'out','T_fin.RData'))
  T_fin <- T 
} else {
  T_fin <- read_csv(file.path('empirical', '2_pipeline',f, source,'out','base', 'T_fin.csv')) %>%
    pdata.frame(index=c( "id", "wave" )) %>%
    # create binary vars
    mutate(pred_subj_bin=as.numeric(fin_lit_subj >=1)) %>%
    mutate(pred_test_bin=as.numeric(fin_lit_test >=1))
}


# label waves
waves <- colnames(T_fin) %>%
  str_subset("w\\d")

# label financial literacy vars because we need to remove them
fincon <- c('prob_intqr','nround','refresher','f_nointerest','f_easy')

# label household role vars
hhroles <- c('shop_groceries_nsing','shop_major_nsing','prep_meals_nsing','decide_finance_nsing')

xnames <- c("pessimist",              "q_unemployment",         "q_rent",                
            "q_lending"   ,           "q_interest"     ,        "q_inflation",           
            "q_property"   ,          "q_growth"        ,       "q_fuel"  ,              
            "q_dax"         ,         "q_tax"            ,      "exphp_point" ,          
            "expint_sav"     ,        "si_major"          ,     "si_essential" ,         
            "si_clothing"     ,       "si_entz"            ,    "si_mobility"   ,        
            "si_services"      ,      "si_holiday"          ,   "si_housing"     ,       
            "si_reserves"       ,     "eduschool"            ,  "eduwork"         ,      
            "hhchildren"         ,    "hhinc"                 , "pinc"             ,     
            "age"                 ,   "citysize"               ,"female"            ,    
            "eastgerman"             ,"east1989"           ,   
            "part_time"             , "unemployed"             ,"retired"      )

xtvnames <- c("pessimist",              "q_unemployment",         "q_rent",                
            "q_lending"   ,           "q_interest"     ,        "q_inflation",           
            "q_property"   ,          "q_growth"        ,       "q_fuel"  ,              
            "q_dax"         ,         "q_tax"            ,      "exphp_point" ,          
            "expint_sav"     ,        "si_major"          ,     "si_essential" ,         
            "si_clothing"     ,       "si_entz"            ,    "si_mobility"   ,        
            "si_services"      ,      "si_holiday"          ,   "si_housing"     ,       
            "si_reserves"       ,           
            "hhchildren"         ,    "hhinc"                 , "pinc"             ,     
            "age"                 ,      
            
            "part_time"             , "unemployed"             ,"retired"      )

xnames <- c("eduschool"  ,
             "hhinc"                 ,   
            "age"                 ,   "citysize"               ,"female"            ,    
            "eastgerman"             ,"east1989"           )

xtvnames <- c("hhinc" , 
              "age"  )

# Include time varying as averages to control 

T_mean <- degroup(
  T,
  c(xtvnames,'lpred_subj','ppred_test','lpred_test','ppred_subj',hhroles),
  "id",
  center = "mean",
  suffix_demean = "_within",
  suffix_groupmean = "_between",
  add_attributes = TRUE,
  verbose = TRUE
)

T_c <- cbind(T_fin,T_mean) %>%
  pdata.frame(index=c( "id", "wave" ) ) 

## -- define dependent variables

# in log
if (l == 'log') {
  T$y = log(T$y-min(T$y)+1)
}
# without log it's just y


## -- Run different specifications

# baseline 
f <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
b <- plm( f, data=filter(T_c, non_single == 1), effect = "individual", model = "pooling")


# introducing predicted test ordered logit
f <- as.formula(paste('y ~','factor(wave) +', 'lpred_test + lpred_test_between +', 
                      paste(xnames, collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
tl <- plm( f, data=filter(T_c, non_single == 1), effect = "individual", model = "pooling")

# introducing predicted test poisson
f <- as.formula(paste('y ~','factor(wave) +', 'ppred_test + ppred_test_between +', 
                      paste(xnames, collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
tp <- plm( f, data=filter(T_c, non_single == 1), effect = "individual", model = "pooling")

# introducing predicted subjective bol
f <- as.formula(paste('y ~','factor(wave) +', 'lpred_subj + lpred_subj_between +', 
                      paste(xnames, collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
sl <- plm( f, data=filter(T_c, non_single == 1), effect = "individual", model = "pooling")

# introducing predicted subjective p
f <- as.formula(paste('y ~','factor(wave) +', 'ppred_subj + ppred_subj_between +', 
                      paste(xnames, collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
sp <- plm( f, data=filter(T_c, non_single == 1), effect = "individual", model = "pooling")

f <- as.formula(paste('y ~','factor(wave) +', 'lpred_test + lpred_test_between +', 
                      paste(xnames, collapse='+'),'+',
                      paste(hhroles, collapse='+'),'+',
                      paste(paste(hhroles,"_between",sep = ""), collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
a <- plm( f, data=T_c, effect = "individual", model = "pooling")

f <- as.formula(paste('y ~','factor(wave) +', 'lpred_test:female + lpred_test_between +', 
                      paste(xnames, collapse='+'),'+',
                      paste(hhroles, collapse='+'),'+',
                      paste(paste(hhroles,":female",sep = ""), collapse='+'),'+',
                      paste(paste(hhroles,"_between",sep = ""), collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
i <- plm( f, data=filter(T_c, non_single == 1), effect = "individual", model = "pooling")

f <- as.formula(paste('y ~','factor(wave) +', 
                      paste(xnames, collapse='+'),'+',
                      paste(hhroles, collapse='+'),'+',
                      paste(paste(hhroles,"_between",sep = ""), collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
hh <- plm( f, data=filter(T_c, non_single == 1), effect = "individual", model = "pooling")

# --- Write output

# settings for stargazer
notes <- "The full set of estimators included can be found in the appendix."
omit <- c("wave","between")
omit.labels <- c("Time dummies","Between effects")
title <- "The role of financial confidence"
label <- "tab:regresswithfin"
dep.var.labels <- "Inflation expectation, 12 months ahead, point estimate"
column.labels <- c("","Ordered logit","Poisson","Ordered logit","Poisson","","Ordered logit")

# in which order
desiredOrder <- c("Constant","female","lpred_test","ppred_test","lpred_subj","ppred_subj",hhroles)

writeLines(capture.output(stargazer(b,tl,tp,sl,sp,hh,a,
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, column.labels = column.labels,
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = TRUE)), 
           file.path(outline,l, 'code_regresswithfin.tex'))



