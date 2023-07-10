# ------------
# Introduction
# ------------

## This file computes a regression to show who is in the tails of the distribution


rm(list=ls())
NAME <- 'code04_tail' ## Name of the R file goes here (without the file extension!)
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
library(moments)
library(xtable)
library(plm)


## --------
## Settings
## --------
### Any settings go here
f <- 'finlit'
S <- c('BOP-HH','FRBNY')



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

## Add subfolders

for(s in 1:length(S)){
  if (!dir.exists(file.path(pipeline,'out',S[s]))) {
    dir.create(file.path(pipeline,'out',S[s]))
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

for(s in 1:length(S)){
  
  
  ## Load data from pipeline folder --
  T <- read_csv(file.path('empirical', '2_pipeline',f, 'code01_fitlit','out',S[s], 'T.csv')) %>%
    pdata.frame(index=c( "id", "date" ))
  assign(paste("T_",S[s],sep = ""),T)
  
  # define variable of interest: dummy if someone is in the tail
  T$tail = as.numeric(T$y>=quantile(T$y,0.9))
  
  # do multivariate analysis
  
  # demeaning
  T_mean <- degroup(
    T,
    c("age","hhinc"),
    "id",
    center = "mean",
    suffix_groupmean = "_between",
    add_attributes = TRUE,
    verbose = TRUE
  )
  T_c <- cbind(T,T_mean) %>%
    pdata.frame(index=c( "id", "date" ) )
  
  # Model 1: base
  ###############
  
  eq <- as.formula(tail ~ factor(date) + age + single + female + hhinc + educ + factor(region) + age_between + hhinc_between)
  
  m <- glm( eq, data=T_c, family = "binomial")
  assign(paste("m_",S[s],sep = ""),m)
  
  # --- Assessing model fit for logistic
  
  # needs to be added manually in the stargazer file
  mfr2 <- pscl::pR2(m)["McFadden"]
  assign(paste("mfr2_",S[s],sep = ""),mfr2)
  
  # calculate sensitivity
  sen <- sensitivity(T_c$tail, fitted.values(m))
  assign(paste("sen_",S[s],sep = ""),sen)
  
  #calculate specificity
  spe <- specificity(T_c$tail, fitted.values(m))
  assign(paste("spe_",S[s],sep = ""),spe)
  
  # caluclate optimal cutoff
  opt <- optimalCutoff(T_c$tail, fitted.values(m))[1]
  
  #calculate total misclassification error rate
  mise <- misClassError(T_c$tail, fitted.values(m), threshold=opt)
  assign(paste("mise_",S[s],sep = ""),mise) 
  
  
  # Model 2: include financial literacy
  #####################################
  
  if(S[s]=='BOP-HH'){
    # for bop and fin-lit-test there cannot be date factor as all one date
    eq <- as.formula(tail ~ fin_lit_test  + age + single + female + hhinc + educ + factor(region) + age_between + hhinc_between)
  }
  if(S[s]=='FRBNY'){
    eq <- as.formula(tail ~ factor(date) + fin_lit_test  + age + single + female + hhinc + educ + factor(region) + age_between + hhinc_between)
  }
  
  # run regressions
  b <- glm( eq, data=T_c, family = "binomial")
  assign(paste("b_",S[s],sep = ""),b)
  
  # needs to be added manually in the stargazer file
  mfr2 <- pscl::pR2(b)["McFadden"]
  assign(paste("mfr2_","fin",S[s],sep = ""),mfr2)
  
  # calculate sensitivity
  sen <- sensitivity(T_c$tail, fitted.values(b))
  assign(paste("sen_","fin",S[s],sep = ""),sen)
  
  #calculate specificity
  spe <- specificity(T_c$tail, fitted.values(b))
  assign(paste("spe_","fin",S[s],sep = ""),spe)
  
  # caluclate optimal cutoff
  opt <- optimalCutoff(T_c$tail, fitted.values(b))[1]
  
  #calculate total misclassification error rate
  mise <- misClassError(T_c$tail, fitted.values(b), threshold=opt)
  assign(paste("mise_","fin",S[s],sep = ""),mise) 
  
  # Model 3: With predicted literacy
  ##################################
  
  # include financial literacy
  eq <- as.formula(tail ~ factor(date) + lfinpred  + age + single + female + hhinc + educ + factor(region) + age_between + hhinc_between)
  
  # run regressions
  best <- glm( eq, data=T_c, family = "binomial")
  assign(paste("best_",S[s],sep = ""),best)
  
  # needs to be added manually in the stargazer file
  mfr2 <- pscl::pR2(best)["McFadden"]
  assign(paste("mfr2_","est",S[s],sep = ""),mfr2)
  
  # calculate sensitivity
  sen <- sensitivity(T_c$tail, fitted.values(best))
  assign(paste("sen_","est",S[s],sep = ""),sen)
  
  #calculate specificity
  spe <- specificity(T_c$tail, fitted.values(best))
  assign(paste("spe_","est",S[s],sep = ""),spe)
  
  # caluclate optimal cutoff
  opt <- optimalCutoff(T_c$tail, fitted.values(best))[1]
  
  #calculate total misclassification error rate
  mise <- misClassError(T_c$tail, fitted.values(best), threshold=opt)
  assign(paste("mise_","est",S[s],sep = ""),mise) 

}




# write output

# settings for stargazer
title <- "Multivariate: Determinates of being in the tail"
omit <- c("factor(date)","factor(region)","between")
omit.labels <- c("Time dummies","Regional dummies","Between effects")
label <- "tab:charactertail"
dep.var.labels <- "Dummy variable: in tail of inflation expectations distribution"
column.labels <- c("BOP", "SCE")

r2 <- c("Mc Fadden $R^2$",round(`mfr2_BOP-HH`, digits = 2),round(`mfr2_finBOP-HH`, digits = 2),round(`mfr2_estBOP-HH`, digits = 2),round(mfr2_FRBNY, digits = 2),round(mfr2_finFRBNY, digits = 2),round(mfr2_estFRBNY, digits = 2))
mise <- c("Missclassification Error Rate",round(`mise_BOP-HH`, digits = 2),round(`mise_finBOP-HH`, digits = 2),round(`mise_estBOP-HH`, digits = 2),round(mise_FRBNY, digits = 2),round(mise_finFRBNY, digits = 2),round(mise_estFRBNY, digits = 2))
sen <- c("Sensitivity",round(`sen_BOP-HH`, digits = 2),round(`sen_finBOP-HH`, digits = 2),round(`sen_estBOP-HH`, digits = 2),round(sen_FRBNY, digits = 2),round(sen_finFRBNY, digits = 2),round(sen_estFRBNY, digits = 2))
spe <- c("Specificity",round(`spe_BOP-HH`, digits = 2),round(`spe_finBOP-HH`, digits = 2),round(`spe_estBOP-HH`, digits = 2),round(spe_FRBNY, digits = 2),round(spe_finFRBNY, digits = 2),round(spe_estFRBNY, digits = 2))
add.lines <- list(r2,mise,sen,spe)


# in which order
desiredOrder <- c("Constant","female","age","educ","hhinc","single","fin_lit_test","lfinpred")


# shorten names manually for stargazer
mbop <- `m_BOP-HH`
bbop <- `b_BOP-HH`
ebop <- `best_BOP-HH`
msce <- m_FRBNY
bsce <- b_FRBNY
esce <- best_FRBNY
writeLines(capture.output(stargazer(mbop,bbop,ebop,msce,bsce,esce,
                                    title = title, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, column.labels = column.labels,
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels,
                                    add.lines = add.lines)), 
           file.path(outline, 'code_charactertail.tex'))
