# ------------
# Introduction
# ------------

## Want to know who are these people in the tail

rm(list=ls())
NAME <- 'code10_charactertail' ## Name of the R file goes here (without the file extension!)
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
library(pscl) # for McFadden R2
library(caret) # for variable importance
library(InformationValue) # for optimal cutoff
library(xtable)

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
  outline <- file.path('empirical', '3_output','results',f,NAME)
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
  
  # remove duplicated values
  T <- T[!duplicated(T[c('id','date')]), ]
  
  # define variable of interest: dummy if someone is in the tail
  T$tail = as.numeric(T$y>=quantile(T$y,0.9))
  
  # do multivariate analysis
  
  # independent variables
  xnames <- setdiff(colnames(T),'date') %>%
    setdiff(c('y','tail')) %>%
    setdiff('month') %>%
    setdiff('year') %>%
    setdiff('survey') %>%
    setdiff('id') 
  
  # demeaning
  T_mean <- degroup(
    T,
    c("age","hhinc","quali","single"),
    "id",
    center = "mean",
    suffix_groupmean = "_between",
    add_attributes = TRUE,
    verbose = TRUE
  )
  T_c <- cbind(T,T_mean) %>%
    pdata.frame(index=c( "id", "date" ) )
  
  # model
  eq <- as.formula(paste('tail ~ factor(date) +', paste(xnames, collapse='+')))
  
  m <- glm( eq, data=T_c, family = "binomial")
  assign(paste("m_",S[i],sep = ""),m)
  
  
  # --- Assessing model fit for logistic
  
  # needs to be added manually in the stargazer file
  mfr2 <- pscl::pR2(m)["McFadden"]
  assign(paste("mfr2_",S[i],sep = ""),mfr2)
  
  # calculate sensitivity
  sen <- sensitivity(T_c$tail, fitted.values(m))
  assign(paste("sen_",S[i],sep = ""),sen)
  
  #calculate specificity
  spe <- specificity(T_c$tail, fitted.values(m))
  assign(paste("spe_",S[i],sep = ""),spe)
  
  # caluclate optimal cutoff
  opt <- optimalCutoff(T_c$tail, fitted.values(m))[1]
  
  #calculate total misclassification error rate
  mise <- misClassError(T_c$tail, fitted.values(m), threshold=opt)
  assign(paste("mise_",S[i],sep = ""),mise) 
  
  
  ## --- Characterise demographics in tail
  
  M <- group_by(T, female, tail) %>%
    summarise(age = mean(age), eduschool = mean(eduschool), hhinc = mean(hhinc), single = mean(single)) %>%
    t
  
  Mp <- group_by(T, female) %>%
    summarise(agep = t.test(age[tail == 0], age[tail == 1])$p.value, educp = t.test(eduschool[tail == 0], eduschool[tail == 1])$p.value, hhincp = t.test(hhinc[tail == 0], hhinc[tail == 1])$p.value, singlep = t.test(single[tail == 0], single[tail == 1])$p.value) %>%
    t
    
  M <- data.frame(malnotail = (M[3:6,1]), maltail = (M[3:6,2]), malp = (Mp[2:5,1]), femnotail = (M[3:6,3]), femtail = (M[3:6,4]), femp = (Mp[2:5,2]))
  assign(paste("M_",S[i],sep = ""),M)
  
}

## --- financial literacy sample

T <- read_csv(file.path('empirical', '2_pipeline','bopreg', 'code03_compilepanel.m','out','base', 'T_fin.csv')) %>%
  pdata.frame(index=c( "id", "wave" )) 

T <- dplyr::rename(T, region = eastgerman, quali = q_inflation) %>%
  mutate(tail = as.numeric(y>=quantile(y,0.9))) %>%
  mutate(T,single = as.numeric(non_single == 0)) 

T_mean <- degroup(
  T,
  c("age","hhinc","quali"),
  "id",
  center = "mean",
  suffix_groupmean = "_between",
  add_attributes = TRUE,
  verbose = TRUE
)

T_c <- cbind(T,T_mean) %>%
  pdata.frame(index=c( "id", "wave" ) ) 

# include financial literacy
eq <- as.formula(paste('tail ~ factor(wave) +', paste(xnames, collapse='+'), ' + fin_lit_test '))

# run regressions
b <- glm( eq, data=T_c, family = "binomial")

# needs to be added manually in the stargazer file
mfr2 <- pscl::pR2(m)["McFadden"]
assign(paste("mfr2_","fin",sep = ""),mfr2)

# calculate sensitivity
sen <- sensitivity(T_c$tail, fitted.values(m))
assign(paste("sen_","fin",sep = ""),sen)

#calculate specificity
spe <- specificity(T_c$tail, fitted.values(m))
assign(paste("spe_","fin",sep = ""),spe)

# caluclate optimal cutoff
opt <- optimalCutoff(T_c$tail, fitted.values(m))[1]

#calculate total misclassification error rate
mise <- misClassError(T_c$tail, fitted.values(m), threshold=opt)
assign(paste("mise_","fin",sep = ""),mise) 


## --- financial literacy estimated sample

load(file.path('empirical', '2_pipeline', 'bopreg','code09_fitlit','out','T_fin.RData'))

T <- dplyr::rename(T, region = eastgerman, quali = q_inflation) %>%
  mutate(tail = as.numeric(y>=quantile(y,0.9))) %>%
  mutate(T,single = as.numeric(non_single == 0)) 

T_mean <- degroup(
  T,
  c("age","hhinc","quali","lpred_test"),
  "id",
  center = "mean",
  suffix_groupmean = "_between",
  add_attributes = TRUE,
  verbose = TRUE
)

T_c <- cbind(T,T_mean) %>%
  pdata.frame(index=c( "id", "wave" ) ) 

# include financial literacy
eq <- as.formula(paste('tail ~ factor(wave) +', paste(xnames, collapse='+'), ' + lpred_test '))

# run regressions
best <- glm( eq, data=T_c, family = "binomial")

# needs to be added manually in the stargazer file
mfr2 <- pscl::pR2(m)["McFadden"]
assign(paste("mfr2_","est",sep = ""),mfr2)

# calculate sensitivity
sen <- sensitivity(T_c$tail, fitted.values(m))
assign(paste("sen_","est",sep = ""),sen)

#calculate specificity
spe <- specificity(T_c$tail, fitted.values(m))
assign(paste("spe_","est",sep = ""),spe)

# caluclate optimal cutoff
opt <- optimalCutoff(T_c$tail, fitted.values(m))[1]

#calculate total misclassification error rate
mise <- misClassError(T_c$tail, fitted.values(m), threshold=opt)
assign(paste("mise_","est",sep = ""),mise) 

# write output

# settings for stargazer
title <- "Multivariate: Determinates of being in the tail"
omit <- c("factor","between")
omit.labels <- c("Time dummies","Between effects")
label <- "tab:charactertail"
dep.var.labels <- "Dummy variable: in tail of inflation expectations distribution"
column.labels <- c("BOP", "SCE", "MSC")

r2 <- c("Mc Fadden $R^2$",round(`mfr2_BOP-HH`, digits = 2),round(mfr2_fin, digits = 2),round(mfr2_est, digits = 2),round(mfr2_FRBNY, digits = 2),round(mfr2_Michigan, digits = 2))
mise <- c("Missclassification Error Rate",round(`mise_BOP-HH`, digits = 2),round(mise_fin, digits = 2),round(mise_est, digits = 2),round(mise_FRBNY, digits = 2),round(mise_Michigan, digits = 2))
sen <- c("Sensitivity",round(`sen_BOP-HH`, digits = 2),round(sen_fin, digits = 2),round(sen_est, digits = 2),round(sen_FRBNY, digits = 2),round(sen_Michigan, digits = 2))
spe <- c("Specificity",round(`spe_BOP-HH`, digits = 2),round(spe_fin, digits = 2),round(spe_est, digits = 2),round(spe_FRBNY, digits = 2),round(spe_Michigan, digits = 2))
add.lines <- list(r2,mise,sen,spe)


# in which order
desiredOrder <- c("Constant","female","age","eduschool","hhinc","single","lpred_test","region")

writeLines(capture.output(stargazer(`m_BOP-HH`,b,best,m_FRBNY,m_Michigan,
                                    title = title, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, column.labels = column.labels,
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels,
                                    add.lines = add.lines)), 
           file.path(outline, 'code_charactertail.tex'))


## save characetristic file

xtable(`M_BOP-HH`, type="latex")
xtable(`M_Michigan`, type="latex")
xtable(`M_FRBNY`, type="latex")

