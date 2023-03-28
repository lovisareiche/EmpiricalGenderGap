# ------------
# Introduction
# ------------

## This file 

rm(list=ls())
NAME <- 'code11_regresswithfin' 
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
if (!dir.exists(file.path(outline,source))) {
  dir.create(file.path(outline,source))
}




# ---------
# Main code
# ---------

## -- Load data from pipeline folder --

if(source == 'code09_fitlit'){
  load(file.path('empirical', '2_pipeline', f,source,'out','T_fin.RData'))
  T_fin <- T %>%
    rename(fin_lit = lpred_test) ## choose here if test or subjective!
} else {
  T_fin <- read_csv(file.path('empirical', '2_pipeline',f, source,'out','base', 'T_fin.csv')) %>%
    pdata.frame(index=c( "id", "wave" )) %>%
    rename(fin_lit = fin_lit_test)
}


# label waves
waves <- colnames(T_fin) %>%
  str_subset("w\\d")

# label financial literacy vars because we need to remove them
fincon <- c('prob_intqr','nround','refresher','f_nointerest','f_easy')

# label household role vars
hhroles <- c('shop_groceries','shop_major','prep_meals','decide_finance')


xnames <- c("eduschool"  , "non_single",
             "hhinc"                 ,   "q_inflation",
            "age"                 ,   "citysize"               ,"female"            ,    
            "eastgerman"             ,"east1989"           )

xtvnames <- c("hhinc" , "q_inflation", "non_single",
              "age"  )

# Include time varying as averages to control 

T_mean <- degroup(
  T_fin,
  c(xtvnames,'fin_lit',hhroles),
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
b <- plm( f, data=T_c, effect = "individual", model = "pooling")


# introducing predicted test ordered logit
f <- as.formula(paste('y ~','factor(wave) +', 'fin_lit + fin_lit_between +', 
                      paste(xnames, collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
tl <- plm( f, data=T_c, effect = "individual", model = "pooling")

# introducing grocery shopping
f <- as.formula(paste('y ~','factor(wave) +', 'shop_groceries + shop_groceries_between + ', 
                      paste(xnames, collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
g <- plm( f, data=T_c, effect = "individual", model = "pooling")

# introducing main shopping
f <- as.formula(paste('y ~','factor(wave) +', 'shop_groceries + shop_groceries_between + shop_major + shop_major_between + ', 
                      paste(xnames, collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
m <- plm( f, data=T_c, effect = "individual", model = "pooling")

# introducing meal prep
f <- as.formula(paste('y ~','factor(wave) +', 'shop_groceries + shop_groceries_between + shop_major + shop_major_between + prep_meals + prep_meals_between + ', 
                      paste(xnames, collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
p <- plm( f, data=T_c, effect = "individual", model = "pooling")

# introducing financial decision
f <- as.formula(paste('y ~','factor(wave) +', 
                      paste(xnames, collapse='+'),'+',
                      paste(hhroles, collapse='+'),'+',
                      paste(paste(hhroles,"_between",sep = ""), collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
hh <- plm( f, data=T_c, effect = "individual", model = "pooling")

# introducing tested literacy
f <- as.formula(paste('y ~','factor(wave) +', 'fin_lit + fin_lit_between + ', 
                      paste(xnames, collapse='+'),'+',
                      paste(hhroles, collapse='+'),'+',
                      paste(paste(hhroles,"_between",sep = ""), collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
a <- plm( f, data=T_c, effect = "individual", model = "pooling")

# introducing interactions
# with grocery shopping
f <- as.formula(paste('y ~','factor(wave) +', 'fin_lit + fin_lit_between + shop_groceries + shop_groceries_between + shop_groceries:fin_lit + ', 
                      paste(xnames, collapse='+'),'+',
                      paste(paste(hhroles,"_between",sep = ""), collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
gi <- plm( f, data=T_c, effect = "individual", model = "pooling")

# introducing main shopping
f <- as.formula(paste('y ~','factor(wave) +', 'fin_lit + fin_lit_between + shop_groceries + shop_groceries_between + shop_major + shop_major_between + shop_groceries:fin_lit + shop_major:fin_lit + ', 
                      paste(xnames, collapse='+'),'+',
                      paste(paste(hhroles,"_between",sep = ""), collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
mi <- plm( f, data=T_c, effect = "individual", model = "pooling")

# introducing meal prep
f <- as.formula(paste('y ~','factor(wave) +', 'fin_lit + fin_lit_between + shop_groceries + shop_groceries_between + shop_major + shop_major_between + prep_meals + prep_meals_between +  shop_groceries:fin_lit + shop_major:fin_lit + prep_meals:fin_lit +', 
                      paste(xnames, collapse='+'),'+',
                      paste(paste(hhroles,"_between",sep = ""), collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
pi <- plm( f, data=T_c, effect = "individual", model = "pooling")

# full interactions
f <- as.formula(paste('y ~','factor(wave) +',  'fin_lit + fin_lit_between + shop_groceries + shop_groceries_between + shop_major + shop_major_between + prep_meals + prep_meals_between + decide_finance + decide_finance_between +  shop_groceries:fin_lit + shop_major:fin_lit + prep_meals:fin_lit + decide_finance:fin_lit +', 
                      paste(xnames, collapse='+'),'+',
                      paste(paste(hhroles,"_between",sep = ""), collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
i <- plm( f, data=T_c, effect = "individual", model = "pooling")


# --- Write output

# settings for stargazer
notes <- "The full set of estimators included can be found in the appendix."
omit <- c("wave","between")
omit.labels <- c("Time dummies","Between effects")
title <- "The role of financial confidence"
label <- "tab:regresswithfin"
dep.var.labels <- "Inflation expectation, 12 months ahead, point estimate"


# in which order
desiredOrder <- c("Constant","female","fin_lit",hhroles)

writeLines(capture.output(stargazer(b,tl,g,m,p,hh,a,
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE)), 
           file.path(outline, source,'code_regresswithfin.tex'))

label <- "tab:regresswithfinint"
writeLines(capture.output(stargazer(a,gi,mi, pi, i,
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE)), 
           file.path(outline, source,'code_regresswithfin_int.tex'))


# ------------
# Robustness
# ------------


# Test 1: baseline and all with no controls

f <- as.formula(paste('y ~','factor(wave) +', 'female'))
bnc <- plm( f, data=T_c, effect = "individual", model = "pooling")

f <- as.formula(paste('y ~','factor(wave) +', 'fin_lit + fin_lit_between + ', 
                      'female +',
                      paste(hhroles, collapse='+'),'+',
                      paste(paste(hhroles,"_between",sep = ""), collapse='+')))
anc <- plm( f, data=T_c, effect = "individual", model = "pooling")


# Test 2: Baseline with no between effects
f <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+')))
bnb <- plm( f, data=T_c, effect = "individual", model = "pooling")

f <- as.formula(paste('y ~','factor(wave) +', 'fin_lit + fin_lit + ', 
                      paste(xnames, collapse='+'),'+',
                      paste(hhroles, collapse='+')))
anb <- plm( f, data=T_c, effect = "individual", model = "pooling")


# Test 3: non extreme outliers

f <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
b90 <- plm( f, data=T_c[T_c$y<quantile(T_c$y,0.9),], effect = "individual", model = "pooling")

f <- as.formula(paste('y ~','factor(wave) +', 'fin_lit + fin_lit_between + ', 
                      paste(xnames, collapse='+'),'+',
                      paste(hhroles, collapse='+'),'+',
                      paste(paste(hhroles,"_between",sep = ""), collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
a90 <- plm( f, data=T_c[T_c$y<quantile(T_c$y,0.9),], effect = "individual", model = "pooling")

label <- "tab:regresswithfinrob"
writeLines(capture.output(stargazer(b,a,bnc,anc,bnb,anb,b90,a90,
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE)), 
           file.path(outline, source,'code_regresswithfin_rob.tex'))


## for subsamples



f <- as.formula(paste('y ~','factor(wave) +', 
                      paste(xnames, collapse='+'),'+',
                      paste(hhroles, collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
hhhigh <- plm( f, data=filter(T_c, fin_lit > median(fin_lit)), effect = "individual", model = "pooling")







## Lasso Test

library(glmnet)

#define response variable
y <- T_c$y

#define matrix of predictor variables
x <- data.matrix(T_c[,c(paste(xnames),paste(hhroles),paste(hhroles,"_between",sep = ""),paste(xtvnames,"_between",sep = ""),'fin_lit','fin_lit_between' )])


#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)


#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

# create "models" that show variable importance (absolute value of z startstic) and VIF (variance inflation factor)
lasso <- a
lasso$coefficients <- coefficients(best_model)


## Decile regression

# pre define vars
C_female <- data.frame()
SE_female <- data.frame()
C_fin <- data.frame()
SE_fin <- data.frame()
C_groc <- data.frame()
SE_groc <- data.frame()
O <- data.frame()
Q <- data.frame()
R <- data.frame()
AR <- data.frame()

# Baseline
form <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+'),'+',
                            paste(paste(xtvnames,"_between",sep = ""), collapse='+')))

for (ii in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) {
  m <- plm( form, data=filter(T_c, y<=quantile(y,ii)), effect = "individual", model = "pooling")
  assign(paste("m_",ii,sep = ""),m)
  
  # collect all coefficient estimates and standard errors for variables of interest
  C_female[1,ii*10] <- coefficients(m)["female"]
  SE_female[1,ii*10] <- coefficients(summary(m))[, "Std. Error"]["female"]
  
  # collect observations, percentiles, R2 and adjuster R2
  O[1,ii*10] <- nrow(filter(T_c, y<=quantile(y,ii)))
  Q[1,ii*10] <- quantile(T_c$y,ii)
  h <- summary(m)["r.squared"]
  R[1,ii*10] <- h$r.squared["rsq"]
  AR[1,ii*10] <- h$r.squared["adjrsq"]
}

# INCLUDING PREDICT TEST
form <- as.formula(paste('y ~','factor(wave) +', 'fin_lit + fin_lit_between +', 
                         paste(xnames, collapse='+'),'+',
                         paste(paste(xtvnames,"_between",sep = ""), collapse='+')))

for (ii in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) {
  findec <- plm( form, data=filter(T_c, y<=quantile(y,ii)), effect = "individual", model = "pooling")
  assign(paste("findec_",ii,sep = ""),findec)
  
  # collect all coefficient estimates and standard errors for variables of interest
  C_female[2,ii*10] <- coefficients(findec)["female"]
  SE_female[2,ii*10] <- coefficients(summary(findec))[, "Std. Error"]["female"]
  C_fin[1,ii*10] <- coefficients(findec)["fin_lit"]
  SE_fin[1,ii*10] <- coefficients(summary(findec))[, "Std. Error"]["fin_lit"]
  
  # collect observations, percentiles, R2 and adjuster R2
  O[2,ii*10] <- nrow(filter(T_c, y<=quantile(y,ii)))
  Q[2,ii*10] <- quantile(T_c$y,ii)
  h <- summary(m)["r.squared"]
  R[2,ii*10] <- h$r.squared["rsq"]
  AR[2,ii*10] <- h$r.squared["adjrsq"]
}

# Including household roles

form <- as.formula(paste('y ~','factor(wave) +', 
                      paste(xnames, collapse='+'),'+',
                      paste(hhroles, collapse='+'),'+',
                      paste(paste(hhroles,"_between",sep = ""), collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))

for (ii in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) {
  hhdec <- plm( form, data=filter(T_c, y<=quantile(y,ii)), effect = "individual", model = "pooling")
  assign(paste("hhdec_",ii,sep = ""),hhdec)
  
  # collect all coefficient estimates and standard errors for variables of interest
  C_female[3,ii*10] <- coefficients(hhdec)["female"]
  SE_female[3,ii*10] <- coefficients(summary(hhdec))[, "Std. Error"]["female"]
  C_groc[1,ii*10] <- coefficients(hhdec)["shop_groceries"]
  SE_groc[1,ii*10] <- coefficients(summary(hhdec))[, "Std. Error"]["shop_groceries"]
  
  # collect observations, percentiles, R2 and adjuster R2
  O[3,ii*10] <- nrow(filter(T_c, y<=quantile(y,ii)))
  Q[3,ii*10] <- quantile(T_c$y,ii)
  h <- summary(m)["r.squared"]
  R[3,ii*10] <- h$r.squared["rsq"]
  AR[3,ii*10] <- h$r.squared["adjrsq"]
}


D <- data.frame(t(C_female[1,]),t(C_female[2,]),t(C_female[3,]),t(C_fin),t(C_groc),t(SE_female[1,]),t(SE_female[2,]),t(SE_female[3,]),t(SE_fin),t(SE_groc),t(O[1,])) %>%
  dplyr::rename(female_b = X1, female_fin = X2, female_hh = X3, fin = X1.1, groc = X1.2, female_b_se = X1.3, female_fin_se = X2.1,female_hh_se = X3.1, fin_se = X1.4, groc_se = X1.5, obs = X1.6) %>%
  mutate(female_u = female_b + 1.96*female_b_se, female_fin_u = female_fin + 1.96*female_fin_se, female_hh_u = female_hh + 1.96*female_hh_se, fin_u = fin + 1.96*fin_se, groc_u = groc + 1.96*groc_se) %>%
  mutate(female_l = female_b - 1.96*female_b_se, female_fin_l = female_fin - 1.96*female_fin_se, female_hh_l = female_hh - 1.96*female_hh_se, fin_l = fin - 1.96*fin_se, groc_l = groc - 1.96*groc_se) %>%
  mutate(decile = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) %>%
  mutate(decilevaly = c(quantile(`T_c`$y,0.1),quantile(`T_c`$y,0.2),quantile(`T_c`$y,0.3),quantile(`T_c`$y,0.4),quantile(`T_c`$y,0.5),quantile(`T_c`$y,0.6),quantile(`T_c`$y,0.7),quantile(`T_c`$y,0.8),quantile(`T_c`$y,0.9),quantile(`T_c`$y,1)))%>%
  mutate(decilevalfin = c(quantile(`T_c`$fin_lit,0.1),quantile(`T_c`$fin_lit,0.2),quantile(`T_c`$fin_lit,0.3),quantile(`T_c`$fin_lit,0.4),quantile(`T_c`$fin_lit,0.5),quantile(`T_c`$fin_lit,0.6),quantile(`T_c`$fin_lit,0.7),quantile(`T_c`$fin_lit,0.8),quantile(`T_c`$fin_lit,0.9),quantile(`T_c`$fin_lit,1)))


# save as delimited text file

library(caroline) # for delimited text file

write.delim(D, file = file.path(pipeline, 'out', 'D.txt'), sep = "\t")


# Test joint significance

f <- as.formula(paste('y ~','factor(wave) +',  'fin_lit + fin_lit_between + shop_groceries + shop_groceries_between + shop_major + shop_major_between + prep_meals + prep_meals_between + decide_finance + decide_finance_between +  shop_groceries:fin_lit + shop_major:fin_lit + prep_meals:fin_lit + decide_finance:fin_lit +', 
                      paste(xnames, collapse='+'),'+',
                      paste(paste(hhroles,"_between",sep = ""), collapse='+'),'+',
                      paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
i <- lm( f, data=T_c)


library(multcomp)


summary(glht(i, "(Intercept) + fin_lit*3.05=0"))
