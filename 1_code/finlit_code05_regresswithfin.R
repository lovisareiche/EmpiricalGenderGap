# ------------
# Introduction
# ------------

## This file shows how higher financial literacy increases inflexp 

rm(list=ls())
NAME <- 'code05_regresswithfin' ## Name of the R file goes here (without the file extension!)
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
library(plm)
library('datawizard') # between effects
library(caret) # find correlation



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

if (dir.exists(file.path('empirical', '3_output','results',f))){
  outline <- file.path('empirical', '3_output','results',f,NAME)
} else {
  outline <- file.path('3_output','results',f,NAME)
}

if (!dir.exists(file.path('empirical', '3_output','results',f,NAME))) {
  dir.create(outline)
}


for(s in 1:length(S)){
  if (!dir.exists(file.path(outline,S[s]))) {
    dir.create(file.path(outline,S[s]))
  }
}


# ---------
# Main code
# ---------

for(s in 1:length(S)){
  
  
  ## Load data from pipeline folder 
  
  T <- read_csv(file.path('empirical', '2_pipeline',f, 'code01_fitlit','out',S[s], 'T.csv')) %>%
    pdata.frame(index=c( "id", "date" ))
  assign(paste("T_",S[s],sep = ""),T)
  
  
  # do multivariate analysis
  
  # demeaning
  T_mean <- degroup(
    T,
    c("age","hhinc","single"),
    "id",
    center = "mean",
    suffix_groupmean = "_between",
    add_attributes = TRUE,
    verbose = TRUE
  )
  T_c <- cbind(T,T_mean) %>%
    pdata.frame(index=c( "id", "date" ) )
  
  # Obtain the names of the predictor variables
  predictor_names <- c("age","single","female","hhinc","educ","age_between","hhinc_between")
  
  # Check for multicollinearity
  cor_matrix <- cor(T_c[, predictor_names])
  highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.8, names = TRUE, verbose = TRUE)
  
  # Function to check and attach "_between" if necessary
  check_and_attach <- function(element) {
    if (!grepl("_between", element))
      element <- paste0(element, "_between")
    return(element)
  }
  
  # Remove second variable in highly correlated pairs
  if (length(highly_correlated) > 0) {
    variable_to_remove <- sapply(highly_correlated,check_and_attach)
    predictor_names <- predictor_names[!predictor_names %in% variable_to_remove]
  }
  
  
  # baseline 
  eq <- as.formula(paste('y ~ factor(date) + factor(region) +', paste(predictor_names, collapse = '+')))
  b <- plm( eq, data=T_c, effect = "individual", model = "pooling")
  
  
  # introducing predicted test ordered logit
  eq <- as.formula(paste('y ~ factor(date) + factor(region) + lfinpred + ', paste(predictor_names, collapse = '+')))
  tl <- plm( eq, data=T_c, effect = "individual", model = "pooling")
  
  
  # introducing predicted test ordered logit interaction with female
  eq <- as.formula(paste('y ~ factor(date) + factor(region) + lfinpred + lfinpred:female +', paste(predictor_names, collapse = '+')))
  tlint <- plm( eq, data=T_c, effect = "individual", model = "pooling")
  
  # introducing literacy test
  if(S[s]=='BOP-HH'){
    # for bop and fin-lit-test there cannot be date factor as all one date
    eq <- as.formula(paste('y ~  factor(region) + fin_lit_test + ', paste(predictor_names, collapse = '+')))
  }
  if(S[s]=='FRBNY'){
    eq <- as.formula(paste('y ~ factor(date) + factor(region) + fin_lit_test + ', paste(predictor_names, collapse = '+')))
  }
  tt <- plm( eq, data=T_c, effect = "individual", model = "pooling")
  
  
  # introducing test interaction with female
  if(S[s]=='BOP-HH'){
    # for bop and fin-lit-test there cannot be date factor as all one date
    eq <- as.formula(paste('y ~  factor(region) + fin_lit_test + fin_lit_test:female + ', paste(predictor_names, collapse = '+')))
  }
  if(S[s]=='FRBNY'){
    eq <- as.formula(paste('y ~ factor(date) + factor(region) + fin_lit_test + fin_lit_test:female + ', paste(predictor_names, collapse = '+')))
  }
  ttint <- plm( eq, data=T_c, effect = "individual", model = "pooling")
  
  # --- Write output
  
  # settings for stargazer
  omit <- c("factor","between")
  omit.labels <- c("Time dummies","Between effects")
  title <- "The role of financial confidence"
  label <- "tab:regresswithfin"
  dep.var.labels <- "Inflation expectation (12 months ahead, point estimate)"
  
  
  # in which order
  desiredOrder <- c("Constant","female","fin_lit_test","lpredfin","age","single","educ","hhinc")
  
  writeLines(capture.output(stargazer(b,tl,tlint,tt,ttint,
                                      title = title, label = label, 
                                      omit = omit, omit.labels = omit.labels, 
                                      model.names = FALSE, 
                                      align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                      order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                      dep.var.labels = dep.var.labels, no.space = FALSE)), 
             file.path(outline, S[s],'code_regresswithfin.tex'))
             
}


############## For Plotting
###########################################
# Extract the coefficient estimates and standard errors
# run for both s=1 and s=2
coefficients <- coef(tlint)
vcovm <- vcovHC(tlint)

# Specify the coefficients you want to sum
coefs_to_sum <- c("female", "lfinpred:female")

# Find the indices of these coefficients in the coefficient vector
coef_indices <- which(names(coefficients) %in% coefs_to_sum)

# Compute the standard error of the sum of coefficients
se_sum_of_coefs <- sqrt(vcovm[coef_indices[1],coef_indices[1]] + vcovm[coef_indices[2],coef_indices[2]] + 2*vcovm[coef_indices[1],coef_indices[2]])
lb <- 1.96*se_sum_of_coefs

# when the lower bound hits zero
x = as.numeric((coefficients[coefs_to_sum[1]]-lb)/(-coefficients[coefs_to_sum[2]]))

# Calculate the share of values above x
share_below_x <- mean(T$lfinpred < x)

# Calculate the number of women with values below x
women_below_x <- mean(T$female[T$lfinpred < x])



