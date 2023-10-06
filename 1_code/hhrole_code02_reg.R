# ------------
# Introduction
# ------------

## This file computes regression interacting financial literacy and experience

rm(list=ls())
NAME <- 'code02_reg' 
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
library(lubridate) # for ymd
library(caret) # find correlation
library(MASS) # for ordered logistic

## --------
## Settings
## --------
### Any settings go here

f <- 'hhrole'


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

if (dir.exists(file.path('empirical', '3_output','results',f))){
  outline <- file.path('empirical', '3_output','results',f,NAME)
} else {
  outline <- file.path('3_output','results',f,NAME)
}

if (!dir.exists(outline)) {
  dir.create(outline)
}



# ---------
# Main code
# ---------

## -- Load data from pipeline folder --
T <- read_csv(file.path('empirical', '0_data', 'manual','BOP-HH', 'T_exp.csv')) %>%
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  pdata.frame( index=c( "id", "date" ) )


# label household role vars
hhroles <- c('shop_groceries','shop_major','prep_meals','decide_finance')

## -- Detour: compute financial literacy score as in finlit_code01_fitlit 
#########################################################################

# Define variables used in regression
  finlitnames <- c("round", "refresher", "qeasy", "qinterest")
  # formula
  eq <- as.formula(paste('fin_lit_test ~ factor(region) + age + female + single + hhinc + educ +', paste(finlitnames, collapse='+')))

# define data used for predicting:
Tsub <- na.omit(T)

# Convert dependent variable to ordered factor
Tsub$fin_lit_test <- ordered(Tsub$fin_lit_test, levels = c(0, 1, 2, 3),
                             labels = c("Low", "Medium", "High", "Very High"))

# ordinal logistic regression on sub sample for which we have the test
lfin <- polr(eq, data= Tsub, method = "logistic",Hess=TRUE)
# predict out of sample
T$lfinpred <- predict(lfin, newdata = T, type="probs")[,4]   #gets Prob(>=3) 

# Save T with new predicted variables
write_csv(T, file.path(pipeline, 'out','T.csv'))

## -- Multivariate analysis
###############################

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
predictor_names <- c("age","single","female","hhinc","educ","age_between","hhinc_between","single_between")

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

# introducing financial literacy
eq <- as.formula(paste('y ~ factor(date) + factor(region) + lfinpred + ', paste(predictor_names, collapse = '+')))
f <- plm( eq, data=T_c, effect = "individual", model = "pooling")

# introducing shopping
eq <- as.formula(paste('y ~ factor(date) + factor(region) +', paste(predictor_names, collapse = '+'), '+', paste(hhroles, collapse = '+')))
e <- plm( eq, data=T_c, effect = "individual", model = "pooling")

# both together
eq <- as.formula(paste('y ~ factor(date) + factor(region) + lfinpred +', paste(predictor_names, collapse = '+'), '+', paste(hhroles, collapse = '+')))
fe <- plm( eq, data=T_c, effect = "individual", model = "pooling")

# interaction
eq <- as.formula(paste('y ~ factor(date) + factor(region) + lfinpred +', paste(predictor_names, collapse = '+'), '+', paste(hhroles, collapse = '+'), '+', paste(paste0(hhroles,":lfinpred"),collapse = '+')))
feint <- plm( eq, data=T_c, effect = "individual", model = "pooling")

# interaction only non singles
eq <- as.formula(paste('y ~ factor(date) + factor(region) + lfinpred +', paste(predictor_names, collapse = '+'), '+', paste(hhroles, collapse = '+'), '+', paste(paste0(hhroles,":lfinpred"),collapse = '+')))
sfeint <- plm( eq, data=filter(T_c, single==0), effect = "individual", model = "pooling")

# --- Write output

# settings for stargazer
title <- "Financial Confidence and Experience"
label <- "tab:regresswithfinexp"
dep.var.labels <- "Inflation expectation (12 months ahead, point estimate)"


# in which order
desiredOrder <- c("Constant","female","lfinpred",hhroles,paste0(hhroles,":lfinpred"),"age","single","educ","hhinc")

writeLines(capture.output(stargazer(b,f,e,fe,feint,sfeint,
                                    title = title, label = label, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE)), 
           file.path(outline,'code_regresswithfinexp.tex'))


coefficients <- coef(feint)
vcovm <- vcovHC(feint)

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
