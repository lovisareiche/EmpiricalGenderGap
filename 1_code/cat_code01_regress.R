# ------------
# Introduction
# ------------

## This file 

rm(list=ls())
NAME <- 'code01_regress' 
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

## --------
## Settings
## --------
### Any settings go here

f <- 'cat'


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
T <- read_csv(file.path('empirical', '0_data', 'manual','FRBNY', 'T_cat.csv')) %>%
  mutate(date = ymd(paste0(year, "-", month, "-01"))) %>%
  pdata.frame( index=c( "id", "date" ) )


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
predictor_names <- c("age","single","female","hhinc","educ","fin_lit_test","decide_finance","age_between","hhinc_between","single_between")

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

# gas
eq <- as.formula(paste('y_gas ~ factor(date) + factor(region) +', paste(predictor_names, collapse = '+')))
ga <- plm( eq, data=T_c, effect = "individual", model = "pooling")

# food
eq <- as.formula(paste('y_food ~ factor(date) + factor(region) +', paste(predictor_names, collapse = '+')))
f <- plm( eq, data=T_c, effect = "individual", model = "pooling")

# educ
eq <- as.formula(paste('y_educ ~ factor(date) + factor(region) +', paste(predictor_names, collapse = '+')))
e <- plm( eq, data=T_c, effect = "individual", model = "pooling")

# medic
eq <- as.formula(paste('y_medic ~ factor(date) + factor(region) +', paste(predictor_names, collapse = '+')))
m <- plm( eq, data=T_c, effect = "individual", model = "pooling")

# rent
eq <- as.formula(paste('y_rent ~ factor(date) + factor(region) +', paste(predictor_names, collapse = '+')))
r <- plm( eq, data=T_c, effect = "individual", model = "pooling")

# gold
eq <- as.formula(paste('y_gold ~ factor(date) + factor(region) +', paste(predictor_names, collapse = '+')))
go <- plm( eq, data=T_c, effect = "individual", model = "pooling")

# --- Write output

# settings for stargazer
title <- "Expectations about specific prices"
label <- "tab:cat"
dep.var.labels <- "Inflation expectation (12 months ahead, point estimate)"


# Compute the difference between coefficient for female in models b and the others
coef_diff <- vector("numeric", length = 7)
coef_diff[1] <- 0
coef_diff[2] <- coef(b)["female"] - coef(ga)["female"]
coef_diff[3] <- coef(b)["female"] - coef(f)["female"]
coef_diff[4] <- coef(b)["female"] - coef(e)["female"]
coef_diff[5] <- coef(b)["female"] - coef(m)["female"]
coef_diff[6] <- coef(b)["female"] - coef(r)["female"]
coef_diff[7] <- coef(b)["female"] - coef(go)["female"]

# Compute the standard error of the difference
se_diff <- vector("numeric", length = 7)
se_diff[1] <- 0
se_diff[2] <- sqrt(vcov(b)["female", "female"] + vcov(ga)["female", "female"])
se_diff[3] <- sqrt(vcov(b)["female", "female"] + vcov(f)["female", "female"])
se_diff[4] <- sqrt(vcov(b)["female", "female"] + vcov(e)["female", "female"])
se_diff[5] <- sqrt(vcov(b)["female", "female"] + vcov(m)["female", "female"])
se_diff[6] <- sqrt(vcov(b)["female", "female"] + vcov(r)["female", "female"])
se_diff[7] <- sqrt(vcov(b)["female", "female"] + vcov(go)["female", "female"])

# Convert the differences and standard errors into a string format
diff_str <- sprintf("%0.2f", coef_diff)
se_diff_str <- sprintf("(%0.2f)", se_diff)


# Compute p-values
p_values <- 2 * (1 - pnorm(abs(coef_diff / se_diff)))


# in which order
desiredOrder <- c("Constant","female","age","single","educ","hhinc","decide_finance","fin_lit_test")

writeLines(capture.output(stargazer(b,ga,f,e,m,r,go,
                                    append = list(diff_str, se_diff_str),
                                    title = title, label = label, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE)), 
           file.path(outline,'code_cat.tex'))

