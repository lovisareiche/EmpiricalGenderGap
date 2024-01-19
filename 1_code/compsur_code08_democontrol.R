# ------------
# Introduction
# ------------

## Compares gender gaps for singles and non-singles from different surveys

rm(list=ls())
NAME <- 'code08_democontrol' ## Name of the R file goes here (without the file extension!)
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
library(moments) # to compute skew
library(zoo) # for date vector
library(xtable) # for latex conversion
library(stargazer) # for latex table
library('plm')
library('datawizard')
library(lubridate)
library(caret)

library(scatterplot3d) # to create scatterplot
library(rgl) # to save scatterplot

## --------
## Settings
## --------
### Any settings go here

S <- c('BOP-HH','Michigan','FRBNY')
# BOP-HH, Michigan, FRBNY - all you have
f <- 'compsur'


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
  outline <- file.path('empirical','3_output','results',f,NAME)
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
    mutate(survey = S[i], date = ymd(paste0(year, "-", month, "-01"))) %>%
    pdata.frame( index=c( "id", "date" ) )
  
  if(S[i]=="Michigan"){
  # run this correction due to error in panel recording on one observation which is assigned a new id
  T$id <- as.numeric(as.character(T$id))  # Convert 'id' column to numeric
  new_id <- min(setdiff(seq_len(max(T$id) + 1), T$id))
  T["106873-1991-06-01", "id"] <- new_id
  }
  
  assign(paste("T_",S[i],sep = ""),T)
  
  # do multivariate analysis
  
  
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
  predictor_names <- c("age","female","hhinc","educ","single","age_between","hhinc_between","single_between")
  
  # Check for multicollinearity
  cor_matrix <- cor(T_c[complete.cases(T_c), predictor_names])
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
  
  
  
  eq <- as.formula(paste('y ~ factor(date) + factor(region) +', paste(predictor_names, collapse = '+')))
  m <- plm( eq, data=T_c, effect = "individual", model = "pooling" )
  assign(paste("m_",S[i],sep = ""),m)
  
  eq <- as.formula(paste('y ~ factor(date) + factor(region) + age:female + hhinc:female + educ:female + ', paste(predictor_names, collapse = '+')))
  mint <- plm( eq, data=T_c, effect = "individual", model = "pooling" )
  assign(paste("mint_",S[i],sep = ""),mint)
  
  # Assuming you have the regression model stored as "mint" in R
  # Assuming you have the vectors T$age and T$educ available
  
  # Extract the coefficients from the model
  coefficients <- coef(mint)
  
  # age and educ
  
  # Calculate the implied gender gap for each combination of age and education using median hhinc
  implied_gender_gap <- coefficients["female"] + coefficients["female:hhinc"] * median(T$hhinc) +
    coefficients["female:educ"] * T$educ +
    coefficients["age:female"] * T$age
  
  # Create a 3D scatter plot
  scatterplot3d(T$age, T$educ, implied_gender_gap,
                xlab = "Age", ylab = "Education", zlab = "Implied Gender Gap",
                main = "Implied Gender Gap by Age and Education",
                color = ifelse(implied_gender_gap < 0, "red", "blue"), angle = 45)
  
  # Save the plot as a PNG file
  dev.copy(png, file.path(outline,paste("3D_educ_age_",S[i],".png",sep = )))
  dev.off()
  
  # age and hhinc
  
  # Calculate the implied gender gap for each combination of age and hhinc using median educ
  implied_gender_gap <- coefficients["female"] + coefficients["female:educ"] * median(T$educ) +
    coefficients["female:hhinc"] * T$hhinc +
    coefficients["age:female"] * T$age
  
  # Create a 3D scatter plot
  scatterplot3d(T$age, T$educ, implied_gender_gap,
                xlab = "Age", ylab = "Income", zlab = "Implied Gender Gap",
                main = "Implied Gender Gap by Age and Income",
                color = ifelse(implied_gender_gap < 0, "red", "blue"), angle = 45)
  
  # Save the plot as a PNG file
  dev.copy(png, file.path(outline,paste("3D_hhinc_age_",S[i],".png",sep = )))
  dev.off()
  
}

mb <- `m_BOP-HH`
mib <- `mint_BOP-HH`
mf <- m_FRBNY
mif <- mint_FRBNY
mm <- m_Michigan
mim <- mint_Michigan

# write output

# settings for stargazer
title <- "The Gender Gap and Demographic Controls"
omit <- c("factor","between")
omit.labels <- c("Year dummies","Between effects")
label <- "tab:democontrol"
dep.var.labels <- "Inflation expectation, 12 months ahead, point estimate"

# in which order
desiredOrder <- c("Constant","female","age","eduschool","hhinc")

writeLines(capture.output(stargazer(mb,mib,mf,mif,mm,mim,
                                    title = title, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels)), 
           file.path(outline, 'code_democontrol.tex'))




