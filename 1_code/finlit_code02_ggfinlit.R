# ------------
# Introduction
# ------------

## This file computes a table comparing the financial literacy scores for men and women


rm(list=ls())
NAME <- 'code02_ggfinlit' ## Name of the R file goes here (without the file extension!)
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
    pdata.frame(index=c( "id", "date" )) %>%
    mutate(lfinpred_dummy = lfinpred >= median(lfinpred, na.rm = TRUE))
  assign(paste("T_",S[s],sep = ""),T)
  
  ## compute means by gender
  
  if(S[s]=='BOP-HH'){
    F <- T %>%
      group_by(female) %>%
      summarise(
      # Standard deviations
        fin_lit_test_sd = round(var(fin_lit_test, na.rm = TRUE), 2),
        lfinpred_sd = round(sd(lfinpred, na.rm = TRUE), 2),
        refresher_sd = round(sd(refresher, na.rm = TRUE), 2),
        round_sd = round(sd(round, na.rm = TRUE), 2),
        qinterest_sd = round(sd(qinterest, na.rm = TRUE), 2),
        qeasy_sd = round(sd(qeasy, na.rm = TRUE), 2),
        y_sd = round(sd(y, na.rm = TRUE), 2),
        
      # Means
        fin_lit_test = round(mean(fin_lit_test, na.rm = TRUE), 2),
        lfinpred = round(mean(lfinpred, na.rm = TRUE), 2),
        refresher = round(mean(refresher, na.rm = TRUE), 2),
        round = round(mean(round, na.rm = TRUE), 2),
        qinterest = round(mean(qinterest, na.rm = TRUE), 2),
        qeasy = round(mean(qeasy, na.rm = TRUE), 2),
        y = round(mean(y, na.rm = TRUE), 2)) 
        
    # Transpose the dataframe
    F <- as.data.frame(t(F))
    # Sort alphabetically by row names
    F <- F[order(rownames(F)), ]
    }
  if(S[s]=='FRBNY'){
    F <- T %>%
      group_by(female) %>%
      summarise(
        # Standard deviations
        fin_lit_test_sd = round(var(fin_lit_test, na.rm = TRUE), 2),
        lfinpred_sd = round(sd(lfinpred, na.rm = TRUE), 2),
        refresher_sd = round(sd(refresher, na.rm = TRUE), 2),
        round_sd = round(sd(round, na.rm = TRUE), 2),
        qinterest_sd = round(sd(qinterest, na.rm = TRUE), 2),
        qeasy_sd = NA,
        y_sd = round(sd(y, na.rm = TRUE), 2),
        
        # Means
        fin_lit_test = round(mean(fin_lit_test, na.rm = TRUE), 2),
        lfinpred = round(mean(lfinpred, na.rm = TRUE), 2),
        refresher = round(mean(refresher, na.rm = TRUE), 2),
        round = round(mean(round, na.rm = TRUE), 2),
        qinterest = round(mean(qinterest, na.rm = TRUE), 2),
        qeasy = NA,
        y = round(mean(y, na.rm = TRUE), 2))
    
    # Transpose the dataframe
    F <- as.data.frame(t(F))
    
    # Sort alphabetically by row names
    F <- F[order(rownames(F)), ]
  }
  
  E <- T %>%
    group_by(female,lfinpred_dummy) %>%
    summarise(
      # Standard deviations
      y_sd = round(sd(y, na.rm = TRUE), 2),
      
      # Means
      y = round(mean(y, na.rm = TRUE), 2)) 
  
  # Transpose the dataframe and reorder
  E <- E %>%
    pivot_wider(
      names_from = lfinpred_dummy,       # Create columns based on "female" values (0 and 1)
      values_from = c(y, y_sd),  # Spread the "y" and "y_sd" values
      names_prefix = "lit_"   # Prefix for new columns
    )
  E <- as.data.frame(t(E)) 
  E <- E[rownames(E) != "female", ]
  E <- E[order(grepl("FALSE$", rownames(E)), decreasing = TRUE), ]
  
  F <- rbind(F,E)
  assign(paste("F_",S[s],sep = ""),F)
  
}
 

# Combine the tables into a single dataframe
merged <- cbind(`F_BOP-HH`, F_FRBNY)

# Loop through and wrap numbers in parentheses for rows that contain '_sd' in the name
merged[grepl("_sd", rownames(merged)), ] <- 
  lapply(merged[grepl("_sd", rownames(merged)), ], function(x) paste0("(", x, ")"))


# Save output
  
writeLines(capture.output(xtable(merged, 
            caption = "Comparing the male and female subsamples", 
            label = "tab:ggfinlit")),
            file.path(outline, 'code_ggfinlit.tex'))
  
