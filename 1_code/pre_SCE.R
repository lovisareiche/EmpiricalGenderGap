# ------------
# Introduction
# ------------

## Coverts different inflation expectation surveys in a common format
## saves T, T_fin and T_exp in 0_data, manual, BOP-HH

rm(list=ls())
NAME <- 'SCE' ## Name of the R file goes here (without the file extension!)
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
library(haven)
library(dplyr)
library(readxl)

## --------
## Settings
## --------
### Any settings go here

s <- 'FRBNY'

# BOP-HH, Michigan, FRBNY

f <- 'pre'


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

if (!dir.exists(file.path(pipeline,'out',s))) {
  dir.create(file.path(pipeline,'out',s))
}


# ---------
# Functions
# ---------

# Custom function for the Triangular distribution
triangular_dist <- function(x, a, b, c) {
  u <- (x - a) / (c - a)
  v <- (b - a) / (c - a)
  
  x <- ifelse(u < v, a + sqrt(u * v * (c - a) * (b - a)), c - sqrt((1 - u) * (1 - v) * (c - a) * (c - b)))
  
  return(x)
}

probabilistic_funcfit <- function(Xt, innerbinedges, allt) {
  # create output vectors
  mn <- rep(0, nrow(Xt))
  md <- rep(0, nrow(Xt))
  intqr <- rep(0, nrow(Xt))
  Xt[Xt == -6666] <- 0
  
  # mark respondents unusable
  row <- which(Xt == -9999, arr.ind = TRUE)[, 1]  # drop out
  if (length(row) > 0) {
    mn[row] <- -9999
    md[row] <- -9999
    intqr[row] <- -9999
  }
  row <- which(Xt == -9998, arr.ind = TRUE)[, 1]  # no answer
  if (length(row) > 0) {
    mn[row] <- -9998
    md[row] <- -9998
    intqr[row] <- -9998
  }
  row <- which(Xt == -9997, arr.ind = TRUE)[, 1]  # don't know
  if (length(row) > 0) {
    mn[row] <- -9997
    md[row] <- -9997
    intqr[row] <- -9997
  }
  row <- which(Xt == -5555, arr.ind = TRUE)[, 1]  # coding error by Bbk
  if (length(row) > 0) {
    mn[row] <- -5555
    md[row] <- -5555
    intqr[row] <- -5555
  }
  
  # count how many bins someone uses
  nbin <- rowSums(Xt > 0)
  
  # find observations with probability mass in non-contiguous bins
  discon <- rep(0, nrow(Xt))
  for (i in 1:nrow(Xt)) {
    if (nbin[i] > 1) {
      posmas <- which(Xt[i, ] > 0)
      subseq <- seq(posmas[1], posmas[length(posmas)])
      if (!identical(posmas, subseq)) {
        discon[i] <- 1
      }
    }
  }
  
  # we cannot use those respondents here
  mn[discon == 1] <- -5555
  md[discon == 1] <- -5555
  intqr[discon == 1] <- -5555
  
  # remove all those with only zero
  mn[nbin == 0] <- -6666
  md[nbin == 0] <- -6666
  intqr[nbin == 0] <- -6666
  
  # treat infinite bins
  infbin <- rep(0, nrow(Xt))
  infbin[Xt[, 1] == 100] <- 1
  infbin[Xt[, ncol(Xt)] == 100] <- 1
  
  mn[infbin == 1] <- -5555
  md[infbin == 1] <- -5555
  intqr[infbin == 1] <- -5555
  
  # fit distributions and calculate values
  for (i in 1:nrow(Xt)) {
    if (intqr[i] == 0) {  # if we haven't already preassigned a value
      if (nbin[i] == 1) {  # case 1: all mass in one bin
        if (infbin[i] == 0) {
          x <- which(Xt[i, ] > 0)  # find out where probability mass is
          a <- innerbinedges[x - 1]  # lower edge of bin
          c <- innerbinedges[x]  # upper edge of bin
          
          it <- list(a = a, b = a + (c - a) / 2, c = c)  # Define the triangular parameters
          
          mn[i] <- mean(triangular_dist(allt, it$a, it$b, it$c))
          md[i] <- median(triangular_dist(allt, it$a, it$b, it$c))
          q1 <- quantile(triangular_dist(allt, it$a, it$b, it$c), 0.25, na.rm = TRUE)
          q3 <- quantile(triangular_dist(allt, it$a, it$b, it$c), 0.75, na.rm = TRUE)
          intqr[i] <- q3 - q1
        }
      } else if (nbin[i] == 2) {  # case 2: mass in two bins, fit isosceles triangle
        tryCatch({
          if (discon[i] == 0) {
            x <- which(Xt[i, ] > 0)  # find out where probability mass is
            v1 <- Xt[i, x[1]]  # value in first bin
            v2 <- Xt[i, x[2]]  # value in second bin
            
            if (v1 < v2) {  # we use value 2 fully and value 1 only partially
              f <- (innerbinedges[x[2]] - innerbinedges[x[1]]) / v2 * 100  # full length of support
              a <- innerbinedges[x[1]] - (f - (innerbinedges[x[2]] - innerbinedges[x[1]]))  # lower edge of bin
              c <- innerbinedges[x[2]]  # upper edge of bin
            } else if (v1 > v2) {
              f <- (innerbinedges[x[1]] - innerbinedges[x[1] - 1]) / v1 * 100  # full length of support
              a <- innerbinedges[x[1] - 1]  # lower edge of bin
              c <- innerbinedges[x[1]] + (f - (innerbinedges[x[1]] - innerbinedges[x[1] - 1]))  # upper edge of bin
            }
            
            it <- list(a = a, b = a + (c - a) / 2, c = c)  # Define the triangular parameters
            
            mn[i] <- mean(triangular_dist(allt, it$a, it$b, it$c))
            md[i] <- median(triangular_dist(allt, it$a, it$b, it$c))
            q1 <- quantile(triangular_dist(allt, it$a, it$b, it$c), 0.25, na.rm = TRUE)
            q3 <- quantile(triangular_dist(allt, it$a, it$b, it$c), 0.75, na.rm = TRUE)
            intqr[i] <- q3 - q1
          }
        }, error = function(e) {
          mn[i] <- -5555
          md[i] <- -5555
          intqr[i] <- -5555
        })
      } else if (nbin[i] >= 3) {  # case 3: more than 3 bins, fit generalized beta function
        if (discon[i] == 0) {
          F_t <- rep(0, length(allt))
          for (ii in seq_along(allt)) {
            if (ii == 1) {
              F_t[ii] <- Xt[i, ii] / 100
            } else if (ii > 1) {
              if (!(allt[ii] %in% innerbinedges)) {
                F_t[ii] <- F_t[ii - 1]
              } else {
                F_t[ii] <- F_t[ii - 1] + Xt[i, allt[ii] == innerbinedges] / 100
              }
            }
          }
          
          fun <- function(a) sum((beta_homemade(allt, a[1], a[2], min(allt), max(allt)) - F_t)^2)
          a <- optim(c(2, 2), fun)$par
          
          mn[i] <- (a[1] * max(allt) + a[2] * min(allt)) / (a[1] + a[2])
          md[i] <- ((a[1] - 1/3) / (a[1] + a[2] - 2/3)) * (max(allt) - min(allt)) + min(allt)
          
          try({
            q1 <- allt[which(beta_homemade(allt, a[1], a[2], min(allt), max(allt)) > 0.25)][1]
            q3 <- allt[which(beta_homemade(allt, a[1], a[2], min(allt), max(allt)) > 0.75)][1]
            intqr[i] <- q3 - q1
          }, silent = TRUE)
        }
      }
    }
  }
  
  return(list(mn = mn, md = md, intqr = intqr))
}

beta_homemade <- function(t, a, b, l, r) {
  out <- numeric(length(t))
  
  for (i in seq_along(t)) {
    if (t[i] <= l) {
      out[i] <- 0
    } else if (l < t[i] && t[i] <= r) {
      out[i] <- pbeta((t[i] - l) / (r - l), shape1 = a, shape2 = b)
    } else if (r < t[i]) {
      out[i] <- 1
    }
  }
  
  out
}

# ---------
# Main code
# ---------

# Load
#######

# manually design excel such that the top line has the variable names

# Create an empty data frame to store the combined data
combined_data <- data.frame()


# Generate the dataset names based on the wave numbers
dataset_names <- c("FRBNY-SCE-Public-Microdata-Complete-13-16.xlsx", "FRBNY-SCE-Public-Microdata-Complete-17-19.xlsx", "frbny-sce-public-microdata-complete-20-present.xlsx")

# Define the column types for specific columns
col_types <- c(rep("guess", 140), QNUM8 = "numeric", QNUM9 = "numeric", rep("guess", 220-2-140))


# Loop through each dataset and merge it with the combined data frame
for (dataset_name in dataset_names) {
  # Read the csv dataset
  dataset <- read_excel(file.path('empirical', '0_data', 'external',s, dataset_name), guess_max = 10000)
  
  # Merge the dataset with the combined data frame
  if (nrow(combined_data) == 0) {
    # If it's the first dataset, assign it to the combined data frame
    combined_data <- dataset
  } else {
    # If it's not the first dataset, merge it with the combined data frame
    combined_data <- merge(combined_data, dataset, by = intersect(names(combined_data), names(dataset)), all = TRUE)
  }
}

T <- combined_data %>%
  # rename
  mutate(female = as.numeric(Q33==1), single = Q38 - 1, year = as.numeric(substr(date, 1, 4)), month = as.numeric(substr(date, 5, 6)), QNUM2 = as.numeric(QNUM2), QNUM8 = as.numeric(QNUM8), QNUM9 = as.numeric(QNUM9)) %>%
  dplyr::rename(y = Q8v2part2, quali = Q8v2, age = Q32, educ = Q36, hhinc = Q47, regionCAT = `_REGION_CAT`, id = userid,decide_finance = Q46) %>%
  # demographic variables only asked the first time someone participates
  group_by(id) %>%
  mutate(female = ifelse(is.na(female), first(female[!is.na(female)]), female),single = ifelse(is.na(single), first(single[!is.na(single)]), single), age = ifelse(is.na(age), first(age[!is.na(age)]), age), hhinc = ifelse(is.na(hhinc), first(hhinc[!is.na(hhinc)]), hhinc), educ = ifelse(is.na(educ), first(educ[!is.na(educ)]), educ), QNUM2 = ifelse(is.na(QNUM2), first(QNUM2[!is.na(QNUM2)]), QNUM2), QNUM8 = ifelse(is.na(QNUM8), first(QNUM8[!is.na(QNUM8)]), QNUM8), QNUM9 = ifelse(is.na(QNUM9), first(QNUM9[!is.na(QNUM9)]), QNUM9), ddecide_finance = ifelse(is.na(decide_finance), first(decide_finance[!is.na(decide_finance)]), decide_finance)) %>%
  mutate(diff_months = (year - first(year)) * 12 + (month - first(month))) %>%
  mutate(age = ifelse(diff_months <= 12, age, age + floor(diff_months / 12))) %>%
  ungroup %>%
  filter(abs(y) <= 95 & !is.na(female) & !is.na(single) & !is.na(hhinc) & !is.na(educ) & !is.na(regionCAT) & !is.na(age) & abs(educ)<=8 & !is.na(quali)  & abs(age)<=100  & abs(age)>15) %>%
  mutate(region = as.numeric(factor(regionCAT))) 
T$quali[T$quali == 1] = 5
T$quali[T$quali == 2] = 1

# Save T
##########

T_base <- T %>%
  # select standard panel
  dplyr::select(female,single,age,educ,hhinc,region,y,year,month,id,quali) %>%
  subset(!duplicated(T))


write_csv(T_base,file.path('empirical', '0_data', 'manual',s, 'T.csv')) 



## Add Fin lit
#############

# intqr
# is already computed in SCE

# add to T
T$intqr <- T$Q9_iqr

# rounding

# Assign 1 to 'is_multiple_of_5' if 'variable' is a multiple of 5
T$round[T$y %% 5 == 0] <- 1
T$round[!T$y %% 5 == 0] <- 0

# feedback

# add to T
T$qinterest <- T$Q48

# refresher

# Sort the dataframe by participant ID, year, and month
T <- T %>% 
  arrange(id, year, month)

# Add 'refresher' dummy variable
T <- T %>% 
  group_by(id) %>% 
  mutate(refresher = ifelse(duplicated(id), 1, 0)) %>% 
  ungroup()

# fin lit test

# Initialize a variable to count correct points
T$fin_lit_test <- 0

# Check if QNUM2 is equal to 242 and add 1 point if true
T$fin_lit_test <- ifelse(!is.na(T$QNUM2) & T$QNUM2 == 242, T$fin_lit_test + 1, T$fin_lit_test)

# Check if QNUM8 is equal to 3 and add 1 point if true
T$fin_lit_test <- ifelse(!is.na(T$QNUM8) & T$QNUM8 == 3, T$fin_lit_test + 1, T$fin_lit_test)

# Check if QNUM9 is equal to 2 and add 1 point if true
T$fin_lit_test <- ifelse(!is.na(T$QNUM9) & T$QNUM9 == 2, T$fin_lit_test + 1, T$fin_lit_test)


# clean
########

T_fin <- select(T,female,single,age,educ,hhinc,region,y,year,month,id,quali,intqr,round,qinterest,refresher,fin_lit_test) %>%
  # remove intqr obs with absolute val greater 12 (ie -5555)
  filter(abs(intqr)<=100) %>%
  # remove nas
  na.omit %>%
  # remove qinterest and qeasy in abs val greater than 5
  filter(abs(qinterest) <= 5)

# save
###### 

write_csv(T_fin,file.path('empirical', '0_data', 'manual',s, 'T_fin.csv')) 


## Add Household Experience
###########################


# clean
########

T_exp <- select(T,female,single,age,educ,hhinc,region,y,year,month,id,quali,intqr,round,qinterest,refresher,fin_lit_test,decide_finance) %>%
  # remove intqr obs with absolute val greater 12 (ie -5555)
  filter(abs(intqr)<=100) %>%
  # remove nas
  na.omit %>%
  # remove qinterest and qeasy in abs val greater than 5
  filter(abs(qinterest) <= 5) %>%
  # remove non existent experience
  filter(abs(decide_finance) <= 5)

# save
###### 

write_csv(T_exp,file.path('empirical', '0_data', 'manual',s, 'T_exp.csv')) 


## Add Categorical Expectations
###############################

# rename

T <- rename(T,y_gas = C4_1, y_food = C4_2, y_medic = C4_3, y_educ = C4_4, y_rent = C4_5, y_gold = C4_6 )

# clean

T_cat <- select(T,female,single,age,educ,hhinc,region,y,year,month,id,quali,intqr,round,qinterest,refresher,fin_lit_test,decide_finance, y_gold, y_rent, y_educ, y_medic, y_food, y_gas) %>%
  # remove intqr obs with absolute val greater 12 (ie -5555)
  filter(abs(intqr)<=100) %>%
  # remove nas
  na.omit %>%
  # remove qinterest and qeasy in abs val greater than 5
  filter(abs(qinterest) <= 5) %>%
  # remove non existent experience
  filter(abs(decide_finance) <= 5) %>%
  # remove expectations beong 100% 
  filter(abs(y_gas)<=100) %>%
  filter(abs(y_food)<=100) %>%
  filter(abs(y_medic)<=100) %>%
  filter(abs(y_educ)<=100) %>%
  filter(abs(y_rent)<=100) %>%
  filter(abs(y_gold)<=100) 
  

# save
###### 

write_csv(T_cat,file.path('empirical', '0_data', 'manual',s, 'T_cat.csv')) 
