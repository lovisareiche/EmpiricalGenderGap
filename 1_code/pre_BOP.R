# ------------
# Introduction
# ------------

## Coverts different inflation expectation surveys in a common format

rm(list=ls())
NAME <- 'BOP' ## Name of the R file goes here (without the file extension!)
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
library(Triangular)
library(optim)


## --------
## Settings
## --------
### Any settings go here

s <- 'BOP-HH'

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


# Create an empty data frame to store the combined data
combined_data <- data.frame()

# Create a vector of wave numbers from 01 to 33
wave_numbers <- sprintf("%02d", 1:33)

# Generate the dataset names based on the wave numbers
dataset_names <- paste0("bophh_suf_202204_v01_wave", wave_numbers, ".dta")


# Loop through each dataset and merge it with the combined data frame
for (dataset_name in dataset_names) {
  # Read the Stata dataset
  dataset <- read_dta(file.path('empirical', '0_data', 'external',s, dataset_name))
  
  # Convert labelled variables to numeric without labels
  dataset <- mutate_if(dataset, is.labelled, as.numeric)
  
  # Merge the dataset with the combined data frame
  if (nrow(combined_data) == 0) {
    # If it's the first dataset, assign it to the combined data frame
    combined_data <- dataset
  } else {
    # If it's not the first dataset, merge it with the combined data frame
    combined_data <- merge(combined_data, dataset, by = intersect(names(combined_data), names(dataset)), all = TRUE)
  }
}

# Replace missing values (NA) for non-existent variables in each wave
combined_data[is.na(combined_data)] <- -6666

# Sort the combined data frame by a common variable (e.g., participant ID)
combined_data <- combined_data[order(combined_data$id), ] %>%
  filter(abs(inflexppoint) <= 95 & abs(eduschool) <= 8 & abs(hhinc) <= 13 & abs(expmacroquali_e) <= 5 & abs(infdef) <= 2 & abs(gender) <= 2 & abs(eduwork)<=10)

# Cleaning and defining vars:
###############################

# y

# Assign negative sign to infexppoint when infdef is 2
combined_data$inflexppoint[combined_data$infdef == 2] <- -as.numeric(combined_data$inflexppoint[combined_data$infdef == 2])

# female

# Assign 1 to 'female' when 'gender' is 2
combined_data$female[combined_data$gender == 2] <- 1
combined_data$female[combined_data$gender == 1] <- 0

# single 

# Assign 1 to 'single' when 'hhsize' is 1
combined_data$single[combined_data$hhsize == 1] <- 1
combined_data$single[combined_data$hhsize > 1] <- 0

# eduschool

# Assign 0 to 'response' when 'eduschool' is 8
combined_data$response[combined_data$eduschool == 8] <- 0
# Drop rows where 'eduschool' is 7
combined_data <- combined_data[combined_data$eduschool != 7, ]

# eduwork

# Assign 0 to 'response' when 'eduwork' is 10
combined_data$response[combined_data$eduwork == 10] <- 0
# Drop rows where 'eduschool' is 7
combined_data <- combined_data[combined_data$eduwork != 9, ]

# educ 

combined_data$educ <- combined_data$eduschool + combined_data$eduwork

# Save T
##########

T <- combined_data %>%
  dplyr::rename(y = inflexppoint, quali = expmacroquali_e) %>%
  # select standard panel
  dplyr::select(female,single,age,educ,hhinc,region,y,year,month,id,quali)


write_csv(T,file.path(pipeline,'out', 'T.csv')) 


## Add Fin lit
#############

# intqr

# Extract the data from the variables expmacroquali_a to expmacroquali_j
data <- as.matrix(cbind(combined_data$infexprob_a, combined_data$infexprob_b, combined_data$infexprob_c,
                        combined_data$infexprob_d, combined_data$infexprob_e, combined_data$infexprob_f,
                        combined_data$infexprob_g, combined_data$infexprob_h, combined_data$infexprob_i,
                        combined_data$infexprob_j))

# Set the innerbinedges and allt variables accordingly (example values)
innerbinedges <- c(-12, -8, -4, -2, 0, 2, 4, 8, 12)  # Update with the appropriate bin edges
allt <- c(-12:1:12)  # Update with the appropriate time points

# Apply the probabilistic_funcfit function to the data
result <- probabilistic_funcfit(data, innerbinedges, allt)

# add to T
T$intqr <- result$intqr

# rounding

# Assign 1 to 'is_multiple_of_5' if 'variable' is a multiple of 5
T$round[T$y %% 5 == 0] <- 1
T$round[!T$y %% 5 == 0] <- 0

# feedback

# Reverse the order of 'qinterest'
combined_data$qinterest <- recode(combined_data$qinterest, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)

# add to T
T$qinterest <- combined_data$qinterest
T$qeasy <- combined_data$qeasy

# refresher

# Sort the dataframe by participant ID, year, and month
combined_data <- combined_data %>% 
  arrange(id, year, month)
T <- T %>% 
  arrange(id, year, month)

# Add 'refresher' dummy variable
T <- T %>% 
  group_by(id) %>% 
  mutate(refresher = ifelse(duplicated(id), 1, 0)) %>% 
  ungroup()

# clean
########

# remove intqr obs with absolute val greater 12 (ie -5555)
T_fin <- filter(T,abs(intqr)<=12) %>%
  # remove nas
  na.omit %>%
  # remove qinterest and qeasy in abs val greater than 5
  filter(abs(qeasy) <= 5 & abs(qinterest) <= 5)

# save
###### 

write_csv(T_fin,file.path(pipeline,'out', 'T_fin.csv')) 


## Add Household Experience
###########################

# rename
T$shop_groceries <- recode(combined_data$mainshopper_a, `1` = 3, `2` = 2, `3` = 1)
T$shop_major <- recode(combined_data$mainshopper_b, `1` = 3, `2` = 2, `3` = 1)
T$prep_meals <- recode(combined_data$mainshopper_c, `1` = 3, `2` = 2, `3` = 1)
T$decide_finance <- recode(combined_data$mainshopper_d, `1` = 3, `2` = 2, `3` = 1)

# clean
########

# remove intqr obs with absolute val greater 12 (ie -5555)
T_exp <- filter(T,abs(intqr)<=12) %>%
  # remove nas
  na.omit %>%
  # remove qinterest and qeasy in abs val greater than 5
  filter(abs(qeasy) <= 5 & abs(qinterest) <= 5) %>%
  # remove non existent experience
  filter(abs(shop_groceries) <= 3, abs(shop_major) <= 3, abs(prep_meals) <= 3, abs(decide_finance) <= 3)

# save
###### 

write_csv(T_exp,file.path(pipeline,'out', 'T_exp.csv')) 
