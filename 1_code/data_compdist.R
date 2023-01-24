# ------------
# Introduction
# ------------

## Compares distributions from different surveys

rm(list=ls())
NAME <- 'data_compdist' ## Name of the R file goes here (without the file extension!)
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

## --------
## Settings
## --------
### Any settings go here

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

if (dir.exists(file.path('empirical', '2_pipeline'))){
  pipeline <- file.path('empirical', '2_pipeline', NAME)
} else {
  pipeline <- file.path('2_pipeline', NAME)
}

if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  for (folder in c('out', 'store', 'tmp')){
    dir.create(file.path(pipeline, folder))
  }
}


### The code below will automatically create an output folder for this code file if it does not exist.

if (!dir.exists(file.path('empirical', '3_output','results',NAME))) {
  dir.create(file.path('empirical', '3_output','results',NAME))
}


# ---------
# Functions
# ---------

# ---------
# Main code
# ---------


## -- Load data from pipeline folder --

for(i in 1:length(S)){
  T <- read_csv(file.path('empirical', '2_pipeline', 'data_align','out',S[i], 'T.csv')) %>%
    mutate(survey = S[i])
  assign(paste("T_",S[i],sep = ""),T)
    if(i==1){
      F <- T
    }
    if(i>1){
      F <- rbind(F,T)
    }
  }

rm(T)


## -- compute moments --

T <- group_by(F,survey,single,female) %>%
  summarise(mn = round(mean(y),2), md = median(y), std = round(sd(y),2), skew = round(skewness(y),2))

T_diff <- group_by(T,survey,single) %>%
  summarise(dmn = diff(mn), dmd = diff(md), dstd = diff(std), dskew = diff(skew))

# split sample by single or not

F_single = filter(F,single==1)
F_nsingle = filter(F,single==0) 

# run wilcoxin and kolmogorov smirnoff tests on subsamples

for(i in 1:length(S)){
  w <-  wilcox.test(y~female, data = filter(F_single, survey == S[i]))
  assign(paste("w_single_",S[i],sep = ""),w)
}

for(i in 1:length(S)){
  w <-  wilcox.test(y~female, data = filter(F_nsingle, survey == S[i]))
  assign(paste("w_nsingle_",S[i],sep = ""),w)
}

for(i in 1:length(S)){
  ks <-  ks.test(F_single$y[F_single$survey == S[i] & F_single$female == 0],F_single$y[F_single$survey == S[i] & F_single$female == 1])
  assign(paste("ks_single_",S[i],sep = ""),ks)
}
  
for(i in 1:length(S)){
  ks <-  ks.test(F_nsingle$y[F_nsingle$survey == S[i] & F_nsingle$female == 0],F_nsingle$y[F_nsingle$survey == S[i] & F_nsingle$female == 1])
  assign(paste("ks_nsingle_",S[i],sep = ""),ks)
}

T$w <- round(c(rep(`w_nsingle_BOP-HH`$p.value,2),rep(`w_single_BOP-HH`$p.value,2),rep(w_nsingle_Michigan$p.value,2),rep(w_single_Michigan$p.value,2),rep(w_nsingle_FRBNY$p.value,2),rep(w_single_FRBNY$p.value,2)))
T$ks <- round(c(rep(`ks_nsingle_BOP-HH`$p.value,2),rep(`ks_single_BOP-HH`$p.value,2),rep(ks_nsingle_Michigan$p.value,2),rep(ks_single_Michigan$p.value,2),rep(ks_nsingle_FRBNY$p.value,2),rep(ks_single_FRBNY$p.value,2)))
T_diff$w <- round(c(`w_nsingle_BOP-HH`$p.value,`w_single_BOP-HH`$p.value,w_nsingle_Michigan$p.value,w_single_Michigan$p.value,w_nsingle_FRBNY$p.value,w_single_FRBNY$p.value))
T_diff$ks <- round(c(`ks_nsingle_BOP-HH`$p.value,`ks_single_BOP-HH`$p.value,ks_nsingle_Michigan$p.value,ks_single_Michigan$p.value,ks_nsingle_FRBNY$p.value,ks_single_FRBNY$p.value))

# remove data greater than 90th percentile

F_single = filter(F,single==1 & y < quantile(F$y,0.90))
F_nsingle = filter(F,single==0 & y < quantile(F$y,0.90)) 

# run wilcoxin and kolmogorov smirnoff tests on subsamples

for(i in 1:length(S)){
  w <-  wilcox.test(y~female, data = filter(F_single, survey == S[i]))
  assign(paste("w_single_",S[i],sep = ""),w)
}

for(i in 1:length(S)){
  w <-  wilcox.test(y~female, data = filter(F_nsingle, survey == S[i]))
  assign(paste("w_nsingle_",S[i],sep = ""),w)
}

for(i in 1:length(S)){
  ks <-  ks.test(F_single$y[F_single$survey == S[i] & F_single$female == 0],F_single$y[F_single$survey == S[i] & F_single$female == 1])
  assign(paste("ks_single_",S[i],sep = ""),ks)
}

for(i in 1:length(S)){
  ks <-  ks.test(F_nsingle$y[F_nsingle$survey == S[i] & F_nsingle$female == 0],F_nsingle$y[F_nsingle$survey == S[i] & F_nsingle$female == 1])
  assign(paste("ks_nsingle_",S[i],sep = ""),ks)
}

T$w_90 <- round(c(rep(`w_nsingle_BOP-HH`$p.value,2),rep(`w_single_BOP-HH`$p.value,2),rep(w_nsingle_Michigan$p.value,2),rep(w_single_Michigan$p.value,2),rep(w_nsingle_FRBNY$p.value,2),rep(w_single_FRBNY$p.value,2)))
T$ks_90 <- round(c(rep(`ks_nsingle_BOP-HH`$p.value,2),rep(`ks_single_BOP-HH`$p.value,2),rep(ks_nsingle_Michigan$p.value,2),rep(ks_single_Michigan$p.value,2),rep(ks_nsingle_FRBNY$p.value,2),rep(ks_single_FRBNY$p.value,2)))
T_diff$w_90 <- round(c(`w_nsingle_BOP-HH`$p.value,`w_single_BOP-HH`$p.value,w_nsingle_Michigan$p.value,w_single_Michigan$p.value,w_nsingle_FRBNY$p.value,w_single_FRBNY$p.value))
T_diff$ks_90 <- round(c(`ks_nsingle_BOP-HH`$p.value,`ks_single_BOP-HH`$p.value,ks_nsingle_Michigan$p.value,ks_single_Michigan$p.value,ks_nsingle_FRBNY$p.value,ks_single_FRBNY$p.value))


#
# transpose
T <- t(T)
T_diff <- t(T_diff)



xtable(T)
xtable(T_diff)
