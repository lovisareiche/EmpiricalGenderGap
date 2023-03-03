# ------------
# Introduction
# ------------

## Compares distributions from different surveys

rm(list=ls())
NAME <- 'code06_gghighermoments' ## Name of the R file goes here (without the file extension!)
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
library(lattice)

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
  outline <- file.path('3_output','results',f,NAME)
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
  T <- read_csv(file.path('empirical', '2_pipeline',f, 'code01_align','out',S[i], 'T.csv')) %>%
    mutate(survey = S[i])
  assign(paste("T_",S[i],sep = ""),T)
    if(i==1){
      F <- T
      F_p25 <- filter(T,y < quantile(y,0.9))
    }
    if(i>1){
      F <- rbind(F,T)
      F_p25 <- rbind(F_p25,filter(T,y < quantile(y,0.9)))
    }
  }

rm(T)


## -- compute moments --

T <- group_by(F,survey,female) %>%
  dplyr::summarise(mn = round(mean(y),2), md = median(y), std = round(sd(y),2), p10 = round(quantile(y,0.1),2), p25 = round(quantile(y,0.25),2), p75 = round(quantile(y,0.75),2), p90 = round(quantile(y,0.9),2))

# run wilcoxin and kolmogorov smirnoff tests on subsamples

for(i in 1:length(S)){
  w <-  wilcox.test(y~female, data = filter(F, survey == S[i]),alternative = c("less"))
  assign(paste("w_",S[i],sep = ""),w)
}

for(i in 1:length(S)){
  t <-  t.test(y~female, data = filter(F, survey == S[i]),alternative = c("less"))
  assign(paste("t_",S[i],sep = ""),t)
}

for(i in 1:length(S)){
  ks <-  ks.test(F$y[F$survey == S[i] & F$female == 0],F$y[F$survey == S[i] & F$female == 1])
  assign(paste("ks_",S[i],sep = ""),ks)
}
  

T$t <- round(c(rep(`t_BOP-HH`$p.value,2),rep(t_FRBNY$p.value,2),rep(t_Michigan$p.value,2)),2)
T$w <- round(c(rep(`w_BOP-HH`$p.value,2),rep(w_FRBNY$p.value,2),rep(w_Michigan$p.value,2)),2)
T$ks <- round(c(rep(`ks_BOP-HH`$p.value,2),rep(ks_FRBNY$p.value,2),rep(ks_Michigan$p.value,2)),2)


# Same for subsample

Tsub <- group_by(F_p25,survey,female) %>%
  dplyr::summarise(mn = round(mean(y),2), md = median(y), std = round(sd(y),2), p10 = round(quantile(y,0.1),2), p25 = round(quantile(y,0.25),2), p75 = round(quantile(y,0.75),2), p90 = round(quantile(y,0.9),2))


for(i in 1:length(S)){
  w <-  wilcox.test(y~female, data = filter(F_p25, survey == S[i]),alternative = c("less"))
  assign(paste("w_",S[i],sep = ""),w)
}

for(i in 1:length(S)){
  t <-  t.test(y~female, data = filter(F_p25, survey == S[i]),alternative = c( "less"))
  assign(paste("t_",S[i],sep = ""),t)
}

for(i in 1:length(S)){
  ks <-  ks.test(F_p25$y[F_p25$survey == S[i] & F_p25$female == 0],F_p25$y[F_p25$survey == S[i] & F_p25$female == 1])
  assign(paste("ks_",S[i],sep = ""),ks)
}


T$t_p25 <- round(c(rep(`t_BOP-HH`$p.value,2),rep(t_FRBNY$p.value,2),rep(t_Michigan$p.value,2)),2)
T$w_p25 <- round(c(rep(`w_BOP-HH`$p.value,2),rep(w_FRBNY$p.value,2),rep(w_Michigan$p.value,2)),2)
T$ks_p25 <- round(c(rep(`ks_BOP-HH`$p.value,2),rep(ks_FRBNY$p.value,2),rep(ks_Michigan$p.value,2)),2)


#
# transpose
T <- t(T)
Tsub <- t(Tsub)

V7 <- c("ECFIN",0,6.4,4.1,9.7,0.7,1.5,9.1,15.9,0,0,0,0,0,0)
V8 <- c("ECFIN",1,7.8,4.9,11.5,0.8,1.7,11.3,19.8,0,0,0,0,0,0)

T <- cbind(T,V7,V8)


xtable(T)
xtable(Tsub)


histogram(F_p25$y[F_p25$female == 0])


