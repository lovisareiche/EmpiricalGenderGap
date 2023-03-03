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
    }
    if(i>1){
      F <- rbind(F,T)
    }
  }

rm(T)


## -- compute moments --

T <- group_by(F,survey,female) %>%
  summarise(mn = round(mean(y),2), md = median(y), std = round(sd(y),2), p10 = round(quantile(y,0.1),2), p25 = round(quantile(y,0.25),2), p75 = round(quantile(y,0.75),2), p90 = round(quantile(y,0.9),2))

T_diff <- group_by(T,survey) %>%
  summarise(dmn = diff(mn), dmd = diff(md), dstd = diff(std), dp10 = diff(p10), dp25 = diff(p25), dp75 = diff(p75), dp90 = diff(p90))


# run wilcoxin and kolmogorov smirnoff tests on subsamples

for(i in 1:length(S)){
  w <-  wilcox.test(y~female, data = filter(F, survey == S[i]))
  assign(paste("w_",S[i],sep = ""),w)
}

for(i in 1:length(S)){
  t <-  t.test(y~female, data = filter(F, survey == S[i]))
  assign(paste("t_",S[i],sep = ""),t)
}

for(i in 1:length(S)){
  ks <-  ks.test(F$y[F$survey == S[i] & F$female == 0],F$y[F$survey == S[i] & F$female == 1])
  assign(paste("ks_",S[i],sep = ""),ks)
}
  

T$t <- round(c(rep(`t_BOP-HH`$p.value,2),rep(t_Michigan$p.value,2),rep(t_FRBNY$p.value,2)))
T$w <- round(c(rep(`w_BOP-HH`$p.value,2),rep(w_Michigan$p.value,2),rep(w_FRBNY$p.value,2)))
T$ks <- round(c(rep(`ks_BOP-HH`$p.value,2),rep(ks_Michigan$p.value,2),rep(ks_FRBNY$p.value,2)))
T_diff$t <- round(c(`t_BOP-HH`$p.value,t_Michigan$p.value,t_FRBNY$p.value))
T_diff$w <- round(c(`w_BOP-HH`$p.value,w_Michigan$p.value,w_FRBNY$p.value))
T_diff$ks <- round(c(`ks_BOP-HH`$p.value,ks_Michigan$p.value,ks_FRBNY$p.value))



# remove data greater than 90th percentile

F = filter(F,y < quantile(F$y,0.90))

# run wilcoxin and kolmogorov smirnoff tests on subsamples

for(i in 1:length(S)){
  w <-  wilcox.test(y~female, data = filter(F, survey == S[i]))
  assign(paste("w_",S[i],sep = ""),w)
}

for(i in 1:length(S)){
  t <-  t.test(y~female, data = filter(F, survey == S[i]))
  assign(paste("t_",S[i],sep = ""),t)
}

for(i in 1:length(S)){
  ks <-  ks.test(F$y[F$survey == S[i] & F$female == 0],F$y[F$survey == S[i] & F$female == 1])
  assign(paste("ks_",S[i],sep = ""),ks)
}


T$t_p90 <- round(c(rep(`t_BOP-HH`$p.value,2),rep(t_Michigan$p.value,2),rep(t_FRBNY$p.value,2)))
T$w_p90 <- round(c(rep(`w_BOP-HH`$p.value,2),rep(w_Michigan$p.value,2),rep(w_FRBNY$p.value,2)))
T$ks_p90 <- round(c(rep(`ks_BOP-HH`$p.value,2),rep(ks_Michigan$p.value,2),rep(ks_FRBNY$p.value,2)))
T_diff$t_p90 <- round(c(`t_BOP-HH`$p.value,t_Michigan$p.value,t_FRBNY$p.value))
T_diff$w_p90 <- round(c(`w_BOP-HH`$p.value,w_Michigan$p.value,w_FRBNY$p.value))
T_diff$ks_p90 <- round(c(`ks_BOP-HH`$p.value,ks_Michigan$p.value,ks_FRBNY$p.value))


#
# transpose
T <- t(T)
T_diff <- t(T_diff)

V7 <- c("ECFIN",0,6.4,4.1,9.7,0.7,1.5,9.1,15.9,0,0,0,0,0,0)
V8 <- c("ECFIN",1,7.8,4.9,11.5,0.8,1.7,11.3,19.8,0,0,0,0,0,0)

T <- cbind(T,V7,V8)

V <- vector()
V[1] <- "ECFIN"
V[2:length(V8)-1] <- round(as.numeric(V8[3:length(V8)])-as.numeric(V7[3:length(V8)]),2)

T_diff <- cbind(T_diff,V)


xtable(T)
xtable(T_diff)


