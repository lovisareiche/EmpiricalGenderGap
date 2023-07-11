# ------------
# Introduction
# ------------

## Compares distributions from different surveys

rm(list=ls())
NAME <- 'code11_normalising' ## Name of the R file goes here (without the file extension!)
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
library(MASS) # for fitting distributions
library(caroline) # for writing output

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
  T <- read_csv(file.path('empirical', '2_pipeline',f, 'code01_align','out',S[i], 'T.csv')) %>%
    mutate(survey = S[i]) %>%
    filter(y > 0) # only take strictly positive expectations
  assign(paste("T_",S[i],sep = ""),T)
  if(i==1){
    F <- T
  }
  if(i>1){
    F <- rbind(F,T)
  }
  
  
  ## --- Draw histogram
  
  # create a vector of histogram breaks
  x <- seq(0-0.25,100+0.25,by = 0.5)
  
  
  h <- hist(T$y, breaks = x, freq = FALSE,
             col = alpha('#238a8DFF',0.8), main = "Histogram of Inflation Expectations",
             xlim = c(0,30), xlab = "point estimate of inflation in 12 months")
  
  ## --- Save numbers in csv
  
  H <- cbind(h$mids,h$counts,h$density)
  assign(paste("H_",S[i],sep = ""),H)
  write.delim(H, file = file.path(pipeline, 'out', paste('H_',S[i],'.txt',sep = "")), sep = "\t")
  
  ## ---- Kernel Density Plot
  
  #plot first kernel density plot
  kd <- density(log(T$y, base = 10),bw = "nrd0", adjust = 5)
  plot(kd, col='blue', lwd=2)
  
  
  ## --- Save numbers in csv
  
  K <- cbind(kd$x,kd$y)
  assign(paste("K_",S[i],sep = ""),K)
  write.delim(K, file = file.path(pipeline, 'out', paste('K_',S[i],'.txt',sep = "")), sep = "\t")
  
  # Fit female distribution
  fitf <- fitdistr(T$y[T$female==1],"lognormal")
  assign(paste("fitf_",S[i],sep = ""),fitf)
  
  fitm <- fitdistr(T$y[T$female==0],"lognormal")
  assign(paste("fitm_",S[i],sep = ""),fitm)
  
  fit <- fitdistr(T$y,"lognormal")
  assign(paste("fit_",S[i],sep = ""),fit)
  write.delim(K, file = file.path(pipeline, 'out', paste('K_',S[i],'.txt',sep = "")), sep = "\t")
  
  
}

rm(T)

hist(`T_BOP-HH`$y,col="gray",breaks=50,freq=FALSE)
with(as.list(coef(`fitf_BOP-HH`)),
     curve(dlnorm(x,meanlog,sdlog),
           add=TRUE,col="red",lwd=2))
with(as.list(coef(`fitm_BOP-HH`)),
     curve(dlnorm(x,meanlog,sdlog),
           add=TRUE,col="blue",lwd=2))

