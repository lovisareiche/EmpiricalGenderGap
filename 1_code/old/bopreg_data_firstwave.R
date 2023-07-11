# ------------
# Introduction
# ------------

## Create a chart to show when survey participants joined the panel

rm(list=ls())
NAME <- 'data_firstwave' ## Name of the R file goes here (without the file extension!)
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
library("viridis")  # colour

## --------
## Settings
## --------
### Any settings go here
f <- 'bopreg'


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

if (!dir.exists(file.path('empirical', '3_output','results',f,NAME))) {
  outline <- file.path('empirical', '3_output','results',f,NAME)
  dir.create(outline)
}

# ---------
# Functions
# ---------


# ---------
# Main code
# ---------


## -- Load data from pipeline folder --

T <- read_csv(file.path('empirical', '2_pipeline', f,'code03_compilepanel.m','out','base', 'T.csv')) 



## -- Create firstwave vector


W <- mutate(T,firstwave = floor(T$id/100000)) %>%
  group_by(wave,firstwave) %>%
  summarise(obsfirstwave = n()) %>%
  ungroup 
 

## -- Fill missing values

wave <- sort(unique(W$wave))
firstwave <- sort(unique(W$firstwave))

W_full <- matrix(0, nrow = length(firstwave),ncol = length(wave))
for (i in 1:length(wave)){
  w <- wave[i]
  for (ii in 1:length(firstwave)) {
    o <- firstwave[ii]
    if (any(W$wave == w & W$firstwave == o)) {
      W_full[ii,i] <- W$obsfirstwave[W$wave == w & W$firstwave == o]
    }
  }
}

wnames <- paste("w",wave,sep = "")
fwnames <- paste("w",firstwave,sep = "")

## -- Save Bar Plot

jpeg(file.path(outline,"firstwave.jpg"), width = 1000, height = 700)


barplot(W_full, main = "Wave of first survey participation", names.arg = wnames,
        xlab = "Observations", ylab = "Wave", col = viridis(length(firstwave)))
legend("topleft", fwnames, fill = viridis(length(firstwave)))

dev.off()



# ----------
# Leftovers
# ----------
## Here you leave any code snippets or temporary code that you don't need but don't want to delete just yet

write.csv(W_full, file = file.path(pipeline, 'out', 'W.csv'))
 
