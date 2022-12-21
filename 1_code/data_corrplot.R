# ------------
# Introduction
# ------------

## Overview of correlation matrix

rm(list=ls())
NAME <- 'data_corrplot' ## Name of the R file goes here (without the file extension!)
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
library(corrplot)
library(PerformanceAnalytics)

## --------
## Settings
## --------
### Any settings go here


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

# Function to add histograms
panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  his <- hist(x, plot = FALSE)
  breaks <- his$breaks
  nB <- length(breaks)
  y <- his$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = rgb(0, 1, 1, alpha = 0.5), ...)
  # lines(density(x), col = 2, lwd = 2) # Uncomment to add density lines
}

# Function to add correlation coefficients
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}


# Change the place_points function within the corrplot function. To do so, run:
#   trace(corrplot, edit=TRUE)
# 
# Then replace on line 443
# 
# place_points = function(sig.locs, point) {
#   text(pos.pNew[, 1][sig.locs], pos.pNew[, 2][sig.locs], 
#       labels = point, col = pch.col, cex = pch.cex, 
#       lwd = 2)
#   
# with:
#  
# # adjust text(X,Y ...) according to your needs, here +0.25 is added to the Y-position    
# place_points = function(sig.locs, point) {
#   text(pos.pNew[, 1][sig.locs], (pos.pNew[, 2][sig.locs])+0.25, 
#       labels = point, col = pch.col, cex = pch.cex, 
#       lwd = 2)

# ---------
# Main code
# ---------

#change the corrplot function as described above 
trace(corrplot, edit=TRUE)


## -- Load data from pipeline folder --
# select variables for correlation plot

T <- read_csv(file.path('empirical', '2_pipeline', 'code03_compilepanel.m','out','base', 'T.csv'))
female <- T$female
T <-  select(T, 'live_alone','shop_groceries','shop_major', 'prep_meals', 'decide_finance','pessimist','prob_intqr','f_nointerest','f_easy','refresher','nround')
res1 <- cor.mtest(T, conf.level = .95)

## -- Save Bar Plot

jpeg(file.path('empirical','3_output','results', NAME,"corrplot.jpg"), width = 1000, height = 700)

corrplot.mixed(cor(T),
               lower = "number", 
               upper = "circle",
               tl.col = "black",
               tl.pos = 'd',
               tl.srt = -45,
               tl.cex = 0.75,
               p.mat = res1$p,
               insig = "label_sig",
               sig.level = c(.001, .01, .05),
               pch.cex = 0.9,
               pch.col = "black",
               upper.col = viridis(200),
               lower.col = viridis(200))

dev.off()

# Plotting the correlation matrix
pairs(T,
      lower.panel = panel.cor,    # Correlation panel
      diag.panel = panel.hist,    # histogram
      upper.panel = NULL)         # omit upper panel

jpeg(file.path('empirical','3_output','results', NAME,"chartcorrelation.jpg"), width = 1000, height = 700)

chart.Correlation(T, histogram = TRUE, method = "pearson",lower.panel = NULL)

dev.off()




# ----------
# Leftovers
# ----------
## Here you leave any code snippets or temporary code that you don't need but don't want to delete just yet


## -- only roles for non singles
T_roles <- select(T,"shop_groceries","shop_major","prep_meals","decide_finance") 


jpeg(file.path('empirical','3_output','results', NAME,"corrplot_familyhhroles.jpg"), width = 1000, height = 700)
res1 <- cor.mtest(T_roles[T$live_alone == 0,], conf.level = .95)

corrplot.mixed(cor(T_roles[T$live_alone == 0,]),
               lower = "number", 
               upper = "circle",
               tl.col = "black",
               tl.pos = 'd',
               tl.srt = -45,
               tl.cex = 1,
               p.mat = res1$p,
               insig = "label_sig",
               sig.level = c(.001, .01, .05),
               pch.cex = 0.9,
               pch.col = "black",
               upper.col = viridis(200),
               lower.col = viridis(200))

dev.off()

## --- Split by gender

jpeg(file.path('empirical','3_output','results', NAME,"corrplot_female.jpg"), width = 1000, height = 700)
res1 <- cor.mtest(T[female == 1,], conf.level = .95)

corrplot.mixed(cor(T[female == 1,]),
               lower = "number", 
               upper = "circle",
               tl.col = "black",
               tl.pos = 'd',
               tl.srt = -45,
               tl.cex = 0.75,
               p.mat = res1$p,
               insig = "label_sig",
               sig.level = c(.001, .01, .05),
               pch.cex = 0.9,
               pch.col = "black",
               upper.col = viridis(200),
               lower.col = viridis(200))

dev.off()

jpeg(file.path('empirical','3_output','results', NAME,"corrplot_male.jpg"), width = 1000, height = 700)
res1 <- cor.mtest(T[female == 0,], conf.level = .95)

corrplot.mixed(cor(T[female == 0,]),
               lower = "number", 
               upper = "circle",
               tl.col = "black",
               tl.pos = 'd',
               tl.srt = -45,
               tl.cex = 0.75,
               p.mat = res1$p,
               insig = "label_sig",
               sig.level = c(.001, .01, .05),
               pch.cex = 0.9,
               pch.col = "black",
               upper.col = viridis(200),
               lower.col = viridis(200))

dev.off()
