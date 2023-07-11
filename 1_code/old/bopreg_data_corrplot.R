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
library(Hmisc)
library(xtable)

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

# Function to create correlation table with significance stars
# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
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

## -- Load data from pipeline folder --
# select variables for correlation plot

T <- read_csv(file.path('empirical', '2_pipeline', f,'code03_compilepanel.m','out','base', 'T.csv'))
female <- T$female
T <-  select(T, 'non_single','shop_groceries_nsing','shop_major_nsing', 'prep_meals_nsing', 'decide_finance_nsing','pessimist','prob_intqr','f_nointerest','f_easy','refresher','nround')
res1 <- cor.mtest(T, conf.level = .95)


# Make correlation table

corstars(T, result="latex")

#change the corrplot function as described above 
trace(corrplot, edit=TRUE)

## -- Save Bar Plot

jpeg(file.path(outline,"corrplot.jpg"), width = 1000, height = 700)

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

jpeg(file.path(outline,"chartcorrelation.jpg"), width = 1000, height = 700)

chart.Correlation(T, histogram = TRUE, method = "pearson",lower.panel = NULL)

dev.off()




# ----------
# Leftovers
# ----------
## Here you leave any code snippets or temporary code that you don't need but don't want to delete just yet


## -- only roles for non singles
T_roles <- select(T,"shop_groceries_nsing","shop_major_nsing","prep_meals_nsing","decide_finance_nsing") 


jpeg(file.path(outline,"corrplot_familyhhroles.jpg"), width = 1000, height = 700)
res1 <- cor.mtest(T_roles[T$non_single == 0,], conf.level = .95)

corrplot.mixed(cor(T_roles[T$non_single == 0,]),
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

jpeg(file.path(outline,"corrplot_female.jpg"), width = 1000, height = 700)
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

jpeg(file.path(outline,"corrplot_male.jpg"), width = 1000, height = 700)
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
