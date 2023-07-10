# ------------
# Introduction
# ------------

## Compares timeseries expectations from different surveys

rm(list=ls())
NAME <- 'code07_quantreg' ## Name of the R file goes here (without the file extension!)
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
library('datawizard')
library(stargazer)
library(quantreg)
library('plm')
library(zoo)
library(plm)
library(caret)

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

C_female <- data.frame()
SE_female <- data.frame()
C_single <- data.frame()
SE_single <- data.frame()
C_age <- data.frame()
SE_age <- data.frame()
C_educ <- data.frame()
SE_educ <- data.frame()
C_hhinc <- data.frame()
SE_hhinc <- data.frame()
O <- data.frame()
Q <- data.frame()
R <- data.frame()
AR <- data.frame()

for(i in 1:length(S)){
  T <- read_csv(file.path('empirical', '2_pipeline',f, 'code01_align','out',S[i], 'T.csv')) %>%
    mutate(survey = S[i], date = ymd(paste0(year, "-", month, "-01"))) %>%
    pdata.frame( index=c( "id", "date" ) )
  
  T <- T[!duplicated(T[c('id','date')]), ]
  
  assign(paste("T_",S[i],sep = ""),T)
  if(i==1){
    F <- T
  }
  if(i>1){
    F <- rbind(F,T)
  }
  
  # define names
  xnames <- colnames(T) %>%
    setdiff('year') %>%
    setdiff('month') %>%
    setdiff('y') %>%
    setdiff(c('survey','id','date','region'))
  xtinames <- c("female")
  xtvnames <- setdiff(xnames,xtinames)
  
  # bind between effects
  T_mean <- degroup(
    T,
    xtvnames,
    "id",
    center = "mean",
    suffix_groupmean = "_between",
    add_attributes = TRUE,
    verbose = TRUE
  )
  
  T_c <- cbind(T,T_mean) %>%
    pdata.frame(index=c( "id", "date"))
  
  # Obtain the names of the predictor variables
  predictor_names <- c( xnames, paste(xtvnames, "_between", sep = ""))
  
  # Check for multicollinearity
  cor_matrix <- cor(na.omit(T_c[, predictor_names]))
  highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.8, names = TRUE, verbose = TRUE)
  
  # Function to check and attach "_between" if necessary
  check_and_attach <- function(element) {
    if (!grepl("_between", element))
      element <- paste0(element, "_between")
    return(element)
  }
  
  # Remove second variable in highly correlated pairs
  if (length(highly_correlated) > 0) {
    variable_to_remove <- sapply(highly_correlated,check_and_attach)
    predictor_names <- predictor_names[!predictor_names %in% variable_to_remove]
  }
  
  # Create the formula with the updated predictor names
  formula <- as.formula(paste('y ~ factor(date) + factor(region) +', paste(predictor_names, collapse = '+')))
  
  for (ii in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) {
    
    # run on quintiles
    m <- plm( formula, data=filter(T_c, y <= quantile(y, ii)), effect = "individual", model = "pooling")
    assign(paste("m_",ii,sep = ""),m)
    
    # collect all coefficient estimates and standard errors for variables of interest
    C_female[i,ii*10] <- coefficients(m)["female"]
    SE_female[i,ii*10] <- coefficients(summary(m))[, "Std. Error"]["female"]
    C_single[i,ii*10] <- coefficients(m)["single"]
    SE_single[i,ii*10] <- coefficients(summary(m))[, "Std. Error"]["single"]
    C_age[i,ii*10] <- coefficients(m)["age"]
    SE_age[i,ii*10] <- coefficients(summary(m))[, "Std. Error"]["age"]
    C_educ[i,ii*10] <- coefficients(m)["educ"]
    SE_educ[i,ii*10] <- coefficients(summary(m))[, "Std. Error"]["educ"]
    C_hhinc[i,ii*10] <- coefficients(m)["hhinc"]
    SE_hhinc[i,ii*10] <- coefficients(summary(m))[, "Std. Error"]["hhinc"]
    
    # collect observations, percentiles, R2 and adjuster R2
    O[i,ii*10] <- nrow(filter(T_c, y<=quantile(y,ii)))
    Q[i,ii*10] <- quantile(T_c$y,ii)
    h <- summary(m)["r.squared"]
    R[i,ii*10] <- h$r.squared["rsq"]
    AR[i,ii*10] <- h$r.squared["adjrsq"]
  }
  

  # write latex
  
  # settings for stargazer
  omit <- c("factor(date)","factor(region)")
  omit.labels <- c("Time dummies","Regional dummies")
  title <- "Qunatile regression"
  label <- paste("tab:quantreg",S[i],sep="")
  dep.var.labels <- "Inflation expectation, 12 months ahead, point estimate"
  column.labels <- c("Bottom 20%", "Bottom 40%", "Bottom 60%", "Bottom 80%", "Full Sample")
  
  # in which order
  desiredOrder <- c("Constant","female","single","age","educ","hhinc")
  
  writeLines(capture.output(stargazer(m_0.2,m_0.4,m_0.6,m_0.8,m_1,
                                      title = title, label = label, 
                                      omit = omit, omit.labels = omit.labels, 
                                      model.names = FALSE, column.labels = column.labels,
                                      align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                      order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                      dep.var.labels = dep.var.labels)), 
             file.path(outline, paste("code_quantreg",S[i],".txt",sep="")))
  
}

rm(T)

## Format to use in pgf plot

BOP <- data.frame(t(C_female[1,]),t(C_single[1,]),t(C_age[1,]),t(C_educ[1,]),t(C_hhinc[1,]),t(SE_female[1,]),t(SE_single[1,]), t(SE_age[1,]),t(SE_educ[1,]),t(SE_hhinc[1,]),t(O[1,])) %>%
  dplyr::rename(female = X1, single = X1.1, age = X1.2, educ = X1.3, hhinc = X1.4,female_se = X1.5, single_se = X1.6, age_se = X1.7, educ_se = X1.8, hhinc_se = X1.9, obs = X1.10) %>%
  mutate(female_u = female + female_se, single_u = single + single_se, age_u = age + age_se, educ_u = educ + educ_se, hhinc_u = hhinc + hhinc_se) %>%
  mutate(female_l = female - female_se, single_l = single - single_se, age_l = age - age_se, educ_l = educ - educ_se, hhinc_l = hhinc - hhinc_se) %>%
  mutate(decile = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) %>%
  mutate(decileval = c(quantile(`T_BOP-HH`$y,0.1),quantile(`T_BOP-HH`$y,0.2),quantile(`T_BOP-HH`$y,0.3),quantile(`T_BOP-HH`$y,0.4),quantile(`T_BOP-HH`$y,0.5),quantile(`T_BOP-HH`$y,0.6),quantile(`T_BOP-HH`$y,0.7),quantile(`T_BOP-HH`$y,0.8),quantile(`T_BOP-HH`$y,0.9),quantile(`T_BOP-HH`$y,1)))


MSC <- data.frame(t(C_female[2,]),t(C_single[2,]),t(C_age[2,]),t(C_educ[2,]),t(C_hhinc[2,]),t(SE_female[2,]),t(SE_single[2,]), t(SE_age[2,]),t(SE_educ[2,]),t(SE_hhinc[2,]),t(O[2,])) %>%
  dplyr::rename(female = X2, single = X2.1, age = X2.2, educ = X2.3, hhinc = X2.4,female_se = X2.5, single_se = X2.6, age_se = X2.7, educ_se = X2.8, hhinc_se = X2.9, obs = X2.10) %>%
  mutate(female_u = female + female_se, single_u = single + single_se, age_u = age + age_se, educ_u = educ + educ_se, hhinc_u = hhinc + hhinc_se) %>%
  mutate(female_l = female - female_se, single_l = single - single_se, age_l = age - age_se, educ_l = educ - educ_se, hhinc_l = hhinc - hhinc_se) %>%
  mutate(decile = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) %>%
  mutate(decileval = c(quantile(T_Michigan$y,0.1),quantile(T_Michigan$y,0.2),quantile(T_Michigan$y,0.3),quantile(T_Michigan$y,0.4),quantile(T_Michigan$y,0.5),quantile(T_Michigan$y,0.6),quantile(T_Michigan$y,0.7),quantile(T_Michigan$y,0.8),quantile(T_Michigan$y,0.9),quantile(T_Michigan$y,1)))


SCE <- data.frame(t(C_female[3,]),t(C_single[3,]),t(C_age[3,]),t(C_educ[3,]),t(C_hhinc[3,]),t(SE_female[3,]),t(SE_single[3,]), t(SE_age[3,]),t(SE_educ[3,]),t(SE_hhinc[3,]),t(O[3,])) %>%
  dplyr::rename(female = X3, single = X3.1, age = X3.2, educ = X3.3, hhinc = X3.4,female_se = X3.5, single_se = X3.6, age_se = X3.7, educ_se = X3.8, hhinc_se = X3.9, obs = X3.10) %>%
  mutate(female_u = female + female_se, single_u = single + single_se, age_u = age + age_se, educ_u = educ + educ_se, hhinc_u = hhinc + hhinc_se) %>%
  mutate(female_l = female - female_se, single_l = single - single_se, age_l = age - age_se, educ_l = educ - educ_se, hhinc_l = hhinc - hhinc_se) %>%
  mutate(decile = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) %>%
  mutate(decileval = c(quantile(T_FRBNY$y,0.1),quantile(T_FRBNY$y,0.2),quantile(T_FRBNY$y,0.3),quantile(T_FRBNY$y,0.4),quantile(T_FRBNY$y,0.5),quantile(T_FRBNY$y,0.6),quantile(T_FRBNY$y,0.7),quantile(T_FRBNY$y,0.8),quantile(T_FRBNY$y,0.9),quantile(T_FRBNY$y,1)))



# save as delimited text file

library(caroline) # for delimited text file

write.delim(BOP, file = file.path(pipeline, 'out', 'BOP.txt'), sep = "\t")
write.delim(MSC, file = file.path(pipeline, 'out', 'MSC.txt'), sep = "\t")
write.delim(SCE, file = file.path(pipeline, 'out', 'SCE.txt'), sep = "\t")
