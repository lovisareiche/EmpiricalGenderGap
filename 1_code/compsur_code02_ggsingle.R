# ------------
# Introduction
# ------------

## Compares gender gaps for singles and non-singles from different surveys

rm(list=ls())
NAME <- 'code02_ggsingle' ## Name of the R file goes here (without the file extension!)
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
library(moments) # to compute skew
library(zoo) # for date vector
library(xtable) # for latex conversion
library(stargazer) # for latex table
library('plm')
library('datawizard')
library(lubridate)
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

for(i in 1:length(S)){
  
  # load aligned data
  
  T <- read_csv(file.path('empirical', '2_pipeline',f, 'code01_align','out',S[i], 'T.csv')) %>%
    mutate(survey = S[i], date = ymd(paste0(year, "-", month, "-01"))) %>%
    pdata.frame( index=c( "id", "date" ) )
  assign(paste("T_",S[i],sep = ""),T)
  
  # do t test and wolcoxon rank test
  
  Ts <- filter(T,single == 1)
  tgaps = t.test(Ts$y[Ts$female == 1],Ts$y[Ts$female == 0])
  assign(paste("tgaps_",S[i],sep = ""),tgaps)
  
  wgaps = wilcox.test(Ts$y[Ts$female == 1],Ts$y[Ts$female == 0])
  assign(paste("wgaps_",S[i],sep = ""),wgaps)
  
  Tn <- filter(T,single == 0)
  tgapn = t.test(Tn$y[Tn$female == 1],Tn$y[Tn$female == 0])
  assign(paste("tgapn_",S[i],sep = ""),tgapn)
  
  wgapn = wilcox.test(Tn$y[Tn$female == 1],Tn$y[Tn$female == 0])
  assign(paste("wgapn_",S[i],sep = ""),wgapn)
  
  # do multivariate analysis
  
  
  T_mean <- degroup(
    T,
    c("age","hhinc"),
    "id",
    center = "mean",
    suffix_groupmean = "_between",
    add_attributes = TRUE,
    verbose = TRUE
  )
  
  T_c <- cbind(T,T_mean) %>%
    pdata.frame(index=c( "id", "date" ) )
  
  # Obtain the names of the predictor variables
  predictor_names <- c("age","female","hhinc","educ","age_between","hhinc_between")
  
  # Check for multicollinearity
  cor_matrix <- cor(T_c[, predictor_names])
  highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.8, names = TRUE, verbose = TRUE)
  
  # Remove second variable in highly correlated pairs
  if (length(highly_correlated) > 0) {
    variable_to_remove <- highly_correlated
    predictor_names <- predictor_names[!predictor_names %in% variable_to_remove]
  }
  

  eq <- as.formula(paste('y ~ factor(date) + factor(region) +', paste(predictor_names, collapse = '+')))
  n <- plm( eq, data=filter(T_c,single ==0), effect = "individual", model = "pooling" )
  assign(paste("n_",S[i],sep = ""),n)
  s <- plm( eq, data=filter(T_c,single ==1), effect = "individual", model = "pooling" )
  assign(paste("s_",S[i],sep = ""),s)
  
  ## manually add to table:
  
  # Compute the difference between coefficient for female in models s and n
  coef_diff <- coef(n)["female"] - coef(s)["female"]
  assign(paste("coef_diff_",S[i],sep = ""),coef_diff)
  
  # Compute the standard error of the difference
  se_diff <- sqrt(vcov(n)["female", "female"] + vcov(s)["female", "female"])
  assign(paste("se_diff_",S[i],sep = ""),se_diff)
  
}

# write output

# settings for stargazer
title <- "Multivariate: The gender gap for singles and non-singles"
omit <- c("factor")
omit.labels <- c("Year dummies")
label <- "tab:ggsinglemulti"
dep.var.labels <- "Inflation expectation, 12 months ahead, point estimate"

# in which order
desiredOrder <- c("Constant","female","age","eduschool","hhinc")

writeLines(capture.output(stargazer(`n_BOP-HH`,`s_BOP-HH`,n_FRBNY,s_FRBNY,n_Michigan,s_Michigan,
                                    title = title, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels)), 
           file.path(outline, 'code_ggsinglemulti.tex'))


## Plot for Poster 


library(ggplot2)

coef <- c(`coef_diff_BOP-HH`, coef_diff_FRBNY, coef_diff_Michigan)  # Coefficients
ster <- c(`se_diff_BOP-HH`, se_diff_FRBNY, se_diff_Michigan)  # Standard errors
coef_names <- c("BOP", "SCE", "MSC")  # Coefficient names



# Calculate upper and lower bounds of the confidence interval
ci_lower <- coef - 1.96 * ster
ci_upper <- coef + 1.96 * ster

# Create a data frame for plotting
df <- data.frame(coef = coef, ci_lower = ci_lower, ci_upper = ci_upper, coef_names = coef_names)

# Plotting
plot <- ggplot(df, aes(x = coef_names, y = coef, ymin = ci_lower, ymax = ci_upper)) +
  geom_col(fill = rgb(0, 38, 78, maxColorValue = 255), width = 0.5) +
  geom_errorbar(width = 0.2, color = rgb(255, 204, 0, maxColorValue = 255), size = 1) +
  geom_vline(xintercept = 0, color = "black", size = 0.8) +  # Add the zero axis
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  theme_minimal()

# Save the combined plot
ggsave(file.path(outline,"plot.png"), plot, width = 14, height = 9, units = "cm")


# Calculate the t-statistic
t_statistic <- coef_diff_FRBNY / se_diff_FRBNY
# Calculate the p-value
p_value <- 2 * (1 - pt(abs(t_statistic), 115491))

# Print the p-value
print(p_value)
