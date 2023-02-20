# ------------
# Introduction
# ------------

## Compares distributions from financially literate and illiterate

rm(list=ls())
NAME <- 'code14_finhighermoments' ## Name of the R file goes here (without the file extension!)
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

load(file.path('empirical', '2_pipeline',f, 'code09_fitlit','out', 'T_fin.RData')) 

# create binary vector for test and subj
T <- mutate(T,pred_subj = as.numeric(lpred_subj>median(lpred_subj)), pred_test = as.numeric(lpred_test>median(lpred_test)))

T_sub <- filter(T, y <= 4 & y >= 0)

## all for subjective

## -- compute moments --

F1 <- group_by(T,pred_subj) %>%
  summarise(mn = round(mean(y),2), md = median(y), std = round(sd(y),2), p10 = round(quantile(y,0.1),2), p25 = round(quantile(y,0.25),2), p75 = round(quantile(y,0.75),2), p90 = round(quantile(y,0.9),2))

# run wilcoxin and kolmogorov smirnoff tests on subsamples

  w <-  wilcox.test(y~pred_subj, data = T,alternative = c("less"))

  t <-  t.test(y~pred_subj, data = T,alternative = c("less"))

  ks <-  ks.test(T$y[T$pred_subj == 0],T$y[T$pred_subj == 1])



F1$t <- round(rep(t$p.value,2),2)
F1$w <- round(rep(w$p.value,2),2)
F1$ks <- round(rep(ks$p.value,2),2)


# run wilcoxin and kolmogorov smirnoff tests on subsamples

w <-  wilcox.test(y~pred_subj, data = T_sub,alternative = c("less"))

t <-  t.test(y~pred_subj, data = T_sub,alternative = c("less"))

ks <-  ks.test(T_sub$y[T_sub$pred_subj == 0],T_sub$y[ T_sub$pred_subj == 1])


F1$t_sub <- round(rep(t$p.value,2),2)
F1$w_sub <- round(rep(w$p.value,2),2)
F1$ks_sub <- round(rep(ks$p.value,2),2)

# transpose
F1 <- t(F1)


## All for Test


## -- compute moments --

F2 <- group_by(T,pred_test) %>%
  summarise(mn = round(mean(y),2), md = median(y), std = round(sd(y),2), p10 = round(quantile(y,0.1),2), p25 = round(quantile(y,0.25),2), p75 = round(quantile(y,0.75),2), p90 = round(quantile(y,0.9),2))

# run wilcoxin and kolmogorov smirnoff tests on subsamples

w <-  wilcox.test(y~pred_test, data = T,alternative = c("less"))

t <-  t.test(y~pred_test, data = T,alternative = c("less"))

ks <-  ks.test(T$y[T$pred_test == 0],T$y[T$pred_test == 1])



F2$t <- round(rep(t$p.value,2),2)
F2$w <- round(rep(w$p.value,2),2)
F2$ks <- round(rep(ks$p.value,2),2)


# run wilcoxin and kolmogorov smirnoff tests on subsamples

w <-  wilcox.test(y~pred_test, data = T_sub,alternative = c("less"))

t <-  t.test(y~pred_test, data = T_sub,alternative = c("less"))

ks <-  ks.test(T_sub$y[T_sub$pred_test == 0],T_sub$y[ T_sub$pred_subj == 1])


F2$t_sub <- round(rep(t$p.value,2),2)
F2$w_sub <- round(rep(w$p.value,2),2)
F2$ks_sub <- round(rep(ks$p.value,2),2)

# transpose
F2 <- t(F2)

xtable(cbind(F1,F2))


## Show women have lower financial literacy

W <- T %>%
  group_by(female) %>%
  summarise(pred_subj = mean(lpred_subj), 
            lpred_test = mean(lpred_test),
            prob_intqr = mean(prob_intqr), 
            refresher = mean(refresher), 
            nround = mean(nround), 
            f_nointerest = mean(f_nointerest), 
            f_easy = mean(f_easy)) %>%
  select(-female) %>%
  # tranpose
  t %>%
  # make dataframe
  as.data.frame %>%
  # change variable names
  rename("Male subsample" = "V1", "Female subsample" = "V2")

w <-  ks.test(T$lpred_subj[T$female==0],T$lpred_subj[T$female==1])




## -- Save output

writeLines(capture.output(xtable(W, 
                                 caption = "Comparing the male and female subsamples", 
                                 label = "ggfinlit")),
           file.path(outline, 'gendergapfinlit.tex'))



T <- transform(T, percentile=(findInterval(y, quantile(T$y, seq(0,1, by=.1)))-1)/10)

jpeg(file.path(pipeline,"boxplot_test.jpg"), width = 1000, height = 700)
boxplot(lpred_test~percentile,
        data=T,
        xlab="Percentile of inflation expectation distribution",
        ylab="Predicted Test Score",
        col="steelblue",
        border="black"
)
dev.off()

jpeg(file.path(pipeline,"boxplot_subj.jpg"), width = 1000, height = 700)
boxplot(lpred_subj~percentile,
        data=T,
        xlab="Percentile of inflation expectation distribution",
        ylab="Predicted Subjective Literacy",
        col="steelblue",
        border="black"
)
dev.off()
