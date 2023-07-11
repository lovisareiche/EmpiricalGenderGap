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
  outline <- file.path('empirical','3_output','results',f,NAME)
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

  w <-  wilcox.test(y~pred_subj, data = T)

  t <-  t.test(y~pred_subj, data = T)

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

w <-  wilcox.test(y~pred_test, data = T)

t <-  t.test(y~pred_test, data = T)

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


### Histogram

## -- Separate inflation expectations by s

y1 <- T$y[T["pred_test"]==0]
y2 <- T$y[T["pred_test"]==1]

## --- Draw histogram

# create a vector of histogram breaks
x <- seq(-10-0.25,50+0.25,by = 0.5)


h1 <- hist(y1[y1>=-10 & y1<=50], breaks = x, freq = FALSE,
           col = alpha('#238a8DFF',0.8), main = paste("Histogram of Inflation Expectations"),
           xlim = c(-10,30), xlab = "point estimate of inflation in 12 months")
h2 <- hist(y2[y2>=-10 & y2<=50], breaks = x, freq = FALSE,
           col = alpha('#FDE725FF',0.7), 
           xlim = c(-10,30), add = TRUE)

## ---- Overlaid Kernel Density Plot

#plot first kernel density plot
kd1 <- density(y1[y1>=-10 & y1<=50],bw = "nrd0", adjust = 3)
plot(kd1, col='blue', lwd=2)

#plot second kernel density plot
kd2 <- density(y2[y2>=-10 & y2<=50],bw = "nrd0", adjust = 3)
lines(kd2, col='red', lwd=2)

## --- Save numbers in csv

K <- cbind(kd1$x,kd1$y,kd2$x,kd2$y)
write.delim(K, file = file.path(pipeline, 'out', 'K_test.txt'), sep = "\t")



## --- Save numbers in csv
H <- cbind(h1$mids,h1$counts,h1$density,h2$counts,h2$density)
write.delim(H, file = file.path(pipeline, 'out', paste('H_test','.txt',sep = "")), sep = "\t")





T <- transform(T, percentile=(findInterval(y, quantile(T$y, seq(0,1, by=.1)))-1)/10)

jpeg(file.path(pipeline,"boxplot_test.jpg"), width = 1000, height = 700)
b <- boxplot(lpred_test~percentile,
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

med <- data.frame()
uq <- data.frame()
lq <- data.frame()
uw <- data.frame()
lw <- data.frame()

for (i in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) {
  sub <- T[T$y<=quantile(T$y,i),]
  med[1,i*10] <- quantile(sub$lpred_test,0.5)
  uq[1,i*10] <- quantile(sub$lpred_test,0.75)
  lq[1,i*10] <- quantile(sub$lpred_test,0.25)
  uw[1,i*10] <- max(sub$lpred_test)
  lw[1,i*10] <- min(sub$lpred_test)
  
}

M <- data.frame(c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),t(med),t(uq),t(lq),t(uw),t(lw))

# save as delimited text file

library(caroline) # for delimited text file

write.delim(M, file = file.path(pipeline, 'out', 'D.txt'), sep = "\t")