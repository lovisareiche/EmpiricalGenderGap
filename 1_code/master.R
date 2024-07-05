
# Master code for "That's what she said"
# ----------------------------------------


PROJECT <- 'EmpiricalGenderGap'
PROJECT_DIR <- 'D:/Lovisa/Studium/Oxford/Department of Economics/DPhil' ## Change this to the directory in which your project folder is located, make sure to avoid using single backslashes (i.e. \)!

setwd(file.path(PROJECT_DIR, PROJECT))

code_source <- file.path('empirical', '1_code')


## ----------------------------------------
## Start to load all relevant data sources
## ----------------------------------------

# Preparing BOP

source(file.path('empirical', '1_code',"pre_BOP.R"))

# Preparing SCE

source(file.path('empirical', '1_code',"pre_SCE.R"))


# Aligning all three surveys
source(file.path('empirical', '1_code',"compsur_code01_align.R"))
# Requires that Michigan survey in 0_data/manual/MSC as T.csv

# can start running from here if the files above have been run once

------------------------------------------------------------------------

## Section 3: Experience and financial confidence in the data
###############################################################

# Table 2: Confirming the presence of traditional gender roles in the BOP-HH
# --------
source(file.path('empirical', '1_code',"hhrole_code01_summary.R"))


## Section 4: The Effects of Financial Confidence and Shopping Experience
#########################################################################

# Table 3: The role of financial confidence and experience
#---------
source(file.path('empirical', '1_code',"hhrole_code02_reg.R"))

# Figure 1: The effect of grocery shopping involvement on inflation expectations for different levels of financial confidence
#---------
# the slope of the figure is calculated in the file above 
# the histograms will follow below: "finlit_code06_finhist.R
# finlitgrocpred


## Section 5: Financial Confidence Channel
##########################################


# Table Table 9: Explaining financial confidence through financial confidence variables
# -------
# prep BOP and SCE 
# produce predicted test scores 
source(file.path('empirical', '1_code',"finlit_code01_fitlit.R"))


# Table 4: Financial literacy and confidence of men and women
# -------
source(file.path('empirical', '1_code',"finlit_code02_ggfinlit.R"))


# Figure 2: Kernel Density plots split by literacy. 
#---------
# use output in latex code to plot
source(file.path('empirical', '1_code',"finlit_code03_finlitdist.R"))
# kdpredbop
# kdpredsce
# kdtestbop
# kdtestsce


# Table 5: The impact of financial confidence on the gender gap (BOP)
# Table 12: The impact of financial confidence on the gender gap (SCE) 
#---------
source(file.path('empirical', '1_code',"finlit_code05_regresswithfin.R"))
# output in two files, one for BOP and one for SCE


# Figure 4: The gender gap for different levels of financial confidence (BOP-HH)
# Figure F5: The gender gap for different levels of financial confidence (SCE)
#-----------
# the slope of the figure is calculated in finlit_code05_regresswithfin.R
source(file.path('empirical', '1_code',"finlit_code06_finhist.R"))
# finlitfemaleintboppred
# finlitfemaleintscepred


# Figure 5: The gender gap along deciles in the inflation expectations distribution
# Table 14: Quantile regression
## -------
source(file.path('empirical', '1_code',"compsur_code07_quantreg.R"))
# bopfemalequant
# scefemalequant
# mscfemalequant


## Section 6: Robustness Exercises
##################################

# align all three surveys
source(file.path('empirical', '1_code',"compsur_code01_align.R"))


# Table 6: Comparing the gender gap in inflation expectations for singles and non-singles
# --------
source(file.path('empirical', '1_code',"compsur_code02_ggsingle.R"))


# Table 7: Comparing the gender gap in inflation expectations for singles and non-singles
# --------
source(file.path('empirical', '1_code',"compsur_code02_tsgendergap.R"))


## Appendix
############

# Figure B1: Histogram and fitted distribution of inflation expectation point forecasts
# Figure F3: Histogram of inflation expectation point forecasts of men and women
#-----------
source(file.path('empirical', '1_code',"compsur_code08_histogramming.R"))


# Figure D1: Binscatter of interquartile range and predicted financial confidence
#-----------
source(file.path('empirical', '1_code',"finlit_code06_intqrcorr.R"))


# Table 10: The gender gap and demographic controls
# Figure F1: Implied gender gap by demographics
#----------
source(file.path('empirical', '1_code',"compsur_code08_democontrol.R"))


# Table 11: Expectations about specific prices
#----------
source(file.path('empirical', '1_code',"cat_code01_regress.R"))


# Table 13: Higher moments of the gender gap in inflation expectations
#----------
source(file.path('empirical', '1_code',"compsur_code06_gghighermoments.R"))


# Table 15 & 16: Time series regressions
#---------------
source(file.path('empirical', '1_code',"compsur_code03_addquali.R"))
source(file.path('empirical', '1_code',"compsur_code04_tsreg.R"))



# Not included in paper

# Compute table to analyse determinants of being in the tail. Output saved as code for the table.
# source(file.path('empirical', '1_code',"finlit_code04_tail.R"))


