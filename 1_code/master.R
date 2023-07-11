
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


##############
##EXPERIENCE##
##############

## -------
## Table 1
## -------

# investigate the gender gap for single and nonsingle sample in three surveys. Output saved as code for the table.
source(file.path('empirical', '1_code',"compsur_code02_ggsingle.R"))
# note that the code also poduces a plot of the results which can be used for a poster

## -------
## Table 4
## -------

# compute gender gap per period in all surveys and microlevel analysis
source(file.path('empirical', '1_code',"compsur_code02_tsgendergap.R"))
# code saves code_tsmicro in the output folder

## ---------
## Table 2-3
## ---------

# computes balance statistic per perios
source(file.path('empirical', '1_code',"compsur_code03_addquali.R"))
source(file.path('empirical', '1_code',"compsur_code04_tsreg.R"))
# code saves code_tsreg_mean (comparing different surveys) and code_tsreg_msc (analysis of MSC) in the output folder

######################
##Financial Literacy##
######################

## -------
## Table 14
## -------

# prep BOP and SCE 
# produce predicted test scores 
source(file.path('empirical', '1_code',"finlit_code01_fitlit.R"))


## -------
## Table 5
## -------

# Compute table to compare financial literacy for men and women. Output saved as code for the table.
source(file.path('empirical', '1_code',"finlit_code02_ggfinlit.R"))


## -------
## Figure 5
## -------

# Kernel Density plots split by literacy. Output saved as text in pipeline to be used in latex code.
source(file.path('empirical', '1_code',"finlit_code03_finlitdist.R"))
# kdpredbop
# kdpredsce
# kdtestbop
# kdtestsce


## -------
## Table 6
## -------

# Compute table to anal<yse determinants of being in the tail. Output saved as code for the table.
source(file.path('empirical', '1_code',"finlit_code04_tail.R"))


## -------
## Table 7
## -------

# Compute table to show moments of inflexp for men and women. Output saved as code for the table.
source(file.path('empirical', '1_code',"compsur_code06_gghighermoments.R"))


## -------
## Figure 9 (and Table 13)
## -------

# Compute quantile regression. Output saved as code for the table and in pipeline as text file for the figures.
source(file.path('empirical', '1_code',"compsur_code07_quantreg.R"))
# bopfemalequant
# scefemalequant
# mscfemalequant


## -------
## Table 8 and 9
## -------

# Compute table to analyse impact of fin lit on gender gap. Output saved as code for the table.
source(file.path('empirical', '1_code',"finlit_code05_regresswithfin.R"))
# output in two files, one for BOP and one for SCE


## -------
## Table 11
## -------

# Compute histograms for the figures. Output saved as text file in pipeline to be used in latex.
source(file.path('empirical', '1_code',"finlit_code06_finhist.R"))
# finlitfemaleintboppred
# finlitfemaleintscepred
# note that these codes also use the regression coefficients computed for the table above (8)


###################
##Household Roles##
###################

# Note: only BOP

## -------
## Table 10
## -------

# Compute table to check presence of traditional gender norms. Output saved as code for the table.
source(file.path('empirical', '1_code',"hhrole_code01_summary.R"))


## -------
## Table 11
## -------

# Compute table with regression including finlit and experience. Output saved as code for the table.
source(file.path('empirical', '1_code',"hhrole_code02_reg.R"))


