
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

# 
# # Stata to CSV
# # source: empirical/0_data/external/BOP-HH 
# # save them as csv in: empirical/pipeline/out
# source(file.path(code_source,"import_dta.R"))
# 
# setwd(file.path(PROJECT_DIR, PROJECT, code_source))
# 
# # Processing of CSV files in Matlab to save in one .mat file
# # run bopreg_code01_load.m
# system("matlab -nodisplay -r \"run('bopreg_code01_load.m'); exit\"")
# # run bopreg_code02_prepvars.m
# system("matlab -nodisplay -r \"run('bopreg_code02_prepvars.m'); exit\"")
# # run bopreg_code03_compilepanel.m
# system("matlab -nodisplay -r \"run('bopreg_code03_compilepanel.m'); exit\"")

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


