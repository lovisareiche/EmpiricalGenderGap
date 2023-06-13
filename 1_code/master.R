
# Master code for "That's what she said"
# ----------------------------------------


PROJECT <- 'EmpiricalGenderGap'
PROJECT_DIR <- 'D:/Lovisa/Studium/Oxford/Department of Economics/DPhil' ## Change this to the directory in which your project folder is located, make sure to avoid using single backslashes (i.e. \)!

setwd(file.path(PROJECT_DIR, PROJECT))

code_source <- file.path('empirical', '1_code')

## Start to load all relevant data sources

# Stata to CSV
# source: empirical/0_data/external/BOP-HH 
# save them as csv in: empirical/pipeline/out
source(file.path(code_source,"import_dta.R"))

# Processing of CSV files in Matlab to save in one .mat file
# run bopreg_code01_load.m
system("matlab -nodisplay -r \"run('~/empirical/1_code/bopreg_code01_load.m'); exit\"")
# run bopreg_code02_prepvars.m
system("matlab -nodisplay -r \"run('~/empirical/1_code/bopreg_code02_prepvars.m'); exit\"")
# run bopreg_code03_compilepanel.m
system("matlab -nodisplay -r \"run('~/empirical/1_code/bopreg_code03_compilepanel.m'); exit\"")



