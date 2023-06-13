# ------------
# Introduction
# ------------

## Here you can include a brief description of the main purpose of this file.

NAME <- 'import_dta' ## Name of the R file goes here (without the file extension!)
PROJECT <- 'EmpiricalGenderGap'
PROJECT_DIR <- 'D:/Lovisa/Studium/Oxford/Department of Economics/DPhil/' ## Change this to the directory in which your project folder is located, make sure to avoid using single backslashes (i.e. \)!

# ------------
# Preamble
# ------------

## -------
## Imports
## -------
### All the library imports go here

#install.packages('haven')
library(haven)

## --------
## Settings
## --------
### Any settings go here


## ---------------------
## Set working directory
## ---------------------
### The code below will traverse the path upwards until it finds the root folder of the project.

setwd(file.path(PROJECT_DIR,PROJECT))

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

# ---------
# Main code
# ---------

# Reference examples on how to save and load data
#
## -- Load data from 0_data folder --

w01 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave01.dta'))
w02 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave02.dta'))
w03 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave03.dta'))
w04 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave04.dta'))
w05 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave05.dta'))
w06 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave06.dta'))
w07 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave07.dta'))
w08 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave08.dta'))
w09 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave09.dta'))
w10 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave10.dta'))
w11 <- read_dta(file.path('empirical', '0_data', 'external', 'BOP-HH','bophh_suf_202204_v01_wave11.dta'))
w12 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave12.dta'))
w13 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave13.dta'))
w14 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave14.dta'))
w15 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave15.dta'))
w16 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave16.dta'))
w17 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave17.dta'))
w18 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave18.dta'))
w19 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave19.dta'))
w20 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave20.dta'))
w21 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave21.dta'))
w22 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave22.dta'))
w23 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave23.dta'))
w24 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave24.dta'))
w25 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave25.dta'))
w26 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave26.dta'))
w27 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave27.dta'))
w28 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave28.dta'))
w29 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave29.dta'))
w30 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave30.dta'))
w31 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave31.dta'))
w32 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave32.dta'))
w33 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave33.dta'))
#w34 <- read_dta(file.path('empirical', '0_data', 'external','BOP-HH', 'bophh_suf_202204_v01_wave34.dta'))


## -- Save data to pipeline folder -- 

write.csv(w01, file = file.path(pipeline, 'out', 'w01.csv'))
write.csv(w02, file = file.path(pipeline, 'out', 'w02.csv'))
write.csv(w03, file = file.path(pipeline, 'out', 'w03.csv'))
write.csv(w04, file = file.path(pipeline, 'out', 'w04.csv'))
write.csv(w05, file = file.path(pipeline, 'out', 'w05.csv'))
write.csv(w06, file = file.path(pipeline, 'out', 'w06.csv'))
write.csv(w07, file = file.path(pipeline, 'out', 'w07.csv'))
write.csv(w08, file = file.path(pipeline, 'out', 'w08.csv'))
write.csv(w09, file = file.path(pipeline, 'out', 'w09.csv'))
write.csv(w10, file = file.path(pipeline, 'out', 'w10.csv'))
write.csv(w11, file = file.path(pipeline, 'out', 'w11.csv'))
write.csv(w12, file = file.path(pipeline, 'out', 'w12.csv'))
write.csv(w13, file = file.path(pipeline, 'out', 'w13.csv'))
write.csv(w14, file = file.path(pipeline, 'out', 'w14.csv'))
write.csv(w15, file = file.path(pipeline, 'out', 'w15.csv'))
write.csv(w16, file = file.path(pipeline, 'out', 'w16.csv'))
write.csv(w17, file = file.path(pipeline, 'out', 'w17.csv'))
write.csv(w18, file = file.path(pipeline, 'out', 'w18.csv'))
write.csv(w19, file = file.path(pipeline, 'out', 'w19.csv'))
write.csv(w20, file = file.path(pipeline, 'out', 'w20.csv'))
write.csv(w21, file = file.path(pipeline, 'out', 'w21.csv'))
write.csv(w22, file = file.path(pipeline, 'out', 'w22.csv'))
write.csv(w23, file = file.path(pipeline, 'out', 'w23.csv'))
write.csv(w24, file = file.path(pipeline, 'out', 'w24.csv'))
write.csv(w25, file = file.path(pipeline, 'out', 'w25.csv'))
write.csv(w26, file = file.path(pipeline, 'out', 'w26.csv'))
write.csv(w27, file = file.path(pipeline, 'out', 'w27.csv'))
write.csv(w28, file = file.path(pipeline, 'out', 'w28.csv'))
write.csv(w29, file = file.path(pipeline, 'out', 'w29.csv'))
write.csv(w30, file = file.path(pipeline, 'out', 'w30.csv'))
write.csv(w31, file = file.path(pipeline, 'out', 'w31.csv'))
write.csv(w32, file = file.path(pipeline, 'out', 'w32.csv'))
write.csv(w33, file = file.path(pipeline, 'out', 'w33.csv'))
#write.csv(w34, file = file.path(pipeline, 'out', 'w34.csv'))

