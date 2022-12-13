# Codes

This folder contains all codes used. There are two pathways to follow:

## 1. Follow original Matlab Codes

### code01_load
This file imports the csv files of all waves specified in w and transfers them into a structure with one table for each wave.
Structures are chosen such that the code can only be run for new waves as updates follow.

### code02_prepvars
This file prepares all used variables such that they can be used in a regression. 
In the first part, some variables are dealth with individually. It is advised to run these codes only once per variable and wave. Note: this section of the code takes about 24h to run (if all waves are used). For updates: run this only for new waves to sava time.
The main section can be run several times and runs fairly quick.

### code03_compilepanel
This file compiles the final panel in one table. It also removes variables which have a lot of missing values.
We can run this code for a range of compositions. These have to be chosen under t.

### code04_ols
This file runs the first regression, the OLS regression.
Here we run a "normal" OLS without time fixed effects and a pooled OLS with time effects to account for the panel structure.
Assumptions OLS: No heteroskedasticity, random effects, no time fixed effects
Assumptions PO: Random effects and predeterminedness.

### code05_randomeffects
This file runs the Random Effects regression and performs the Beusch Pagan LM test.
Assumptions: Random effects and strict exogeneity.
Beusch Pagan Test has null of no random effects

### code06_hausmantaylor
Hausman Taylor estimation. The sample is split into 4 groups, time varying and time invariant exogenous and endogenous variables. 
We use fixed effects estimation to estimate all time varying (to control for endogeneity) and the apply an IV approach to recover the time invariant endogenous.
Assumptions: Fixed effects and strict exogeneity.

### WIP
code07_iv and code08_pesaranols are work in progress.

## 2. Follow new estimation in R

### Setup
Use the first three files in Matlab as indicated above.

### code04_regress
This file computes the regressions for the cleaned unbalanced dataset.
#### Settings: 
- choose combination of included vars (base, interactions, etc) in t          
- choose if model runs on logs or level
#### Estimates: 
- OLS (including a linear model and a least squared dummy variable model)
- Variable Coefficients Model (allow coefficients to vary between individuals or time periods)
- Between Estimator (OLS on time averaged)
- Random effects estimator (uses walhus method)
- Feasible GLS estimator 
- Hausman Taylor type estimators
- Including between effects as controls

### code05_compest
Create a latex file to compare the estimators.
#### Settings: 
- choose combination of vars (typically base)
- level or logs
#### Output files: 
- summary statistics 
- comparison of y.OLS, y.PO, y.RE, y.FEt, y.LSDVt_control
#### Note:
Some variables are not shown in final table. This needs to be specified in the code.

