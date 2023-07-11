# ------------
# Introduction
# ------------

## Coverts different inflation expectation surveys in a common format

rm(list=ls())
NAME <- 'posterior_lognormal' ## Name of the R file goes here (without the file extension!)
PROJECT <- 'EmpiricalGenderGap'
PROJECT_DIR <- 'D:/Lovisa/Studium/Oxford/Department of Economics/DPhil' ## Change this to the directory in which your project folder is located, make sure to avoid using single backslashes (i.e. \)!

# ------------
# Preamble
# ------------

## -------
## Imports
## -------
### All the library imports go here

#install (if not already installed) and load ggplot2
if(!(require(ggplot2))){install.packages('ggplot2')}

library('tidyverse')
library('ggplot2')


## --------
## Settings
## --------
### Any settings go here

# experience:
epsilon <- 0

# financial literacy
delta <- 0.3

# prior variables
mu0 <- 1*5-2-4*log(2)
sigma0 <- 2 + delta

# signal
sigmas <- 1 + epsilon
mus <- log(2) - 0.5*sigmas^2


## ---------------------
## Set working directory
## ---------------------
### The code below will traverse the path upwards until it finds the root folder of the project.

setwd(file.path(PROJECT_DIR, PROJECT))

## ----------------------------------
## Set  up pipeline folder if missing
## ----------------------------------
### The code below will automatically create a pipeline folder for this code file if it does not exist.

if (dir.exists(file.path('empirical', '2_pipeline'))){
  pipeline <- file.path('empirical', '2_pipeline', NAME)
} else {
  pipeline <- file.path('2_pipeline',NAME)
}

if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  for (folder in c('out', 'store', 'tmp')){
    dir.create(file.path(pipeline, folder))
  }
}

## Add subfolders

if (!dir.exists(file.path(pipeline,'out'))) {
  dir.create(file.path(pipeline,'out'))
}


# ---------
# Functions
# ---------

# ---------
# Main code
# ---------


# compute parameters of the posterior distribution

sigmah <- sqrt(sigmas^2*sigma0^2/(sigmas^2 + sigma0^2))

muh <- (mu0*sigmas^2 + log(2)*sigma0^2 + 0.5*sigmas^2*sigma0^2)/(sigmas^2 + sigma0^2)


# Generate x values
x <- seq(0, 30, length.out = 300)

# Calculate the density values for each distribution
prior_density <- dlnorm(x, meanlog = mu0, sdlog = sigma0^2)
signal_density <- dlnorm(x, meanlog = mus, sdlog = sigmas^2)
posterior_density <- dlnorm(x, meanlog = muh, sdlog = sigmah^2)


# Calculate the means of each distribution
mean_prior <- exp(mu0 + (sigma0^2 / 2))
mean_signal <- exp(mus + (sigmas^2 / 2))
mean_posterior <- exp(muh + (sigmah^2 / 2))


# Create a data frame with x values and density values
df <- data.frame(x = x, prior = prior_density, signal = signal_density, posterior = posterior_density)

# Melt the data frame to long format
df_long <- reshape2::melt(df, id.vars = "x", variable.name = "distribution", value.name = "density")

# Create the overlay plot
overlay_plot <- ggplot(df_long, aes(x = x, y = density, color = distribution)) +
  geom_line(size = 1.2) +
  geom_vline(aes(xintercept = mean_prior), linetype = "dotted", color = "#F2D383", size = 1 ) +
  geom_vline(aes(xintercept = mean_signal), linetype = "dotted", color = "#A1C4D0", size = 1 ) +
  geom_vline(aes(xintercept = mean_posterior), linetype = "dotted", color = "#002147", size = 1 ) +
  labs(x = "x", y = "Density") +
  scale_color_manual(values = c("#F2D383", "#A1C4D0", "#020047")) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "horizontal", panel.border = element_blank())


# Show the overlay plot
print(overlay_plot)

# Save the overlay plot as a JPG file with specified dimensions
ggsave(file.path(pipeline,'out', "overlay_plot_delta.jpg"), plot = overlay_plot, width = 9, height = 6.88, units = "cm")
