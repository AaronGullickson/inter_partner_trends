## check_packages.R

#Run this script to check for packages that the other R scripts will use. If missing, try to install.
#code borrowed from here:
#http://www.vikram-baliga.com/blog/2015/7/19/a-hassle-free-way-to-verify-that-r-packages-are-installed-and-loaded

#add new packages to the chain here
packages = c(
  "here", # absolute requirement always
  "knitr", # for processing quarto
  "readr","haven", "googlesheets4", # I/O
  "tidyverse","lubridate","broom", #tidyverse and friends
  "tidymodels", "poissonreg", # for tidyd modeling
  "ggrepel", # plotting extras
  "marginaleffects", # for computing log-odds from models
  "modelsummary","gt", # for table output
  "tidycensus", # for statefip names
  "MoMAColors" # for fun
)

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})