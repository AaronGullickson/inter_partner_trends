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
  "ggrepel", # plotting extras
  "srvyr", # for adjusting by sample design
  "marginaleffects", # for computing log-odds from models
  "mice", # for imputing values
  "modelsummary","gt", # for table output
  "tidycensus", # for statefip names
  "tictoc", "progress", # for tracking processing time
  "parallel", "future", "furrr", # for parallel processing
  "fs" # for file system interaction
)

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})