## ---- pkg-dependencies ----

# Script dependencies ----
# All packages needed for script
pkg_list = c("tm", "tidyverse", "lubridate", "ggwordcloud", "gghighlight", "knitr", "rmarkdown")
# Determine what packages are NOT installed already.
to_install_pkgs = pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
# Install the missing packages
if(length(to_install_pkgs)) {
  install.packages(to_install_pkgs, repos = "https://cloud.r-project.org")
}

# Load all packages
pkg_loaded = sapply(pkg_list, require, character.only = TRUE)