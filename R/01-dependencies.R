## ---- pkg-dependencies ----

# Script dependencies ----
# The packages these scripts actually call, rather than the tidyverse
# meta-package: it attaches nine and depends on around a hundred, where the
# figures and the word cloud need five of them.
pkg_list = c("tm", "ggplot2", "dplyr", "readr", "stringr", "lubridate",
             "ggwordcloud", "gghighlight", "knitr", "rmarkdown")
# Determine what packages are NOT installed already.
to_install_pkgs = pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
# Install the missing packages
if(length(to_install_pkgs)) {
  install.packages(to_install_pkgs, repos = "https://cloud.r-project.org")
}

# Load all packages
pkg_loaded = sapply(pkg_list, require, character.only = TRUE)