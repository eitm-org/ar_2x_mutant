library(testthat)
library(here)
library(tidyverse)

source(here("R", "functions.R"))

#copy data from dropbox
data_path <- dropbox_downloader("https://www.dropbox.com/scl/fo/sfi0emihuw9hrrpgga37m/ACcWHePD-ryJIKUMcb1QLho?rlkey=jteaorvj3b929yzeupcqfcdzt&dl=0")
#filter for 2xmutant files only
files <- list.files(data_path, recursive = TRUE)
files <- files[grepl("2x", files, ignore.case = TRUE) & grepl("analysis request forms", files, ignore.case = TRUE)]
#convert to dataframe
df <- data.frame(filepath = files) %>%
  mutate(filename = basename(filepath)) %>%
  #remove any files that might appear in multiple folders
  distinct(filename)
asdf