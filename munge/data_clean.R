library(testthat)
library(here)
library(tidyverse)
library(plater)

source(here("R", "functions.R"))

#filter for 2xmutant files only
files <- list.files(here("data", "unzipped"),
                    recursive = TRUE,
                    full.names = TRUE)
#convert to dataframe
df <- data.frame(filepath = files) %>%
  mutate(filename = basename(filepath)) %>%
  filter(grepl("2x", filename) & grepl("analysis request forms", filepath) & !grepl("//~$", filename)) %>%
  #remove any files that might appear in multiple folders
  distinct(filename, .keep_all = TRUE) %>%
  mutate(data = map(filepath, xl2plater)) %>%
  unnest(data)
