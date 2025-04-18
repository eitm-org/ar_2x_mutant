require(tidyverse)
require(janitor)
require(curl)
#^this package will help u download the files
require(zip)
#^this package will help u unzip the files
require(here)
#^this is a very cool package that helps with filepaths
#but it's only an R thing, other languages u have to setwd()
#here's some info about the here library it is cool: https://here.r-lib.org/
require(readxl)

#' Title
#'
#' @param dropbox_link a link to a dropbox *FOLDER* that you *HAVE ACCESS TO*
#'
#' @returns a link to the local directory the contents of the dropbox folder have been downloaded to
#' @export
#'
#' @examples
#' dropbox_downloader("https://www.dropbox.com/scl/fo/sfi0emihuw9hrrpgga37m/ACcWHePD-ryJIKUMcb1QLho?rlkey=jteaorvj3b929yzeupcqfcdzt&dl=0)
#' ^but you need access to the Speckle_Imaging folder in dropbox for this to work
dropbox_downloader <- function(dropbox_link) {
  #if you don't have a data folder to put your data in, make one right now!
  if (!dir.exists(here("data", "unzipped"))) {
    if (!dir.exists(here("data"))) {
      dir.create(here("data"))
    }
    dir.create(here("data", "unzipped"))
  }
  #dropbox link to  SandboxAQ/Data Related
  #when you copy it, replace the "0" at the end with a "1"
  #1 signals to ur computer to download the docs at the link, not just open it
  #why? i have no idea
  # dropbox_link <- "https://www.dropbox.com/scl/fo/sfi0emihuw9hrrpgga37m/ACcWHePD-ryJIKUMcb1QLho?rlkey=jteaorvj3b929yzeupcqfcdzt&dl=0"
  #if user doesn't replace 0 at end with a 1, i'll do that here
  if (substr(dropbox_link, nchar(dropbox_link), nchar(dropbox_link)) == 0) {
    dropbox_link <- str_replace(dropbox_link, "0$", "1")
  }
  destination_dropbox <- file.path(here("data"), "dropbox_data.zip")
  #download dropbox folder as a zip file
  curl::multi_download(url = dropbox_link, destfile = destination_dropbox)
  #unzip the file
  local_path <- here("data", "unzipped")
  # zip::unzip(zipfile = destination_dropbox, exdir = local_path)
  if (dir.exists(local_path)) {
    system(paste("rm -r", local_path))
  }
  system(paste("unzip", destination_dropbox, "-d", local_path))
  return(local_path)
}

#' Title
#'
#' @param csv filepath to a csv that contains data which may or may not be in plater format
#'
#' @returns bool-- can this csv be read with plater::read_plate() ?
#' @export
#'
#' @examples
#' #filter for only sheets that can be reformatted with the plater function
#' df["plated"] <- unlist(lapply(df$tempname, can_it_plate))
#' df <- df %>%
#'  filter(plated == TRUE)
can_it_plate <- function(csv) {
  tryCatch({
    suppressWarnings(read_plate(csv))
    return(TRUE)
  }, error = function(cond) {
    return(FALSE)
  }, warning = function(cond) {
    return(FALSE)
  })
}

#' Title
#'
#' @param df0 a dataframe that may contain columns with the string "concentration" in their names
#'
#' @returns a dataframe with these columns converted to numeric data type
#' @export
#'
#' @examples
#' plates <- map(df$tempname, read_plate) %>%
#' map(clean_names) %>%
#' #make sure all the concentration columns are numeric
#'  map(make_concs_num)
make_concs_num <- function(df0) {
  numcols <- names(df0)[grepl("concentration", names(df0))]
  #remove any characters from the concentration columns
  df0[numcols] <- sapply(df0[numcols], function(x) as.numeric(str_remove_all(x, "[:alpha:]")))
  return(df0)
}

#' Title
#'
#' @param filepath filepath to a plater-formatted excel file
#'
#' @returns a column-formatted dataframe of all data contained in the excel file (all sheets)
#' @export
#'
#' @examples
#' df <- data.frame(filepath = files) %>%
#' mutate(filename = basename(filepath)) %>%
#'   distinct(filename, .keep_all = TRUE) %>%
#'    mutate(data = map(filepath, xl2plater))
xl2plater <- function(filepath) {
  print(filepath)
  #make temporary directory for csvs
  temp_filepath <- here("data", "temp")
  if (!dir.exists(temp_filepath)) {
    if (!dir.exists(here("data"))) {
      dir.create(here("data"))
    }
    dir.create(temp_filepath)
  }
  #read in all data sheets
  df <- data.frame(sheets = excel_sheets(filepath)) %>%
    mutate(
      data = map(sheets, function(x)
        read_excel(filepath, sheet = x, range = cell_cols("A:M"))),
      tempname = paste0(here(temp_filepath, sheets), ".csv")
    )
  #write to temporary .csv files
  map2(df$data, df$tempname, function(x, y)
    write_csv(x, y))
  #filter for only sheets that can be reformatted with the plater function
  df["plated"] <- unlist(lapply(df$tempname, can_it_plate))
  df <- df %>%
    filter(plated == TRUE)
  #finally, make a dataframe of all plates
  plates <- map(df$tempname, read_plate) %>%
    map(clean_names) %>%
    #make sure all the concentration columns are numeric
    map(make_concs_num)
  plates_df <- bind_rows(plates)
  #delete temporary csv directory
  system(paste("rm -r", temp_filepath))
  return(plates_df)
}


