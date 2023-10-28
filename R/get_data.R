#' File Reader
#'
#' Finds a file in the current folder (`getwd()`) and all immediate subfolders
#' matching the string given (no extension necessary). If a file is found, it
#' is read according to its extension; if multiple files matching the string
#' are found, a warning is given and the first matching file found is read and
#' returned. If no matching file is found, and empty tibble is returned.
#'
#' @param file_name a string to be matched exactly (with or without file
#' extension)
#' @param folder_ the top-level folder in which to start searching for the file
#' @param ... currently ignored
#'
#' @importFrom dplyr tibble select
#' @importFrom data.table fread
#' @importFrom readxl read_excel
#' @importFrom haven read_dta
#' @importFrom tools file_ext
#'
#' @return a tibble with the data read from the file matching the string
#' expression given
#'
#' @examples None
#'
#' @export
get_data <- function(file_name, ..., folder_ = NA) {

  if (is.na(folder_)) {
    folder <- "."
  } else {
    folder <- folder_
  }

  top_files_available <- file_name %>%
    sprintf("%s/%s*", folder, .) %>%
    Sys.glob()

  sub_files_available <- file_name %>%
    sprintf("%s/*/%s*", folder, .) %>%
    Sys.glob()

  top_files <- length(top_files_available)
  sub_files <- length(sub_files_available)

  if ((top_files == 0) && (sub_files == 0)) {

    "No files found in %s matching '%s'. Empty table returned." %>%
      sprintf(., folder, file_name) %>%
      warning(call. = FALSE)

    return(tibble())
  }

  if (top_files > 0) {

    if (top_files > 1) {
      "Multiple matching files found: %s. The first will be chosen." %>%
        sprintf(., top_files_available) %>%
        warning(call. = FALSE)
    }

    the_final <- top_files_available[1]

  } else if (length(sub_files_available) > 0) {

    if (sub_files > 1) {
      "Multiple matching files found: %s. The first will be chosen." %>%
        sprintf(., sub_files_available) %>%
        warning(call. = FALSE)
    }

    the_final <- sub_files_available[1]
  }

  extension <- file_ext(the_final)

  return({
    if (extension == "csv") {
      fread(the_final)
    } else if (grepl("xl", extension)) {
      read_excel(the_final)
    } else if (extension == "dta") {
      read_dta(the_final)
    } else if (extension == "rds") {
      readDRS(the_final)
    } else {
      gsub(" +", " ", paste(
          "'.%s' is not a recognised data file extension. Either change",
          " the file request '%s', or add the extension '.%s' to the",
          "`get_data` function. Empty frame returned.",
          sep=" "
        ) %>%
        sprintf(extension, file_name, extension, folder) %>%
        warning(call. = FALSE))
      tibble() # the return
    }
  } %>%
    tibble()
  )
}

#' Alias for `get_data`
#' @export
get_table <- function(file_name, ..., folder_ = NA) {
  return( get_data(file_name, ..., folder_ = folder_) )
}