# miscellaneous functions

#' Reads multiple files into one data frame
#'
#' This is essentially a wrapper for \code{data.table::fread}, which will read
#' a batch of files in the same location (directory or zip file).
#'
#' @param dir The folder (or zip file) where the files should be read from
#' @param ... Arguments to pass to \code{data.table::fread}
#'
#' @return A data frame containing all of data, with a column to identify which
#' file a row came from
#' @export
read_batch <- function(dir = NULL,
                       ...) {
  orig_dir <- dir # for a warning later

  if (tools::file_ext(dir) == "zip") {
    utils::unzip(dir, exdir = 'tmp') # unzip the provided zip file
    dir <- here::here('tmp') # set dir to the unzipped location
  } else {
    if (!dir.exists(dir)) dir <- here::here(dir) # if it doesn't exist here, try specifically making it absolute
    if (!dir.exists(dir)) stop(paste0("Directory '", orig_dir, "' not found")) # if it still doesn't exist, break
  }


  files <- list.files(dir, full.names = FALSE)
  names(files) <- files # this makes the name of each list object the file name
  files <- file.path(dir, files)

  data <- lapply(files, data.table::fread, ...) %>% # read the files into a list
    dplyr::bind_rows(.id = "file") # bind them together, attach the name of each object (file) as an id column

  if (tools::file_ext(orig_dir) == "zip") unlink('tmp', recursive = T) # delete unzipped folder

  data
}

#' Not Null
#'
#' @description Negates \code{is.null}.
#'
#' @param x an R object to be tested
#' @return a logical
#' @export
not.null <- function(x) !is.null(x)

#' Not NA
#'
#' @description Negates \code{is.na}.
#'
#' @param x an R object to be tested
#' @return a logical vector
#' @export
not.na <- function(x) !is.na(x)

#' @title Not In
#' @description Negates \code{\%in\%}.
#' @param x the object being tested
#' @param y the vector `x` might be in
#' @return a logical vector
#' @export
`%notin%` <- function(x, y) !(x %in% y)
