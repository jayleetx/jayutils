# miscellaneous functions
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
