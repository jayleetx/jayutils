#seq2 <- Vectorize(seq.default, vectorize.args = c("from", "to"), SIMPLIFY = FALSE)
#mat_split <- function(x) split(x, rep(seq_len(ncol(x)), each = nrow(x)))

#expand_basic <- function(x) {
#  start <- stringr::str_extract(x, "^\\d+")
#  end <- stringr::str_extract(x, "\\d+$")
#  seq2(start,end)
#}


#' Expands a range of integers into all the integers it contains
#'
#' @param x A character vector containing condensed ranges of numbers;
#' "a-b" or "a - b".
#' @param sep The character separating the integers for output. Defaults to `NULL`,
#' which will return a list of integer vectors.
#' @param replace Controls if the expanded range is extracted into a list (`FALSE`) or replaced
#' within the strings (`TRUE)`)
#' @return Either a list of character vectors (if `replace == FALSE`) or a character vector
#' similar to the input vector.
#' @examples
#' `expand_numeric("Precincts 1-5")`
#' `expand_numeric("Precincts 1-5,7-9", replace = TRUE)`
#'
#' @importFrom magrittr %>%
#' @export

expand_numeric <- function(x, sep = NULL, replace = FALSE) {
  if(is.null(sep) & isTRUE(replace)) stop("If `sep` is set to `NULL`, there can be no string replacement.")
#  sep2 <- ifelse(stringr::str_detect(sep, "-"), " andzzz ", sep)
  q <- x

  paste_symbol <- function(string) paste(string, collapse = sep)

  expand_basic <- function(x) {
    start <- stringr::str_extract(x, "^\\d+")
    end <- stringr::str_extract(x, "\\d+$")
    paste_symbol(seq(start,end))
  }


#  nums <- stringr::str_extract_all(q, "\\d+ ?- ?\\d+") %>%
#    lapply(expand_basic) %>%
#    rapply(paste0, how = "replace", collapse = sep2)
  if(replace == TRUE) {
    gsubfn::gsubfn(pattern = "\\d+ ?- ?\\d+", replacement = expand_basic, x = q)
  } else if (replace == FALSE) {
    gsubfn::strapply(pattern = "\\d+ ?- ?\\d+", FUN = expand_basic, X = q)
  } else stop("`replace` must be a logical element (TRUE/FALSE)")



#  while(any(stringr::str_detect(q, "\\d+ ?- ?\\d+"))) {
#    i <- 1 + 1
#    if(i >= 20) break
#    # catch digits, optional space, and dash; remove dash
#    start <- sapply(stringr::str_extract_all(q, "\\d+ ?-"), stringr::str_remove, "-")
#    # catch dash, optional space, and digits; remove dash
#    end <- sapply(stringr::str_extract_all(q, "- ?\\d+"), stringr::str_remove, "-")
#
#    y <- Map(seq2, start, end) %>%
#      lapply(mat_split) %>%
#      unlist(y, recursive = FALSE) %>%
#      sapply(paste, collapse = sep2)
#
#
#    if(replace == TRUE) {
#      q <- stringr::str_replace_all(q, "\\d+-\\d+", y)
#    } else q <- y
#  }
#  if(stringr::str_detect(sep, "-")) q <- stringr::str_replace_all(q, " andzzz ", sep)
#  q
}



