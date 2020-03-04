#' Write lines of text to a file
#'
#' @param x The first piece of text to write
#' @param ... Further text to write
#' @param outfile The file to write to
#' @param sep Character to separate arguments of \code{...}, defaults to new line
#' @export
writer <- function(x, ..., outfile = NULL, sep = "\n") {
  cat(paste0(x, ...), file=outfile, append=TRUE, sep=sep)
}

#' @describeIn writer Write an R chunk to a file
#'
#' This is a wrapper for \code{writer} that formats the text as an R chunk
#' @param options Character vector detailing the R chunk options
#' @param label A name to give the R chunk
#' @export
chunk_wrapper <- function(x, ..., outfile = NULL,
                          options=c("echo=FALSE", "warning=FALSE"), label=NULL) {
  writer(paste0("```{r", ifelse(is.null(label), ", ", paste0(" ", label, ", ")),
                paste0(options, collapse=", "), "}"),
         outfile = outfile)
  writer(paste0(x, "\n", paste(..., sep = "\n")), outfile = outfile)
  writer("```\n", outfile = outfile)
}

#' @describeIn writer Write a secret R chunk to a file
#'
#' Wrapper for \code{chunk_wrapper} with specific options set to hide output
#' @export
secret_chunk_wrapper <- function(x, ..., outfile = NULL,
                                 options=c("echo=FALSE", "include=FALSE",
                                           "warning=FALSE", "message=FALSE",
                                           "error=FALSE"), label=NULL) {
  chunk_wrapper(x, ..., outfile=outfile, options=options, label=label)
}
