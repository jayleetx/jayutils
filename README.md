
# jay_utils

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/jayleetx/jayutils.svg?branch=master)](https://travis-ci.org/jayleetx/jayutils)
<!-- badges: end -->

Just a handful of functions that are useful to me! Chances are I won't be removing any of them from here but this isn't a "stable" R package by any means...

## Index of functions

### Plotting helpers

- `ggplot_colors()` displays the hex codes and physical appearance of the standard `ggplot2` color scheme for `n` groups.

### Weighted survey code

- `weighted_tabyl()` is a weighted data version of `janitor::tabyl()`.

- `weighted_crosstab()` is a one-liner for some common crosstab machinations done using `janitor`.

### File writers

- `writer()` writes lines to an output text file.

- `chunk_wrapper()` wraps `writer()` to include the standard R chunk notation and options.

`secret_chunk_wrapper()` is a version of `chunk_wrapper()` with the defaults to fully hide a chunk (`warning`, `message`, `include`, `error`, `echo` are all `FALSE`).

### Negations

- `not.null()` negates `is.null()`.

- `not.na()` negates `is.na()`.

- The infix `%notin%` negates `%in%`.

### Miscellaneous

- `read_batch()` uses `data.table::fread()` to read multiple files in the same folder (or zip archive) in one step.
