#' Shows the default ggplot colors
#'
#' @param n The number of colors needed
#' @return A character vector of the hex codes, and a plot showing their colors
#' @export
ggplot_colors <- function(n) {
  scales::show_col(scales::hue_pal()(n))
  scales::hue_pal()(n)
}
