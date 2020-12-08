#' Survey weighted method for `tabyl`
#'
#' `janitor::tabyl()`, while great, doesn't work with weighted data. This is a
#' start to fixing that; it takes a `tbl_svy` object and does `tabyl` on it
#' (roughly). Currently it only works for 1 or 2 variables, and really does not
#' look like `tabyl`'s internals so be careful.
#'
#' @param data A weighted data object of class `tbl_svy`
#' @param var1 The variable to be tabulated, or the grouping variable
#' @param var2 (Optional) the variable to be tabulated within each `var1` group
#' @param show_na Whether rows/columns that are `NA` should be included
#' @param ... Currently unused
#'
#' @return A `data.frame` containing weighted counts in each one-way or two-way
#' combination of the input variables.
#'
#' @importFrom janitor tabyl
#' @export
tabyl.tbl_svy <- function(data, var1, var2, show_na = TRUE, ...) {
  original <- data
  n <- percent <- valid_percent <- NULL


  if (missing(var2)) {

    if (data %>% srvyr::pull({{ var1 }}) %>% is.numeric()) {
      data <- srvyr::mutate_at(data, dplyr::vars({{ var1 }}),
                        function(x) factor(x,
                                           levels = data %>% srvyr::pull({{ var1 }}) %>% unique() %>% sort(),
                                           ordered = FALSE))
    }

    # detect ordered-ness, grab original levels and un-order for srvyr
    ordered <- data %>% srvyr::pull({{ var1 }}) %>% is.ordered()
    if (ordered) {
      orig_levels <- data %>% srvyr::pull({{ var1 }}) %>% levels()
      data <- srvyr::mutate_at(data, dplyr::vars({{ var1 }}), function(x) factor(x, ordered = FALSE))
    }

    # srvyr to get breakdown
    out <- data %>%
      srvyr::group_by({{var1}}) %>%
      srvyr::summarize(n = srvyr::survey_total(na.rm = TRUE),
                percent = srvyr::survey_mean(na.rm = TRUE)) %>%
      dplyr::select(-dplyr::ends_with("_se"))

    if (ordered) {
      out <- dplyr::mutate_at(out, dplyr::vars({{ var1 }}),
                       function(x) factor(x, levels = orig_levels, ordered = TRUE))
    }

    if (data %>% srvyr::pull({{ var1 }}) %>% is.factor()) {
      out <- tidyr::complete(out, {{var1}}) %>%
        dplyr::mutate_at(dplyr::vars(c("n", "percent")), function(x) tidyr::replace_na(x, 0))
    }

    if (show_na & NA %in% out[[1]]) {
      na_n <- data %>%
        srvyr::summarize(n = srvyr::survey_total({{var1}} %in% c(NA))) %>%
        dplyr::select(-dplyr::ends_with("_se"))

      out <- out %>%
        dplyr::filter(!is.na({{var1}})) %>%
        dplyr::bind_rows(na_n) %>%
        dplyr::transmute({{var1}},
                  n,
                  valid_percent = percent,
                  percent = n / sum(n)) %>%
        dplyr::select(1:2, percent, valid_percent)


    } else {
      out <- out %>%
        dplyr::filter(!is.na({{var1}}))
    }
    attr(out, "tabyl_type") <- "one_way"

  } else {
    orig_levels1 <- data %>% srvyr::pull({{ var1 }}) %>% levels()
    orig_levels2 <- data %>% srvyr::pull({{ var2 }}) %>% levels()
    ordered1 <- data %>% srvyr::pull({{ var2 }}) %>% is.ordered()
    ordered2 <- data %>% srvyr::pull({{ var2 }}) %>% is.ordered()
    if (ordered2) {
      data <- srvyr::mutate_at(data, dplyr::vars({{ var2 }}), function(x) factor(x, ordered = FALSE))
    }


    out <- data %>%
      srvyr::group_by({{var1}}, {{var2}}) %>%
      srvyr::summarize(n = srvyr::survey_total(na.rm = TRUE)) %>%
     dplyr::select(-dplyr::ends_with("_se"))#  %>%
    # pivot_wider(names_from = {{var2}}, values_from = n) %>%
    # mutate_at(vars(2:ncol(.)), function(x) replace_na(x, 0))

    # return full factor levels and flesh out full combinations
    out <- dplyr::mutate_at(out, dplyr::vars({{ var1 }}),
                     function(x) factor(x, levels = orig_levels1, ordered = ordered1)) %>%
      dplyr::mutate_at(dplyr::vars({{ var2 }}),
                function(x) factor(x, levels = orig_levels2, ordered = ordered2))  %>%
      tidyr::complete({{var1}}, {{var2}}) %>%
      dplyr::distinct() %>%
      dplyr::mutate_at(dplyr::vars(c("n")), function(x) tidyr::replace_na(x, 0))

    # deal with NA entries
    if (show_na) {
      # count cases by var2 where var1 is na
      na1 <- dplyr::slice(out, 0)
      if (NA %in% out[[1]]) {
        na1 <- data %>%
          srvyr::group_by({{ var2 }}) %>%
          srvyr::summarize(n = srvyr::survey_total({{var1}} %in% c(NA))) %>%
          dplyr::select(-dplyr::ends_with("_se")) %>%
          dplyr::mutate_at(dplyr::vars({{ var2 }}),
                    function(x) factor(x, levels = orig_levels2, ordered = ordered2))
      }
      # count cases by var1 where var2 is na
      na2 <- dplyr::slice(out, 0)
      if (NA %in% out[[2]]) {
        na2 <- data %>%
          srvyr::group_by({{ var1 }}) %>%
          srvyr::summarize(n = srvyr::survey_total({{var2}} %in% c(NA))) %>%
          dplyr::select(-dplyr::ends_with("_se")) %>%
          dplyr::mutate_at(dplyr::vars({{ var1 }}),
                    function(x) factor(x, levels = orig_levels1, ordered = ordered1))
      }
      # count cases where both are na
      na12 <- dplyr::slice(out, 0)
      if (NA %in% out[[1]] & NA %in% out[[2]]) {
        na2 <- data %>%
          srvyr::summarize(n = srvyr::survey_total({{ var1 }} %in% c(NA) & {{ var2 }} %in% c(NA))) %>%
          dplyr::select(-dplyr::ends_with("_se")) %>%
          dplyr::mutate_at(dplyr::vars({{ var1 }}),
                    function(x) factor(x, levels = orig_levels1, ordered = ordered1)) %>%
          dplyr::mutate_at(dplyr::vars({{ var2 }}),
                    function(x) factor(x, levels = orig_levels2, ordered = ordered2))
      }

      # stick 'em together and attach to out
      nas <- dplyr::bind_rows(na1, na2, na12)

      out <- out %>%
        dplyr::filter(!is.na({{ var1 }}) & !is.na({{ var2 }})) %>%
        dplyr::bind_rows(nas)
    } else {
      out <- out %>%
        dplyr::filter(!is.na({{ var1 }}) & !is.na({{ var2 }}))
    }


    out <- out %>%
      tidyr::pivot_wider(names_from = {{ var2 }}, values_from = n)


  }

  as.data.frame(out)
}

#' A weighted two-way crosstab generator
#'
#' Uses `tabyl.tbl_svy` to create and format a two-way crosstab.
#'
#' @param data A weighted data object of class `tbl_svy`
#' @param var1 The variable to be tabulated, or the grouping variable
#' @param var2 (Optional) the variable to be tabulated within each `var1` group
#' @param show_na Whether rows/columns that are `NA` should be included
#' @param drop_set Which non-NA classes should be dropped
#' @param output Whether the cells should show weighted counts, proportions, or both
#' @param totals Whether totals should be included as a row, column, both, or neither
#' @param pct_denom Which direction the percentages should be calculated on
#' @param round_n How many digits to round counts to
#' @param round_pct How many digits to round percentages to
#'
#' @return A `data.frame` object formatted as specified
#'
#' @importFrom janitor tabyl
#' @export
weighted_crosstab <- function(data, var1, var2,
                              show_na = FALSE,
                              drop_set = c("No response", "Not applicable", "Refuse"),
                              output = c("n", "pct", "both"),
                              totals = c("none", "as row", "as col", "both"),
                              pct_denom = c("row", "col", "all"),
                              round_n = 1,
                              round_pct = 1
) {
  out <- janitor::tabyl(data, {{ var1 }}, {{ var2 }}, show_na = show_na) %>%
    dplyr::select(-dplyr::any_of(drop_set)) %>%
    janitor::adorn_rounding(digits = round_n)

  if (totals[1] == "as row") {
    out <- janitor::adorn_totals(out, where = 'row')
  } else if (totals[1] == "as col") {
    out <- janitor::adorn_totals(out, where = 'col')
  } else if (totals[1] == "both") {
    out <- janitor::adorn_totals(out, where = c('row', 'col'))
  }

  if (output[1] %in% c("pct", "both")) {
    out <- janitor::adorn_percentages(out, denominator = pct_denom[1]) %>%
      janitor::adorn_pct_formatting(digits = round_pct)

    if (output[1] == "both") {
      out <- janitor::adorn_ns(out)
    }
  }

  out
}
