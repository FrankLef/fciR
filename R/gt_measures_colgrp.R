#' Create a table of effect-measure modifications with their CI
#'
#' Create a table of effect-measure modifications with their CI.
#'
#' The table will have 3 columns for each of the 2 strata and the
#' difference column. The measure name, the estimates and the
#' confidence interval.
#'
#' @param df Dataframe of results.
#' @param var_grp Name of variable used to group the columns.
#' @param digits Integer, number of digits to the right of the decimal.
#' @param title The title of the table.
#' @param subtitle Subtitle of the table.
#'
#' @importFrom dplyr select rename mutate mutate_if mutate_at
#' @importFrom tidyr unite pivot_wider pivot_longer
#' @importFrom tidyselect all_of everything
#' @importFrom gt cols_label tab_footnote cells_column_labels
#'  tab_spanner_delim cell_borders cells_body px
#' @importFrom rlang .data
#'
#' @return A \code{gt_tbl} object
#' @export
gt_measures_colgrp <- function(df, var_grp = "group",
                               digits = 3,
                               title = "Title", subtitle = "Subtitle") {

  # confidence interval label used in footnote
  ci_label <- sprintf("%.0f%% confidence interval", 100 * (1 - df$.alpha[1]))

  # prepare the dataframe to create the table
  df <- df |>
    select(!matches("alpha|method")) |>
    # mutate(across(.cols = where(is.numeric), .fns = round, digits)) |>
    dplyr::mutate_if(.predicate = is.numeric, .funs = \(x) round(x, digits = digits)) |>
    # mutate(across(.cols = c(".estimate", ".lower", ".upper"), .fns = format)) |>
    mutate_at(.vars = c(".estimate", ".lower", ".upper"), .funs = format) |>
    # mutate(across(.cols = c(".estimate", ".lower", ".upper"), .fns = trimws)) |>
    mutate_at(.vars = c(".estimate", ".lower", ".upper"), .funs = trimws) |>
    dplyr::rename(Estimator = all_of("term"),
           Estimate = all_of(".estimate")) |>
    tidyr::unite(col = "CI", all_of(".lower"), all_of(".upper"), sep = ", ") |>
    mutate(CI = paste0("(", .data$CI, ")")) |>
    mutate(Estimate = as.character(.data[["Estimate"]])) |>
    pivot_longer(cols = c("Estimate", "CI"), names_to = "meas", values_to = "value") |>
    tidyr::unite(col = "heading", all_of(var_grp), all_of("meas"), sep = "_") |>
    pivot_wider(names_from = "heading", values_from = "value")

  gt::gt(df) |>
    gt_basic(title = title, subtitle = subtitle) |>
    tab_spanner_delim(delim = "_", columns = everything()) |>
    tab_style(
      style = cell_borders(sides = "left", color = "grey60",
                           weight = px(1.5), style = "solid"),
      locations = cells_body(columns = matches("Estimate$"))
    ) |>
    tab_style(
      style = cell_borders(sides = "left", color = "grey60",
                           weight = px(1.5), style = "solid"),
      locations = cells_column_labels(columns = matches("Estimate$"))
    ) |>
    tab_footnote(
      footnote = ci_label,
      locations = cells_column_labels(columns = matches("CI$"))
    )
}
