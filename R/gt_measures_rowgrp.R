#' Create a table of effect measures with their CI
#'
#' Create a table of effect measures with their CI.
#'
#' The table will have 3 columns. The measure name, the estimates and the
#' confidence interval.
#'
#' @param df Dataframe of results.
#' @param rowgroup Name of column with row group names.
#' @param rowname Name of column with row names.
#' @param digits Integer, number of digits to the right of the decimal.
#' @param title The title of the table.
#' @param subtitle Subtitle of the table.
#'
#' @importFrom dplyr select rename mutate mutate_if mutate_at
#' @importFrom tidyr unite
#' @importFrom gt cols_label tab_footnote tab_options
#' @importFrom rlang .data
#'
#' @return A \code{gt_tbl} object
#' @export
gt_measures_rowgrp <- function(df, rowgroup = "term", rowname = "model",
                               digits = 3,
                               title = "Title", subtitle = "Subtitle") {

  # confidence interval label used in footnote
  ci_label <- sprintf("%.0f%% confidence interval", 100 * (1 - df$.alpha[1]))

  df <- df %>%
    select(!matches("alpha|method")) %>%
    # mutate(across(.cols = where(is.numeric), .fns = round, digits)) %>%
    dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = digits) %>%
    unite(col = "CI", ".lower", ".upper", sep = ", ") %>%
    mutate(CI = paste0("(", .data$CI, ")"))


  gt::gt(df, rowname_col = rowname, groupname_col = rowgroup) %>%
    gt_basic(title, subtitle) %>%
    cols_label(
      .estimate = "Estimate") %>%
    tab_footnote(
      footnote = ci_label,
      locations = cells_column_labels(columns = matches("CI$"))
    ) %>%
    tab_options(
      row_group.font.weight = "bold"
    )
}
