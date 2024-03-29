#' Create a table of effect measures with their CI
#'
#' Create a table of effect measures with their CI.
#'
#' The table will have 3 columns. The measure name, the estimates and the
#' confidence interval.
#'
#' @param df Dataframe of measures.
#' @param digits Integer, number of digits to the right of the decimal.
#' @param title The title of the table.
#' @param subtitle Subtitle of the table.
#'
#' @importFrom dplyr select mutate
#' @importFrom tidyr unite
#' @importFrom gt cols_label tab_footnote cells_column_labels
#' @importFrom rlang .data
#'
#' @return A \code{gt_tbl} object
#' @export
gt_measures <- function(df, digits = 3,
                        title = "Title", subtitle = "Subtitle") {

  # confidence interval label used in footnote
  ci_label <- sprintf("%.0f%% confidence interval", 100 * (1 - df$.alpha[1]))

  df <- df %>%
    dplyr::select(c("term", ".estimate", ".lower", ".upper")) %>%
    # mutate(across(.cols = where(is.numeric), .fns = round, digits = digits)) %>%
    dplyr::mutate_if(.predicate = is.numeric, .funs = round, digits = digits) %>%
    tidyr::unite(col = "CI", ".lower", ".upper", sep = ", ") %>%
    dplyr::mutate(CI = paste0("(", .data$CI, ")"))


  gt::gt(df) %>%
    gt_basic(title, subtitle) %>%
    gt::cols_label(
      term = "Measure",
      .estimate = "Estimate") %>%
    gt::tab_footnote(
      footnote = ci_label,
      locations = gt::cells_column_labels(columns = matches("CI$"))
    )
}
