#' Create a table from the result of simulating doubly robust
#'
#' Create a table from the result of simulating doubly robust.
#'
#' The table shows the result by grouping the ss with separate column groups.
#'
#' @param df Dataframe of measures.
#' @param title The title of the table.
#' @param subtitle Subtitle of the table.
#'
#' @importFrom dplyr %>%
#' @importFrom tidyselect matches everything
#' @importFrom gt gt tab_spanner_delim cols_align tab_style cell_borders
#'  cells_body
#' @importFrom rlang .data
#'
#' @return A \code{gt_tbl} object
#' @export
gt_standdr <- function(df, title = "Title", subtitle = "Subtitle") {

  gt::gt(df) %>%
    gt_basic(title = title, subtitle = subtitle) %>%
    gt::tab_spanner_delim(delim = "_", columns = everything(), split = "first") %>%
    gt::cols_align(
      align = "left",
      columns = c("estimator", "description")
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "left", color = "grey60",
                               weight = px(1.5), style = "solid"),
      locations = gt::cells_body(columns = matches(".*mean$"))
    )
}
