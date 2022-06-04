#' Create a plot of effect-measures modifications
#'
#' Create a plot of effect-measures modifications
#'
#' Create a plot of effect-measures modifications. The measures are shoen
#' as solid lines. The actual effects are shown in dashed lines.
#'
#' @param df Dataframe to plot.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#'
#' @importFrom dplyr %>% filter
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_text
#'  scale_linetype_manual theme_minimal theme labs element_blank
#' @importFrom rlang .data
#'
#' @return A ggplot object.
#' @export
ggp_measures_modif <- function(df, title = "Change in effect measures",
                               subtitle = "Effect-Measure Modification and Statistical Interaction") {

  df <- df %>%
    filter(.data[["group"]] != "diff")

  ggplot(df,
         aes(x = .data[["group"]], y = .data[[".estimate"]],
             color = .data[["term"]], linetype = .data[["term"]],
             group = .data[["term"]])) +
    geom_line(size = 1) +
    geom_point(mapping = aes(fill = .data[["term"]]), size = 3, shape = 21) +
    geom_text(mapping = aes(label = round(.data[[".estimate"]], 1)),
              nudge_x = 0.05, nudge_y = 0.05) +
    scale_linetype_manual(values = c("EYT0" = "dashed", "EYT1" = "dashed",
                                     "OR" = "solid", "RD" = "solid",
                                     "RR" = "solid", "RR*" = "solid")) +
    theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_blank()) +
    labs(title = title, subtitle = subtitle, y = "estimated expected value")
}
