#' Plot of effect measures
#'
#' Plot of effect measures.
#'
#' Plot of effect measures with causal power (CF) and attributable fraction
#' (AF). See secion 3.3, p. 44-45 for more details.
#'
#' @param data Data frame with effect measures.
#' @param title Title.
#' @param subtitle Subtitle.
#' @param vline geom_vline specs.
#' @param pointrange geom_pointrange specs.
#' @param text geom_text_repel specs.
#' @param scale_color scale_color_manual color specs.
#' @param text_size text size for title and axis as a factor of base font size.
#'
#' @importFrom ggplot2 element_text rel
#' @importFrom ggrepel geom_text_repel
#'
#' @return A ggplot object.
#' @export
ggp_measures <- function(data, title = "Title", subtitle = "Subtitle",
                         vline = list(colors = c("lightseagreen", "violet"),
                                      linetype = "solid", size = 3, alpha = 0.5),
                         pointrange = list(size = 1, fatten = 2),
                         text = list(size = 3, color = "navy", digits = 2),
                         scale_color = list(zero = "darkgreen", one = "magenta"),
                         text_size = list(title = 0.9, y_axis = 0.9)) {

  df <- ggp_measures_df(data)

  ggplot(df, aes(x = .data[[".estimate"]], xmin = .data[[".lower"]],
                  xmax = .data[[".upper"]], y = .data[["term"]] ,
                  color = .data[["group"]])) +
    ggplot2::geom_vline(xintercept = c(0, 1), color = vline$colors,
                        linetype = vline$linetype, size = vline$size, alpha = vline$alpha) +
    ggplot2::geom_pointrange(aes(color = .data[["group"]]),
                             size = pointrange$size, fatten = pointrange$fatten) +
    ggrepel::geom_text_repel(aes(x = .data[[".estimate"]], y = .data[["term"]],
                                 label = round(.data[[".estimate"]], text$digits)),
                             size = text$size, color = text$color) +
    ggrepel::geom_text_repel(aes(x = .data[[".lower"]], y = .data[["term"]],
                                 label = round(.data[[".lower"]], text$digits)),
                             size = text$size, color = text$color) +
    ggrepel::geom_text_repel(aes(x = .data[[".upper"]], y = .data[["term"]],
                                 label = round(.data[[".upper"]], text$digits)),
                             size = text$size, color = text$color) +
    ggplot2::scale_x_continuous(breaks = c(0, 1)) +
    ggplot2::scale_color_manual(values = c("zero" = scale_color$zero,
                                           "one" = scale_color$one),
                       guide = "none") +
    ggthemes::theme_hc() +
    theme(title = ggplot2::element_text(color = "midnightblue",
                                        size = ggplot2::rel(text_size$title)),
          axis.text.y =
            ggplot2::element_text(color = "navy", face = "bold",
                                  size = ggplot2::rel(text_size$y_axis))) +
    labs(title = title,
         subtitle = subtitle,
         caption = "\U2020 Assuming the treatment cannot harm anyone.",
         x = NULL, y = NULL)
}

#' Create the data fame for \code{ggp_measures}
#'
#' @param data Data frame to plot.
#'
#' @importFrom rlang .data abort format_error_bullets
#' @importFrom dplyr bind_rows
#'
#' @seealso ggp_measures
#'
#' @return Data frame.
ggp_measures_df <- function(data) {
  the_names <- c("RD", "RR", "RR*", "OR")
  the_groups <- c("RD" = "zero", "RR" = "one", "RR*" = "one",
                  "OR" = "one", "AF" = "zero", "CP" = "zero")

  check <- sum(the_names %in% data$term)
  if (check != length(the_names)) {
    msg_head <- cli::col_yellow("All 4 effect measures must be included.")
    msg_body <- c("x" = sprintf("Number of effect measures: %d", check))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ggp_measures_df_error")
  }

  # filter and order the data
  pos <- match(the_names, data$term, nomatch = 0L)
  df1 <- data[pos, ]

  conf <- 1 - df1$.alpha[1]

  df2 <- data.frame(
    term = c("AF\U2020", "CP\U2020"),
    .estimate = c(1 - 1 / df1$.estimate[df1$term == "RR"],
            1 - 1 / df1$.estimate[df1$term == "RR*"]),
    conf = conf,
    .lower = c(1 - 1 / df1$.lower[df1$term == "RR"],
            1 - 1 / df1$.lower[df1$term == "RR*"]),
    .upper = c(1 - 1 / df1$.upper[df1$term == "RR"],
            1 - 1 / df1$.upper[df1$term == "RR*"]))
  df <- bind_rows(df1, df2) %>%
    mutate(group = the_groups)
  df
}
