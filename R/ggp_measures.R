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

  # add color groups to the data
  the_groups <- c("RD" = "zero", "RR" = "one", "RR*" = "one", "OR" = "one")
  pos <- match(data$term, names(the_groups), nomatch = 0)
  # only use the effect measure in the plot
  data <- data |>
    filter(.data$term %in% names(the_groups)) |>
    mutate(group = the_groups[pos])


  ggplot(data, aes(x = .data$.estimate, xmin = .data$.lower,
                  xmax = .data$.upper, y = .data$term ,
                  color = .data[["group"]])) +
    ggplot2::geom_vline(xintercept = c(0, 1), color = vline$colors,
                        linetype = vline$linetype, linewidth = vline$size, alpha = vline$alpha) +
    ggplot2::geom_pointrange(aes(color = .data[["group"]]),
                             size = pointrange$size, fatten = pointrange$fatten) +
    ggrepel::geom_text_repel(aes(x = .data$.estimate, y = .data$term,
                                 label = round(.data$.estimate, text$digits)),
                             size = text$size, color = text$color) +
    ggrepel::geom_text_repel(aes(x = .data$.lower, y = .data$term,
                                 label = round(.data$.lower, text$digits)),
                             size = text$size, color = text$color) +
    ggrepel::geom_text_repel(aes(x = .data$.upper, y = .data$term,
                                 label = round(.data$.upper, text$digits)),
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
         x = NULL, y = NULL)
}
