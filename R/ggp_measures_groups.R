#' Plot of effect measures by group
#'
#' Plot of effect measures by group.
#'
#' Plot of effect measures by group. Usually with different method such as
#' DiD and outcome.  See exercise 7.1 and 7.2 for examples.
#'
#' @param data Data frame with effect measures.
#' @param group Name of group variable.
#' @param title Title.
#' @param subtitle Subtitle.
#' @param vline geom_vline specs.
#' @param pointrange geom_pointrange specs.
#' @param text geom_text_repel specs.
#' @param text_size text size for title and axis as a factor of base font size.
#'
#' @importFrom ggplot2 element_text rel geom_pointrange geom_vline
#'  scale_x_continuous scale_color_brewer position_dodge
#' @importFrom ggrepel geom_text_repel
#'
#' @return ggplot
#' @export
ggp_measures_groups <- function(data, group = "method",
                                title = "Title", subtitle = "Subtitle",
                                vline = list(colors = c("lightseagreen", "violet"),
                                             linetype = "solid", size = 3, alpha = 0.5),
                                pointrange = list(size = 1, fatten = 2),
                                text = list(size = 3, color = "navy", digits = 2),
                                text_size = list(title = 0.9, y_axis = 0.9)) {

  # filter and order the data
  the_names <- c("RD", "RR", "RR*", "OR")
  data <- data[data$name %in% the_names, ]

  ggplot(data,
         aes(x = .data$.estimate, xmin = .data$.lower,
             xmax = .data$.upper,
             y = .data$term, color = .data[[group]])) +
    ggplot2::geom_pointrange(size = pointrange$size, fatten = pointrange$fatten,
                    position = ggplot2::position_dodge(width = 0.75)) +
    ggplot2::geom_vline(xintercept = c(0, 1), color = vline$colors,
               linetype = vline$linetype, size = vline$size, alpha = vline$alpha) +
    ggrepel::geom_text_repel(aes(x = .data$.estimate, y = .data$term,
                                 label = round(.data$.estimate, text$digits)),
                             size = text$size, color = text$color) +
    ggplot2::scale_x_continuous(breaks = c(0, 1)) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggthemes::theme_hc() +
    ggplot2::theme(title = ggplot2::element_text(color = "midnightblue",
                               size = ggplot2::rel(text_size$title)),
          axis.text.y = element_text(color = "navy", face = "bold",
                                     size = ggplot2::rel(text_size$y_axis)),
          legend.position = "bottom",
          legend.title = ggplot2::element_blank()) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL)
}
