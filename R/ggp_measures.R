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
#'
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 element_text
#' @importFrom ggrepel geom_text_repel
#'
#' @return A ggplot object.•
#' @export
ggp_measures <- function(data, title = "Title", subtitle = "Subtitle") {
  the_names <- c("RD", "RR", "RR*", "OR")
  stopifnot(all(the_names %in% data$name))

  conf <- data$conf[1]

  pdf1 <- data %>%
    filter(.data[["name"]] %in% the_names)
  pdf2 <- data.frame(
    name = c("AF\U2020", "CP\U2020"),
    est = c(1 - 1 / data$est[data$name == "RR"],
            1 - 1 / data$est[data$name == "RR*"]),
    conf = conf,
    lci = c(1 - 1 / data$lci[data$name == "RR"],
            1 - 1 / data$lci[data$name == "RR*"]),
    uci = c(1 - 1 / data$uci[data$name == "RR"],
            1 - 1 / data$uci[data$name == "RR*"]))
  pdf <- bind_rows(pdf1, pdf2) %>%
    mutate(group = c("zero", "one", "one", "one", "zero", "zero"))

  ggplot(pdf, aes(x = .data[["est"]], xmin = .data[["lci"]],
                  xmax = .data[["uci"]], y = .data[["name"]] ,
                  color = .data[["group"]])) +
    ggplot2::geom_pointrange(aes(color = .data[["group"]]), fatten = 3, size = 1) +
    ggplot2::geom_vline(xintercept = c(0, 1), color = c("lightseagreen", "violet"),
               linetype = "longdash", size = 1) +
    ggrepel::geom_text_repel(aes(x = .data[["est"]], y = .data[["name"]],
                                 label = round(.data[["est"]], 2)),
                             size = 4, color = "navy") +
    ggrepel::geom_text_repel(aes(x = .data[["lci"]], y = .data[["name"]],
                                 label = round(.data[["lci"]], 2)),
                             size = 4, color = "navy") +
    ggrepel::geom_text_repel(aes(x = .data[["uci"]], y = .data[["name"]],
                                 label = round(.data[["uci"]], 2)),
                             size = 4, color = "navy") +
    ggplot2::scale_x_continuous(breaks = c(0, 1)) +
    ggplot2::scale_color_manual(values = c("zero" = "darkgreen", "one" = "magenta"),
                       guide = "none") +
    ggthemes::theme_hc() +
    theme(title = ggplot2::element_text(color = "midnightblue")) +
    labs(title = title,
         subtitle = subtitle,
         caption = "\U2020 Assuming the treatment cannot harm anyone.",
         x = NULL, y = NULL)
}
