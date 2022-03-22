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
#' @param digits Number of digits to round off.
#'
#' @importFrom ggplot2 element_text
#' @importFrom ggrepel geom_text_repel
#'
#' @return A ggplot object.
#' @export
ggp_measures <- function(data, title = "Title", subtitle = "Subtitle",
                         digits = 2) {

  df <- ggp_measures_df(data)
  conf <- df$conf[1]

  ggplot(df, aes(x = .data[["est"]], xmin = .data[["lci"]],
                  xmax = .data[["uci"]], y = .data[["name"]] ,
                  color = .data[["group"]])) +
    ggplot2::geom_vline(xintercept = c(0, 1), color = c("lightseagreen", "violet"),
               linetype = "solid", size = 4, alpha = 0.5) +
    ggplot2::geom_pointrange(aes(color = .data[["group"]]), fatten = 3, size = 1) +
    ggrepel::geom_text_repel(aes(x = .data[["est"]], y = .data[["name"]],
                                 label = round(.data[["est"]], digits)),
                             size = 4, color = "navy") +
    ggrepel::geom_text_repel(aes(x = .data[["lci"]], y = .data[["name"]],
                                 label = round(.data[["lci"]], digits)),
                             size = 4, color = "navy") +
    ggrepel::geom_text_repel(aes(x = .data[["uci"]], y = .data[["name"]],
                                 label = round(.data[["uci"]], digits)),
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

#' Create the data fame for \code{ggp_measures}
#'
#' @param data Data frame to plot.
#'
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows
#'
#' @seealso ggp_measures
#'
#' @return Data frame.
ggp_measures_df <- function(data) {
  the_names <- c("RD", "RR", "RR*", "OR")
  stopifnot(all(the_names %in% data$name))

  # filter and order the data
  pos <- match(the_names, data$name, nomatch = 0L)
  df1 <- data[pos, ]

  conf <- df1$conf[1]

  # pdf1 <- data %>%
  #   filter(.data[["name"]] %in% the_names)
  df2 <- data.frame(
    name = c("AF\U2020", "CP\U2020"),
    est = c(1 - 1 / df1$est[df1$name == "RR"],
            1 - 1 / df1$est[df1$name == "RR*"]),
    conf = conf,
    lci = c(1 - 1 / df1$lci[df1$name == "RR"],
            1 - 1 / df1$lci[df1$name == "RR*"]),
    uci = c(1 - 1 / df1$uci[df1$name == "RR"],
            1 - 1 / df1$uci[df1$name == "RR*"]))
  df <- bind_rows(df1, df2) %>%
    mutate(group = c("zero", "one", "one", "one", "zero", "zero"))
  df
}
