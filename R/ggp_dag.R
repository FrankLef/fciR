#' Plot a DAG with ggplot and node names with subscripts
#'
#' Plot a DAG with ggplot and node names with subscripts.
#'
#' The DAG will be plotted using \code{ggplot2} and the nodes' names can have
#' subscript as in mathematical notations. See the \code{source} below for
#' more details.
#'
#' @param dag dag object
#' @param layout Name of layout, default is "sugiyama".
#' @param text_labels If NULL (default) the nodes' names are used as is. If
#' a vector of text labels is given they MUST BE IN ALPHABETICAL ORDER.
#' @param text_size Integer. Size of the nodes' name.
#' @param box_padding Box padding for \code{geom_dag_label_repel}
#' @param legend_pos Legend position of ggplot.
#'
#' @source https://www.r-bloggers.com/2019/10/how-to-use-math-symbols-with-ggdag-2/
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% arrange pull
#' @importFrom ggdag tidy_dagitty ggdag_status geom_dag_point theme_dag_blank
#'  geom_dag_text geom_dag_label_repel
#' @importFrom dagitty exposures outcomes latents
#' @importFrom ggplot2 scale_color_manual theme element_rect
#'
#' @return ggplot object
#' @export
ggp_dag <- function(dag, layout = "sugiyama", text_labels = NULL, text_size = 7,
                    box_padding = 1, legend_pos = "bottom") {

  # the default colors
  bg_colr <- "snow"
  label_colr <- "oldlace"
  text_colr <- "midnightblue"
  status_colrs <- c("latent" = "snow3", "exposure" = "burlywood", "outcome" = "aquamarine")

  # Check if any variable was given a status
  is_status <- any(length(exposures(dag)) > 0, length(outcomes(dag)) > 0,
                   length(latents(dag)) > 0)
  if (is_status) {
    status_guide <- "legend"
  } else {
    status_guide <- "none"
  }

  # create the dagitty object
  out <- dag %>%
    tidy_dagitty(layout = layout) %>%
    arrange(.data[["name"]])  # must sort by name to use mathematical expressions

  # get the names of the variables
  the_names <- out$data %>%
    pull(.data[["name"]])
  the_names <- sort(unique(the_names))
  # the_names <- sort(unique(out$data[, "name"]))

  # flag if label column is available
  is_label <- "label" %in% names(out$data)

  # create the plot
  out <- out %>%
    ggdag_status(color = .data[["status"]]) +
    geom_dag_point(aes(color = .data[["status"]])) +
    scale_color_manual(values = status_colrs, na.value = bg_colr,
                       guide = status_guide) +
    ggdag::theme_dag_blank(panel.background =
                             element_rect(fill=bg_colr, color=bg_colr)) +
    theme(legend.position = legend_pos,
          legend.title = element_blank())

  # add the node names
  if (is.null(text_labels)) {
    out <- out +
      geom_dag_text(size = text_size, color = text_colr)
  } else {
    # a common mistake is to forget a variable
    stopifnot(length(text_labels) == length(the_names))
    out <- out +
      geom_dag_text(size = text_size, color = text_colr, parse = TRUE,
                    label = text_labels)
  }

  # add the labels when the label column is available
  if (is_label) {
    out <- out +
      geom_dag_label_repel(aes(label = .data[["label"]]), color = text_colr,
                           fill = label_colr, box.padding = box_padding)
  }

  out
}
