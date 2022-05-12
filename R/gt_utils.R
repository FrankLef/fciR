#' Basic format of table created with \code{gt}
#'
#' Basic format of table created with \code{gt}.
#'
#' Format of table created with \code{gt} for the heading, row stripping,
#' source note, etc.
#'
#' @param tbl Object of class \code{gt_tbl}.
#' @param title String. Table title.
#' @param subtitle String. Table subcols_align title.
#'
#' @importFrom dplyr %>%
#' @importFrom tidyselect matches
#' @importFrom gt tab_header opt_row_striping tab_style cell_text cells_title
#'  tab_source_note tab_options html
#'
#' @return Formatted \code{gt_tbl} object.
#' @export
gt_basic <- function(tbl, title, subtitle) {

  tbl %>%
    gt::tab_header(
      title = gt::html(paste0("<strong>", title, "</strong>")),
      subtitle = gt::html(paste0("<strong>", subtitle, "</strong>"))) %>%
    gt::cols_align(align = "center", columns = !matches("name")) %>%
    gt::opt_row_striping() %>%
    gt::tab_style(
      style = gt::cell_text(color = "midnightblue"),
      locations = gt::cells_title(groups = "title")) %>%
    gt::tab_style(
      style = gt::cell_text(color = "midnightblue"),
      locations = gt::cells_title(groups = "subtitle")) %>%
    gt::tab_source_note(
      source_note = "Fundamentals of Causal Inference, Babette A. Brumback, 2022"
    ) %>%
    gt::tab_options(
      heading.title.font.weight = "bold",
      heading.subtitle.font.weight = "bold",
      heading.background.color = "gainsboro",
      column_labels.font.weight = "bold")
}

#' Convert \code{gt_tbl} to \code{ggplot}
#'
#' @param obj Object of class \code{gt_tbl}.
#' @param ext Image file exension. Default is ".png".
#' @param ... Additional arguments for \code{gt::gtsave}.
#'
#' @importFrom magick image_ggplot
#'
#' @return Object of class \code{ggplot}.
#' @export
gt2ggp <- function(obj, ext = ".png", ...) {
  a_file <- tempfile(fileext = ext)
  gt_image <- gt::gtsave(obj, filename = a_file, expand = 5)
  ggp_image <- magick::image_read(path = a_file) %>%
    magick::image_ggplot(interpolate = TRUE)
  ggp_image
}
