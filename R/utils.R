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
