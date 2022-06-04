#' Create a list of argumaents used by ggp_x custom functions
#'
#' @param data Data object.
#' @param type String to identify the desired set of arguments.
#'
#' @return List of argments including \code{data}
#' @export
ggp_format <- function(data, type = c("none", "measures_tbl")) {
  type <- match.arg(type)

  if (type == "none") {
    out <- data
  } else if (type == "measures_tbl") {
    out <- list(data = data,
                title = NULL, subtitle = NULL,
                vline = list(colors = c("lightseagreen", "violet"),
                             linetype = "solid", size = 3, alpha = 0.5),
                pointrange = list(size = 1, fatten = 2),
                text = list(size = 3, color = "navy", digits = 2),
                text_size = list(title = 0.9, y_axis = 0.9))
  } else {
    msg_head <- cli::col_yellow("Invalide type argument.")
    msg <- sprintf("\"%s\" is an invalid type.", type)
    msg_body <- c("x" = msg)
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "ggp_format_error")
  }

  out
}
