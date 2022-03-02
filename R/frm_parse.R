#' Parse and validate formula for estimate.
#'
#' @param frm String or formula to parse
#'
#' @importFrom rlang abort format_error_bullets
#'
#' @return Object of class \code{Formula}
#' @export
frm_parse <- function(frm) {

  check <- inherits(try(Formula::as.Formula(frm), silent = TRUE),
                    what = "Formula")
  if (check) {
    frm <- Formula::as.Formula(frm)
    frm_len <- length(frm)
  } else {
    msg <- sprintf("frm argument must a valid string or formula.")
    msg_head <- cli::col_yellow(msg)
    msg_body <- c("i" = sprintf("Formula: %s", deparse(frm)),
                  "i" = "A valid formula format is y ~ x + ...")
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "frm_parse_error1")
  }

  # verify the formula as a whole
  frm <- frm_parse_check(frm)

  # very the formula pattern with regex
  frm <- frm_parse_rgx(frm)

  # this is important for what is coming
  stopifnot(length(frm)[1] == 1, length(frm)[2] >= 1)
  frm
}


#' Validate formula as a whole.
#'
#' @param frm String or formula to parse
#'
#' @importFrom rlang abort format_error_bullets
#'
#' @return Object of class \code{Formula}
#' @export
frm_parse_check <- function(frm) {

  frm_text <- deparse(frm)
  frm_text <- gsub(" ", replacement = "", x = frm_text)

  # can only have 1 tildes in the formula
  check <- lengths(regmatches(frm_text, gregexpr("~", frm_text)))
  if (check != 1) {
    msg <- sprintf("There must be exactly one formula with ~.")
    msg_head <- cli::col_yellow(msg)
    msg_body <- c("i" = sprintf("There is %d ~ in the formula.", check),
                  "i" = sprintf("Formula: %s.", frm_text))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "frm_parse_check_error1")
  }


  # cannot have both side the same thing
  check <- rlang::f_lhs(frm) != rlang::f_rhs(frm)
  if (!check) {
    msg <- sprintf("Both sides of the formula are the same.")
    msg_head <- cli::col_yellow(msg)
    msg_body <- c("i" = sprintf("Formula: %s.", frm_text))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "frm_parse_check_error2")
  }

  frm
}


#' Validate formula's text patterns
#'
#' @param frm String or formula to parse
#'
#' @importFrom rlang abort format_error_bullets
#'
#' @return Object of class \code{Formula}
#' @export
frm_parse_rgx <- function(frm) {

  # deparse and remove white spaces
  frm_text <- deparse(frm)
  frm_text <- gsub(" ", replacement = "", x = frm_text)

  # split the string with |
  the_frm <- strsplit(frm_text, split = "[|]")
  the_frm <- unlist(the_frm)  # convert list to vector
  frm_n <- length(the_frm)
  stopifnot(frm_n != 0)

  # validate the basic formula
  frm_basic <- the_frm[1]
  patrn <- "[A-Za-z]+~.+"
  check <- grep(pattern = patrn, x = frm_basic)
  if (length(check) == 0) {
    msg <- sprintf("The basic formula is invalid.")
    msg_head <- cli::col_yellow(msg)
    msg_body <- c("i" = sprintf("Formula: %s.", frm_basic))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "frm_parse_rgx_error1")
  }

  # validate the extras
  if (frm_n > 1) {
    # the formulas
    frm_extras <- the_frm[2:frm_n]

    # the regex pattern
    patrn_func <- "(cond|do)"
    patrn_val <- "[(][[:alnum:]]+=[[:digit:]]+[)]"
    patrn <- paste0(patrn_func, patrn_val)

    check <- length(grep(pattern = patrn, x = frm_extras))
    # length of check must be the same as length of extras
    # the difference is the nb of unmatched items
    check <- frm_n - 1 - check
    if (check != 0) {
      msg <- sprintf("At least 1 extra formula is invalid.")
      msg_head <- cli::col_yellow(msg)
      msg_body <- c("i" = sprintf("%d invalid extra formulas.", check))
      msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
      rlang::abort(
        message = msg,
        class = "frm_parse_rgx_error2")
    }
  }

  frm
}
