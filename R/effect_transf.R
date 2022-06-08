#' Convert Dataframe of Effect Measures to Its Inverse
#'
#' #' Convert dataframe of effect measures to its inverse.
#'
#' The \code{.estimate}, \code{.lower} and \code{.upper} will be trabsformed
#' and the \code{term} will be renamed.
#'
#'
#' @param data Dataframe of effect measures.
#' @param transf Type of conversion. Must be one of
#' \code{c("identity", "exp", "expit")}, default is \code{identity}.
#'
#' @return Data.frame
#' @export
effect_transf <- function(data, transf = c("identity", "exp", "expit")) {
  transf = match.arg(transf)

  if (transf == "identity") {
    # do nothing if identity
    out <- data
  } else if (transf == "exp") {
    out <- effect_transf_proc(data, prefix = "log")
  } else if (transf == "expit") {
    out <- effect_transf_proc(data, prefix = "logit")
  } else {
    msg <- sprintf("%s is an invalid transformation.", transf)
    stop(msg)
  }

  out
}

#' Convert Dataframe of Effect Measures to Its Inverse
#'
#' #' Convert dataframe of effect measures to its inverse.
#'
#' The \code{.estimate}, \code{.lower} and \code{.upper} will be trabsformed
#' and the \code{term} will be renamed.
#'
#'
#' @param data Dataframe of effect measures.
#' @param prefix Type of conversion. Must be one of
#' \code{c("log", "logit")}.
#'
#' @return Data.frame
#' @export
effect_transf_proc <- function(data, prefix = c("log", "logit")) {

  # locate the rows with "^log" and "logit"
  rgx <- paste0("^", prefix, ".+")
  is_log <- grepl(pattern = "^log.+", data$term)
  is_logit <- grepl(pattern = "^logit.+", data$term)
  # exclude logit from log
  is_log <- is_log & !is_logit

  if (prefix == "log") {
    # there must be at least 1 log
    stopifnot(any(is_log))
    out <- within(data, {
      # remove the prefix
      term[is_log] <- sub(pattern = "^log", x = term[is_log], replacement = "")
      # transform
      .lower[is_log] <- exp(.lower[is_log])
      .estimate[is_log] <- exp(.estimate[is_log])
      .upper[is_log] <- exp(.upper[is_log])
    })

  } else if (prefix == "logit") {
    # there must be at least 1 logit
    stopifnot(any(is_logit))
    out <- within(data, {
      # remove the prefix
      term[is_logit] <- sub(pattern = "^logit", x = term[is_logit], replacement = "")
      # transform
      .lower[is_logit] <- plogis(.lower[is_logit])
      .estimate[is_logit] <- plogis(.estimate[is_logit])
      .upper[is_logit] <- plogis(.upper[is_logit])
    })
  } else {
    msg <- sprintf("%s is an invalid prefix.", prefix)
    stop(msg)
  }

  out
}
