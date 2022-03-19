#' Convert a data.frame to long format
#'
#' Convert a data.frame to long format.
#'
#' Convert a data.frame to long format. A tie column will be added with 0 for
#' baseline and 1 for final.
#'
#' @param data Dataframe of data to convert to long format.
#' @param outcomes Character vector of base and final variables.
#' @param outcome Name that will be used for the single outcome variable.
#' @param timevar Name that will be used for the time variable.
#' @param names_to Name that will be used for the name variable.
#'
#' @importFrom rlang .data :=
#' @importFrom tidyr pivot_longer all_of
#' @importFrom dplyr if_else
#'
#' @seealso did_linear did_loglinear did_logistic
#'
#' @return Dataframe in long format.
#' @export
did_longer <- function(data, outcomes = c("Y0", "Y1"),
                       outcome = "Y", names_to = "var", timevar = "time") {
  # convert data to long format
  # IMPORTANT: pivot_longer returns a tibble which causes problems with boot.
  #            Make sure it is a data.frame.
  data <- pivot_longer(data, all_of(outcomes),
                       names_to = names_to, values_to = outcome) %>%
    mutate(!!timevar := if_else(.data[[names_to]] == outcomes[1], 0, 1)) %>%
    as.data.frame()
}
