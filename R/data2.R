#' Table 3.2
#'
#' See chapter 3.
#'
#' @docType data
#'
#' @format Dataframe with 6 rows and 6 variables.
#' \describe{
#'   \item{term}{name of estimator}
#'   \item{.lower}{lower confidence interval}
#'   \item{.estimate}{estimate}
#'   \item{.upper}{upper confidence interval}
#'   \item{.alpha}{alpha value confidence interval width = 1 - alpha}
#'   \item{.method}{method used used to estimate interval}
#' }
"fci_tbl_03_02"

#' Table 4.2
#'
#' See chapter 4.
#'
#' @docType data
#'
#' @format Dataframe with 18 rows and 6 variables.
#' \describe{
#'   \item{estimator}{name of estimator}
#'   \item{group}{grouping variable}
#'   \item{est}{estimate}
#'   \item{conf}{confidence interval width}
#'   \item{lci}{lower confidence interval}
#'   \item{uci}{upper confidence interval}
#' }
"fci_tbl_04_02"

#' Table 5.1
#'
#' See section 5.2..
#'
#' @docType data
#'
#' @format Dataframe with 1000 rows and 4 variables.
#' \describe{
#'   \item{A}{exposure}
#'   \item{Y0}{potential outcome 0}
#'   \item{Y1}{potential outcome 1}
#'   \item{Y}{outcome}
#' }
"fci_tbl_05_01"

#' Table 6.1
#'
#' See chapter 6.
#'
#' @docType data
#'
#' @format Dataframe with 4 rows and 6 variables.
#' \describe{
#'   \item{term}{name of estimator}
#'   \item{.lower}{lower confidence interval}
#'   \item{.estimate}{estimate}
#'   \item{.upper}{upper confidence interval}
#'   \item{.alpha}{alpha value confidence interval width = 1 - alpha}
#'   \item{.method}{method used used to estimate interval}
#' }
"fci_tbl_06_01"

#' Table 6.4
#'
#' See chapter 6.
#'
#' @docType data
#'
#' @format Dataframe with 4 rows and 6 variables.
#' \describe{
#'   \item{term}{name of estimator}
#'   \item{.lower}{lower confidence interval}
#'   \item{.estimate}{estimate}
#'   \item{.upper}{upper confidence interval}
#'   \item{.alpha}{alpha value confidence interval width = 1 - alpha}
#'   \item{.method}{method used used to estimate interval}
#' }
"fci_tbl_06_04"

#' Table 6.7
#'
#' See chapter 6.
#'
#' @docType data
#'
#' @format Dataframe with 4 rows and 6 variables.
#' \describe{
#'   \item{term}{name of estimator}
#'   \item{.lower}{lower confidence interval}
#'   \item{.estimate}{estimate}
#'   \item{.upper}{upper confidence interval}
#'   \item{.alpha}{alpha value confidence interval width = 1 - alpha}
#'   \item{.method}{method used used to estimate interval}
#' }
"fci_tbl_06_07"

#' Table 6.9
#'
#' See chapter 6.
#'
#' @docType data
#'
#' @format Dataframe with 4 rows and 6 variables.
#' \describe{
#'   \item{term}{name of estimator}
#'   \item{.lower}{lower confidence interval}
#'   \item{.estimate}{estimate}
#'   \item{.upper}{upper confidence interval}
#'   \item{.alpha}{alpha value confidence interval width = 1 - alpha}
#'   \item{.method}{method used used to estimate interval}
#' }
"fci_tbl_06_09"

#' Table 6.3 and 6.14
#'
#' See chapter 6, section 6.3.
#'
#' @docType data
#'
#' @format Dataframe with 4 rows and 5 variables.
#' \describe{
#'   \item{ss}{nb ofcovariates}
#'   \item{estimator}{estimate}
#'   \item{description}{description of estimator}
#'   \item{mean}{mean}
#'   \item{sd}{standard deviation}
#'   \item{pva}{p-value}
#' }
"fci_tbl_06_13"

#' Table 7.2
#'
#' See chapter 7, section 7.2.
#'
#' @docType data
#'
#' @format Dataframe with 13 rows and 7 variables.
#' \describe{
#'   \item{method}{method of estimation}
#'   \item{name}{name of estimator}
#'   \item{Truth}{true value}
#'   \item{est}{estimate}
#'   \item{conf}{confidence interval width}
#'   \item{lci}{lower confidence interval}
#'   \item{uci}{upper confidence interval}
#' }
"fci_tbl_07_02"


#' Simulation 8.1
#'
#' See chapter 8, section 8.3.
#'
#' @docType data
#'
#' @format Dataframe with 13 rows and 7 variables.
#' \describe{
#'   \item{A}{Exposure}
#'   \item{S}{Surrogate marker}
#'   \item{Y}{Outcome}
#'   \item{Ydot0}{Potential outcome Y(., S(0))}
#'   \item{Ydot1}{Potential outcome Y(., S(1))}
#' }
"fci_sim_08_01"

#' Table 9.1
#'
#' See chapter 9, section 9.3.
#'
#' @docType data
#'
#' @format Dataframe with 10 rows and 7 variables.
#' \describe{
#'   \item{method}{method of estimation}
#'   \item{name}{name of estimator}
#'   \item{Truth}{true value}
#'   \item{est}{estimate}
#'   \item{conf}{confidence interval width}
#'   \item{lci}{lower confidence interval}
#'   \item{uci}{upper confidence interval}
#' }
"fci_tbl_09_01"

#' Table 9.1 using qt() instead of 1.96 for CI
#'
#' See chapter 9, section 9.3.
#'
#' @docType data
#'
#' @format Dataframe with 10 rows and 7 variables.
#' \describe{
#'   \item{method}{method of estimation}
#'   \item{name}{name of estimator}
#'   \item{Truth}{true value}
#'   \item{est}{estimate}
#'   \item{conf}{confidence interval width}
#'   \item{lci}{lower confidence interval}
#'   \item{uci}{upper confidence interval}
#' }
"fci_tbl_09_01a"
