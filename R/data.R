#' What-If study (Cook et al (2019))
#'
#' See section 1.2.3.1 for details.
#'
#' @docType data
#'
#' @format Dataframe with 165 rows and 4 variables.
#' \describe{
#'   \item{T}{1 = naltrexone, 0 = placebo}
#'   \item{A}{1 = reduced drinking for prior 30 days}
#'   \item{H}{1 = unsuppresed HIV load}
#'   \item{Y}{1 = unsuppresed HIV load at month 4}
#' }
#'
"whatifdat"

#' Double What-If study Simulation
#'
#' See section 1.2.3.2 for details.
#'
#' @docType data
#'
#' @format Dataframe with 1000 rows and 6 variables.
#' \describe{
#'   \item{AD0}{HIV antiretroviral adherence at time 0}
#'   \item{VL0}{Viral load at time 1}
#'   \item{T}{1 = naltrexone}
#'   \item{A}{1 = reduced drinking}
#'   \item{AD1}{HIV antiretroviral adherence at time 0}
#'   \item{VL1}{Viral load at time 1}
#' }
#'
"doublewhatifdat"

#' Mortality Rates by Age and Country
#'
#' See section 1.2.1. Mortality rates by age group in the US and China in
#' 2019.
#'
#' @docType data
#'
#' @format Dataframe with 4 rows and 5 variables.
#' \describe{
#'   \item{T}{1 = US, 0 = China}
#'   \item{H}{1 = > 65 years old}
#'   \item{deaths}{nb of deaths}
#'   \item{population}{size of population}
#'   \item{Y}{Mortality Rate = deaths / population}
#' }
"mortality"

#' Mortality Rates by Age and Country in long format.
#'
#' See section 1.2.1. Mortality rates by age group in the US and China in
#' 2019.
#'
#' @docType data
#'
#' @format Dataframe with 4 rows and 5 variables.
#' \describe{
#'   \item{T}{1 = US, 0 = China}
#'   \item{H}{1 = > 65 years old}
#'   \item{Y}{ 1 = deaths proportion, 0 = living proportion}
#'   \item{population}{size of population}
#'   \item{p}{proportion}
#' }
"mortality_long"

#' RECOVERY trial of dexamethasone COVID-10 Collaborative Group
#'
#' See section beginning of chapter 4.
#'
#' @docType data
#'
#' @format Dataframe with 8 rows and 4 variables.
#' \describe{
#'   \item{M}{1 = invasive mechanical ventilation prior to treatment}
#'   \item{T}{1 = dexamethasone}
#'   \item{Y}{1 = mortality}
#'   \item{n}{number of observations}
#' }
"recovery"


#' Admissions data from the NCES IPEDS 2018-2019 provisionally.
#'
#' See section 1.2.2 for details. Admissions data collected in the Fall 2018.
#'
#' @docType data
#'
#' @format Dataframe with 1217 rows and 3 variables.
#' \describe{
#'   \item{selective}{1 = institution admitted less than 50% of applicants}
#'   \item{female}{1 = more than 69% of students admitted were women}
#'   \item{highmathsat}{1 = average of the 25th and 75th precentiles of SAT was
#'   higher than 600}
#' }
#'
#' @source \url{nces.ed.gov}
#'
"nces"


#' Dataset from 2018 GSS
#'
#' See section 1.2.4. Data on 2348 surveyed individuals.
#'
#' @docType data
#'
#' @format Dataframe with 2348 rows and 12 variables.
#' \describe{
#'   \item{age}{todo}
#'   \item{gt65}{todo}
#'   \item{attend}{todo}
#'   \item{gthsedu}{todo}
#'   \item{magthsedu}{todo}
#'   \item{pagthsedu}{todo}
#'   \item{fair}{todo}
#'   \item{owngun}{todo}
#'   \item{conservative}{todo}
#'   \item{trump}{todo}
#'   \item{white}{todo}
#'   \item{female}{todo}
#' }
#' an exported object
#'
#' @source \url{www.gss.norc.org}
"gss"

#' Children's Oncology Group from Robins 1989
#'
#' See section 1.2.5. Data from Children's Oncology Group.
#'
#' @docType data
#'
#' @format Dataframe with 1190 rows and 4 variables.
#' \describe{
#'   \item{A1}{1 = experimental therapy, 0 = standard therapy}
#'   \item{H2}{1 = response data is available}
#'   \item{A2}{1 = patient received bone marrow transplant}
#'   \item{Y}{1 = patient surviving at two years}
#' }
"cogdat"


#' Table 3.2 data
#'
#' See chapter 3.
#'
#' @docType data
#'
#' @format Dataframe with 6 rows and 5 variables.
#' \describe{
#'   \item{estimator}{name of estimator}
#'   \item{est}{estimate}
#'   \item{conf}{confidence interval width}
#'   \item{lci}{lower confidence interval}
#'   \item{uci}{upper confidence interval}
#' }
"fci_tbl_03_02"

#' Table 4.2 data
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

#' Table 5.1 data.
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
"fci_dag01"
