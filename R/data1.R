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

#' What-If study (Cook et al (2019)) with extended data
#'
#' See section 1.2.3.1 for details.
#'
#' @docType data
#'
#' @format Dataframe with 165 rows and 11 variables.
#' \describe{
#'   \item{vl0}{Viral load at time 0}
#'   \item{vlcont0}{Viral load at time 0}
#'   \item{artad0}{todo}
#'   \item{Vl4}{Viral load after 4 months}
#'   \item{Vlcont4}{Viral load after 4 months}
#'   \item{artad4}{todo}
#'   \item{audit0}{todo}
#'   \item{T}{1 = naltrexone}
#'   \item{A}{1 = reduced drinking}
#'   \item{lvlcont0}{todo}
#'   \item{lvlcont4}{todo}
#' }
#'
"whatif2dat"

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

#' University of Florida Sepsis and Critical Illness (2017)
#'
#' See exercise 1 of chapter 4.
#'
#' @docType data
#'
#' @format Dataframe with 1190 rows and 4 variables.
#' \describe{
#'   \item{gt65}{1 = greater than 65 years of age}
#'   \item{shock}{1 = presence of septic shock at study entry}
#'   \item{zubrod45}{1 = Zubrod score of 4 or 5 one year after study entry}
#' }
"sepsis"

#' University of Florida Sepsis and Critical Illness (2017)
#'
#' See exercise 1 of chapter 4.
#'
#' @docType data
#'
#' @format Dataframe with 1190 rows and 4 variables.
#' \describe{
#'   \item{shock}{1 = presence of septic shock at study entry}
#'   \item{zubrodbase}{1 = Zubrod score of 3 or 4 at baseline}
#'   \item{zubrod1yr}{1 = Zubrod score of 4 or 5 one year after study entry}
#' }
"sepsisb"
