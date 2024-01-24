#' Data frame of time exposed at risk for a stock of general/insured population
#'
#' @description  Computes for each integer age and each combination of age and seasonal quarter
#'               the total time exposed at risk (in years) of a (stock) population of survivors
#'               (expected survivors) during a given year.
#'
#' @author Jose M. Pavia \email{pavia@@uv.es}
#' @author Josep Lledo \email{josep.lledo@@uv.es}
#' @references Pavia, JM and Lledo, J (2022). Estimation of the Combined Effects of Ageing and Seasonality on Mortality Risk. An application to Spain. *Journal of the Royal Statistical Society, Series A (Statistics in Society)*, 185(2), 471-497. \doi{10.1111/rssa.12769}
#'
#' @param date.birth A character vector with the dates of birth in format either "yyyy-mm-dd" or "yyyy-mm-dd hour:min:secs" (for instance, "2016-01-20 12:00:00") of the members of the population.
#'                   If "hour:min:secs" is omitted the function imputes either "12:00:00", if `random.b = FALSE`, or
#'                   a random hour by default.
#' @param year A numeric vector indicating the year for which the total time exposed at risk (by
#'             quarter) of the population is to be computed.
#' @param type A character argument informing if the total time exposed to risk is computed either since
#'             the beginning of the year or from the end of the year, depending when the census (stock) of population
#'             (portfolio) has been made. Only two values are allowed: `"forward"` and `"backward"`.
#'             If `type = "forward"` the time exposed to risk is computed since the beginning of the year
#'             (i.e., it is assumed that the population counting has been performed at the beginning of the year
#'             of interest).
#'             If `type = "backward"` the time exposed to risk is computed from the end of the year
#'             (i.e., it is assumed that the population counting has been performed at the end of the year of interest).
#' @param random.b A `TRUE/FALSE` argument indicating whether the exact moment ("hour:min:secs") when the birth occurs within the day is randomly selected. If TRUE, this overwrites "hour:min:secs" in `date.birth` even if those have been declared. By default, TRUE.
#' @param constant.age.year A `TRUE/FALSE` argument indicating whether the length of the year should be constant, 365.25 days, or variable,
#'                      depending on the time lived for the person in each year since her/his dates of birth and event. By default, FALSE.
#'                      The advantage of using a non-constant (person-dependent) length of year is congruence when
#'                      estimating time exposed at risk: in each year the time exposed along the time and age axes will coincide.
#'
#' @return
#' A data frame with the time exposed at risk for each (potential) combination of integer age and age and
#' season/calendar quarter of the population. The data frame has the following components:
#'    \item{age}{ Integer age to which the time exposed at risk corresponds.}
#'    \item{quarter.age}{ Age quarter to which the time exposed at risk corresponds.}
#'    \item{quarter.calendar}{ Calendar (time, season) quarter to which the time exposed at risk corresponds.}
#'    \item{time.exposed}{ Total time (in years) exposed at risk of the population during the quarter determined for the
#'                         combination of `age`, `quarter.age` and `quarter.calendar`.}
#'
#' @note
#' Using the notation of a general population, denoting by P the stock of population counted either at the
#' beginning or the end of the year, and by E, I, D and B the emigrants, immigrants, deaths and births recorded
#' during the year, to compute the total time exposed to risk they relate as follows:
#'
#' If the census (stock) of the population is performed at the beginning of the year of interest,
#' it is initially assumed that all the people is going to survive (is going to be at risk) up to
#' the end of the year. In this case `type = "forward"` should be used and the total time exposed
#' at risk, in each age a and (r, s) quarter, is through:
#' T(a, r, s) = time_exposed_stock(P) + time_exposed_ins(I) - time_exposed_ins(E) -
#' time_exposed_ins(D) + time_exposed_newborns(B).
#'
#' If the census (stock) of population is performed at the end of the year of interest, only the people
#' who survives up to that date is included in the counting. In this case `type = "backward"`
#' should be used and the total time exposed at risk, in each age a and (r, s) quarter, is computed
#' using the expression: T(a, r, s) = time_exposed_stock(P) - time_exposed_outs(I) + time_exposed_outs(E) +
#' time_exposed_outs(D).
#'
#' @seealso \code{\link{time_exposed_ins}}, \code{\link{time_exposed_outs}}, \code{\link{time_exposed_newborns}}
#'
#' @export
#'
#' @examples
#' dates.b <- c("1920-05-13", "1999-04-12", "2019-01-01")
#' out <- time_exposed_stock(dates.b, year = 2019, type = "backward")

time_exposed_stock <- function(date.birth,
                               year,
                               type,
                               random.b = TRUE,
                               constant.age.year = FALSE){

  if( !(type %in% c("forward", "backward") )){
    stop("Error: only 'forward' and 'backward' values are allowed for the 'type' argument")
  }

  date.birth <- as.character(date.birth)
  fechas.b <- max(as.numeric(substr(date.birth, 1L, 4L)))

  if (fechas.b >= year & type == "forward")
    stop("Error: At least one of the birth dates is posterior to the beginning of the year of exposition")

  if (fechas.b > year & type == "backward")
    stop("Error: At least one of the birth dates is posterior to the end of the year of exposition")

  if (type == "forward"){
    date.event <- paste0(year, "-01-01 00:00:00")
    coord.time <- rep(0, length(date.birth))
    time_exposed_comp <- time_exposed_ins
  }

  if (type == "backward"){
    date.event <- paste0(year + 1, "-01-01 00:00:00")
    coord.time <- rep(1, length(date.birth))
    time_exposed_comp <- time_exposed_outs
  }

  xx <- as.data.frame(matrix(NA, nrow = length(date.birth), ncol = 6L))
  names(xx) <- c("coord.age", "coord.time", "age.last.birthday", "exact.age.at.event", "quarter.age", "quarter.calendar")
  xx$exact.age.at.event <- exact_age(date.birth = date.birth, date.event = date.event,
                                         random.b = random.b, random.e = FALSE, constant.age.year = constant.age.year)
  xx$coord.time <- coord.time
  xx$age.last.birthday <- floor(xx$exact.age.at.event)
  xx$coord.age <- xx$exact.age.at.event - xx$age.last.birthday
  xx$quarter.age <- floor(4*xx$coord.age) + 1
  xx$quarter.calendar <- ifelse(type == "backward", 4, 1)

  output <- time_exposed_comp(xx)
  output <- methods::new("qlifetable", output)
  return(output)
}
