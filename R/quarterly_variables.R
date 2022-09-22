#' Data frame of quarterly variables
#'
#' @description Computes punctual risk coordinates in the Lexis diagram and quarterly biometric
#'              variables of a population.
#'
#' @author Jose M. Pavia \email{pavia@@uv.es}
#' @author Josep Lledo \email{josep.lledo@@uv.es}
#' @references Pavia, JM and Lledo, J (2022). Estimation of the Combined Effects of Ageing and Seasonality on Mortality Risk. An application to Spain. *Journal of the Royal Statistical Society, Series A (Statistics in Society)*, 185(2), 471-497. \doi{10.1111/rssa.12769}
#'
#' @param date.birth A character vector with the dates of birth in format either "yyyy-mm-dd" or "yyyy-mm-dd hour:min:secs" (for instance, "2016-01-20 12:00:00") of a population.
#'                   If "hour:min:secs" is omitted the function imputes either "12:00:00", if `random.b = FALSE`, or
#'                   a random hour by default.
#' @param date.event A character vector with the dates of events in format either "yyyy-mm-dd" or "yyyy-mm-dd hour:min:secs" (for instance, "2016-01-20 12:00:00") of a population.
#'                   If "hour:min:secs" is omitted the function imputes either "12:00:00", if `random.e = FALSE`, or
#'                   a random hour, by default. This vector must have either length 1, when the aim is to compute
#'                   the exact age or the (1x1-Lexis) age coordinate of all the members of the population in the same temporal point or the same
#'                   length as `date.birth` when the aim is to compute for each member
#'                   of the population the exact age or the (1x1-Lexis) age coordinate in the moment of the event (e.g., death).
#' @param random.b A `TRUE/FALSE` argument indicating whether the exact moment ("hour:min:secs") when the birth occurs within the day is randomly selected. If TRUE, this overwrites "hour:min:secs" in `date.birth` even if those have been declared. By default, TRUE.
#' @param random.e A `TRUE/FALSE` argument indicating whether the exact moment ("hour:min:secs") when the event occurs within the day is randomly selected. If TRUE, this overwrites "hour:min:secs" in `date.event` even if those have been declared. By default, TRUE.
#' @param constant.age.year A `TRUE/FALSE` argument indicating whether the length of the year should be constant, 365.25 days, or variable,
#'                      depending on the time lived for the person in each year since her/his dates of birth and event. By default, FALSE.
#'                      The advantage of using a non-constant (person-dependent) length of year is congruence when
#'                      estimating time exposed at risk: in each year the time exposed along the time and age axes will coincide.
#'
#' @return
#' A data.frame with the following components:
#'    \item{coord.age}{ Time elapsed, measure in years, between the last birthday and the date when the event happens.}
#'    \item{coord.time}{ Time coordinate: time elapsed, measure in years, between the begining of the year and the date when the event happens.}
#'    \item{age.last.birthday}{ The integer age at last birthday.}
#'    \item{exact.age.at.event}{ Time elapsed, measure in years, between the dates of birth and event.}
#'    \item{quarter.age}{ Age quarter when the event happens.}
#'    \item{quarter.calendar}{ Calendar (time, season) quarter to which the time exposed at risk corresponds.}
#'    \item{year}{ Year when the event happens.}
#' @export
#'
#' @note
#' In the age axis, the length of the years are assumed either constant 365.25 days (`constant.age.year = TRUE`) or
#' variable (`constant.age.year = FALSE`), depending on the person. In the time axis, the length of the year is either
#' 365 in non-leap years and 366 in leap years. The advantage of using a non-constant (person-dependent) length of year
#' in the age axis is that in each year the lengths of the years when computing `coord.age` and `coord.time`
#' in both axis are equal.
#'
#' @examples
#' dates.b <- c("1920-05-13", "1999-04-12", "2019-01-01")
#' dates.e <- c("2002-03-23", "2009-04-12", "2019-01-01")
#' quarterly_variables(dates.b, dates.e)
#
quarterly_variables <- function(date.birth, date.event, random.b = TRUE, random.e = TRUE, constant.age.year = FALSE){

  if (length(date.birth) != length(date.event) & length(date.event) != 1L)
    stop("The lengths of date.birth and date.event differ.")

  date.birth <- as.character(date.birth)
  date.event <- as.character(date.event)

  if (random.e){
    n <- length(date.event)
    hh <- formatC(sample(0L:23L, n, replace = T), width = 2L, format = "d", flag = "0")
    mm <- formatC(sample(0L:59L, n, replace = T), width = 2L, format = "d", flag = "0")
    ss <- formatC(sample(0L:59L, n, replace = T), width = 2L, format = "d", flag = "0")
    date.event <- paste(substr(date.event, 1L, 10L), paste(hh, mm, ss, sep = ":"))
  } else {
    if (nchar(date.event[1L]) == 10L)
      date.event <- paste(substr(date.event, 1L, 10L), "12:00:00")
  }

  if (random.b){
    n <- length(date.birth)
    hh <- formatC(sample(0L:23L, n, replace = T), width = 2L, format = "d", flag = "0")
    mm <- formatC(sample(0L:59L, n, replace = T), width = 2L, format = "d", flag = "0")
    ss <- formatC(sample(0L:59L, n, replace = T), width = 2L, format = "d", flag = "0")
    date.birth <- paste(substr(date.birth, 1L, 10L), paste(hh, mm, ss, sep = ":"))
  } else {
    if (nchar(date.birth[1L]) == 10L)
      date.birth <- paste(substr(date.birth, 1L, 10L), "12:00:00")
  }

  date.birth <- as.POSIXct(date.birth, tz = "GMT", tryFormats = "%Y-%m-%d %H:%M:%OS")
  date.event <- as.POSIXct(date.event, tz = "GMT", tryFormats = "%Y-%m-%d %H:%M:%OS")

  if (random.e){
    same <- which(substr(date.birth, 1L, 10L) == substr(date.event, 1L, 10L))
    birth <- date.birth[same]
    date.event[same] <- simula_post(fijo = birth)
  }
  if (!random.e & random.b){
    same <- which(substr(date.birth, 1L, 10L) == substr(date.event, 1L, 10L))
    event <- date.event[same]
    date.birth[same] <- simula_ant(fijo = event)
  }

  output <- as.data.frame(matrix(NA, nrow = length(date.birth), ncol = 7L))
  names(output) <- c("coord.age", "coord.time", "age.last.birthday", "exact.age.at.event", "quarter.age", "quarter.calendar", "year")
  output$exact.age.at.event <- exact_age(date.birth = date.birth, date.event = date.event,
                              random.b = FALSE, random.e = FALSE, constant.age.year = constant.age.year)
  output$coord.time <- coord_time(date.event = date.event, random.e = FALSE)
  output$age.last.birthday <- floor(output$exact.age.at.event)
  output$coord.age <- output$exact.age.at.event - output$age.last.birthday
  output$quarter.age <- floor(4*output$coord.age) + 1
  output$quarter.calendar <- floor(4*output$coord.time) + 1
  output$year <- as.numeric(format(date.event, "%Y"))

#  class(output) <- c("quarterly_variables")

  return(output)
}
