#' Time elapsed (in years) since last birthday and the date of the event
#'
#' @description Computes the time(s) elapsed (in years) between the date(s) of last birthday and the date(s) of event(s) for each member of a population.
#'              The age coordinate(s) corresponding to an 1x1-year Lexis diagram, using by default the same length of year employed to compute the (related) time coordinate(s).
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
#'                   for all the members of the population the age coordinate in an 1x1-year Lexis diagram
#'                   in the same temporal point, or the same length as `date.birth`, when the aim is to compute for
#'                   each member of the population the age coordinate in the moment of the event (e.g., death).
#' @param random.b A `TRUE/FALSE` argument indicating whether the exact moment ("hour:min:secs") when the birth occurs within the day is randomly selected. If TRUE, this overwrites "hour:min:secs" in `date.birth` even if those have been declared. By default, TRUE.
#' @param random.e A `TRUE/FALSE` argument indicating whether the exact moment ("hour:min:secs") when the event occurs within the day is randomly selected. If TRUE, this overwrites "hour:min:secs" in `date.event` even if those have been declared. By default, TRUE.
#' @param constant.age.year A `TRUE/FALSE` argument indicating whether the length of the year should be constant, 365.25 days, or variable,
#'                      depending on the time lived for the person in each year since her/his dates of birth and event. By default, FALSE.
#'                      The advantage of using a non-constant (person-dependent) length of year is congruence when
#'                      estimating time exposed at risk: in each year the time exposed along the time and age axes will coincide.
#'
#' @return
#' A numeric vector of the same length as data.birth
#'
#' @note
#' If `constant.age.year = FALSE` (default), the length of the year for each person is computed as a weighted average of the lengths
#' of the years that the person has lived between his/her dates of birth and event using as weight the time lived for the person during
#' each year.
#'
#' @seealso \code{\link{coord_time}}, \code{\link{exact_age}}
#'
#' @export
#'
#' @examples
#' dates.b <- c("1920-05-13", "1999-04-12", "2019-01-01")
#' dates.e <- c("2002-03-23", "2009-04-12", "2019-01-01")
#' coord_age(dates.b, dates.e)
#
# @importFrom stats runif
#

coord_age <- function(date.birth, date.event, random.b = TRUE, random.e = TRUE, constant.age.year = FALSE){

#  if (length(date.birth) != length(date.event) & length(date.event) != 1L)
#    stop("The lengths of date.birth and date.event differ.")

#  date.birth <- as.character(date.birth)
#  date.event <- as.character(date.event)

#  if (random.e){
#    n <- length(date.event)
#    hh <- formatC(sample(0L:23L, n, replace = T), width = 2L, format = "d", flag = "0")
#    mm <- formatC(sample(0L:59L, n, replace = T), width = 2L, format = "d", flag = "0")
#    ss <- formatC(sample(0L:59L, n, replace = T), width = 2L, format = "d", flag = "0")
#    date.event <- paste(substr(date.event, 1L, 10L), paste(hh, mm, ss, sep = ":"))
#  } else {
#    if (nchar(date.event[1L]) == 10L)
#      date.event <- paste(substr(date.event, 1L, 10L), "12:00:00")
#  }

#  if (random.b){
#    n <- length(date.birth)
#    hh <- formatC(sample(0L:23L, n, replace = T), width = 2L, format = "d", flag = "0")
#    mm <- formatC(sample(0L:59L, n, replace = T), width = 2L, format = "d", flag = "0")
#    ss <- formatC(sample(0L:59L, n, replace = T), width = 2L, format = "d", flag = "0")
#    date.birth <- paste(substr(date.birth, 1L, 10L), paste(hh, mm, ss, sep = ":"))
#  } else {
#    if (nchar(date.birth[1L]) == 10L)
#      date.birth <- paste(substr(date.birth, 1L, 10L), "12:00:00")
#  }

#  date.birth <- as.POSIXct(date.birth, tz = "GMT", tryFormats = "%Y-%m-%d %H:%M:%OS")
#  date.event <- as.POSIXct(date.event, tz = "GMT", tryFormats = "%Y-%m-%d %H:%M:%OS")
#  year <- as.numeric(format(date.event, "%Y"))

#  if (random.e){
#    same <- which(substr(date.birth, 1L, 10L) == substr(date.event, 1L, 10L))
#    birth <- date.birth[same]
#    date.event[same] <- simula_post(fijo = birth)
#  }
#  if (!random.e & random.b){
#    same <- which(substr(date.birth, 1L, 10L) == substr(date.event, 1L, 10L))
#    event <- date.event[same]
#    date.birth[same] <- simula_ant(fijo = event)
#  }

  age.at.event <- exact_age(date.birth = date.birth, date.event = date.event,
                            random.b = FALSE, random.e = FALSE,
                            constant.age.year = constant.age.year)

  Coord.y <- age.at.event - floor(age.at.event)

  return(Coord.y)
}
