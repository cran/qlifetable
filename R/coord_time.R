#' Time elapsed (in years) since the beginning of the year
#'
#' @description  Computes the time(s) elapsed (in years) between the beginning of the year and the date(s) of the event(s). The time coordinate(s) in a Lexis diagram.
#'
#' @author Jose M. Pavia \email{pavia@@uv.es}
#' @author Josep Lledo \email{josep.lledo@@uv.es}
#' @references Pavia, JM and Lledo, J (2022). Estimation of the Combined Effects of Ageing and Seasonality on Mortality Risk. An application to Spain. *Journal of the Royal Statistical Society, Series A (Statistics in Society)*, 185(2), 471-497. \doi{10.1111/rssa.12769}

#' @param date.event A character vector with the dates of events in format either "yyyy-mm-dd" or "yyyy-mm-dd hour:min:secs" (for instance, "2016-01-20 12:00:00") of a population.
#'                   If "hour:min:secs" is omitted the function imputes either "12:00:00", if `random.e = FALSE`, or
#'                   a random hour, by default.
#' @param random.e A `TRUE/FALSE` argument indicating whether the exact moment ("hour:min:secs") when the event occurs within the day is randomly selected. This overwrites "hour:min:secs" in `date.event` even if this has been declared.
#'
#' @return
#' A numeric vector of the same length as data.event
#'
#' @seealso \code{\link{coord_age}}, \code{\link{exact_age}}
#'
#' @export
#'
#' @note
#' The length of the year is 365 days in non-leap years and 366 days in leap years.
#'
#' @examples
#' dates <- c("2002-03-23", "2009-04-12", "2019-01-01")
#' coord_time(dates)
#' dates <- "2019-01-01 14:00:00"
#' coord_time(dates, FALSE)
#
#

coord_time <- function(date.event, random.e = TRUE){

  date.event <- as.character(date.event)

  if (random.e){
    n <- length(date.event)
    hh <- formatC(sample(0L:23L, n, replace = T), width = 2L, format = "d", flag = "0")
    mm <- formatC(sample(0L:59L, n, replace = T), width = 2L, format = "d", flag = "0")
    ss <- formatC(sample(0L:59L, n, replace = T), width = 2L, format = "d", flag = "0")
    date.event <- paste(substr(date.event, 1L, 10L), paste(hh, mm, ss, sep = ":"))
  } else {
  if (nchar(date.event[1]) == 10L)
    date.event <- paste(substr(date.event, 1L, 10L), "12:00:00")
  }
  date.event <- as.POSIXct(date.event, tz = "GMT", tryFormats = "%Y-%m-%d %H:%M:%OS")
  year <- as.numeric(format(date.event, "%Y"))

  start.year <- as.POSIXct(paste0(format(date.event, "%Y"), "-01-01 00:00:00"),
                           tz = "GMT", tryFormats = "%Y-%m-%d %H:%M:%OS")

  Coord.x <- date.event - start.year
  Coord.x <- as.double(Coord.x / time_year(Coord.x, "x", year))

  return(Coord.x)
}
