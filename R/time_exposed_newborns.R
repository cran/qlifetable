#' Data frame of time exposed at risk for a population of newborns
#'
#' @description  Computes for each combination of age and seasonal quarter
#'               the total time exposed at risk (in years) of a population of newborns,
#'               during the year of their birth, this is up to the end of the year when they born.
#'
#' @author Jose M. Pavia \email{pavia@@uv.es}
#' @author Josep Lledo \email{josep.lledo@@uv.es}
#' @references Pavia, JM and Lledo, J (2022). Estimation of the Combined Effects of Ageing and Seasonality on Mortality Risk. An application to Spain. *Journal of the Royal Statistical Society, Series A (Statistics in Society)*, 185(2), 471-497. \doi{10.1111/rssa.12769}
#'
#' @param date.birth A character vector with the dates of birth in format either "yyyy-mm-dd" or "yyyy-mm-dd hour:min:secs" (for instance, "2016-01-20 12:00:00") of the members of the population.
#'                   If "hour:min:secs" is omitted the function imputes either "12:00:00", if `random.b = FALSE`, or
#'                   a random hour by default.
#' @param random.b A `TRUE/FALSE` argument indicating whether the exact moment ("hour:min:secs") when the birth occurs within the day is randomly selected. If TRUE, this overwrites "hour:min:secs" in `date.birth` even if those have been declared. By default, TRUE.
#'
#' @return
#' A data frame with the time exposed at risk for each (potential) combination of integer age and age and
#' season quarter of the population. The data frame has the following components:
#'    \item{age}{ Integer age to which the time exposed at risk corresponds.}
#'    \item{quarter.age}{ Age quarter to which the time exposed at risk corresponds.}
#'    \item{quarter.calendar}{ Calendar (time, season) quarter to which the time exposed at risk corresponds.}
#'    \item{time.exposed}{ Total time (in years) exposed at risk of the population during the quarter determined for the
#'                         combination of `age`, `quarter.age` and `quarter.season`.}
#'
#' @seealso \code{\link{time_exposed_stock}}, \code{\link{time_exposed_outs}}, \code{\link{time_exposed_ins}}
#'
#' @export
#'
#' @importFrom methods new
#'
#' @examples
#' dates.b <- c("1920-05-13", "1999-04-12", "2019-01-01")
#' out <- time_exposed_newborns(dates.b)

time_exposed_newborns <- function(date.birth,
                               random.b = TRUE){

  date.birth <- as.character(date.birth)

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

  start.year <- paste0(substr(date.birth, 1L, 4L), "-01-01 00:00:00")

  date.birth <- as.POSIXct(date.birth, tz = "GMT", tryFormats = "%Y-%m-%d %H:%M:%OS")
  start.year <- as.POSIXct(start.year, tz = "GMT", tryFormats = "%Y-%m-%d %H:%M:%OS")

  year <- as.numeric(format(start.year, "%Y"))

  coord.time <- date.birth - start.year

  xx <- as.data.frame(matrix(NA, nrow = length(date.birth), ncol = 7L))
  names(xx) <- c("coord.age", "coord.time", "age.last.birthday", "exact.age.at.event", "quarter.age", "quarter.calendar", "year")
  xx$exact.age.at.event <- 0L
  xx$coord.time <- as.double(coord.time / time_year(coord.time, "x", year))
  xx$age.last.birthday <- 0L
  xx$coord.age <- 0L
  xx$quarter.age <- 1L
  xx$quarter.calendar <- floor(4L * xx$coord.time) + 1L

  output <- time_exposed_ins(xx)
  output <- output[1L:16L, ]
  output <- methods::new("qlifetable", output)
 #  output <- class("qlifetable")
  return(output)
  }
