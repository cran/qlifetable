#' Randomly distributes the excess of recorded births in a given day
#'
#' @description Randomly distributes a number of births equivalent to the excess of registered births on a given day of a year among the different days of that year.
#'
#' @author Josep Lledo \email{josep.lledo@@uv.es}
#' @author Jose M. Pavia \email{pavia@@uv.es}
#' @references Pavia, JM and Lledo, J (2022). Estimation of the Combined Effects of Ageing and Seasonality on Mortality Risk. An application to Spain. *Journal of the Royal Statistical Society, Series A (Statistics in Society)*, 185(2), 471-497. \doi{10.1111/rssa.12769}

#' @param date.birth A character vector with the dates of birth in format either "yyyy-mm-dd" or "yyyy-mm-dd hour:min:secs" (for instance, "2016-01-20 12:00:00") of a population.
#' @param day A character vector in format "mm-dd" with the day of the year for which the (assumed) excess must be randomly distributed. By default, "01-01".
#' @param maximum.excess A numeric value indicating the percentage of births registered above the average to be surpassed in the target day in order to consider that in that day
#'                       an excess of births has been artificially recorded.
#' @param date.event A character vector with the dates of events in format either "yyyy-mm-dd" or "yyyy-mm-dd hour:min:secs" (for instance, "2016-01-20 12:00:00") linked to the
#'                   population of births. By default, NULL. When the dates of births are linked to some dates of events and both
#'                   occur in the same year, it can happen that some imputed dates of births be posterior to the dates of events.
#'                   The inclusion of this argument (when different of NULL) avoids this possibility happening.
#'
#' @return
#' A numeric vector of the same length and order as data.birth.
#'
#' @seealso \code{\link{time_exposed_ins}}, \code{\link{time_exposed_outs}}
#'
#' @export
#'
#' @note
#' We consider that in a day an excess of births has been registered if the percentage of the number of
#' births recorded in that day surpasses the average number of births registered during the days of the
#' corresponding year in a amount higher than `maximum.excess`.
#'
#' An excess usually happens in official statistics on the first of January. This occurs as a consequence of
#' established protocols in country border systems, because when an immigrant does not know her/his day
#' of birth, the border officials usually record them as January, 1. This provokes an artificial peak of dates of births
#' in that date.
#'
#' @importFrom stats runif
#'
#' @examples
#'
#' dates <- c("1920-05-13", "1999-04-12", "2019-01-01", "2019-01-01",
#'            "2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01", "2022-01-01")
#' distribute_excess(dates)

distribute_excess <- function(date.birth, day = "01-01", maximum.excess = 50, date.event = NULL){

  if (length(date.birth) != length(date.event) & length(date.event) > 1L)
    stop("The lengths of 'date.birth' and 'date.event' arguments differ.")

  date.birth <- as.character(date.birth)
  day <- as.character(day)
  date.birth0 <- date.birth

  l.fecha <- nchar(date.birth[1])

  mes <- as.numeric(substr(day, 1, 2))
  dia <- as.numeric(substr(day, 4, 5))

  if (mes > 12 | mes < 0 | dia > 31 | dia < 0 )
    stop("Invalid format for the 'day' argument.")

  date.birth <- as.POSIXct(substr(date.birth, 1, 10), tz = "GMT", tryFormats = "%Y-%m-%d")

  years <- unique(as.numeric(format(date.birth, "%Y")))
  years <- years[order(years)]

  # Date.event
  if (!is.null(date.event)){
    date.event <- as.character(date.event)
    l.fecha.event <- nchar(date.event[1])
    if (length(date.event) == 1L) date.event <- rep(date.event, length(date.birth))
    if (l.fecha.event == 10){
      date.event <- as.POSIXct(date.event, tz = "GMT", tryFormats = "%Y-%m-%d")
    } else {
      date.event <- as.POSIXct(date.event, tz = "GMT", tryFormats = "%Y-%m-%d %H:%M:%OS")
    }
  }

  for (year in 1:length(years)){

    n.days.year <- ifelse(years[year]%%4L != 0L, 365L, 366L)
    if (years[year]%%100 == 0L & years[year]%%400 != 0L) n.day.year <- 365L

    date.evaluation <- as.POSIXct(paste0(years[year], "-", day),
                                  tz = "GMT", tryFormats = "%Y-%m-%d")

    # Contamos
    total <-  sum(format(date.birth, "%Y") == years[year])
    media <- total/n.days.year
    selec <- which(date.birth == date.evaluation)
    excess <- (length(selec)/media - 1L) * 100L

    if (excess > maximum.excess){
      excess <- round(length(selec) - media)
      # Calculo el inicio de anyo
      start.year <- as.POSIXct(paste0(years[year], "-01-01 00:00:00"),
                               tz = "GMT", tryFormats = "%Y-%m-%d %H:%M:%OS")
      # Generamos aleatoriamente excess valores
      random.values <- stats::runif(n = excess, min = 0L, max = n.days.year)
      # Calculo los nuevos valores
      new.values <- as.character(start.year + (random.values*24L*60L*60L))
      if (l.fecha == 10L) new.values <- substr(new.values, 1L, 10L)
      # Posiciones de reemplazo
      selec <- sample(selec, excess, replace = FALSE)
      date.birth0[selec] <- new.values

      # Births posterior to events
      if (!is.null(date.event)){
        error <- which(date.birth0[selec] > date.event[selec])
        n.error <- length(error)
        if (n.error > 0L){
          distance.seconds <- (date.event[selec][error] - start.year)
          unidades <- units(distance.seconds)
          if (unidades == "days"){
            distance.seconds <- distance.seconds * 24L * 60L * 60L
          } else if (unidades == "hours") {
            distance.seconds <- distance.seconds * 60L * 60L
          } else if (unidades == "mins") {
            distance.seconds <- distance.seconds * 60L
          } else {
            distance.seconds <- distance.seconds
          }
          random.value.error <- stats::runif(n = n.error, min = 0L, max = distance.seconds)
          new.values.error <- as.character(start.year + random.value.error)
          if (l.fecha.event == 10L) new.values.error <- substr(new.values.error, 1L, 10L)
          # Posiciones de reemplazo
          date.birth0[selec][error] <- new.values.error
        }
      }

    } # End if excess
  } # End for

  return(date.birth0)

}

