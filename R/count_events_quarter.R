#' Data frame of number of events occurring in each Lexis-diagram quarter
#'
#' @description  Computes for each integer age and each combination of age and seasonal quarter
#'               the number of events occurring in the population.
#'               The computation is performed using the associated data frame of quarterly variables
#'               corresponding to the population obtained using the \code{\link{quarterly_variables}} function.
#'
#' @author Jose M. Pavia \email{pavia@@uv.es}
#' @author Josep Lledo \email{josep.lledo@@uv.es}
#' @references Pavia, JM and Lledo, J (2022). Estimation of the Combined Effects of Ageing and Seasonality on Mortality Risk. An application to Spain. *Journal of the Royal Statistical Society, Series A (Statistics in Society)*, 185(2), 471-497. \doi{10.1111/rssa.12769}
#'
#' @param x A data.frame output of the \code{\link{quarterly_variables}} function.
#'
#' @return
#' A data frame with the number of events for each (potential) combination of integer age and age and
#' season quarter of the input dataset. The data frame has the following components:
#'    \item{age}{ Integer age to which the time exposed at risk corresponds.}
#'    \item{quarter.age}{ Age quarter to which the time exposed at risk corresponds.}
#'    \item{quarter.calendar}{ Calendar (time, season) quarter to which the time exposed at risk corresponds.}
#'    \item{number.events}{ Number of events that occurred during the quarter determined for the
#'                         combination of `age`, `quarter.age` and `quarter.calendar`.}
#'
#' @note
#' The structure of the dataset is similar to those obtained using the \code{\link{time_exposed_outs}},
#' \code{\link{time_exposed_ins}} and  \code{\link{time_exposed_stock}} functions.
#'
#' @export
#'
#' @examples
#' dates.b <- c("1920-05-13", "1999-04-12", "2019-01-01")
#' dates.e <- c("2002-03-23", "2009-04-12", "2019-01-01")
#' x <- quarterly_variables(dates.b, dates.e)
#' out <- count_events_quarter(x)

count_events_quarter <- function(x){

  max.age <- max(floor(x$age.last.birthday))
  table <- data.frame(matrix(0L, ncol = 3L, nrow = (max.age + 1L) * 4L * 4L))
  colnames(table) <- cbind("age", "quarter.age", "quarter.calendar")
  table$age <- rep(0L:max.age, each = 16L)
  table$quarter.age <- rep(rep(1L:4L, each = 4L), max.age + 1L )
  table$quarter.calendar <- rep(1L:4L, length.out = (max.age + 1L) * 4L * 4L)

  dd <- tapply(x$age.last.birthday,
               list(x$age.last.birthday, x$quarter.age, x$quarter.calendar),
               length)

  nombres <- dimnames(dd)
  tamanyo <- dim(dd)

  table2 <- data.frame("age" = rep(nombres[[1L]], tamanyo[2L]*tamanyo[3L]),
                       "quarter.age" = rep(rep(nombres[[2L]], each = tamanyo[1L]), tamanyo[3L]),
                       "quarter.calendar" = rep(nombres[[3L]], each = tamanyo[1L]*tamanyo[2L]),
                       "number.events" = as.vector(dd))

  table <- merge(table, table2, all.x = TRUE)
  table[is.na(table)] <- 0L

  table <- methods::new("qlifetable", table)
  # class(table) <- c("qlifetable", "data.frame")
  return(table)
}
