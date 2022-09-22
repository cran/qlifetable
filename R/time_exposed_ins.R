#' Data frame of time exposed at risk for a population of immigrants/portfolio entries
#'
#' @description  Computes for each integer age and each combination of age and seasonal quarter
#'               the total time exposed at risk (in years) of a population of immigrants/new policies
#'               (new production) during the year of the event.
#'               The computation is performed using the associated data frame of quarterly variables
#'               corresponding to the population obtained using the \code{\link{quarterly_variables}} function.
#'
#' @author Josep Lledo \email{josep.lledo@@uv.es}
#' @author Jose M. Pavia \email{pavia@@uv.es}
#' @references Pavia, JM and Lledo, J (2022). Estimation of the Combined Effects of Ageing and Seasonality on Mortality Risk. An application to Spain. *Journal of the Royal Statistical Society, Series A (Statistics in Society)*, 185(2), 471-497. \doi{10.1111/rssa.12769}
#'
#' @param x A data.frame output of the \code{\link{quarterly_variables}} function.
#'
#' @return
#' A data frame with the time exposed at risk for each (potential) combination of integer age and age and
#' season quarter of the input dataset. The data frame has the following components:
#'    \item{age}{ Integer age to which the time exposed at risk corresponds.}
#'    \item{quarter.age}{ Age quarter to which the time exposed at risk corresponds.}
#'    \item{quarter.calendar}{ Calendar (time, season) quarter to which the time exposed at risk corresponds.}
#'    \item{time.exposed}{ Total time (in years) exposed at risk of the population during the quarter determined for the
#'                         combination of `age`, `quarter.age` and `quarter.calendar`.}
#'
#' @note
#' The time exposed at risk is computed for each death from the beginning of the year in which the event occurred
#' until the moment of occurrence of the event. Please see the note in the \code{\link{time_exposed_stock}} function.
#'
#' @seealso \code{\link{time_exposed_stock}}, \code{\link{time_exposed_outs}}, \code{\link{time_exposed_newborns}}
#'
#' @export
#'
#' @examples
#' dates.b <- c("1920-05-13", "1999-04-12", "2019-01-01")
#' dates.e <- c("2002-03-23", "2009-04-12", "2019-01-01")
#' x <- quarterly_variables(dates.b, dates.e)
#' out <- time_exposed_ins(x)

time_exposed_ins <- function(x){

  x$triangle <- ifelse (x$coord.age - 0.25 * (x$quarter.age - 1L) > x$coord.time - 0.25 * (x$quarter.calendar - 1L), "upp", "low" )

  max.age <- max(floor(x$age.last.birthday))
  table <- data.frame(matrix(0L, ncol = 4L, nrow = (max.age + 2L) * 4L * 4L))
  colnames(table) <- cbind("age", "quarter.age", "quarter.calendar", "time.exposed")
  table$age <- rep(0L:(max.age + 1L), each = 16L)
  table$quarter.age <- rep(rep(1L:4L, each = 4L), max.age + 2L )
  table$quarter.calendar <- rep(1L:4L, length.out = (max.age + 2L) * 4L * 4L)

  for (k in 0L:max.age){

    # Seleccionamos de la df la edad a estudiar
    df.sel <- x[x$age.last.birthday == k, ]

    if(nrow(df.sel) != 0L){

      for (age.0 in 1L:4L){
        for (season.0 in 1L:4L){

          # Asignamos un ID
          key.df.x.in <- key_assignment_x_in(df.sel$quarter.age,
                                             df.sel$quarter.calendar,
                                             df.sel$triangle,
                                             age.0,
                                             season.0)
          key.df.x.in <- cbind.data.frame("coord.time" = df.sel$coord.time,
                                          "coord.age" = df.sel$coord.age,
                                          key.df.x.in)

          key.df.x.1.in <- key_assignment_x_1_in(df.sel$quarter.age,
                                                 df.sel$quarter.calendar,
                                                 df.sel$triangle,
                                                 age.0,
                                                 season.0)
          key.df.x.1.in <- cbind.data.frame("coord.time" = df.sel$coord.time,
                                            "coord.age" = df.sel$coord.age,
                                            key.df.x.1.in)

          key.df.x.in <- key.df.x.in[!is.na(key.df.x.in$key), ]
          key.df.x.1.in <- key.df.x.1.in[!is.na(key.df.x.1.in$key), ]

          # Calculamos el tiempo de exposicion al riesgo
          time.x <- time_exposed_x_in(key.df.x.in$coord.time,
                                      key.df.x.in$coord.age,
                                      key.df.x.in$key)
          time.x.1 <- time_exposed_x_1_in(key.df.x.1.in$coord.time,
                                          key.df.x.1.in$coord.age,
                                          key.df.x.1.in$key)

          # Sumamos todo el tiempo de exposicion al riesgo
          t.time.x <- sum(time.x, na.rm = TRUE)
          t.time.x.1 <- sum(time.x.1, na.rm = TRUE)

          # Guardar datos
          table$time.exposed[k * 16L + (age.0 - 1L) * 4L + season.0] <- table$time.exposed[k * 16L + (age.0 - 1L) * 4L + season.0] + t.time.x
          table$time.exposed[(k + 1L) * 16L + (age.0 - 1L) * 4L + season.0] <- table$time.exposed[(k + 1L) * 16L + (age.0 - 1L) * 4L + season.0] + t.time.x.1

        } # Next age.0
      } # Next season.0
    } # End if nrow
  } # Next k

  table <- methods::new("qlifetable", table)
  return(table)

}
