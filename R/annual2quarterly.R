#' Estimates quarterly life tables
#'
#' @description  Given an annual life table and a set of estimates seasonal-ageing indexes, estimates the four
#'               quarterly life tables associated to the annual life table.
#'
#' @author Jose M. Pavia \email{pavia@@uv.es}
#' @author Josep Lledo \email{josep.lledo@@uv.es}
#' @references Pavia, JM and Lledo, J (2022). Estimation of the Combined Effects of Ageing and Seasonality on Mortality Risk. An application to Spain. *Journal of the Royal Statistical Society, Series A (Statistics in Society)*, 185(2), 471-497. \doi{10.1111/rssa.12769}
#'
#' @param table.a A data.frame corresponding to the reference annual life table. The life table can be defined via
#'                death rates (`mx`, default) or via death probabilities (`qx`). The first column of `table.a` refers to `age`,
#'                and the second column to either `mx` rates or `qx` probabilities. In case of using death probabilities (`qx`),
#'                `table.a` can have an optional third column, which refers to the the average number of years lived
#'                for those dying with age x, `ax`. If this last column is missing ax is assumed to be constant and equal to 0.5.
#' @param SAIs An object output of the \code{\link{compute_SAI}} function.
#' @param mx A `TRUE/FALSE` argument informing whether `table.a` is either an annual life table of death rates or
#'           an annual table of death probabilities. Default, `FALSE`.
#' @param min.age A non-negative integer informing about the initial minimal age for which quarterly tables must be computed.
#'                This minimum age can be increased depending on the ages for which there are values in `table.a` and in
#'                `SAIs`. Default, 0.
#' @param max.age A positive integer informing about the initial maximum age for which quarterly tables must be computed.
#'                This maximum age can be decreased  depending on the ages for which there are values in `table.a` and in
#'                `SAIs`. Default, 100.
#'
#' @return
#'    A data frame with ten columns
#'    \item{age}{ Integer age to which death rates and probabilities corresponds.}
#'    \item{quarter.age}{ Age quarter to which death rates and probabilities corresponds.}
#'    \item{mx.quarter.birth.1}{ Death rates corresponding to people born during the first quarter of the year.}
#'    \item{mx.quarter.birth.2}{ Death rates corresponding to people born during the second quarter of the year.}
#'    \item{mx.quarter.birth.3}{ Death rates corresponding to people born during the third quarter of the year.}
#'    \item{mx.quarter.birth.4}{ Death rates corresponding to people born during the four quarter of the year.}
#'    \item{qx.quarter.birth.1}{ Death Probabilites corresponding to people born during the first quarter of the year.}
#'    \item{qx.quarter.birth.2}{ Death Probabilites corresponding to people born during the second quarter of the year.}
#'    \item{qx.quarter.birth.3}{ Death Probabilites corresponding to people born during the third quarter of the year.}
#'    \item{qx.quarter.birth.4}{ Death Probabilites corresponding to people born during the four quarter of the year.}
#'
#' @export
#'
#' @examples
#' \donttest{
#' # This can take a while
#'  t.stock <- time_exposed_stock(pop_2006$date.birth, 2006, "forward")
#'  t.stock <- t.stock[t.stock$age <= 100, ]
#'  temp <- quarterly_variables(death_2006$date.birth, death_2006$date.death)
#'  e.death <- count_events_quarter(temp)
#'  e.death <- e.death[e.death$age <= 100, ]
#'  t.birth <- time_exposed_newborns(birth_2006$date.birth)
#'  out <- crude_mx(t.stock, e.death, time.birth = t.birth)
#'  SAI.example <- compute_SAI(out, out)}
#'
#' dates.b <- c("2017-05-13", "2018-04-12", "2018-12-01")
#' t.stock <- time_exposed_stock(dates.b, year = 2020, type = "backward")
#' dates.bd <- c("2018-04-12")
#' dates.d <- c("2020-05-23")
#' x <- quarterly_variables(dates.bd, dates.d)
#' e.death <- count_events_quarter(x)
#' t.death <- time_exposed_outs(x)
#' out <- crude_mx(t.stock, e.death, t.death)
#' SAI.example <- compute_SAI(out, out)

annual2quarterly <- function(table.a, SAIs, mx = FALSE, min.age = 0, max.age = 100){

  argg <- as.list(environment())
  SAIs <- check_a2q(argg)

  if (!mx){
    colnames(table.a)[1L:2L] <- c("age", "mx")
  } else {
    table.a <- q2m(table.a)
  }

  # min and max ages
  min.age <- max(min.age, min(table.a$age), min(SAIs$age))
  max.age <- min(max.age, max(table.a$age), max(SAIs$age))
  if (min.age > max.age)
    stop("Error. The intersection of the sets of ages in 'table.a', 'SAIs' with 'min.age' and 'max.age' is empty.")

  SAIs <- merge(SAIs, table.a, by = "age")
  SAIs <- SAIs[order(SAIs$age, SAIs$quarter.age), ]
  SAIs$mx_q <- SAIs$SAI.lin * SAIs$mx
  SAIs$qx_q <- SAIs$mx_q / (4 + 0.5 * SAIs$mx_q)

  output <- data.frame(matrix(NA, ncol = 10L, nrow = (length(unique(SAIs$age)) * 4L)))
  colnames(output) <- c("age", "quarter.age",	"mx.quarter.birth.1",	"mx.quarter.birth.2",	"mx.quarter.birth.3",
                        "mx.quarter.birth.4",	"qx.quarter.birth.1",	"qx.quarter.birth.2",	"qx.quarter.birth.3",
                        "qx.quarter.birth.4")
  output$age <- rep(unique(SAIs$age), each = 4)
  output$quarter.age <- rep(1L:4L, length(unique(output$age)))

  key <- factor(SAIs$quarter.age):factor(SAIs$quarter.calendar)
  key.w <- c("1:1", "2:2", "3:3", "4:4")
  key.sp <- c("1:2", "2:3", "3:4", "4:1")
  key.su <- c("1:3", "2:4", "3:1", "4:2")
  key.a <- c("1:4", "2:1", "3:2", "4:3")

  output[, c("mx.quarter.birth.1", "qx.quarter.birth.1")] <- SAIs[key %in% key.w, c("mx_q", "qx_q")]
  output[, c("mx.quarter.birth.2", "qx.quarter.birth.2")] <- SAIs[key %in% key.sp, c("mx_q", "qx_q")]
  output[, c("mx.quarter.birth.3", "qx.quarter.birth.3")] <- SAIs[key %in% key.su, c("mx_q", "qx_q")]
  output[, c("mx.quarter.birth.4", "qx.quarter.birth.4")] <- SAIs[key %in% key.a, c("mx_q", "qx_q")]

  return(output)
}


###################################
#### Auxiliary functions

# Translates mortality rates to death probabilities
q2m <- function(tabla){
  if (ncol(tabla) == 2){
    colnames(tabla) <- c("age", "qx")
    ax <- 0.5
  } else {
    colnames(tabla) <- c("age", "qx", "ax")
    ax <- tabla[, 3L]
  }
  qx <- tabla[, 1L]
  tabla$mx <- qx/(1 - (1- ax)*qx)
  return(tabla)
}

# Check inputs of the function
check_a2q <- function(argg){
  if (argg$min.age < 0 | (argg$min.age - floor(argg$min.age)) > 0)
    stop("Error. The argument 'min.age' should be a non-negative integer")
  if (argg$max.age < 1 | (argg$max.age - floor(argg$max.age)) > 0)
    stop("Error. The argument 'max.age' should be a positive integer")
  n.col <- ncol(argg$table.a)
  if (n.col < 2L | n.col > 3L )
    stop("The number of colums of the argument 'table.a' is innapropiate.")
  edad <- argg$table.a[ , 1L]
  if (min(edad) < 0)
    stop("Error. At least an age in  'table.a' is negative.")
  edad <- round(edad) - floor(edad)
  if (max(edad) > 0)
    stop("Error. At least an age in 'table.a' is decimal.")
  mqx <- argg$table.a[ , 2L]
  if (argg$mx){
    if(min(mqx) < 0 | max(mqx) > 2) stop("At least a death rate in 'table.a' is negative or higher than 2.")
  } else {
    if(min(mqx) < 0 | max(mqx) > 1) stop("At least a death probability in 'table.a' is negative or higher than 1.")
  }
  if (n.col == 3L){
    ax <- argg$table.a[, 3L]
      if(min(ax) < 0 | max(ax) > 1) stop("At least an average age in 'table.a' is negative or higher than 1.")
  }

  if (!is.data.frame(argg$table.a))
    stop("Error. The introduced annual life table is not of the data.frame class.")

  if (!("SAI" %in% class(argg$SAIs)))
    stop("Error. The object introduced via the argument 'SAIs' must be an output of the function 'compute_SAI' or 'SAI_shorcut_1.")
  SAIs <- argg$SAIs
  if (length(SAIs) == 3L) SAIs <- SAIs[[1L]]
  return(SAIs)
}
