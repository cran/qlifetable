#' Data frame(s) of crude estimates of quarterly (and annual) central rates of mortality estimated from quarterly summary statistics.
#'
#' @description  Computes for a general/insured population crude estimates of quarterly (and annual)
#'               central rates of mortality by age (in a set of integer ages) for each combination
#'               of age and calendar quarter, given data sets of times exposed-at-risk of
#'               stocks of population and of number of deaths by each integer age for
#'               each combination of age and seasonal quarter, along with data sets of times
#'               exposed-at-risk (or non-exposed) of immigrants/new policies, emigrants/exits/deaths
#'               and/or newborns.
#'
#' @author Jose M. Pavia \email{pavia@@uv.es}
#' @author Josep Lledo \email{josep.lledo@@uv.es}
#' @references Pavia, JM and Lledo, J (2022). Estimation of the Combined Effects of Ageing and Seasonality on Mortality Risk. An application to Spain. *Journal of the Royal Statistical Society, Series A (Statistics in Society)*, 185(2), 471-497. \doi{10.1111/rssa.12769}
#'
#' @param time.stock A data frame containing the total exposure-at-risk times for the stock of
#'                   the population/portfolio, measured either since the start of the year (`type = "forward"`)
#'                   or from the end of the year (`type = "backward"`) throughout the target period (typically a year).
#'                   This data frame must contain the times for a set of consecutive integer ages
#'                   for each combination of age and season/calendar quarter. Typically, this data
#'                   frame is an output of the function \code{\link{time_exposed_stock}} or a data frame
#'                   with the same structure. The set of consecutive integer ages in this object determines the
#'                   range of ages for which crude rates of mortality are estimated by the function.
#' @param events.death  A data frame with the number of deaths recorded in the population/portfolio
#'                      during the target period (typically a year) in a set of integer ages for each combination of age
#'                      and season/calendar quarter. Typically, this is an output of the function
#'                      \code{\link{count_events_quarter}} or a data frame with the same structure.
#'                      If the range of ages does not cover the range of ages in `time.stock`,
#'                      zeros are imputed  for the missing ages.
#' @param time.death A data frame containing either the total non-exposure-at-risk times (when
#'                   `type = "forward"`) or the total exposure-at-risk times (when `type = "backward"`)
#'                   during the target period of the deaths recorded in the population/portfolio.
#'                   Typically, this data frame is an output of the function \code{\link{time_exposed_ins}}
#'                   when `type = "forward"` or an output of the function \code{\link{time_exposed_outs}}
#'                   when `type = "backward"` or a data frame with the same structure. Default, `NULL`.
#'                   If no `time.death` data.frame is provided the total (non-) exposure-at-risk times
#'                   are computed using the information in `events.death` under the hypothesis of
#'                   uniform distribution of deaths within each age-calendar quarter.
#' @param time.outs A data frame containing either the total non-exposure-at-risk times (when
#'                 `type = "forward"`) or the total exposure-at-risk times (when `type = "backward"`)
#'                 during the target period of the emigrants/exits recorded in the population/portfolio.
#'                 Typically, this data frame is an output of the function \code{\link{time_exposed_ins}}
#'                 when `type = "forward"` or an output of the function \code{\link{time_exposed_outs}}
#'                 when `type = "backward"` or a data frame with the same structure. Default, `NULL`.
#' @param time.ins A data frame containing either the total exposure-at-risk times (when
#'                 `type = "forward"`) or the total non-exposure-at-risk times (when `type = "backward"`)
#'                 during the target period (typically a year) of the immigrants/new policies recorded in the population/portfolio.
#'                 Typically, this data frame is an output of the function \code{\link{time_exposed_ins}}
#'                 when `type = "forward"` and an output of the function \code{\link{time_exposed_outs}}
#'                 when `type = "backward"` or a data frame with the same structure. Default, `NULL`.
#' @param time.birth A data frame containing the total exposure-at-risk times of the newborns recorded in
#'                   the population during the target period. Typically, this data frame is an output of the
#'                   function \code{\link{time_exposed_newborns}} or a data frame with the same structure.
#'                   Default, `NULL`.
#' @param type A character string informing if the total time exposed to risk of the stock of
#'             population/portfolio has been computed since the beginning of the year (`"forward"`) or
#'             from the end of the year (`"backward"`). Default, `"forward"`.
#' @param annual A character string informing whether the annual crude central rates of mortality
#'               should also be computed. Default, `FALSE`.
#'
#' @return
#' When `annual = FALSE` a data frame with estimated crude central rates of mortality for the set
#' of integer ages determined by `time.stock` for each combination of age and
#' calendar quarter. The data frame has the following components:
#'    \item{age}{ Integer age to which the crude central rate of mortality corresponds.}
#'    \item{quarter.age}{ Age quarter to which the crude central rate of mortality corresponds.}
#'    \item{quarter.calendar}{ Calendar (time, season) quarter to which the crude central rate of mortality corresponds.}
#'    \item{exposed}{ Total exposure-at-risk times in the target population for each combination of `age`, `quarter.age` and `quarter.calendar`.}
#'    \item{deaths}{ Number of deaths in the target population for each  combination of `age`, `quarter.age` and `quarter.calendar`.}
#'    \item{mx}{ Estimated crude central rate of mortality corresponding to the combination of `age`, `quarter.age` and `quarter.calendar`.}
#' When `annual = TRUE` the output is a list with two data frames `mx.quarterly` and `mx.annual`.
#' `mx.quarterly` is a data frame with the estimated quarterly crude central rates of mortality as just described and
#' `mx.annual` is the corresponding data frame with the estimated annual crude central rates of mortality.
#'
#' @note
#' First, it is the responsibility of the user to assure that all the times exposed-at-risk and number of events
#' correspond to the same target period (typically, the same year).
#' Second, If the date of reference of the stock of population/portfolio is the beginning of the year (`type = "forward"`),
#' the total time exposed at risk for each integer age in each age and calendar quarter is computed through:
#' time_exposed_stock(P) + time_exposed_ins(I) - time_exposed_ins(E) - time_exposed_ins(D) + time_exposed_newborns(B).
#' On the other hand, if the stock of population/portfolio is referenced at the end of the target year
#' (`type = "backward"`), the total time exposed at risk for each age in each age and seasonal quarter is
#' computed slightly different: time_exposed_stock(P) - time_exposed_outs(I) + time_exposed_outs(E) + time_exposed_outs(D).
#' In the above expressions P, I, E, D and B represent, respectively, stocks of population/portfolio,
#' immigrants/new policies, emigrants/exits, deaths and births after being transformed using the
#' function \code{\link{quarterly_variables}}.
#'
#' @importFrom stats aggregate
#'
#' @seealso \code{\link{crude_mx_sh2}}, \code{\link{crude_mx_sh2}}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Do not run, it can take a while
#'  t.stock <- time_exposed_stock(pop_2006$date.birth, 2006, "forward")
#'  t.stock <- t.stock[t.stock$age <= 100, ]
#'  temp <- quarterly_variables(death_2006$date.birth, death_2006$date.death)
#'  e.death <- count_events_quarter(temp)
#'  e.death <- e.death[e.death$age <= 100, ]
#'  t.birth <- time_exposed_newborns(birth_2006$date.birth)
#'  out <- crude_mx(t.stock, e.death, time.birth = t.birth)
#' }
#' dates.b <- c("2017-05-13", "2018-04-12", "2018-12-01")
#' t.stock <- time_exposed_stock(dates.b, year = 2020, type = "backward")
#' dates.bd <- c("2018-04-12")
#' dates.d <- c("2020-05-23")
#' x <- quarterly_variables(dates.bd, dates.d)
#' e.death <- count_events_quarter(x)
#' t.death <- time_exposed_outs(x)
#' out <- crude_mx(t.stock, e.death, t.death)

crude_mx <- function(time.stock,
                     events.death,
                     time.death = NULL,
                     time.outs = NULL,
                     time.ins = NULL,
                     time.birth = NULL,
                     type = "forward",
                     annual = FALSE){

  # Checks
  # Type
  if (!(type %in% c("forward", "backward"))){
    stop("The `type` argument is not correct, only `forward` and `backward` are allowed.")
  }

  # Data frames
  check_df(time.stock)
  names(time.stock) <- c("age", "quarter.age", "quarter.calendar", "time.exposed")

  check_df(events.death)
  colnames(events.death) <- c("age", "quarter.age", "quarter.calendar", "number.events")

  if(!is.null(time.death)){
    check_df(time.death)
    colnames(time.death) <- c("age", "quarter.age", "quarter.calendar", "time.exposed")
  }
  if(!is.null(time.outs)){
    check_df(time.outs)
    colnames(time.outs) <- c("age", "quarter.age", "quarter.calendar", "time.exposed")
  }
  if(!is.null(time.ins)){
    check_df(time.ins)
    colnames(time.ins) <- c("age", "quarter.age", "quarter.calendar", "time.exposed")
  }
  if(!is.null(time.birth)){
    check_df_birth(time.birth)
    colnames(time.birth) <- c("age", "quarter.age", "quarter.calendar", "time.exposed")
  }

  # Define range of ages according to the population stock
  ages <- c(min(time.stock$age):max(time.stock$age))

  # time.birth match with the population stock for age 0
  if ((sum(time.stock$time.exposed[time.stock$ages == 0L]) > 0L) & type == "forward" & is.null(time.birth)){
    warning("Age 0 is included in `time.stock` but `time.birth` has not been declared. Some mx estimates for zero years could be negative.")
  }

  if (ages[1L] > min(events.death$age) | ages[length(ages)] < max(events.death$age)) {
    warning("In `events.death` there are ages outside the range determined by `time.stock`. These data are not used")
  }

  if(is.null(time.death)){
    time.death <- uniform_death(e.death = events.death, type = type)
  }

  # Create objects with **complete.table()** containing the observed data.
  time.death <- complete_table(time.death, ages, "time.death")
  time.outs <- complete_table(time.outs, ages, "time.outs")
  time.ins <- complete_table(time.ins, ages, "time.ins")
  time.birth <- complete_table(time.birth, ages, "time.birth")

  time.stock <- complete_table(time.stock, ages, "time.stock")
  events.death <- complete_table(events.death, ages, "events.death")
  t.mx <- complete_structure(ages)
  t.mx$ID <- paste(t.mx$age, "_", t.mx$quarter.age, "_", t.mx$quarter.calendar, sep = "")

  # Cross-reference all data by ID
  t.mx <- merge(t.mx, time.stock, by = "ID", all.x = TRUE)
  t.mx <- merge(t.mx, events.death, by = "ID", all.x = TRUE)
  t.mx <- merge(t.mx, time.death, by = "ID", all.x = TRUE)
  t.mx <- merge(t.mx, time.outs , by = "ID", all.x = TRUE)
  t.mx <- merge(t.mx, time.ins , by = "ID", all.x = TRUE)
  t.mx <- merge(t.mx, time.birth , by = "ID", all.x = TRUE)

  t.mx <- t.mx[order(as.numeric(t.mx$age)), ]
  # t.mx$time.birth[which(is.na(t.mx$time.birth))] <- 0L
  t.mx[is.na(t.mx)] <- 0L
  t.mx <- compute_mx(t.mx, type)
  t.mx$mx[is.nan(t.mx$mx)] <- 0L # 0/0

  # Prepare the output
  t.mx <- t.mx[, c("age", "quarter.age", "quarter.calendar", "exposed", "deaths", "mx")]
  row.names(t.mx) <- 1L:nrow(t.mx)
  class(t.mx) <- c("data.frame", "crude_mx")

  if (annual){
    t.mx.a <- stats::aggregate(cbind(exposed, deaths) ~ age, t.mx, FUN = sum)
    t.mx.a$mx <- t.mx.a$deaths/t.mx.a$exposed
    t.mx.a$mx[is.nan(t.mx.a$mx)] <- 0L # 0/0
    t.mx <- list("mx.quarterly"= t.mx, "mx.annual" = t.mx.a)
    class(t.mx) <- c("list", "crude_mx")
  }

  return(t.mx)

} # End of function


#######################
# Auxiliary functions #
#######################

## Create and complete objects
complete_table <- function(df, ages, name_value){

  if(is.null(df)){
    age <- rep(ages, each = 16L)
    quarter.age <- rep(1L:4L, length(ages) * 4L)
    quarter.calendar <- rep(rep(1L:4L, length(ages)), each = 4L)
    df <- cbind.data.frame(age, quarter.age, quarter.calendar)
    df$time.exposed <- 0L
  }

  df <- cbind.data.frame(df[, 4L],
                         paste(df$age, "_", df$quarter.age, "_", df$quarter.calendar, sep = ""))
  colnames(df) <- c(name_value, "ID")

  return(df)
}

complete_structure <- function(ages){
  age <- rep(ages, each = 16L)
  quarter.age <- rep(1L:4L, length(ages) * 4L)
  quarter.calendar <- rep(rep(1L:4L, length(ages)), each = 4L)
  df <- cbind.data.frame(age, quarter.age, quarter.calendar)
  return(df)
}
##.........................................

# Calculate mx
compute_mx <- function(df, type = "forward"){

  if (type == "forward"){
    df$exposed <- df$time.stock - df$time.death - df$time.outs + df$time.ins + df$time.birth
    df$deaths <- df$events.death
    df$mx <- df$deaths/df$exposed
  } else {
    df$exposed <- df$time.stock + df$time.death + df$time.outs - df$time.ins
    df$deaths <- df$events.death
    df$mx <- df$deaths/df$exposed
  }

return(df)

}
##.........................................

## checks
check_df <- function(x){
  if (length(dim(x)) != 2L){
    stop("Error. At least one of the introduced data frames does not have the proper structure.")
  }
  if (ncol(x) != 4L){
    stop("Error. At least one of the introduced data frames does not have the proper structure.")
  }
  if(!all(apply(x, 2L, class) %in% c("numeric", "integer"))){
    stop("Error. The class of at least one of columns of the introduced data frames is not numeric.")
  }
  if (max(x[, 2L:3L]) > 4L | min(x[, 2L:3L]) < 1L)
    stop("Error. At least one of he introduced data frames have a quarter higher than 4 or smaller than 1.")

  if (!is.data.frame(x))
    stop("At least one of the objects introduced as a data.frame is not of the data.frame class.")
}

check_df_birth <- function(x){
  if (ncol(x) != 4L | nrow(x) != 16L){
    stop("Error. The birth data frame introduced does not have the proper structure.")
  }
  if(!all(apply(x, 2L, class) %in% c("numeric", "integer"))){
    stop("Error. The class of at least one of columns of the introduced data frames is not numeric.")
  }
  if (max(x[, 2L:3L]) > 4L | min(x[, 2L:3L]) < 1L)
    stop("Error. At least one of he introduced data frames have a quarter higher than 4 or smaller than 1.")
}
##.........................................

## Uniform distribution by quarters of time exposed-at-risk of deaths
uniform_death <- function(e.death, type){

  ages <- max(0L, (min(e.death$age) - 1L)):max(e.death$age)
  df <- as.data.frame(matrix(NA, ncol = 4L, nrow = length(ages) * 16L))
  colnames(df) <- c("age", "quarter.age", "quarter.calendar", "time.exposed")

  df$age <- rep(ages, each = 16L)
  df$quarter.age <- rep(1L:4L, length(ages) * 4L)
  df$quarter.calendar <- rep(rep(1L:4L, length(ages)), each = 4L)
  df$time.exposed <- 0L
  df$ID <- paste(df$age, "_", df$quarter.age, "_", df$quarter.calendar, sep = "")

  for (k in 1L:dim(e.death)[1L]){

    time_rs <- FB(e.death$age[k], e.death$quarter.age[k],
                  e.death$quarter.calendar[k], e.death$number.events[k], type = type)
    time_rs$ID <- paste(time_rs$x, "_", time_rs$r, "_", time_rs$s, sep = "")

    for (j in 1L:dim(time_rs)[1L]){
      df[df$ID == time_rs$ID[j], ]$time.exposed <- df[df$ID == time_rs$ID[j], ]$time.exposed + time_rs$time.exposed[j]
    } # Next j

  } # Next k

  return(df[ ,1L:4L])

}
##.........................................

## Auxiliary functions for uniform_death

# modified modul 4 function
mod4 <- function (result){
  output <- ifelse(result%%4 == 0L, 4L, result%%4L)
  return(output)
}

# age with backward
age_b <- function(r_col, x){

  x_acum <- x_col <- x
  start <- 1L
  for (k in 2L:length(r_col)){

    if (r_col[k] == 4L & start == 1L & r_col[k - 1L] != 4L){
      x_acum <- x_acum - 1L
      x_col <- c(x_col, x_acum)
      start <- 0L
    } else{
      x_col <- c(x_col, x_acum)
    } # End if
  } # Next k

  return(x_col)

}

# age with forward
age_f <- function(r_col, x){

  x_acum <- x_col <- x
  start <- 1L
  for (k in 2L:length(r_col)){

    if (r_col[k] == 1L & start == 1L & r_col[k - 1L] != 1L){
      x_acum <- x_acum + 1L
      x_col <- c(x_col, x_acum)
      start <- 0L
    } else{
      x_col <- c(x_col, x_acum)
    } # End if
  } # Next k

  return(x_col)

}

# Backward column 1
B1 <- function(x, r, n){

  r_col <- c(r, mod4(r - 1L))
  s_col <- rep(1L, 2L)
  x_col <- age_b(r_col, x)
  time.value  <- c(3/8, 1/8)
  time.exposed <- time.value * n * 1/4
  df <- cbind.data.frame(x = x_col, r = r_col, s = s_col, time.exposed = time.exposed)
  df <- df[df$x >= 0L, ]

  return(df)
} # End of function

# Backward column 2
B2 <- function(x, r, n){

  r_col <- c(r, r, mod4(r - 1L), mod4(r - 1L), mod4(r - 2L))
  s_col <- c(2L , 1L, 2L, 1L, 1L)
  x_col <- age_b(r_col, x)
  time.value <- c(3/8, 1/8, 1/8, 6/8, 1/8)
  time.exposed <- time.value * n * 1/4
  df <- cbind.data.frame(x = x_col, r = r_col, s = s_col, time.exposed = time.exposed)
  df <- df[df$x >= 0L,]

  return(df)
} # End of function


# Backward column 3
B3 <- function(x, r, n){

  r_col <- c(r, r, rep(mod4(r - 1L), 3L), mod4 (r - 2L), mod4 (r - 2L), mod4 (r - 3L) )
  s_col <- c(3L, 2L, 2L, 3L, 1L, 1L, 2L, 1L)
  x_col <- age_b(r_col, x)
  time.value <- c(3/8, 1/8,  6/8, 1/8, 1/8, 6/8, 1/8, 1/8)
  time.exposed <- time.value * n * 1/4
  df <- cbind.data.frame(x = x_col, r = r_col, s = s_col, time.exposed = time.exposed)
  df <- df[df$x >= 0L,]

  return(df)
} # End of function

# Backward column 4
B4 <- function(x, r, n){
  r_col <- c(r, r, rep(mod4(r - 1L), 3L), rep(mod4(r - 2L), 3L), mod4(r - 3L), mod4(r - 3L), mod4(r - 4L))
  s_col <- c(4L, 3L, 3L, 4L, 2L, 2L, 3L, 1L, 1L, 2L, 1L)
  x_col <- age_b(r_col, x)
  time.value <- c(3/8, 1/8,  6/8, 1/8, 1/8, 6/8, 1/8, 1/8, 6/8, 1/8, 1/8)
  time.exposed <- time.value * n * 1/4
  df <- cbind.data.frame(x = x_col, r = r_col, s = s_col, time.exposed = time.exposed)
  df <- df[df$x >= 0L,]
  return(df)
} # End of function

# Forward column 4

F4 <- function(x, r, n){
  r_col <- c(r, mod4(r + 1L))
  s_col <- c(4L, 4L)
  x_col <- age_f(r_col, x)
  time.value  <- c(3/8, 1/8)
  time.exposed <- time.value * n * 1/4
  df <- cbind.data.frame(x = x_col, r = r_col, s = s_col, time.exposed = time.exposed)

  return(df)
} # End of function

# Forward column 3
F3 <- function(x, r, n){

  r_col <- c(r, r, mod4(r + 1L), mod4(r + 1L), mod4(r + 2L))
  s_col <- c(3L, 4L, 3L, 4L, 4L)
  x_col <- age_f(r_col, x)
  time.value  <- c(3/8, 1/8, 1/8, 6/8, 1/8)
  time.exposed <- time.value * n * 1/4
  df <- cbind.data.frame(x = x_col, r = r_col, s = s_col, time.exposed = time.exposed)

  return(df)
} # End of function

# Forward column 2
F2 <- function(x, r, n){

  r_col <- c(r, r, rep(mod4(r + 1L), 3L), mod4(r + 2L), mod4(r + 2L), mod4(r + 3L))
  s_col <- c(2L, 3L, 2L, 3L, 4L, 3L, 4L, 4L)
  x_col <- age_f(r_col, x)
  time.value  <- c(3/8, 1/8, 1/8, 6/8, 1/8, 1/8, 6/8, 1/8)
  time.exposed <- time.value * n * 1/4
  df <- cbind.data.frame(x = x_col, r = r_col, s = s_col, time.exposed = time.exposed)

  return(df)
} # End of function

# Forward column 1
F1 <- function(x, r, n){
  r_col <- c(r, r, rep(mod4(r + 1), 3L), rep(mod4(r + 2L), 3L),
             mod4(r + 3), mod4(r + 3), mod4(r + 4))
  s_col <- c(1L, 2L, 1L, 2L, 3L, 2L, 3L, 4L, 3L, 4L, 4L)
  x_col <- age_f(r_col, x)
  time.value  <- c(3/8, 1/8, 1/8, 6/8, 1/8, 1/8, 6/8, 1/8, 1/8, 6/8, 1/8)
  time.exposed <- time.value * n * 1/4
  df <- cbind.data.frame(x = x_col, r = r_col, s = s_col, time.exposed = time.exposed)

  return(df)
} # End of function

## Forward and Backward
FB <- function(x, r, s, n, type){

  if (type == "forward"){
    if (s == 1){
      output <- F1(x, r, n)
    } else if (s == 2){
      output <- F2(x, r, n)
    } else if (s == 3){
      output <- F3(x, r, n)
    } else {
      output <- F4(x, r, n)
    }
  } else{
    if (s == 1){
      output <- B1(x, r, n)
    } else if (s == 2){
      output <- B2(x, r, n)
    } else if (s == 3){
      output <- B3(x, r, n)
    } else {
      output <- B4(x, r, n)
    }

  }

  return(output)
} # End of function






