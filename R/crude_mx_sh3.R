#' Data frame(s) of crude estimates of quarterly (and annual) central rates of mortality estimated using shortcut 3.
#'
#' @description  Computes for a general/insured population crude estimates of quarterly (and annual)
#'               central rates of mortality by age (in a set of integer ages) for each combination
#'               of age and calendar quarter, using quarterly age aggregated data of population stocks and age-season
#'               quarterly counts of deaths, immigrants/new policies (entries) and emigrants/lapses/expirations (exits)
#'               by employing shortcut 3, based on equation (2.9), proposed in Pavia and Lledo (2023).
#'
#' @author Jose M. Pavia \email{pavia@@uv.es}
#' @author Josep Lledo \email{josep.lledo@@uv.es}
#' @references Pavia, JM and Lledo, J (2023). Shortcuts for the construction of sub-annual life tables. *ASTIN Bulletin: The Journal of the International Actuarial Association*, 53(2), 332-350. \doi{10.1017/asb.2023.16}
#'
#' @param pop.start A data frame, corresponding to the target population/portfolio, containing by age the stock of
#'                  population/portfolio by quarter collected at the beginning of the year. This data frame must
#'                  have three columns. The first refers to integer ages (e.g., 20, 21, 22,...),
#'                  the second to the quarter (with values 1, 2, 3 and 4) and the third to the number of people in
#'                  the population/portfolio at the beginning of the year corresponding to each integer age and quarter.
#' @param pop.end A data frame, corresponding to the target population/portfolio, containing by age the
#'                stock of population/portfolio by quarter collected at the end of the year. This data frame must
#'                have three columns. The first refers to integer ages (e.g., 20, 21, 22,...),
#'                the second to the quarter (with values 1, 2, 3 and 4) and the third to the number of people in
#'                the population/portfolio at the end of the year corresponding to each integer age and quarter.
#' @param events.death A data frame with the number of deaths recorded in the population/portfolio
#'                     during the target year in a set of integer ages for each combination of age
#'                     and season/calendar quarter. Typically, this is an output of the function
#'                     \code{\link{count_events_quarter}} or a data frame with the same structure.
#'                     If the range of ages in `events.death` does not cover the range of ages in the intersection of
#'                     ages defined by `pop.start` and `pop.end`, zeros are imputed for the missing ages.
#' @param events.out A data frame with the number of exits (emigrants/lapses/expirations) recorded in
#'                   the population/portfolio during the target year in a set of integer ages for each combination of
#'                   age and season/calendar quarter. Typically, this is an output of the function
#'                   \code{\link{count_events_quarter}} or a data frame with the same structure.
#'                   If the range of ages in `events.out` does not cover the range of ages in the intersection of
#'                   ages defined by `pop.start` and `pop.end`, zeros are imputed for the missing ages.
#' @param events.in A data frame with the number of entries (immigrants/new policies) recorded in
#'                  the population/portfolio during the target year in a set of integer ages for each combination of
#'                  age and season/calendar quarter. Typically, this is an output of the function
#'                  \code{\link{count_events_quarter}} or a data frame with the same structure.
#'                  If the range of ages in `events.in` does not cover the range of ages in the intersection of
#'                  ages defined by `pop.start` and `pop.end`, zeros are imputed for the missing ages.
#' @param annual A character string informing whether the annual crude central rates of mortality
#'               should also be computed. Default, `FALSE`.
#'
#' @return
#' When `annual = FALSE` a data frame with estimated crude central rates of mortality for each combination of age and
#' calendar quarter in the set of integer ages determined by the intersection of ages in `pop.start` and `pop.end`.
#' The data frame has the following components:
#'    \item{age}{ Integer age to which the crude central rate of mortality corresponds.}
#'    \item{quarter.age}{ Age quarter to which the crude central rate of mortality corresponds.}
#'    \item{quarter.calendar}{ Calendar (time, season) quarter to which the crude central rate of mortality corresponds.}
#'    \item{exposed}{ Total exposure-at-risk times in the target population, calulated according to shortcut 2, for each combination of `age`, `quarter.age` and `quarter.calendar`.}
#'    \item{deaths}{ Number of deaths in the target population for each  combination of `age`, `quarter.age` and `quarter.calendar`.}
#'    \item{mx}{ Estimated crude central rate of mortality corresponding to the combination of `age`, `quarter.age` and `quarter.calendar`.}
#' When `annual = TRUE` the output is a list with two data frames `mx.quarterly` and `mx.annual`.
#' `mx.quarterly` is a data frame with the estimated quarterly crude central rates of mortality as just described and
#' `mx.annual` is the corresponding data frame with the estimated annual crude central rates of mortality.
#'
#' @note
#' It is the responsibility of the user to assure that both stocks of population (which determine the ages for
#' which estimates are computed) and number of events correspond to the same year.
#'
#' @importFrom stats aggregate
#'
#' @seealso \code{\link{crude_mx}}, \code{\link{crude_mx_sh2}}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # This can take a while
#'  pop <- 2005 - as.numeric(substr(pop_2006$date.birth, 1 , 4))
#'  pop <- as.data.frame(table(pop))
#'  pop[, 1] <- as.numeric(as.character(pop[, 1]))
#'  pop[, 2] <- pop[, 2]/4
#'  pop <- cbind(apply(pop, 2, rep, each = 4), quarter = rep(1:4, nrow(pop)))[, c(1, 3, 2)]
#'  pop <- as.data.frame(pop)
#'  temp <- quarterly_variables(death_2006$date.birth, death_2006$date.death)
#'  e.death <- count_events_quarter(temp)
#'  temp <- quarterly_variables(emi_2006$date.birth, emi_2006$date.emi)
#'  e.emi <- count_events_quarter(temp)
#'  temp <- quarterly_variables(immi_2006$date.birth, immi_2006$date.immi)
#'  e.immi <- count_events_quarter(temp)
#'  out <- crude_mx_sh3(pop, pop, e.death, e.emi, e.immi)
#' }
#'
#' # Fast example
#' pop.1 <- data.frame(age = c(rep(40, 4), rep(41, 4)), quarter = rep(1:4, 2), people = c(4134, 4353))
#' pop.2 <- data.frame(age = c(rep(40, 4), rep(41, 4)), quarter = rep(1:4, 2), people = c(4250, 4213))
#' dates.b <- c("1980-04-12")
#' dates.d <- c("2020-08-23")
#' x <- quarterly_variables(dates.b, dates.d)
#' e.death <- count_events_quarter(x)
#' dates.b <- c("1980-05-12")
#' dates.e <- c("2020-06-23")
#' x <- quarterly_variables(dates.b, dates.e)
#' e.emi <- count_events_quarter(x)
#' dates.b <- c("1980-07-12")
#' dates.i <- c("2020-12-10")
#' x <- quarterly_variables(dates.b, dates.i)
#' e.immi <- count_events_quarter(x)
#' out <- crude_mx_sh3(pop.1, pop.2, e.death, e.emi, e.immi)

crude_mx_sh3 <- function(pop.start,
                         pop.end,
                         events.death,
                         events.out,
                         events.in,
                         annual = FALSE){

  # Checks
  check_pop_3(pop.start)
  check_pop_3(pop.end)
  names(pop.start) <- names(pop.end) <- c("age", "quarter", "pop")
  check_df(events.death)
  check_df(events.in)
  check_df(events.out)
  names(events.death) <- names(events.in) <- names(events.out) <- c("age", "quarter.age", "quarter.calendar", "number.events")

  pop.start <- pop.start[order(pop.start$age, pop.start$quarter), ]
  pop.end <- pop.end[order(pop.end$age, pop.end$quarter), ]

  # Application of equation (2.9)
  t.mx <- time_exposed_sh3(start = pop.start, end = pop.end, deaths = events.death,
                           outs = events.out, ins = events.in)

  # Estimation of crude mx
  t.mx$mx <- t.mx$deaths/t.mx$exposed
  t.mx$mx[is.nan(t.mx$mx)] <- 0L # 0/0
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


## Auxiliary functions
check_pop_3 <-function(x){
  if (length(dim(x)) != 2L)
    stop("Error. At least one of the data frames of population/portfolio does not have the proper structure.")

  if (ncol(x) != 3L)
    stop("Error. At least one of the data frames of population/portfolio does not have the proper structure.")

  if (!all(apply(x, 2L, class) %in% c("numeric", "integer")))
    stop("Error. The class of at least one of columns of the introduced data frames of stocks of population/portfolio is not numeric.")

  if (any(x < 0L))
    stop("Error. At least one values in data frames of stocks of population/portfolio is negative.")

  if (max(abs(x[, 1L] - round(x[, 1L]))) > 0L)
    stop("Error. At least one ages in data frames of stocks of population/portfolio is not an integer.")

  if (max(x[, 2L]) > 4L | min(x[, 2L]) < 1L)
    stop("Error. At least one of he introduced data frames have a quarter higher than 4 or smaller than 1.")

  if (max(abs(x[, 2L] - round(x[, 2L]))) > 0L)
    stop("Error. At least one quarter in data frames of stocks of population/portfolio is not an integer.")

  if ((nrow(x)%%4) != 0L)
    stop("Error. At least one of the data frames of population/portfolio has an age with missing quarters.")

  if (!is.data.frame(x))
    stop("Error. At least one of the objects introduced as a data.frame is not of the data.frame class.")
}


##.........................................
time_exposed_sh3 <- function(start, end, deaths, outs, ins){

  # Pre-processing
  start.a <- stats::aggregate(pop ~ age, start[, -2L], FUN = sum)
  end.a <- stats::aggregate(pop ~ age, end[, -2L], FUN = sum)
  start.a <- start.a[start.a$pop != 0L, ]
  end.a <- end.a[end.a$pop != 0L, ]
  ages <- intersect(unique(start.a$age), unique(end.a$age))
  if(length(ages) == 0L)
    stop("Error. The intersection of ages with non-null values between `pop.start` and `pop.end` is empty.")
  start <- start[start$age %in% c(min(ages) - 1L, ages), ]
  end <- end[end$age %in% c(ages, max(ages) + 1L), ]
  deaths <- complete_deaths(deaths, ages)
  deaths <- deaths[deaths$age %in% ages, ]
  deaths <- deaths[order(deaths$age, deaths$quarter.age, deaths$quarter.calendar), ]
  outs <- complete_deaths(outs, ages)
  outs <- outs[outs$age %in% ages, ]
  outs <- outs[order(outs$age, outs$quarter.age, outs$quarter.calendar), ]
  ins <- complete_deaths(ins, ages)
  ins <- ins[deaths$age %in% ages, ]
  ins <- ins[order(ins$age, ins$quarter.age, ins$quarter.calendar), ]

  # object for exposition time
  age <- rep(ages, each = 16L)
  quarter.calendar <- rep(1L:4L, length(ages) * 4L)
  quarter.age <- rep(rep(1L:4L, length(ages)), each = 4L)
  df.time.exposed <- cbind.data.frame(age, quarter.age, quarter.calendar)
  df.time.exposed$deaths <- df.time.exposed$exposed <- NA

  # Computation of exposition time
  # First, We add auxiliary zeros in the data.frames to simplify the use of equation (2.9)
  # in Pavia and Lledo (2023).
  # start
  completar <- setdiff((min(ages) - 1L):max(ages), start$age)
  if (length(completar) != 0L){
    age <- rep(completar, each = 4L)
    quarter <- rep(1L:4L, length(completar))
    start <- rbind(data.frame(age, quarter, pop = 0), start)
  }
  # end
  completar <- setdiff(min(ages):(max(ages) + 1L), end$age)
  if (length(completar) != 0L){
    age <- rep(completar, each = 4L)
    quarter <- rep(1L:4L, length(completar))
    end <- rbind(data.frame(age, quarter, pop = 0), end)
  }
  #deaths
  completar <- setdiff((min(ages) - 1L):(max(ages) + 1L), deaths$age)
  age <- rep(completar, each = 16L)
  quarter.age <- rep(1L:4L, length(completar) * 4L)
  quarter.calendar <- rep(rep(1L:4L, length(completar)), each = 4L)
  number.events <- rep(0, length(completar)*16L)
  df <- cbind.data.frame(age, quarter.age, quarter.calendar, number.events)
  deaths <- rbind(deaths, df)
  # outs
  completar <- setdiff((min(ages) - 1L):(max(ages) + 1L), outs$age)
  age <- rep(completar, each = 16L)
  quarter.age <- rep(1L:4L, length(completar) * 4L)
  quarter.calendar <- rep(rep(1L:4L, length(completar)), each = 4L)
  number.events <- rep(0, length(completar)*16L)
  df <- cbind.data.frame(age, quarter.age, quarter.calendar, number.events)
  outs <- rbind(outs, df)
  # ins
  completar <- setdiff((min(ages) - 1L):(max(ages) + 1L), ins$age)
  age <- rep(completar, each = 16L)
  quarter.age <- rep(1L:4L, length(completar) * 4L)
  quarter.calendar <- rep(rep(1L:4L, length(completar)), each = 4L)
  number.events <- rep(0, length(completar)*16L)
  df <- cbind.data.frame(age, quarter.age, quarter.calendar, number.events)
  ins <- rbind(ins, df)

  # Application of equation (2.9)
  for (ii in 1L:nrow(df.time.exposed)){
    s <- df.time.exposed$quarter.calendar[ii]
    r <- df.time.exposed$quarter.age[ii]
    a <- df.time.exposed$age[ii]
    df.time.exposed$deaths[ii] <- deaths$number.events[deaths$age == a & deaths$quarter.age == r & deaths$quarter.calendar == s]
    s.r <- as.numeric((s - r) > 0L)
    r.s <- as.numeric((r - s) > 0L)
    # time exposed stocks
    r0 <- mod4(r - s + 1L)
    r1 <- mod4(r - s)
    pob.0 <- sum(start$pop[start$age == (a - s.r)])
    pob.1 <- sum(end$pop[end$age == (a + r.s)])
    S0 <- (1/2) * start$pop[start$age == (a - s.r) & start$quarter == r0]
    S1 <- (1/2) * end$pop[end$age == (a + r.s) & end$quarter == r1]
    if(r > s & pob.1 == 0){
      S0 <- start$pop[start$age == a & start$quarter == r0] # Special treatment ending years
      S1 <- 0
    }
    if (s > r & pob.0 == 0){
      S0 <- 0
      S1 <- end$pop[end$age == a & end$quarter == r1] # Special treatment initial years
    }
    ST <- S0 + S1

    # Correction time exposed by deaths, outs and ins
    corrector.0 <- corrector.1 <- 1
    if (r > s & pob.1 == 0L) corrector.1 <- 0.5 # Special treatment initial years
    if (s > r & pob.0 == 0L) corrector.0 <- 0.5 # Special treatment ending years

    if (s > 1L){
      for (k in 1L:(s - 1L)){
        ss <- mod4(s - k)
        rr <- mod4(r - k)
        aa <- a - (r - k < 1)
        dd <- deaths$number.events[deaths$age == aa & deaths$quarter.age == rr & deaths$quarter.calendar == ss]
        oo <- outs$number.events[outs$age == aa & outs$quarter.age == rr & outs$quarter.calendar == ss]
        nn <- ins$number.events[ins$age == aa & ins$quarter.age == rr & ins$quarter.calendar == ss]
        ST <- ST - corrector.1 * 0.5 * (dd - nn + oo)
      } # for k: s > 1
    } # if s > 1
    if (s < 4L){
      for (k in 1L:(4L - s)){
        ss <- mod4(s + k)
        rr <- mod4(r + k)
        aa <- a + (r + k > 4)
        dd <- deaths$number.events[deaths$age == aa & deaths$quarter.age == rr & deaths$quarter.calendar == ss]
        oo <- outs$number.events[outs$age == aa & outs$quarter.age == rr & outs$quarter.calendar == ss]
        nn <- ins$number.events[ins$age == aa & ins$quarter.age == rr & ins$quarter.calendar == ss]
        ST <- ST + corrector.0 * 0.5 * (dd - nn + oo)
      } # for k: s < 4
    }  # if s < 4
    df.time.exposed$exposed[ii] <- 0.25 * ST
  } # for x, r, s

  return(df.time.exposed)
}
