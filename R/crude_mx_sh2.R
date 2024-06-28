#' Data frame(s) of crude estimates of quarterly (and annual) central rates of mortality estimated using shortcut 2.
#'
#' @description  Computes for a general/insured population crude estimates of quarterly (and annual)
#'               central rates of mortality by age (in a set of integer ages) for each combination
#'               of age and calendar quarter, using age aggregated data of population stocks and age-season
#'               quarterly counts of deaths by employing shortcut 2, based on equation (2.7),
#'               proposed in Pavia and Lledo (2023).
#'
#' @author Jose M. Pavia \email{pavia@@uv.es}
#' @author Josep Lledo \email{josep.lledo@@uv.es}
#' @references Pavia, JM and Lledo, J (2023). Shortcuts for the construction of sub-annual life tables. *ASTIN Bulletin: The Journal of the International Actuarial Association*, 53(2), 332-350. \doi{10.1017/asb.2023.16}
#'
#' @param pop.start A data frame, corresponding to the target population/portfolio, containing the stock of
#'                  population/portfolio by age collected at the beginning of the year. This data frame must have two columns.
#'                  The first column refers to age and the second to the number of people in the population/portfolio
#'                  corresponding to each age.
#' @param pop.end A data frame, corresponding to the target population/portfolio, containing the stock of
#'                population/portfolio by age collected at the end of the year. This data frame must have two columns.
#'                The first column refers to age and the second to the number of people in the population/portfolio
#'                corresponding to each age.
#' @param events.death  A data frame with the number of deaths recorded in the population/portfolio
#'                      during the target year in a set of integer ages for each combination of age
#'                      and season/calendar quarter. Typically, this is an output of the function
#'                      \code{\link{count_events_quarter}} or a data frame with the same structure.
#'                      If the range of ages in `events.death` does not cover the range of ages in the intersection of
#'                      ages defined by `pop.start` and `pop.end`, zeros are imputed for the missing ages.
#' @param annual A character string informing whether the annual crude central rates of mortality
#'               should also be computed. Default, `FALSE`.
#'
#' @return
#' When `annual = FALSE` a data frame with estimated crude central rates of mortality for each combination of age and
#' calendar quarter in the set of integer ages determined by the intersection of ages of `pop.start` and `pop.end`.
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
#' @seealso \code{\link{crude_mx}}, \code{\link{crude_mx_sh3}}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # This can take a while
#'  pop <- 2005 - as.numeric(substr(pop_2006$date.birth, 1 , 4))
#'  pop <- as.data.frame(table(pop))
#'  pop[, 1] <- as.numeric(as.character(pop[, 1]))
#'  temp <- quarterly_variables(death_2006$date.birth, death_2006$date.death)
#'  e.death <- count_events_quarter(temp)
#'  out <- crude_mx_sh2(pop, pop, e.death)
#' }
#'
#' # Fast example
#' pop.1 <- data.frame(age = c(40, 41), people = c(4134, 4353))
#' pop.2 <- data.frame(age = c(40, 41), people = c(4250, 4213))
#' dates.b <- c("1980-04-12")
#' dates.d <- c("2020-08-23")
#' x <- quarterly_variables(dates.b, dates.d)
#' e.death <- count_events_quarter(x)
#' out <- crude_mx_sh2(pop.1, pop.2, e.death)

crude_mx_sh2 <- function(pop.start,
                         pop.end,
                         events.death,
                         annual = FALSE){

  # Checks
  check_pop(pop.start)
  check_pop(pop.end)
  check_df(events.death)

  names(pop.start) <- names(pop.end) <- c("age", "pop")
  names(events.death) <- c("age", "quarter.age", "quarter.calendar", "number.events")
  pop.start <- pop.start[order(pop.start$age), ]
  pop.end <- pop.end[order(pop.end$age), ]

  # Application of equation (2.7)
  t.mx <- time_exposed_sh2(start = pop.start, end = pop.end, deaths = events.death)

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


#######################
# Auxiliary functions #
#######################

check_pop <-function(x){
  if (length(dim(x)) != 2L)
     stop("Error. At least one of the stocks of population/portfolio does not have the proper structure.")

  if (ncol(x) != 2L)
    stop("Error. At least one of the stocks of population/portfolio does not have the proper structure.")

  if (!all(apply(x, 2L, class) %in% c("numeric", "integer")))
    stop("Error. The class of at least one of columns of the introduced data frames of stocks of population/portfolio is not numeric.")

  if (any(x < 0L))
    stop("Error. At least one values in data frames of stocks of population/portfolio is negative.")

  if (max(abs(x[, 1L] - round(x[, 1L]))) > 0L)
    stop("Error. At least one ages in data frames of stocks of population/portfolio is not an integer.")

  if (!is.data.frame(x))
    stop("Error. At least one of the objects introduced as a data.frame is not of the data.frame class.")
}

##.........................................
time_exposed_sh2 <- function(start, end, deaths){
  # Pre-processing
  start <- start[start$pop != 0L, ]
  end <- end[end$pop != 0L, ]
  ages <- intersect(unique(start$age), unique(end$age))
  if(length(ages) == 0L)
    stop("Error. The intersection of ages with non-null values between `pop.start` and `pop.end` is empty.")
  start <- start[start$age %in% c(min(ages) - 1L, ages), ]
  end <- end[end$age %in% c(ages, max(ages) + 1L), ]
  deaths <- complete_deaths(deaths, ages)
  deaths <- deaths[deaths$age %in% ages, ]
  deaths <- deaths[order(deaths$age, deaths$quarter.age, deaths$quarter.calendar), ]

  # object for time of exposition
  age <- rep(ages, each = 16L)
  quarter.calendar <- rep(1L:4L, length(ages) * 4L)
  quarter.age <- rep(rep(1L:4L, length(ages)), each = 4L)
  df.time.exposed <- cbind.data.frame(age, quarter.age, quarter.calendar)
  df.time.exposed$deaths <- df.time.exposed$exposed <- NA

  # Computation of time exposition

  # First, We add auxiliary zeros in the data.frames to simplify the use of equation (2.7)
  # in Pavia and Lledo (2023).
  completar <- setdiff((min(ages) - 1L):max(ages), start$age)
  if (length(completar) != 0L)
     start <- rbind(data.frame(age = completar, pop = 0), start)
  completar <- setdiff(min(ages):(max(ages) + 1L), end$age)
  if (length(completar) != 0L)
    end <- rbind(data.frame(age = completar, pop = 0), end)
  completar <- setdiff((min(ages) - 1L):(max(ages) + 1L), deaths$age)
  age <- rep(completar, each = 16L)
  quarter.age <- rep(1L:4L, length(completar) * 4L)
  quarter.calendar <- rep(rep(1L:4L, length(completar)), each = 4L)
  number.events <- rep(0, length(completar)*16L)
  df <- cbind.data.frame(age, quarter.age, quarter.calendar, number.events)
  deaths <- rbind(deaths, df)

  # Application of equation (2.7)
  for (ii in 1L:nrow(df.time.exposed)){
    s <- df.time.exposed$quarter.calendar[ii]
    r <- df.time.exposed$quarter.age[ii]
    a <- df.time.exposed$age[ii]
    df.time.exposed$deaths[ii] <- deaths$number.events[deaths$age == a & deaths$quarter.age == r & deaths$quarter.calendar == s]
    s.r <- as.numeric((s - r) > 0L)
    r.s <- as.numeric((r - s) > 0L)

    # time exposed stocks
    pob.0 <- start$pop[start$age == (a - s.r)]
    pob.1 <- end$pop[end$age == (a + r.s)]
    S0 <- (1/8) * start$pop[start$age == (a - s.r)]
    S1 <- (1/8) * end$pop[end$age == (a + r.s)]
    if(r > s & pob.1 == 0){
      S0 <- (1/4) * start$pop[start$age == a] # Special treatment ending years
      S1 <- 0
    }
    if (s > r & pob.0 == 0){
      S0 <- 0
      S1 <- (1/4) * end$pop[end$age == a] # Special treatment initial years
    }
    ST <- S0 + S1
    # Correction time exposed deaths
    corrector.0 <- corrector.1 <- 1
    if (r > s & pob.1 == 0L) corrector.1 <- 0.5 # Special treatment initial years
    if (s > r & pob.0 == 0L) corrector.0 <- 0.5 # Special treatment ending years

    if (s > 1L){
      for (k in 1L:(s - 1L)){
        ss <- mod4(s - k)
        rr <- mod4(r - k)
        aa <- a - (r - k < 1)
        dd <- deaths$number.events[deaths$age == aa & deaths$quarter.age == rr & deaths$quarter.calendar == ss]
        ST <- ST - corrector.1 * 0.5 * dd
      } # for k: s > 1
    } # if s > 1
      if (s < 4L){
        for (k in 1L:(4L - s)){
          ss <- mod4(s + k)
          rr <- mod4(r + k)
          aa <- a + (r + k > 4)
          dd <- deaths$number.events[deaths$age == aa & deaths$quarter.age == rr & deaths$quarter.calendar == ss]
          ST <- ST + corrector.0 * 0.5 * dd
        } # for k: s < 4
      }  # if s < 4
    df.time.exposed$exposed[ii] <- 0.25 * ST
  } # for x, r, s

  return(df.time.exposed)
}

complete_deaths <- function(deaths, ages){
  falta <- setdiff(ages, unique(deaths$age))
  if(length(falta) == 0L){
    return(deaths)
  } else {
    age <- rep(falta, each = 16L)
    quarter.age <- rep(1L:4L, length(falta) * 4L)
    quarter.calendar <- rep(rep(1L:4L, length(falta)), each = 4L)
    df <- cbind.data.frame(age, quarter.age, quarter.calendar)
    df$number.events <- 0L
    deaths <- rbind(deaths, df)
    return(deaths)
  }
}

