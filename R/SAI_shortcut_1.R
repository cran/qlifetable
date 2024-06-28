#' Estimates seasonal-ageing indexes using shortcut 1.
#'
#' @description  Given a set of data frames with the number of deaths recorded in a population/portfolio
#'               during several years (periods) in a set of integer ages for each combination of age and
#'               season/calendar quarter, this function approximates their corresponding seasonal-ageing indexes
#'               by employing shortcut 1, based on equation (2.5), proposed in Pavia and Lledo (2023).
#'
#' @author Jose M. Pavia \email{pavia@@uv.es}
#' @author Josep Lledo \email{josep.lledo@@uv.es}
#' @references Pavia, JM and Lledo, J (2023). Shortcuts for the construction of sub-annual life tables. *ASTIN Bulletin: The Journal of the International Actuarial Association*, 53(2), 332-350. \doi{10.1017/asb.2023.16}
#'
#' @param x A data frame with the number of deaths recorded in the population/portfolio during a period (typically a year)
#'          in a set of integer ages for each combination of age and season/calendar quarter. Usually, this is an output
#'          of the \code{\link{count_events_quarter}} function (or a data frame with the same structure).
#' @param y A data frame with the number of deaths recorded in the population/portfolio during another period
#'          (typically a different year than the one corresponding to `x`) in a set of integer ages for each
#'          combination of age and season/calendar quarter. Usually, this is an output
#'          of the \code{\link{count_events_quarter}} function (or a data frame with the same structure).
#' @param ... Further data frames similar to `x` and `y` corresponding to another period(s)/year(s).
#' @param margins A `TRUE/FALSE` argument informing whether or not the marginal seasonal and ageing indexes should be also
#'                computed. Default, `FALSE`.
#' @param min.age A non-negative integer informing about the initial minimal age for which SAIs must be computed. This minimum age can
#'                be increased depending on the ages for which there are crude mx estimates (exposed-at-risk) in the objects  `x`, `y`, `...`. Default, 0.
#' @param max.age A positive integer informing about the initial maximum age for which SAIs must be computed. This maximum age can
#'                be decreased depending on the maximum ages for which there are crude mx estimates (exposed-at-risk) in the objects  `x`, `y`, `...`.
#'                Default, 100.
#'
#' @return
#' When `margins = FALSE` a data frame with the (raw, normalized and linearized) estimated seasonal-ageing
#' indexes (SAI) corresponding to the set of integer ages determined by `min.age` and `max.age` and the ages
#' for which there is a least a death in the data frames introduced via the `x`, `y` and `...` arguments for
#' each combination of age and calendar quarter. The data frame has the following components:
#'    \item{age}{ Integer age to which the SAIs corresponds.}
#'    \item{quarter.age}{ Age quarter to which the SAIs corresponds.}
#'    \item{quarter.calendar}{ Calendar (time, season) quarter to which the SAIs corresponds.}
#'    \item{SAI.raw}{ Estimates of raw seasonal-ageing indexes for each combination of `age`, `quarter.age` and `quarter.calendar`.}
#'    \item{SAI.norm}{ Estimates of seasonal-ageing indexes, attained after normalizing raw SAIs estimates, for each  combination of `age`, `quarter.age` and `quarter.calendar`.}
#'    \item{SAI.lin}{ Final estimates of seasonal-ageing indexes, attained after linearizing normalized SAIs estimates,  for each combination of `age`, `quarter.age` and `quarter.calendar`.}
#' When `margins = TRUE` the output is a list with three data frames `SAI`, `SAI.age` and `SAI.quarter`.
#' `SAI` is defined as just described above. `SAI.age` and  `SAI.quarter` contains, in a similar vein than `SAI`,
#' the estimated marginal SAIs, corresponding to, respectively, the age quarter and the calendar (time, season)
#' quarter.
#'
#' @importFrom stats aggregate lm predict
#'
#' @export
#'
#' @examples
#' \donttest{
#' # This can take a while
#'  temp <- quarterly_variables(death_2006$date.birth, death_2006$date.death)
#'  e.death <- count_events_quarter(temp)
#'  SAI.example <- SAI_shortcut_1(e.death, e.death)
#' }
#'
#' # Fast example
#' dates.b <- c("2017-05-13", "2018-04-12", "2018-01-01")
#' dates.d <- c("2020-09-23", "2021-10-11", "2021-11-23")
#' x <- quarterly_variables(dates.b, dates.d)
#' e.death <- count_events_quarter(x)
#' SAI.example <- SAI_shortcut_1(e.death, e.death)

SAI_shortcut_1 <- function(x, y, ..., margins = FALSE, min.age = 0, max.age = 100){

  argg <- c(as.list(environment()), list(...))
  tablas <- check_SAI_sh1(argg)

  # check and construction of death quarterly and annual tables
  tablas <- q_a_sh1(tablas)
  tablas_q <- tablas$tablas_q
  tablas_a <- tablas$tablas_a

  # min and max ages
  min.age <- min_age_sh1(min.age, tablas_a)
  max.age <- max_age_sh1(max.age, tablas_a)
  if (min.age > max.age)
    stop("Error. The intersection of the sets of ages for which there are exposed-at-risk in all the tables is empty.")

  # SAIs crude and raw
  SAI.long <- SAI_raw_rs(tablas_q = tablas_q, tablas_a = tablas_a,
                          min.age = min.age, max.age = max.age)

  SAI.lin <- SAI.long$SAI.raw.rs
  # Linearization
  for (r in 1L:4L){
    for (s in 1L:4L){
      selec <- SAI.long$quarter.age == r & SAI.long$quarter.calendar == s & SAI.long$age != 0
      pred <- rep(NA, sum(selec))
      if (sum(!is.na(SAI.long$SAI.raw.rs[selec])) != 0L){
        fit.SAI <- stats::lm(SAI.long$SAI.raw.rs[selec] ~ SAI.long$age[selec])
        pred[!is.na(SAI.long$SAI.raw.rs[selec])] <- stats::predict(fit.SAI)
      }
      SAI.lin[selec] <- pred
    }
  }
  SAI.long$SAI <- SAI.lin
  names(SAI.long) <- c("age", "quarter.age", "quarter.calendar", "SAI.raw", "SAI.norm", "SAI.lin")

  output <- SAI.long
  class(output) <- c("SAI", "data.frame")

  # Margins
  if (margins){
    SAI.margins <- SAI_margins_sh1(tablas_q = tablas_q, tablas_a = tablas_a,
                                   min.age = min.age, max.age = max.age)

    SAI.long.r <- SAI.margins$SAI.r
    SAI.long.s <- SAI.margins$SAI.s
    output <- list("SAI" = SAI.long, "SAI.age" = SAI.long.r, "SAI.calendar" = SAI.long.s)
    class(output) <- c("SAI", "list")
  }

  return(output)

}


############################################################################################
### Auxiliary functions of function SAI_shortcut_1
## Checking inputs
check_SAI_sh1 <- function(argg){
  ll <- length(argg)
  if (argg$min.age < 0 | (argg$min.age - floor(argg$min.age)) > 0)
    stop("The argument 'min.age' should be a non-negative integer")
  if (argg$max.age < 1 | (argg$max.age - floor(argg$max.age)) > 0)
    stop("The argument 'max.age' should be a positive integer")
  if ((argg$y == "")[1L]){
    warning("Only a data frame of deaths events has been declared. SAIs are just computed by division. No modelization is performed to gain strength using several periods. This does not corresponds to the recommended methodology.")
    ll <- 1L
  } else {
    ll <- (1L:ll)[-c(3:5)]
  }
  for (i in ll){
    check_df(argg[[i]])
  }
  argg <- argg[ll]
  return(argg)
}

## Construction of death quarterly and annual tables
q_a_sh1 <- function(tablas){
  ll <- length(tablas)
  tablas_q <- tablas_a <- list()
  for (i in 1L:ll){
    class(tablas[[i]]) <- "data.frame"
    names(tablas[[i]]) <- c("age", "quarter.age", "quarter.calendar", "number.events")
    tablas_q[[i]] <- tablas[[i]]
    t.mx.a <- stats::aggregate(number.events ~ age, data = tablas[[i]], FUN = sum)
    tablas_a[[i]] <- t.mx.a
    names(tablas_q[[i]])[4L] <- "mx_q"
    names(tablas_a[[i]])[2L] <- "mx_a"
    tablas_q[[i]]$mx_q <- 16*tablas_q[[i]]$mx_q
  }
  return(list("tablas_q" = tablas_q, "tablas_a" = tablas_a))
}

min_age_sh1 <- function(min.age, tablas_a){
  ll <- length(tablas_a)
  for (i in 1L:ll){
    min.age <- max(c(min(tablas_a[[i]]$age[tablas_a[[i]]$mx_a != 0]), min.age))
  }
  return(min.age)
}

max_age_sh1 <- function(max.age, tablas_a){
  ll <- length(tablas_a)
  for (i in 1L:ll){
    max.age <- min(c(max(tablas_a[[i]]$age[tablas_a[[i]]$mx_a != 0]), max.age))
  }
  return(max.age)
}


tablas_margin_sh1 <- function(tablas_q, type){
  ll <- length(tablas_q)
  tablas.q.margin <- tablas_q
  if (type == "r"){
    for (i in 1L:ll){
      tablas.q.margin[[i]] <- stats::aggregate(mx_q ~ age + quarter.age,
                                               tablas_q[[i]], FUN = sum)
      tablas.q.margin[[i]] <- tablas.q.margin[[i]][order(tablas.q.margin[[i]]$age), ]
    }
  }
  if (type == "s"){
    for (i in 1L:ll){
      tablas.q.margin[[i]] <- stats::aggregate(mx_q ~ age + quarter.calendar,
                                               tablas_q[[i]], FUN = sum)
      tablas.q.margin[[i]] <- tablas.q.margin[[i]][order(tablas.q.margin[[i]]$age), ]
    }
  }
  return(tablas.q.margin)
}


SAI_margins_sh1 <- function(tablas_q, tablas_a, min.age, max.age){
  tablas.q.r <- tablas_margin_sh1(tablas_q, type = "r")
  tablas.q.s <- tablas_margin_sh1(tablas_q, type = "s")

  ll <- length(tablas_q)
  selec <- tablas.q.r[[1L]]$age >= min.age & tablas.q.r[[1L]]$age <= max.age
  # marging ageing
  SAI.r <- tablas.q.r[[1L]][selec , 1L:2L]
  orden <- order(SAI.r$age, SAI.r$quarter.age)
  SAI.r <- SAI.r[orden, ]
  # marging calendar
  SAI.s <- tablas.q.s[[1L]][selec , 1L:2L]
  SAI.s <- SAI.s[orden, ]

  log.coc.r <- log.coc.s <- NULL
  for (i in 1L:ll){
    selec <- tablas.q.r[[i]]$age >= min.age & tablas.q.r[[i]]$age <= max.age

    # marging ageing
    tablas.q.r[[i]] <- tablas.q.r[[i]][selec, ]
    temp <- merge(tablas.q.r[[i]], tablas_a[[i]], by = "age")
    orden <- order(temp$age, temp$quarter.age)
    temp <- temp[orden, ]
    Rel <- log(temp$mx_q / temp$mx_a)
    Rel[is.infinite(Rel) | is.nan(Rel)] <- -3.5
    log.coc.r <- cbind(log.coc.r, Rel)

    # marging calendar
    tablas.q.s[[i]] <- tablas.q.s[[i]][selec, ]
    temp <- merge(tablas.q.s[[i]], tablas_a[[i]], by = "age")
    orden <- order(temp$age, temp$quarter.calendar)
    temp <- temp[orden, ]
    Rel <- log(temp$mx_q / temp$mx_a)
    Rel[is.infinite(Rel) | is.nan(Rel)] <- -3.5
    log.coc.s <- cbind(log.coc.s, Rel)
  }

  # Computation of raw SAIs
  geom_mean <- function(x) return(exp(mean(as.matrix(x))))
  SAI.r.raw <- SAI.r.raw.rs <- apply(log.coc.r, 1L, geom_mean)
  SAI.s.raw <- SAI.s.raw.rs <- apply(log.coc.s, 1L, geom_mean)
  # Computation of net SAIS
  for (i in min.age:max.age){
    ii <- i - min.age
    temp <- SAI.r.raw[(1L + (ii * 4L)):(4L + (ii * 4L))]
    SAI.r.raw.rs[(1L + (ii * 4L)):(4L + (ii * 4L))] <- 4*temp/sum(temp)
    temp <- SAI.s.raw[(1L + (ii * 4L)):(4L + (ii * 4L))]
    SAI.s.raw.rs[(1L + (ii * 4L)):(4L + (ii * 4L))] <- 4*temp/sum(temp)
  }
  SAI.r <- cbind(SAI.r, SAI.r.raw, SAI.r.raw.rs)
  SAI.s <- cbind(SAI.s, SAI.s.raw, SAI.s.raw.rs)

  SAI.r.lin <- SAI.r$SAI.r.raw.rs
  SAI.s.lin <- SAI.s$SAI.s.raw.rs
  # Linearization
  for (i in 1L:4L){
    selec <- SAI.r$quarter.age == i & SAI.r$age != 0
    fit.SAI <- stats::lm(SAI.r$SAI.r.raw.rs[selec] ~ SAI.r$age[selec])
    SAI.r.lin[selec] <- stats::predict(fit.SAI)
    selec <- SAI.s$quarter.calendar == i & SAI.s$age != 0
    fit.SAI <- stats::lm(SAI.s$SAI.s.raw.rs[selec] ~ SAI.s$age[selec])
    SAI.s.lin[selec] <- stats::predict(fit.SAI)
  }
  SAI.r$SAI <- SAI.r.lin
  SAI.s$SAI <- SAI.s.lin
  names(SAI.r) <- c("age", "quarter.age", "SAI.raw", "SAI.norm", "SAI.lin")
  names(SAI.s) <- c("age", "quarter.calendar", "SAI.raw", "SAI.norm", "SAI.lin")

  return(list("SAI.r" = SAI.r, "SAI.s" = SAI.s))
}

