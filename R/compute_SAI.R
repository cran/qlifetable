#' Estimates seasonal-ageing indexes from  quarterly tables of crude rates estimates.
#'
#' @description  Given a set of quarterly tables of crude rates estimates corresponding to several years (periods),
#'               this function computes their corresponding seasonal-ageing indexes.
#'
#' @author Jose M. Pavia \email{pavia@@uv.es}
#' @author Josep Lledo \email{josep.lledo@@uv.es}
#' @references Pavia, JM and Lledo, J (2022). Estimation of the Combined Effects of Ageing and Seasonality on Mortality Risk. An application to Spain. *Journal of the Royal Statistical Society, Series A (Statistics in Society)*, 185(2), 471-497. \doi{10.1111/rssa.12769}
#'
#' @param x A data.frame/list output of either \code{\link{crude_mx}}, \code{\link{crude_mx_sh2}} or \code{\link{crude_mx_sh3}} functions corresponding to a period (typically a year).
#' @param y A data.frame/list output of either \code{\link{crude_mx}}, \code{\link{crude_mx_sh2}} or \code{\link{crude_mx_sh3}} functions corresponding to another period (typically a different year than the one corresponding to `x`).
#' @param ... Further output(s) of either \code{\link{crude_mx}}, \code{\link{crude_mx_sh2}} or \code{\link{crude_mx_sh3}} functions corresponding to another period(s) (typically different year(s) than the ones corresponding to `x` and `y`).
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
#' indexes (SAI) corresponding to the set of integer ages determined by `min.age` and `max.age` and the exposed-at-risk
#' in the mx crude estimates for each combination of age and calendar quarter.
#' The data frame has the following components:
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
#'  t.stock <- time_exposed_stock(pop_2006$date.birth, 2006, "forward")
#'  t.stock <- t.stock[t.stock$age <= 100, ]
#'  temp <- quarterly_variables(death_2006$date.birth, death_2006$date.death)
#'  e.death <- count_events_quarter(temp)
#'  e.death <- e.death[e.death$age <= 100, ]
#'  t.birth <- time_exposed_newborns(birth_2006$date.birth)
#'  out <- crude_mx(t.stock, e.death, time.birth = t.birth)
#'  SAI.example <- compute_SAI(out, out)
#' }
#'
#' # Fast example
#' dates.b <- c("2017-05-13", "2018-04-12", "2018-12-01")
#' t.stock <- time_exposed_stock(dates.b, year = 2020, type = "backward")
#' dates.bd <- c("2018-04-12")
#' dates.d <- c("2020-05-23")
#' x <- quarterly_variables(dates.bd, dates.d)
#' e.death <- count_events_quarter(x)
#' t.death <- time_exposed_outs(x)
#' out <- crude_mx(t.stock, e.death, t.death)
#' SAI.example <- compute_SAI(out, out)

compute_SAI <- function(x, y, ..., margins = FALSE, min.age = 0, max.age = 100){

  argg <- c(as.list(environment()), list(...))

  # check and extract of quarterly and annual tables
  tablas <- check_SAI(argg)
  tablas <- dec_q_a(tablas)
  tablas_q <- tablas$tablas_q
  tablas_a <- tablas$tablas_a

  # min and max ages
  min.age <- min_age(min.age, tablas_a)
  max.age <- max_age(max.age, tablas_a)
  if (min.age > max.age)
    stop("Error. The intersection of the sets of ages for which there are exposed-at-risk in all the tables is empty.")

  # Complete the tables in missing intermediate ages
  ll <- length(tablas_q)
  for (i in 1L:ll){
    tablas_a[[i]] <- add_ages(tablas_a[[i]], "annual")
    tablas_q[[i]] <- add_ages(tablas_q[[i]], "quarterly")
  }

  # SAIs crude and raw
  SAI.long <- SAI_raw_rs(tablas_q = tablas_q, tablas_a = tablas_a,
                          min.age = min.age, max.age = max.age)

  SAI.lin <- SAI.long$SAI.raw.rs
  # Linearization
  for (r in 1L:4L){
    for (s in 1L:4L){
      selec <- SAI.long$quarter.age == r & SAI.long$quarter.calendar == s & SAI.long$age != 0
      # fit.SAI <- stats::lm(SAI.long$SAI.raw.rs[selec] ~ SAI.long$age[selec])
      pred <- rep(NA, sum(selec))
      # pred[!is.na(SAI.long$SAI.raw.rs[selec])] <- stats::predict(fit.SAI)
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
    SAI.margins <- SAI_margins(tablas_q = tablas_q, tablas_a = tablas_a,
                               min.age = min.age, max.age = max.age)

    SAI.long.r <- SAI.margins$SAI.r
    SAI.long.s <- SAI.margins$SAI.s
    output <- list("SAI" = SAI.long, "SAI.age" = SAI.long.r, "SAI.calendar" = SAI.long.s)
    class(output) <- c("SAI", "list")
  }

  return(output)

}


############################################################################################
### Auxiliary functions of function SAI
## Checking inputs
check_SAI <- function(argg){
  ll <- length(argg)
  if (argg$min.age < 0 | (argg$min.age - floor(argg$min.age)) > 0)
    stop("The argument 'min.age' should be a non-negative integer")
  if (argg$max.age < 1 | (argg$max.age - floor(argg$max.age)) > 0)
    stop("The argument 'max.age' should be a positive integer")
  if ((argg$y == "")[1L]){
    warning("Only an object output of the functions crude_mx() has been declared. SAIs are just computed by division. No modelization is performed to gain strength using several periods. This does not corresponds to the recommended methodology.")
    ll <- 1L
  } else {
    ll <- (1L:ll)[-c(3:5)]
  }
  for (i in ll){
    if(!("crude_mx" %in% class(argg[[i]])))
      stop("At least one of the objects introduced via the arguments 'x', 'y' or '...' is not an output of the family of crude_mx() functions.")
  }
  argg <- argg[ll]
  return(argg)
}

## Decomposing into quarterly and annual tables
dec_q_a <- function(tablas){
  ll <- length(tablas)
  tablas_q <- tablas_a <- list()
  for (i in 1L:ll){
    lt <- length(tablas[[i]])
    if (lt == 6L){
      tablas_q[[i]] <- tablas[[i]]
      t.mx.a <- stats::aggregate(cbind(exposed, deaths) ~ age, tablas[[i]], FUN = sum)
      t.mx.a$mx <- t.mx.a$deaths/t.mx.a$exposed
      t.mx.a$mx[is.nan(t.mx.a$mx)] <- 0L # 0/0
      tablas_a[[i]] <- t.mx.a
    } else{
      tablas_q[[i]] <- tablas[[i]][[1L]]
      tablas_a[[i]] <- tablas[[i]][[2L]]
    }
    names(tablas_q[[i]])[6L] <- "mx_q"
    names(tablas_a[[i]])[4L] <- "mx_a"
#    tablas_q[[i]] <- add_ages(tablas_q[[i]], "quarterly")
#    tablas_a[[i]] <- add_ages(tablas_a[[i]], "annual")
  }
  return(list("tablas_q" = tablas_q, "tablas_a" = tablas_a))
}

min_age <- function(min.age, tablas_a){
  ll <- length(tablas_a)
  for (i in 1L:ll){
    min.age <- max(c(min(tablas_a[[i]]$age[tablas_a[[i]]$exposed != 0]), min.age))
  }
  return(min.age)
}

max_age <- function(max.age, tablas_a){
  ll <- length(tablas_a)
  for (i in 1L:ll){
    max.age <- min(c(max(tablas_a[[i]]$age[tablas_a[[i]]$exposed != 0]), max.age))
  }
  return(max.age)

}

# Complete ages in crude_mx_family tables
add_ages <- function(x, type){
  min.age <- min(x$age)
  max.age <- max(x$age)
  falta <- setdiff(min.age:max.age, unique(x$age))
  if (length(falta) != 0L){
    if (type == "annual"){
      temp <- rep(NA, length(falta))
      temp0 <- rep(0, length(falta))
      df <- data.frame(age = falta, exposed = temp0, deaths = temp0, mx_a = temp)
      x <- rbind(x, df)
      x <- x[order(x$age), ]
    }
    if (type == "quarterly"){
      age <- rep(falta, each = 16L)
      quarter.calendar <- rep(1L:4L, length(falta) * 4L)
      quarter.age <- rep(rep(1L:4L, length(falta)), each = 4L)
      temp <- rep(NA, length(falta)*4L)
      temp0 <- rep(0, length(falta)*4L)
      df <- cbind.data.frame(age, quarter.age, quarter.calendar, exposed = temp0, deaths = temp0, mx_q = temp)
      x <- rbind(x, df)
      x <- x[order(x$age, x$quarter.age, x$quarter.calendar), ]
    }
  }
  return(x)
}

# Computation of raw and adjust
SAI_raw_rs <- function(tablas_q, tablas_a, min.age, max.age){

  ll <- length(tablas_q)
  selec <- tablas_q[[1L]]$age >= min.age & tablas_q[[1L]]$age <= max.age
  SAI.long <- tablas_q[[1L]][selec , 1L:3L]
  orden <- order(SAI.long$age, SAI.long$quarter.age, SAI.long$quarter.calendar)
  SAI.long <- SAI.long[orden, ]
  log.coc <- NULL
  for (i in 1L:ll){
    selec <- tablas_q[[i]]$age >= min.age & tablas_q[[i]]$age <= max.age
    tablas_q[[i]] <- tablas_q[[i]][selec, ]
    temp <- merge(tablas_q[[i]], tablas_a[[i]], by = "age")
    orden <- order(temp$age, temp$quarter.age, temp$quarter.calendar)
    temp <- temp[orden, ]
    Rel <- log(temp$mx_q / temp$mx_a)
    Rel[is.infinite(Rel) | is.nan(Rel)] <- -3.5
    Rel[temp$mx_a == 0] <- NA
    log.coc <- cbind(log.coc, Rel)
  }

  # Computation of raw SAIs
  geom_mean <- function(x) return(exp(mean(as.matrix(x))))
  SAI.raw <- SAI.raw.rs <- apply(log.coc, 1L, geom_mean)
  # Computation of net SAIS
  for (i in min.age:max.age){
    ii <- i - min.age
    temp <- SAI.raw[(1L + (ii * 16L)):(16L + (ii * 16L))]
    SAI.raw.rs[(1L + (ii * 16L)):(16L + (ii * 16L))] <- 16*temp/sum(temp)
  }
  SAI.long <- cbind(SAI.long, SAI.raw, SAI.raw.rs)
  return(SAI.long)
}

tablas_margin <- function(tablas_q, type){
  ll <- length(tablas_q)
  tablas.q.margin <- tablas_q
  if (type == "r"){
    for (i in 1L:ll){
      tablas.q.margin[[i]] <- stats::aggregate(cbind(exposed, deaths) ~ age + quarter.age,
                                               tablas_q[[i]], FUN = sum)
      tablas.q.margin[[i]]$mx_q <- tablas.q.margin[[i]]$deaths / tablas.q.margin[[i]]$exposed
      tablas.q.margin[[i]] <- tablas.q.margin[[i]][order(tablas.q.margin[[i]]$age), ]
    }
  }
  if (type == "s"){
    for (i in 1L:ll){
      tablas.q.margin[[i]] <- stats::aggregate(cbind(exposed, deaths) ~ age + quarter.calendar,
                                               tablas_q[[i]], FUN = sum)
      tablas.q.margin[[i]]$mx_q <- tablas.q.margin[[i]]$deaths / tablas.q.margin[[i]]$exposed
      tablas.q.margin[[i]] <- tablas.q.margin[[i]][order(tablas.q.margin[[i]]$age), ]
    }
  }
  return(tablas.q.margin)
}


SAI_margins <- function(tablas_q, tablas_a, min.age, max.age){
  tablas.q.r <- tablas_margin(tablas_q, type = "r")
  tablas.q.s <- tablas_margin(tablas_q, type = "s")

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
    Rel[is.na(temp$mx_q) & is.na(temp$mx_a)] <- NA
    log.coc.r <- cbind(log.coc.r, Rel)

    # marging calendar
    tablas.q.s[[i]] <- tablas.q.s[[i]][selec, ]
    temp <- merge(tablas.q.s[[i]], tablas_a[[i]], by = "age")
    orden <- order(temp$age, temp$quarter.calendar)
    temp <- temp[orden, ]
    Rel <- log(temp$mx_q / temp$mx_a)
    Rel[is.infinite(Rel) | is.nan(Rel)] <- -3.5
    Rel[is.na(temp$mx_q) & is.na(temp$mx_a)] <- NA
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
    # ageing
    selec <- SAI.r$quarter.age == i & SAI.r$age != 0
    pred <- rep(NA, sum(selec))
    if (sum(!is.na(SAI.r$SAI.r.raw.rs[selec])) != 0L){
      fit.SAI <- stats::lm(SAI.r$SAI.r.raw.rs[selec] ~ SAI.r$age[selec])
      pred[!is.na(SAI.r$SAI.r.raw.rs[selec])] <- stats::predict(fit.SAI)
    }
    SAI.r.lin[selec] <- pred
   # seasonal
    selec <- SAI.s$quarter.calendar == i & SAI.s$age != 0
    pred <- rep(NA, sum(selec))
    if (sum(!is.na(SAI.s$SAI.s.raw.rs[selec])) != 0L){
      fit.SAI <- stats::lm(SAI.s$SAI.s.raw.rs[selec] ~ SAI.s$age[selec])
      pred[!is.na(SAI.s$SAI.s.raw.rs[selec])]  <- stats::predict(fit.SAI)
    }
    SAI.s.lin[selec] <- pred
  }
  SAI.r$SAI <- SAI.r.lin
  SAI.s$SAI <- SAI.s.lin
  names(SAI.r) <- c("age", "quarter.age", "SAI.raw", "SAI.norm", "SAI.lin")
  names(SAI.s) <- c("age", "quarter.calendar", "SAI.raw", "SAI.norm", "SAI.lin")

  return(list("SAI.r" = SAI.r, "SAI.s" = SAI.s))
}
