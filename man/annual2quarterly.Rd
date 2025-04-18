% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annual2quarterly.R
\name{annual2quarterly}
\alias{annual2quarterly}
\title{Estimates quarterly life tables}
\usage{
annual2quarterly(table.a, SAIs, mx = FALSE, min.age = 0, max.age = 100)
}
\arguments{
\item{table.a}{A data.frame corresponding to the reference annual life table. The life table can be defined via
via death probabilities (`qx`, default) or death rates (`mx`). The first column of `table.a` refers to `age`,
and the second column to either `mx` rates or `qx` probabilities. In case of using death probabilities (`qx`),
`table.a` can have an optional third column, which refers to the the average number of years lived
for those dying with age x, `ax`. If this last column is missing ax is assumed to be constant and equal to 0.5.}

\item{SAIs}{An object output of the \code{\link{compute_SAI}} function.}

\item{mx}{A `TRUE/FALSE` argument informing whether `table.a` is either an annual life table of death rates or
an annual table of death probabilities. Default, `FALSE`.}

\item{min.age}{A non-negative integer informing about the initial minimal age for which quarterly tables must be computed.
This minimum age can be increased depending on the ages for which there are values in `table.a` and in
`SAIs`. Default, 0.}

\item{max.age}{A positive integer informing about the initial maximum age for which quarterly tables must be computed.
This maximum age can be decreased  depending on the ages for which there are values in `table.a` and in
`SAIs`. Default, 100.}
}
\value{
A data frame with ten columns
   \item{age}{ Integer age to which death rates and probabilities corresponds.}
   \item{quarter.age}{ Age quarter to which death rates and probabilities corresponds.}
   \item{mx.quarter.birth.1}{ Death rates corresponding to people born during the first quarter of the year.}
   \item{mx.quarter.birth.2}{ Death rates corresponding to people born during the second quarter of the year.}
   \item{mx.quarter.birth.3}{ Death rates corresponding to people born during the third quarter of the year.}
   \item{mx.quarter.birth.4}{ Death rates corresponding to people born during the four quarter of the year.}
   \item{qx.quarter.birth.1}{ Death Probabilites corresponding to people born during the first quarter of the year.}
   \item{qx.quarter.birth.2}{ Death Probabilites corresponding to people born during the second quarter of the year.}
   \item{qx.quarter.birth.3}{ Death Probabilites corresponding to people born during the third quarter of the year.}
   \item{qx.quarter.birth.4}{ Death Probabilites corresponding to people born during the four quarter of the year.}
}
\description{
Given an annual life table and a set of estimates seasonal-ageing indexes, estimates the four
              quarterly life tables associated to the annual life table.
}
\examples{
\donttest{
# This can take a while
 t.stock <- time_exposed_stock(pop_2006$date.birth, 2006, "forward")
 t.stock <- t.stock[t.stock$age <= 100, ]
 temp <- quarterly_variables(death_2006$date.birth, death_2006$date.death)
 e.death <- count_events_quarter(temp)
 e.death <- e.death[e.death$age <= 100, ]
 t.birth <- time_exposed_newborns(birth_2006$date.birth)
 out <- crude_mx(t.stock, e.death, time.birth = t.birth)
 SAI.example <- compute_SAI(out, out)
 dx <- tapply(e.death$number.events, e.death$age, sum)
 Lax <- tapply(t.stock$time.exposed, t.stock$age, sum)
 table.ex <- data.frame(age = 0:100, mx.a = dx/Lax)
 example <- annual2quarterly(table.a = table.ex, SAIs = SAI.example, mx = TRUE)
}

}
\references{
Pavia, JM and Lledo, J (2022). Estimation of the Combined Effects of Ageing and Seasonality on Mortality Risk. An application to Spain. *Journal of the Royal Statistical Society, Series A (Statistics in Society)*, 185(2), 471-497. \doi{10.1111/rssa.12769}

Pavia, JM and Lledo, J (2025). qlifetable: An R package for constructing quarterly life tables. *PLoS ONE*, 20(2), e0315937. \doi{10.1371/journal.pone.0315937}
}
\author{
Jose M. Pavia \email{pavia@uv.es}

Josep Lledo \email{josep.lledo@uv.es}
}
