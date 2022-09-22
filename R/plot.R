#' 	Graphical representation in a 4x4 raster of a qlifetable data frame.
#'
#' @description Plot method for a data frame of events or time exposed occurring in each Lexis-diagram quarter for a set of ages.
#' This is a plot method for the objects typically obtained using the function \code{\link{count_events_quarter}} or whatever of the time_exposed_ functions (e.g., \code{\link{time_exposed_outs}}).
#'
#' @author Josep Lledo \email{josep.lledo@@uv.es}
#' @author Jose M. Pavia, \email{pavia@@uv.es}
#'
#' @param x A data frame of quarterly summary statistics. Typically an output of the function \code{\link{count_events_quarter}} or whatever of the time_exposed_ functions (e.g., \code{\link{time_exposed_outs}}).
#' @param ... Other arguments passed on to methods. Not currently used.
#' @param range.ages A vector of integers informing the aggregation of ages for which the graphical representation should be plotted. Default, NULL, the agggregation of all ages is shown.
#' @param key Type of statistic to be presented in the plot. Either 'numbers' or relative 'percentages'.
#' @param decimal.digits Integer indicating the number of decimal places to be shown. Default, 2.
#' @param color.palette Background base color for cells. Default, "grey".
#' @param alpha.max A number in the interval [0, 1]. Maximum level of transparency to be applied for the background to build the palette. Default, 1.
#' @param alpha.min A number in the interval [0, 1]. Minimum level of transparency to be applied for the background to build the palette. Default, 0.4.
#' @param color.values Base color for numbers printed in each cell. Default, "black".
#' @param big.mark A character string indicating the symbol to be used as thousand separator. Default, NULL.
#' @param size.values A number indicating the font size to be used for inner-cells values. Default, 3.
#' @param legend.name Name to be use as name in the legend. Default, NULL.
#' @param name.labels.age Names to be used for the (y) age axis. Default, c("Q1", "Q2", "Q3", "Q4").
#' @param name.labels.season Names to be used for the (x) season axis. Default, c("Winter", "Spring", "Summer", "Autumn").
#' @param show.plot A TRUE/FALSE indicating if the plot should be displayed as a side-effect. By default, TRUE.
#'
#' @return
#' Invisibly returns the (ggplot) description of the plot, which is a list with components that contain the plot itself, the data, information about the scales, panels, etc.
#'
#' @note ggplot2 is needed to be installed for this function to work.
#'
# @import ggplot2
#'
#' @export
#'
#' @method plot qlifetable
#'
#' @examples
#' dates.b <- c("1920-05-13", "1999-04-12", "2019-01-01")
#' dates.e <- c("2002-03-23", "2009-04-12", "2019-01-01")
#' x <- quarterly_variables(dates.b, dates.e)
#' out <- time_exposed_outs(x)
#' p <- plot(out, show.plot = FALSE)

plot.qlifetable <- function(x, ..., range.ages = NULL, key = "numbers", decimal.digits = 2,
                            color.palette = "grey", alpha.max = 1, alpha.min = 0.4,
                            color.values = "black", big.mark = NULL,
                            size.values = 3, legend.name = NULL,
                            name.labels.age = c("Q1", "Q2", "Q3", "Q4"),
                            name.labels.season = c("Winter", "Spring", "Summer", "Autumn"),
                            show.plot = TRUE
                           ){

  if (key != "numbers" & key != "percentages")
    stop("The 'key' argument is not properly defined. Only 'numbers' and 'percentages' are allowed.")

  if (dim(x)[2L] != 4L | dim(x)[1L] %% 16L != 0L)
    stop("'x' does not have the expected structure")

  names(x) <- c("age", "quarter.age", "quarter.calendar", "number.events")

  datos <- x
  number.decimals <- decimal.digits

  if (!is.null(range.ages))
    datos <- datos[datos$age %in% range.ages, ]

  if (is.null(big.mark)) big.mark <- ""

  total_events <- sum(datos$number.events)

  edades <- dim(datos)[1L]/16L
  tabla <- datos[1L:16L, 1L:3L]
  tabla$Total_quarter <- 0L

  for (i in 1L:edades){
    tabla$Total_quarter <- tabla$Total_quarter + datos$number.events[(1L + (i - 1) * 16L):(i * 16L)]
  }

  tabla$Total_quarter <- tabla$Total_quarter / ifelse(key == "numbers", 1L , total_events)

#  tabla <- datos %>%
#    group_by(quarter.calendar, quarter.age) %>%
#    summarise(Total_quarter = sum(number.events) / ifelse(key == "numbers", 1 , total_events))

  p <- ggplot2::ggplot(tabla,
                       ggplot2::aes(x = as.factor(tabla$quarter.calendar),
                                    y = as.factor(tabla$quarter.age))) +
    ggplot2::geom_raster(ggplot2::aes(fill = tabla$Total_quarter), position = "identity") +
    ggplot2::scale_size_continuous(range = c(min(tabla$Total_quarter), max(tabla$Total_quarter))) +
    ggplot2::scale_fill_continuous(high = scales::alpha(colour = color.palette, alpha = alpha.max),
                                   low =  scales::alpha(colour = color.palette, alpha = alpha.min))  +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::labs(fill = legend.name) +
    ggplot2::scale_x_discrete(labels = name.labels.season) +
    ggplot2::scale_y_discrete(labels = name.labels.age) +
    ggplot2::xlab("Season quarter") +
    ggplot2::ylab("Age quarter") +
    ggplot2::geom_text(ggplot2::aes(label = format(round(tabla$Total_quarter, number.decimals),
                                                   nsmall = number.decimals,
                                                    big.mark = big.mark)),
                       size = size.values,
                       col = color.values)

  if (show.plot) print(p)
  return(p)
}

qlifetable <- setClass("qlifetable", contains = "data.frame")
setMethod("plot", signature(x = "qlifetable", y = "missing"), plot.qlifetable)
