#' 	Graphical representation of a SAI object.
#'
#' @description Plot method for a SAI object. This is a plot method for objects obtained using the \code{\link{compute_SAI}} function
#' or the \code{\link{SAI_shortcut_1}} function.
#'
#' @author Jose M. Pavia, \email{pavia@@uv.es}
#'
#' @param x An object output of the \code{\link{compute_SAI}} or \code{\link{SAI_shortcut_1}} functions.
#' @param ... Other arguments passed on to methods. Not currently used.
#' @param min.age A positive integer informing about the initial minimal age of the range of ages to be represented.
#'                This minimum age can be increased depending on the ages available in the SAI estimates.
#'                Default, 1.
#' @param max.age A positive integer informing about the initial maximum age of the range of ages to be represented.
#'                This maximum age can be decreased depending on the ages available in the SAI estimates.
#'                Default, 100.
#' @param decimal.digits Integer indicating the number of decimal places to be shown. Default, 1.
#' @param name.labels.ageing Names to be used for the (y) ageing axis. Default, c("Q1", "Q2", "Q3", "Q4").
#' @param name.labels.season Names to be used for the (x) season (calendar) axis. Default, c("Winter", "Spring", "Summer", "Autumn").
#' @param title.season Name to be used for identifying the seasonal marginal panels. Default, "Seasonal effects".
#' @param title.ageing Name to be used for identifying the ageing marginal panels. Default, "Ageing effects".
#' @param x.title Name to be used for the x axis. Default, "Age".
#' @param color.main Color of the line corresponding to the estimated, normalized SAI. Default, "black".
#' @param color.lm Color of the lm line. Default, "blue".
#' @param color.hline Color of the hline (reference) line. Default, "red".
#' @param size.labels.y Number informing the size of the labels of SAI axis. Default, 8.
#' @param axis.age.ticks Optional vector with the values to be presented in the age axis.
#' @param limits.joint Optional vector of length 2 with the limits for the SAI axis of the main plot.
#' @param breaks.joint Optional vector with the values to be presented in the SAI axis of the main plot.
#' @param limits.ageing Optional vector of length 2 with the limits for the SAI axis of the marginal ageing SAI plot.
#' @param breaks.ageing Optional vector with the values to be presented in the SAI axis of the marginal ageing SAI plot.
#' @param limits.seasonal Optional vector of length 2 with the limits for the SAI axis of the marginal seasonal (calendar) SAI plot.
#' @param breaks.seasonal Optional vector with the values to be presented in the SAI axis of the marginal seasonal (calendar) SAI plot.
#' @param show.plot A TRUE/FALSE value indicating whether the plot should be displayed as a side-effect. By default, TRUE.
#'
#' @return
#' Invisibly returns the grob description of the plot, which is a list with components that contain the plot itself, the data, information about the scales, panels, etc.
#'
#' @note ggplot2 and gridExtra packages are needed to be installed for this function to work.
#'
# @import ggplot2 grid
#'
#' @export
#' @method plot SAI
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
#'  p <- plot(SAI.example, show.plot = FALSE)
#' }

plot.SAI <- function(x, ..., min.age = 1, max.age = 100, decimal.digits = 1,
                     color.main = "black", color.lm = "blue", color.hline = "red",
                     size.labels.y = 8, axis.age.ticks,
                     limits.joint, breaks.joint, limits.ageing,
                     breaks.ageing, limits.seasonal, breaks.seasonal,
                     name.labels.ageing = c("Q1", "Q2", "Q3", "Q4"),
                     name.labels.season = c("Winter", "Spring", "Summer", "Autumn"),
                     title.season = "Seasonal effects", title.ageing = "Ageing effects",
                     x.title = "Age", show.plot = TRUE){

  if (length(x) == 6L){
    pintar_con <- plot_SAI_basic
#  } else if (inherits(x, "SAI_margins")){
  } else if (length(x) == 3L){
    pintar_con <- plot_SAI_margins
  }

  p <- pintar_con(x = x,
                  ...,
                  min.age = min.age,
                  max.age = max.age,
                  decimal.digits = decimal.digits,
                  color.main = color.main,
                  color.lm = color.lm,
                  color.hline = color.hline,
                  size.labels.y = size.labels.y,
                  axis.age.ticks = axis.age.ticks,
                  limits.joint = limits.joint,
                  breaks.joint = breaks.joint,
                  limits.ageing = limits.ageing,
                  breaks.ageing = breaks.ageing,
                  limits.seasonal = limits.seasonal,
                  breaks.seasonal = breaks.seasonal,
                  name.labels.ageing = name.labels.ageing,
                  name.labels.season = name.labels.season,
                  title.season = title.season,
                  title.ageing = title.ageing,
                  x.title = x.title,
                  show.plot = show.plot)

  if (show.plot) plot(p)
  return(p)
}


plot_SAI_margins <- function(x, min.age = 1, max.age = 100, decimal.digits = 1,
                             color.main = "black", color.lm = "blue", color.hline = "red",
                             size.labels.y = 8, axis.age.ticks,
                             limits.joint, breaks.joint, limits.ageing,
                             breaks.ageing, limits.seasonal, breaks.seasonal,
                             name.labels.ageing = c("Q1", "Q2", "Q3", "Q4"),
                             name.labels.season = c("Winter", "Spring", "Summer", "Autumn"),
                             title.season = "Seasonal effects", title.ageing = "Ageing effects",
                             x.title = "Age", show.plot = TRUE, ...
){


  if (min.age < 0 | (min.age - floor(min.age)) > 0)
    stop("The argument 'min.age' should be a non-negative integer")
  if (max.age < 1 | (max.age - floor(max.age)) > 0)
    stop("The argument 'max.age' should be a positive integer")

  if (!("SAI" %in% class(x)))
    stop("'x' does not have the expected structure. It must be an object output of the SAI function")

  conjunto <- x[[1L]]
  fila <- x[[2L]]
  columna <- x[[3L]]

  min.age <- max(min.age, min(conjunto$age))
  max.age <- min(max.age, max(conjunto$age))

  if (min.age > max.age)
    stop("Error. The intersection of the sets of ages for which there are estimates of SAI available is empty.")

  conjunto <- conjunto[conjunto$age %in% min.age:max.age, ]
  columna <- columna[columna$age %in% min.age:max.age, ]
  fila <- fila[fila$age %in% min.age:max.age, ]

  if (missing(axis.age.ticks)){
    axis.age.ticks <- max.age - min.age
    axis.age.ticks <-  min.age + round(c(0.1, 0.5, 0.9) * axis.age.ticks)
  }

  if (missing(limits.joint))
    limits.joint <- c(min(conjunto$SAI.norm)*0.95, max(conjunto$SAI.norm)*1.05)

  if (missing(breaks.joint))
    breaks.joint <-  round(limits.joint[1L] + c(0.1, 0.5, 0.9) * diff(limits.joint), decimal.digits)

  if (missing(limits.ageing))
    limits.ageing <- c(min(fila$SAI.norm)*0.95, max(fila$SAI.norm)*1.05)

  if (missing(breaks.ageing))
    breaks.ageing <-  round(limits.ageing[1L] + c(0.1, 0.5, 0.9) * diff(limits.ageing), decimal.digits)

  if (missing(limits.seasonal))
    limits.seasonal <- c(min(columna$SAI.norm)*0.95, max(columna$SAI.norm)*1.05)

  if (missing(breaks.seasonal))
    breaks.seasonal <-  round(limits.seasonal[1L] +  c(0.1, 0.5, 0.9) * diff(limits.seasonal), decimal.digits)

  conjunto$Quarter <- factor(factor(conjunto$quarter.age):factor(conjunto$quarter.calendar),
                             levels = c("4:1", "4:2", "4:3", "4:4", "3:1", "3:2", "3:3", "3:4",
                                        "2:1", "2:2", "2:3", "2:4", "1:1", "1:2","1:3", "1:4") )
  fila$Quarter <- factor(factor(fila$quarter.age), levels = c(4, 3, 2, 1),
                         labels = rev(name.labels.ageing))
  columna$Quarter <- factor(factor(columna$quarter.calendar), levels = c(1, 2, 3, 4),
                            labels = name.labels.season)

  # Main figure
  age.j <- conjunto$age
  SAI.norm.j <- conjunto$SAI.norm
  joint.figure <- ggplot2::ggplot(data = conjunto,
                                  ggplot2::aes(x = age.j, y = SAI.norm.j)) +
    ggplot2::geom_line(col = color.main) +
    ggplot2::geom_smooth(formula = y ~ x, se = F, method = "lm",
                         color = color.lm, linewidth = 0.25) +
    ggplot2::geom_hline(yintercept = 1, col = color.hline, lty = 2) +
    ggplot2::scale_x_continuous(breaks = axis.age.ticks) +
    ggplot2::facet_wrap("Quarter") +
    ggplot2::scale_y_continuous(
      name = NULL,
      limits = limits.joint,
      breaks = breaks.joint,
      position = "right") +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = 'white', color = "black"),
      # plot.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      panel.grid = ggplot2::element_line(color = "grey92"),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = size.labels.y)#,
   )



  # Age figure
  age.f <- fila$age
  SAI.norm.f <- fila$SAI.norm
  ageing.figure1 <- ggplot2::ggplot(data = fila,
                                    ggplot2::aes(x = age.f, y = SAI.norm.f)) +
    ggplot2::geom_line(col = color.main) +
    ggplot2::geom_smooth(formula = y ~ x, se = F, method = "lm",
                         color = color.lm, size = 0.25) +
    ggplot2::geom_hline(yintercept = 1, col = color.hline, lty = 2) +
    ggplot2::facet_wrap("Quarter", nrow = 4) +
    ggplot2::scale_y_continuous(
      name = NULL,
      limits = limits.ageing,
      breaks = breaks.ageing,
      position = "right") +
    ggplot2::scale_x_continuous(breaks = axis.age.ticks,
                                #            sec.axis = ggplot2::sec_axis(~ . ,  breaks = axis.age.ticks,
                                #                                                labels = axis.age.ticks)
    ) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = "black"),
      plot.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      panel.grid = ggplot2::element_line(color = "grey92"),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = size.labels.y) ) # +
  # ggplot2::geom_text( ggplot2::aes(x = x, y = y, label = caption),
  #   data = data.frame(x = axis.age.ticks, y = limits.ageing[2L]*1.05, caption = axis.age.ticks),
  #   size = 1 )

  ageing.figure1 <- ggplot2::ggplotGrob(ageing.figure1)


  # Season figure
  age.c <- columna$age
  SAI.norm.c <- columna$SAI.norm
  seasonal.figure1 <- ggplot2::ggplotGrob(
    ggplot2::ggplot(data = columna,
                    ggplot2::aes(x = age.c, y = SAI.norm.c)) +
      ggplot2::geom_line(col = color.main) +
      ggplot2::geom_smooth(formula = y ~ x, se = F, method = "lm",
                           color = color.lm, size = 0.25) +
      ggplot2::geom_hline(yintercept = 1, col = color.hline, lty = 2) +
      ggplot2::facet_wrap("Quarter", nrow = 1) +
      ggplot2::scale_y_continuous(
        name = NULL,
        limits = limits.seasonal,
        breaks = breaks.seasonal,
        position = "right") +
      ggplot2::scale_x_continuous(breaks = axis.age.ticks) +
      ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white", color = "black"),
        #      plot.background = ggplot2::element_rect(fill = 'transparent', color = NA),
        panel.grid = ggplot2::element_line(color = "grey92"),
        axis.text.y = ggplot2::element_text(size = size.labels.y)
      ) + ggplot2::xlab(x.title)
  )

  # Building the plot
  # blank <- ggplot2::ggplot() + ggplot2::theme_void()
  blank <- ggplot2::ggplotGrob(ggplot2::ggplot() + ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "transparent")))

  f <- .01
  names.seasons <- grid::textGrob(name.labels.season,
                                  x = c(.125-f*1, .375-f*2, .625-f*3, .875-f*4),
                                  y = 0.485, gp = grid::gpar(cex = .95))
  names.seasons <- grid::grobTree(blank,
                                  children = grid::gList(names.seasons), name = "names.seasons")

  names.ageing <- grid::textGrob(name.labels.ageing,
                                 x= .55, y = c(.140, .383, .628, .875),
                                 gp = grid::gpar(cex = 10/12))
  names.ageing <- grid::grobTree(blank, children = grid::gList(names.ageing),
                                 name = "names.ageing")

  # ageing.figure1 <- ggplot2::ggplotGrob(ageing.figure0)
  # seasonal.figure1 <- ggplot2::ggplotGrob(seasonal.figure0)
  # ageing.figure1$widths
  # ageing.figure1$widths[5L] <- unit(4, "cm")
  # dist.a <- as.character(unit(ageing.figure1$widths[6L]) + unit(ageing.figure1$widths[5L]))
  # dist.a <- as.numeric(substr(dist.x, 1L, nchar(dist.x) - 2L))
  # dist.a2 <- (max.age - min.age)

  loc.x <- length(seasonal.figure1$grobs[[2L]]$children[[1L]]$children)
  if (loc.x == 4L){
    posiciones <- unique(seasonal.figure1$grobs[[2L]]$children[[1L]]$children[[4L]]$x)
  } else {
    posiciones <- unique(seasonal.figure1$grobs[[2L]]$children[[1L]]$children[[5L]]$x)
  }
  posiciones <- c(0.145, 0.4575, 0.76)
  ticks0 <- grid::segmentsGrob(x0 = posiciones, x1 = posiciones,
                               y0 = rep(1 - 0.007, length(posiciones)),
                               y1 = rep(1 - .018, length(posiciones)),
                               gp = grid::gpar(lwd = 1.35))
  ticks0 <- grid::grobTree(blank, children = grid::gList(ticks0))

  labels.ages0 <- grid::textGrob(axis.age.ticks, x = posiciones,
                                 y = rep(0.35, length(posiciones)),
                                 gp = grid::gpar(cex = 8.7/12, col = "grey30"))
  labels.ages0 <- grid::grobTree(blank, children = grid::gList(labels.ages0))

  #1 ticks <- gridExtra::arrangeGrob( blank, ticks0, blank, blank,
  #1                                 ncol = 4L,
  #1                                 widths = ageing.figure1$widths[c(1L, 5L, 6L, 9L)])
  ticks <- gridExtra::arrangeGrob(ticks0)

  #1 labels.ages <- gridExtra::arrangeGrob( blank, labels.ages0, blank, blank,
  #1                                        ncol = 4L,
  #1                                        widths = ageing.figure1$widths[c(1L, 5L, 6L, 9L)])

  labels.ages <- gridExtra::arrangeGrob(labels.ages0)
  ageing.figure <- grid::grobTree(ticks, children = grid::gList(ageing.figure1))
  ageing.figure <- grid::grobTree(labels.ages, children = grid::gList(ageing.figure))

  ageing.label <- grid::textGrob(title.ageing, x = .48, y = 1.02, gp = grid::gpar(cex = .85))
  ageing.label <- grid::grobTree(blank, children = grid::gList(ageing.label))

  seasonal.label <- grid::textGrob(title.season, x = .50, y = 1.02, gp = grid::gpar(cex = .85))
  seasonal.figure <- grid::grobTree(seasonal.figure1,
                                    children = grid::gList(seasonal.label))

  p <- gridExtra::arrangeGrob(blank, names.seasons, labels.ages,
                              names.ageing, joint.figure, ageing.figure,
                              blank, seasonal.figure, ageing.label,
                              ncol = 3, nrow = 3,
                              widths = grid::unit(c(1, 4, 1.2), c("line", "null", "null")),
                              heights = grid::unit(c(1, 4, 1.5), c("line", "null", "null")))

}


plot_SAI_basic <- function(x, ..., min.age = 1, max.age = 100, decimal.digits = 1,
                           color.main = "black", color.lm = "blue", color.hline = "red",
                           size.labels.y = 8, axis.age.ticks,
                           limits.joint, breaks.joint, limits.ageing,
                           breaks.ageing, limits.seasonal, breaks.seasonal,
                           name.labels.ageing = c("Q1", "Q2", "Q3", "Q4"),
                           name.labels.season = c("Winter", "Spring", "Summer", "Autumn"),
                           title.season = "Seasonal effects", title.ageing = "Ageing effects",
                           x.title = "Age", show.plot = TRUE
){


  if (min.age < 0 | (min.age - floor(min.age)) > 0)
    stop("The argument 'min.age' should be a non-negative integer")
  if (max.age < 1 | (max.age - floor(max.age)) > 0)
    stop("The argument 'max.age' should be a positive integer")

  if (!("SAI" %in% class(x)))
    stop("'x' does not have the expected structure. It must be an object output of the SAI function")

  conjunto <- x

  min.age <- max(min.age, min(conjunto$age))
  max.age <- min(max.age, max(conjunto$age))

  if (min.age > max.age)
    stop("Error. The intersection of the sets of ages for which there are estimates of SAI available is empty.")

  conjunto <- conjunto[conjunto$age %in% min.age:max.age, ]

  if (missing(axis.age.ticks)){
    axis.age.ticks <- max.age - min.age
    axis.age.ticks <-  min.age + round(c(0.1, 0.5, 0.9) * axis.age.ticks)
  }

  if (missing(limits.joint))
    limits.joint <- c(min(conjunto$SAI.norm)*0.95, max(conjunto$SAI.norm)*1.05)

  if (missing(breaks.joint))
    breaks.joint <-  round(limits.joint[1L] + c(0.1, 0.5, 0.9) * diff(limits.joint), decimal.digits)

  conjunto$Quarter <- factor(factor(conjunto$quarter.age):factor(conjunto$quarter.calendar),
                             levels = c("4:1", "4:2", "4:3", "4:4", "3:1", "3:2", "3:3", "3:4",
                                        "2:1", "2:2", "2:3", "2:4", "1:1", "1:2","1:3", "1:4") )

  # Main figure
  age.c <- conjunto$age
  SAI.norm.c <- conjunto$SAI.norm
  joint.figure <- ggplot2::ggplot(data = conjunto,
                                  ggplot2::aes(x = age.c, y = SAI.norm.c)) +
    ggplot2::geom_line(col = color.main) +
    ggplot2::geom_smooth(formula = y ~ x, se = F, method = "lm",
                         color = color.lm, size = 0.25) +
    ggplot2::geom_hline(yintercept = 1, col = color.hline, lty = 2) +
    ggplot2::scale_x_continuous(breaks = axis.age.ticks) +
    ggplot2::facet_wrap("Quarter") +
    ggplot2::scale_y_continuous(
      name = NULL,
      limits = limits.joint,
      breaks = breaks.joint,
      position = "right") +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = "black"),
      panel.grid = ggplot2::element_line(color = "grey92"),
      #      axis.title.x = ggplot2::element_blank(),
      #      axis.text.x = ggplot2::element_blank(),
      #      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = size.labels.y)#,
    ) + ggplot2::xlab(x.title)

  # Building the plot
  # blank <- ggplot2::ggplot() + ggplot2::theme_void()
  blank <- ggplot2::ggplotGrob(ggplot2::ggplot() + ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "white")))

  f <- .01
  names.seasons <- grid::textGrob(name.labels.season,
                                   x = c(.125-f*1, .375-f*2, .625-f*3, .875-f*4),
                                   y = 0.485, gp = grid::gpar(cex = .95))
  names.seasons <- grid::grobTree(blank,
                                  children = grid::gList(names.seasons))

  names.ageing <- grid::textGrob(name.labels.ageing,
                                  x= .55, y = c(.200, .425, .650, .875),
                                  gp = grid::gpar(cex = 10/12))
  names.ageing <- grid::grobTree(blank,
                                 children = grid::gList(names.ageing))

  p <- gridExtra::arrangeGrob(blank, names.seasons,
                              names.ageing, joint.figure,
                              ncol = 2, nrow = 2,
                              widths = grid::unit(c(1, 4), c("line", "null")),
                              heights = grid::unit(c(1, 4), c("line", "null")))

}

# SAI_basic <- setClass("SAI_basic", slots = c(SAI = "data.frame"), contains = "data.frame")
# SAI_basic <- setClass("SAI_basic", contains = "data.frame")
# setMethod("plot", signature(x = "SAI_basic", y = "missing"), plot.SAI)

# SAI_margins <- setClass("SAI_margins", slots = c(SAI = "list"),  contains = "list")
# qlifetable <- setClass("qlifetable", contains = "data.frame")
# setMethod("plot", signature(x = "qlifetable", y = "missing"), plot.qlifetable)
# SAI_margins <- setClass("SAI_margins", contains = "data.frame")
# setMethod("plot", signature("SAI_margins", y = "missing"), definition = plot.SAI)

# SAI <- setClass("SAI", contains = "data.frame")
# setMethod("plot", signature("SAI", y = "missing"), definition = plot.SAI)


