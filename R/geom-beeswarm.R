#' Points, jittered to reduce overplotting using the beeswarm package
#'
#' The beeswarm geom is a convenient means to offset points within categories to reduce overplotting. Uses the beeswarm package
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams position_beeswarm
#' @import ggplot2
#' @seealso
#'  \code{\link{geom_quasirandom}} an alternative method,
#'  \code{\link[beeswarm]{swarmx}} how spacing is determined,
#'  \code{\link[ggplot2]{geom_point}} for regular, unjittered points,
#'  \code{\link[ggplot2]{geom_jitter}} for jittered points,
#'  \code{\link[ggplot2]{geom_boxplot}} for another way of looking at the conditional
#'     distribution of a variable
#' @export
#' @examples
#' 
#'   ggplot2::qplot(class, hwy, data = ggplot2::mpg, geom='beeswarm')
#'   # Generate fake data
#'   distro <- data.frame(
#'     'variable'=rep(c('runif','rnorm'),each=100),
#'     'value'=c(runif(100, min=-3, max=3), rnorm(100))
#'   )
#'   ggplot2::qplot(variable, value, data = distro, geom='beeswarm')
#'   ggplot2::qplot(variable, value, data = distro) +
#'     geom_beeswarm(priority='density',cex=2.5)
geom_beeswarm <- function(mapping = NULL, data = NULL,
  priority = c("ascending", "descending", "density", "random", "none"),cex=2,groupOnX=NULL,dodge.width=0,
  stat='identity',position = "quasirandom", na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE, ...) {
  position <- position_beeswarm(priority = priority, cex = cex, groupOnX=groupOnX,dodge.width=dodge.width)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

