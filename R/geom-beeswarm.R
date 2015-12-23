#' Points, jittered to reduce overplotting using the beeswarm package
#'
#' The beeswarm geom is a convenient means to offset points within categories to reduce overplotting. Uses the beeswarm package
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams position_beeswarm
#' @seealso
#'  \code{\link{geom_quasirandom}} an alternative method,
#'  \code{\link[beeswarm]{swarmx}} how spacing is determined,
#'  \code{\link[ggplot2]{geom_point}} for regular, unjittered points,
#'  \code{\link[ggplot2]{geom_jitter}} for jittered points,
#'  \code{\link[ggplot2]{geom_boxplot}} for another way of looking at the conditional
#'     distribution of a variable
#' @export
geom_beeswarm <- function(mapping = NULL, data = NULL,
  priority = c("ascending", "descending", "density", "random", "none"),cex=2,groupOnX=NULL,
  stat='identity',position = "quasirandom", na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE, ...) {
  position <- position_beeswarm(priority = priority, cex = cex, groupOnX=groupOnX)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

