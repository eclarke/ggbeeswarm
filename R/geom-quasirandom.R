#' Points, jittered to reduce overplotting using the vipor package
#'
#' The quasirandom geom is a convenient means to offset points within categories to reduce overplotting. Uses the vipor package
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams position_quasirandom
#' @seealso
#'  \code{\link[vipor]{offsetX}} how spacing is determined,
#'  \code{\link[ggplot2]{geom_point}} for regular, unjittered points,
#'  \code{\link[ggplot2]{geom_jitter}} for jittered points,
#'  \code{\link{geom_boxplot}} for another way of looking at the conditional
#'     distribution of a variable
#' @export
geom_quasirandom <- function(mapping = NULL, data = NULL,
  width = NULL, varwidth = FALSE, bandwidth=.5,nbins=1000,method='quasirandom',groupOnX=NULL,
  stat='identity',position = "quasirandom", na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE, ...) {
  position <- position_quasirandom(width = width, varwidth = varwidth, bandwidth=bandwidth,nbins=nbins,method=method,groupOnX=groupOnX)

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
