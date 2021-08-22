#' Points, jittered to reduce overplotting using the beeswarm package
#'
#' The beeswarm geom is a convenient means to offset points within categories to
#'  reduce overplotting. Uses the beeswarm package
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams offset_beeswarm
#' @param dodge.width Amount by which points from different aesthetic groups 
#' will be dodged. This requires that one of the aesthetics is a factor.
#' @param groupOnX Deprecated.
#' @param beeswarmArgs Deprecated.
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
#'   ggplot2::ggplot(distro,aes(variable, value)) +
#'     geom_beeswarm(priority='density',size=2.5)
geom_beeswarm <- function(
  mapping = NULL,
  data = NULL,
  stat = 'identity',
  ...,
  method = "swarm",
  cex = 1,
  side = 0L,
  priority = "ascending",
  fast = TRUE,
  dodge.width = NULL,
  groupOnX = NULL,
  beeswarmArgs = list(),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  
  if (!missing(groupOnX)) warning("The `groupOnX` argument of `geom_beeswarm` is deprecated as of ggbeeswarm 0.7.0.9000.")
  if (!missing(beeswarmArgs)) warning("The `beeswarmArgs` argument of `geom_beeswarm` is deprecated as of ggbeeswarm 0.7.0.9000.")
  if (!method %in% c("swarm", "swarm2", "compactswarm", "hex", "square", "centre", "center")) {
    stop(sprintf("The method must be one of: swarm, swarm2, compactswarm, hex, square, center, or centre."))
  }
  
  position <- position_beeswarm(
    method = method,
    cex = cex,
    side = side,
    priority = priority,
    fast = fast,
    dodge.width = dodge.width
  )
  
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
