#' Points, jittered to reduce overplotting using the beeswarm package
#'
#' The beeswarm geom is a convenient means to offset points within categories to
#'  reduce overplotting. Uses the beeswarm package
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams position_beeswarm
#' @param beeswarmArgs `r lifecycle::badge("deprecated")` No longer used.
#' @inherit position_beeswarm details
#' @import ggplot2
#' @seealso
#'  [geom_quasirandom()] an alternative method,
#'  [beeswarm::swarmx()] how spacing is determined,
#'  [ggplot2::geom_point()] for regular, unjittered points,
#'  [ggplot2::geom_jitter()] for jittered points,
#'  [ggplot2::geom_boxplot()] for another way of looking at the conditional
#'     distribution of a variable
#' @export
#' @examples
#' 
#'   # Generate fake data
#'   distro <- data.frame(
#'     'variable'=rep(c('runif','rnorm'),each=100),
#'     'value'=c(runif(100, min=-3, max=3), rnorm(100))
#'   )
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
  corral = "none",
  corral.width = 0.9,
  preserve.data.axis = FALSE,
  groupOnX = NULL,
  orientation = NULL,
  beeswarmArgs = list(),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  
  if (!missing(groupOnX)) {
    lifecycle::deprecate_soft(
      when = "0.7.1", what = "position_beeswarm(groupOnX)", 
      details='The axis to group on is now guessed from the data. To override, specify orientation="x" or "y".'
    )
    if (is.null(groupOnX) || !groupOnX) {
      orientation = "y"
    } else {
      orientation = "x"
    }
  }
    
  if (!missing(beeswarmArgs)) {
    lifecycle::deprecate_soft(
      when = "0.7.1", what = "geom_beeswarm(beeswarmArgs)"
    )
  }

  if (!method %in% c("swarm", "compactswarm", "hex", "square", "centre", "center")) {
    stop(sprintf("The method must be one of: swarm, compactswarm, hex, square, center, or centre."))
  }
  if (!corral %in% c("none", "gutter", "wrap", "random", "omit")) {
    stop(sprintf("The corral argument must be one of: none, gutter, wrap, random, or omit."))
  }
  
  position <- position_beeswarm(
    method = method,
    cex = cex,
    side = side,
    priority = priority,
    fast = fast,
    dodge.width = dodge.width,
    orientation = orientation,
    corral = corral,
    corral.width = corral.width,
    preserve.data.axis = preserve.data.axis
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
