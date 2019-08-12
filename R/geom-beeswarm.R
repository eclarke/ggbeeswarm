#' Points, jittered to reduce overplotting using the beeswarm package
#'
#' The beeswarm geom is a convenient means to offset points within categories to reduce overplotting. Uses the beeswarm package
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams offset_beeswarm
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
  priority = c("ascending", "descending", "density", "random", "none"),
  groupOnX=NULL,
  dodge.width=0,
  beeswarmArgs=list(),
  stat='identity',
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  position <- position_beeswarm(groupOnX=groupOnX,dodge.width=dodge.width)
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBeeswarm,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      priority = priority,
      groupOnX=groupOnX,
      dodge.width=dodge.width,
      beeswarmArgs=beeswarmArgs,
      ...
    )
  )
}

#' geom object for use in geom_beeswarm
#' @export
GeomBeeswarm <- ggplot2::ggproto("GeomBeeswarm", ggplot2::GeomPoint,
  draw_panel = function(data, panel_params, coord, priority = c("ascending", "descending", "density", "random", "none"), cex=1, groupOnX=NULL, dodge.width=0, beeswarmArgs=list(), na.rm = FALSE) {
    if (is.character(data$shape)) {
      data$shape <- ggplot2::translate_shape_string(data$shape)
    }
    coords <- coord$transform(data, panel_params)
    grid::gTree(
      coords=coords,
      priority=priority,
      groupOnX=groupOnX,
      dodge.width=dodge.width,
      beeswarmArgs=beeswarmArgs,
      gp=grid::gpar(
        col = alpha(coords$colour, coords$alpha),
        fill = alpha(coords$fill, coords$alpha),
        # Stroke is added around the outside of the point
        fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
        lwd = coords$stroke * .stroke / 2
      ),
      cl = "beeswarm"
    )
  }
)

#' grid::makeContent function for the grobTree of GeomBeeswarm objects
#'
#' @param self a beeswarm gridTree grob
#' @return a pointsGrob
#' @export
makeContent.beeswarm <- function(self) {
  oSize<-c(
    grid::convertX(grid::grobWidth(grid::textGrob('o',gp=grid::gpar(cex=1))),'native'),
    grid::convertY(grid::grobHeight(grid::textGrob('o',gp=grid::gpar(cex=1))),'native')
  )
  coords<-offset_beeswarm(self$coords,priority=self$priority,cex=self$coords$size[1],groupOnX=self$groupOnX,dodge.width=self$dodge.width,beeswarmArgs=self$beeswarmArgs,oSize=oSize)

  grid::pointsGrob(
    coords$x, coords$y,
    pch = coords$shape
  )
}


