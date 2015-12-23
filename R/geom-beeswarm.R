

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

