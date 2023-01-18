#' Points, jittered to reduce overplotting using the vipor package
#'
#' The quasirandom geom is a convenient means to offset points within categories 
#' to reduce overplotting. Uses the vipor package
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams position_quasirandom
#' @import ggplot2
#' @seealso
#'  [vipor::offsetSingleGroup()] how spacing is determined,
#'  [ggplot2::geom_point()] for regular, unjittered points,
#'  [ggplot2::geom_jitter()] for jittered points,
#'  [geom_boxplot()] for another way of looking at the conditional
#'     distribution of a variable
#' @examples
#'   ggplot2::qplot(class, hwy, data = ggplot2::mpg, geom='quasirandom')
#'   # Generate fake data
#'   distro <- data.frame(
#'     'variable'=rep(c('runif','rnorm'),each=100),
#'     'value'=c(runif(100, min=-3, max=3), rnorm(100))
#'   )
#'   ggplot2::qplot(variable, value, data = distro, geom = 'quasirandom')
#'   ggplot2::ggplot(distro,aes(variable, value)) + geom_quasirandom(width=0.1)
#' @export
geom_quasirandom <- function(
  mapping = NULL,
  data = NULL,
  stat = 'identity',
  ...,
  method = 'quasirandom',
  width = NULL,
  varwidth = FALSE,
  bandwidth = .5,
  nbins = NULL,
  dodge.width = NULL,
  groupOnX = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (!missing(groupOnX)) {
    lifecycle::deprecate_soft(
      when = "0.7.1", what = "geom_quasirandom(groupOnX)", 
      details='ggplot2 now handles this case automatically.'
    )
  }
  if (!method %in% c("quasirandom", "pseudorandom", "smiley", "maxout", "frowney", "minout", "tukey", "tukeyDense")) {
    stop(sprintf("The method must be one of: quasirandom, pseudorandom, smiley, maxout, frowney, minout, tukey, or tukeyDense."))
  }
  
  position <- position_quasirandom(
    method = method,
    width = width, 
    varwidth = varwidth, 
    bandwidth = bandwidth,
    nbins = nbins,
    dodge.width = dodge.width,
    na.rm = na.rm
  )
  
  ggplot2::layer(
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
