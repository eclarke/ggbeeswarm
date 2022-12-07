#' ggbeeswarm extends ggplot2 with violin point/beeswarm plots
#' 
#' This package allows plotting of several groups of one dimensional data as a violin point/beeswarm plot in ggplot2 by arranging data points to resemble the underlying distribution. The development version of this package is on <https://github.com/eclarke/ggbeeswarm>.
#' @docType package
#' @name ggbeeswarm
#' @author Erik Clarke, \email{erikclarke@@gmail.com}
#' @seealso [position_quasirandom()]
#' @examples
#' 
#'   ggplot2::ggplot(ggplot2::mpg,aes(class, hwy)) + geom_quasirandom()
#'   # Generate fake data
#'   distro <- data.frame(
#'     'variable'=rep(c('runif','rnorm'),each=100),
#'     'value'=c(runif(100, min=-3, max=3), rnorm(100))
#'   )
#'   ggplot2::ggplot(distro,aes(variable, value)) + geom_quasirandom()
#'   ggplot2::ggplot(distro,aes(variable, value)) + geom_quasirandom(width=.1)
#' 
NULL

