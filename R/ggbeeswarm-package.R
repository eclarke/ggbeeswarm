#' ggbeeswarm extends ggplot2 with violin point/beeswarm plots
#' 
#' This package allows plotting of several groups of one dimensional data as a violin point/beeswarm plot by arraning data points to resemble the underlying distribution. The development version of this package is on \url{http://github.com/eclarke/ggbeeswarm}.
#' @docType package
#' @name ggbeeswarm
#' @author Erik Clarke, \email{ecl@@mail.med.upenn.edu}
#' @seealso \code{\link{position_quasirandom}}
#' @examples
#' 
#'   ggplot2::qplot(class, hwy, data = ggplot2::mpg, position=position_quasirandom())
#'   # Generate fake data
#'   distro <- data.frame(
#'     'variable'=rep(c('runif','rnorm'),each=100),
#'     'value'=c(runif(100, min=-3, max=3), rnorm(100))
#'   )
#'   ggplot2::qplot(variable, value, data = distro, position = position_quasirandom())
#'   ggplot2::qplot(variable, value, data = distro, position = position_quasirandom(width=0.1))
#' 
NULL

