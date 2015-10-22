#' Violin point-style plots to show overlapping points. x must be discrete.
#' 
#' @family position adjustments
#' @param width the maximum amount of spread (default: 0.4)
#' @param varwidth vary the width by the relative size of each group
#' @param bandwidth the bandwidth adjustment to use when calculating density 
#' Smaller numbers (< 1) produce a tighter "fit". (default: 0.5)
#' @param nbins the number of bins used when calculating density (has little effect with quasirandom/random distribution)
#' @param method the method used for distributing points (quasirandom, pseudorandom, smiley or frowney)
#' @export
#' @import proto
#' @import ggplot2
#' @import violinpoint
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
position_quasirandom <- function (width = NULL, varwidth = FALSE, bandwidth=.5,nbins=1000,method='quasirandom') {
  PositionQuasirandom$new(width = width, varwidth = varwidth, bandwidth=bandwidth,nbins=nbins,method=method)
}

PositionQuasirandom <- proto::proto(ggplot2:::Position, {
  objname <- "quasirandom"
  
  new <- function(., width=NULL, varwidth=FALSE, bandwidth=.5,nbins=1000,method='quasirandom') {
	 .$proto(width=width, varwidth=varwidth, bandwidth=bandwidth,nbins=nbins,method=method)
  }

  # Adjust function is used to calculate new positions (from ggplot2:::Position)
  adjust <- function(., data) {
	 if (empty(data)) return(data.frame())
	 check_required_aesthetics(c("x", "y"), names(data), "position_quasirandom")

	 if (is.null(.$width)) .$width <- ggplot2::resolution(data$x, zero = FALSE) * 0.4
	 
	 trans_x <- NULL
	 trans_y <- NULL
	 
	 if(.$width > 0) {
		trans_x <- function(x) {
		  new_x <- violinpoint::offsetX( #if change the package name then will need to change here
			 data$y,
			 x,
			 width=.$width, 
			 varwidth=.$varwidth, 
			 adjust=.$bandwidth,
			 method=.$method
		  )
		  
		  new_x + x
		}
		  
	 }

	 transform_position(data, trans_x, trans_y)
  }

})

