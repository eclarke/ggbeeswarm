#' Violin point-style plots to show overlapping points. x must be discrete.
#' 
#' @family position adjustments
#' @name position_quasirandom
#' @param width the maximum amount of spread (default: 0.4)
#' @param varwidth vary the width by the relative size of each group
#' @param bandwidth the bandwidth adjustment to use when calculating density 
#' Smaller numbers (< 1) produce a tighter "fit". (default: 0.5)
#' @usage position_quasirandom(width = NULL, varwidth = NULL, bandwidth = NULL)
#' @export
#' @examples
#' 
#' if(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("proto",quietly=TRUE)){
#' #  ggplot2::qplot(class, hwy, data = ggplot2::mpg, position=position_quasirandom())
#'   # Generate fake data
#'   distro <- data.frame(
#'     'variable'=rep(c('runif','rnorm'),each=100),
#'     'value'=c(runif(100, min=-3, max=3), rnorm(100))
#'   )
#' #  ggplot2::qplot(variable, value, data = distro, position = position_quasirandom())
#' #  ggplot2::qplot(variable, value, data = distro, position = position_quasirandom(width=0.1))
#' }
if(requireNamespace("ggplot2",quietly=TRUE)&requireNamespace("proto",quietly=TRUE)){
	position_quasirandom <- function (width = NULL, varwidth = NULL, bandwidth=NULL) {
	  PositionQuasirandom$new(width = width, varwidth = varwidth, bandwidth=bandwidth)
	}

	PositionQuasirandom <- proto::proto(ggplot2:::Position, {
	  objname <- "quasirandom"
	  
	  new <- function(., width=NULL, varwidth=NULL, bandwidth=NULL) {
		 .$proto(width=width, varwidth=varwidth, bandwidth=bandwidth)
	  }

	  # Adjust function is used to calculate new positions (from ggplot2:::Position)
	  adjust <- function(., data) {
		 if (empty(data)) return(data.frame())
		 check_required_aesthetics(c("x", "y"), names(data), "position_quasirandom")

		 if (is.null(.$width)) .$width <- resolution(data$x, zero = FALSE) * 0.4
		 if (is.null(.$varwidth)) .$varwidth <- FALSE
		 if (is.null(.$bandwidth)) .$bandwidth <- 0.5
		 
		 nbins <- 1000
		 
		 trans_x <- NULL
		 trans_y <- NULL
		 
		 if(.$width > 0) {
			trans_x <- function(x) {
			  new_x <- offset_x(
				 data$y, x,
				 width=.$width, 
				 varwidth=.$varwidth, 
				 adjust=.$bandwidth)
			  
			  new_x + x
			}
			  
		 }

		 transform_position(data, trans_x, trans_y)
	  }

	})
}else{
	position_quasirandom <- function (width = NULL, varwidth = NULL, bandwidth=NULL)stop(simpleError("position_quasirandom requires ggplot2"))
}

