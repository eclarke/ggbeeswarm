#' Violin point-style plots to show overlapping points. x must be discrete.
#' 
#' @family position adjustments
#' @param width the maximum amount of spread (default: 0.4)
#' @param varwidth vary the width by the relative size of each group
#' @param bandwidth the bandwidth adjustment to use when calculating density 
#' Smaller numbers (< 1) produce a tighter "fit". (default: 0.5)
#' @export
#' @import proto
#' @import ggplot2
#' @import beeswarm
#' @examples
#' 
#'   ggplot2::qplot(class, hwy, data = ggplot2::mpg, position=position_beeswarm())
#'   # Generate fake data
#'   distro <- data.frame(
#'     'variable'=rep(c('runif','rnorm'),each=100),
#'     'value'=c(runif(100, min=-3, max=3), rnorm(100))
#'   )
#'   ggplot2::qplot(variable, value, data = distro, position = position_beeswarm())
#'   ggplot2::qplot(variable, value, data = distro, position = position_beeswarm(width=0.1))
#'
position_beeswarm <- function (width = NULL, varwidth = NULL, bandwidth=NULL) {
	PositionBeeswarm$new(width = width, varwidth = varwidth, bandwidth=bandwidth)
}

PositionBeeswarm <- proto::proto(ggplot2:::Position, {
	objname <- "quasirandom"

	new <- function(., width=NULL, varwidth=NULL, bandwidth=NULL) {
		.$proto(width=width, varwidth=varwidth, bandwidth=bandwidth)
	}

	# Adjust function is used to calculate new positions (from ggplot2:::Position)
	adjust <- function(., data) {
		if (empty(data)) return(data.frame())
		check_required_aesthetics(c("x", "y"), names(data), "position_beeswarm")

		if (is.null(.$width)) .$width <- resolution(data$x, zero = FALSE) * 0.4
		if (is.null(.$varwidth)) .$varwidth <- FALSE
		if (is.null(.$bandwidth)) .$bandwidth <- 0.5

		.$newXY<- NULL

		trans_x <- NULL
		trans_y <- NULL

		if(.$width > 0) {
			setNewXY<-function(){
				if(!is.null(.$newXY))return(NULL)
				.$newXY<-do.call(rbind,ave(data$y,data$x,FUN=function(yy)split(beeswarm::swarmx(0,yy),1:length(yy))))
				.$newXY$x<-.$newXY$x+data$x
				.$newXY$oldX<-data$x
				.$newXY$oldY<-data$y
				return(NULL)
			}

			trans_x <- function(x) {
				setNewXY()
				if(any(.$newXY$oldX!=x))stop(simpleError('Mismatch between expected x and x in position'))
				.$newXY$x
			}
			trans_y <- function(y) {
				setNewXY()
				if(any(.$newXY$oldY!=y))stop(simpleError('Mismatch between expected y and y in position'))
				.$newXY$y
			}
		}

		transform_position(data, trans_x, trans_y)
	}

})

