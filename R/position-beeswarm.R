#' Violin point-style plots to show overlapping points. x must be discrete.
#' 
#' @family position adjustments
#' @param priority Method used to perform point layout (see \code{\link{swarmx}})
#' @param cex Scaling for adjusting point spacing (see \code{\link{swarmx}})
#' @export
#' @import proto
#' @import ggplot2
#' @import beeswarm
#' @seealso \code{\link{position_quasirandom}}, \code{\link{swarmx}} 
#' @examples
#' 
#'   ggplot2::qplot(class, hwy, data = ggplot2::mpg, position=position_beeswarm())
#'   # Generate fake data
#'   distro <- data.frame(
#'     'variable'=rep(c('runif','rnorm'),each=100),
#'     'value'=c(runif(100, min=-3, max=3), rnorm(100))
#'   )
#'   ggplot2::qplot(variable, value, data = distro, position = position_beeswarm())
#'   ggplot2::qplot(variable, value, data = distro, position = 
#'                  position_beeswarm(priority='density'),cex=2.5)
#'
position_beeswarm <- function (priority = c("ascending", "descending", "density", "random", "none"),cex=2) {
	PositionBeeswarm$new(priority = priority,cex=cex)
}

PositionBeeswarm <- proto::proto(ggplot2:::Position, {
	objname <- "beeswarm"

	new <- function(., priority=c("ascending", "descending", "density", "random", "none"),cex=2) {
		.$proto(priority=priority,cex=cex)
	}

	# Adjust function is used to calculate new positions (from ggplot2:::Position)
	adjust <- function(., data) {
		if (empty(data)) return(data.frame())
		check_required_aesthetics(c("x", "y"), names(data), "position_beeswarm")

		# more unique entries in x than y suggests y (not x) is categorical
		if(length(unique(data$y)) < length(unique(data$x))) {
			swap_xy<-TRUE
		} else {
			swap_xy<-FALSE
		}

		.$newXY<- NULL

		setNewXY<-function(){
			if(!is.null(.$newXY))return(NULL)
			#need to replace cex and xsize and ysize with options from ggplot
			if(swap_xy) {
				.$newXY<-do.call(rbind,ave(data$x,data$y,FUN=function(xx)split(beeswarm::swarmy(xx,0,cex=.$cex,priority=.$priority),1:length(xx)))) 
				.$newXY$y<-.$newXY$y+data$y
				.$newXY$oldX<-data$x
				.$newXY$oldY<-data$y
			} else {
				.$newXY<-do.call(rbind,ave(data$y,data$x,FUN=function(yy)split(beeswarm::swarmx(0,yy,cex=.$cex,priority=.$priority),1:length(yy)))) 
				.$newXY$x<-.$newXY$x+data$x
				.$newXY$oldX<-data$x
				.$newXY$oldY<-data$y
			}
			return(NULL)
		}

		trans_x <- function(x) {
			setNewXY()
			if(!swap_xy & any(.$newXY$oldX!=x))stop(simpleError('Mismatch between expected x and x in position'))
			#beeswarm returns both x and y coordinates but it seems that x should not be changed.
			#Just in case it does, we'll throw an error and investigate
			if(swap_xy & any(.$newXY$oldX!=.$newXY$x))stop(simpleError('x position moved by beeswarm. Please make sure this is desired'))
			.$newXY$x
		}
		trans_y <- function(y) {
			setNewXY()
			if(any(.$newXY$oldY!=y))stop(simpleError('Mismatch between expected y and y in position'))
			#beeswarm returns both x and y coordinates but it seems that y should not be changed.
			#Just in case it does, we'll throw an error and investigate
			if(!swap_xy & any(.$newXY$oldY!=.$newXY$y))stop(simpleError('y position moved by beeswarm. Please make sure this is desired'))
			.$newXY$y
		}
		 
		transform_position(data, trans_x, trans_y)
	}

})

