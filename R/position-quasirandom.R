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
#' @import vipor
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
  ggproto(NULL,PositionQuasirandom,width = width, varwidth = varwidth, bandwidth=bandwidth,nbins=nbins,method=method)
}

PositionBeeswarm <- ggproto("PositionBeeswarm",ggplot2:::Position, required_aes=c('x','y'),
  compute_layer=function(data,params,panel){
	# Adjust function is used to calculate new positions (from ggplot2:::Position)

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
)

PositionQuasirandom <- ggproto("PositionQuasirandom",ggplot2:::Position,required_aes=c('x','y'),
  setup_params=function(self,data){
    list(width=self$width,varwidth=self$varwidth,bandwidth=self$bandwidth,nbins=self$nbins,method=self$method)
  },
  compute_layer= function(data,params,panel) {
	 data <- remove_missing(data, vars = c("x","y"), name = "position_quasirandom")
	 if (empty(data)) return(data.frame())
	
	 trans_x <- NULL
	 trans_y <- NULL

	 # more unique entries in x than y suggests y (not x) is categorical	
	 if(length(unique(data$y)) < length(unique(data$x))) {
		 if (is.null(.$width)) .$width <- ggplot2::resolution(data$y, zero = FALSE) * 0.4
	 } else {
		 if (is.null(.$width)) .$width <- ggplot2::resolution(data$x, zero = FALSE) * 0.4
	 }
	 
 
	 if(.$width > 0) {
		if(length(unique(data$y)) < length(unique(data$x))) {
			trans_y <- function(y) {
		  	  new_y <- vipor::offsetX( 
				 data$x,
			 	 y,
			 	 width=.$width, 
			 	 varwidth=.$varwidth, 
				 adjust=.$bandwidth,
				 method=.$method
		  	  )
		  
		  	  new_y + y
			}
		} else {
			trans_x <- function(x) {
		  	  new_x <- vipor::offsetX( 
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
	 }

	 transform_position(data, trans_x, trans_y)
  }
)

