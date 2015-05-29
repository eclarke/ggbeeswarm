#' Beeswarm-style plots to show overlapping points. x must be discrete.
#' 
#' @import proto 
#' @family position adjustments
#' @param width the maximum amount of spread (default: 0.4)
#' @param var_width vary the width by the relative size of each group
#' @param bandwidth the bandwidth adjustment to use when calculating density 
#' Smaller numbers (< 1) produce a tighter "fit". (default: 0.5)
#' @export
#' @examples
#' 
#' qplot(class, hwy, data = mpg, position=position_beeswarm())
#' # Generate fake data
#' distro <- melt(data.frame(list(runif=runif(100, min=-3, max=3), rnorm=rnorm(100))))
#' qplot(variable, value, data = distro, position = position_beeswarm())
#' qplot(variable, value, data = distro, position = position_beeswarm(width=0.1))
position_beeswarm <- function (width = NULL, var_width = NULL, bandwidth=NULL) {
  if (!require(ggplot2)) {
    stop("position_beeswarm requires ggplot2")
  }
  PositionBeeswarm$new(width = width, var_width = var_width, bandwidth=bandwidth)
}

PositionBeeswarm <- proto(ggplot2:::Position, {
  objname <- "beeswarm"
  
  new <- function(., width=NULL, var_width=NULL, bandwidth=NULL) {
    .$proto(width=width, var_width=var_width, bandwidth=bandwidth)
  }

  # Adjust function is used to calculate new positions (from ggplot2:::Position)
  adjust <- function(., data) {
    if (empty(data)) return(data.frame())
    check_required_aesthetics(c("x", "y"), names(data), "position_beeswarm")

    if (is.null(.$width)) .$width <- resolution(data$x, zero = FALSE) * 0.4
    if (is.null(.$var_width)) .$var_width <- FALSE
    if (is.null(.$bandwidth)) .$bandwidth <- 0.5
    
    nbins <- 1000
    
    trans_x <- NULL
    trans_y <- NULL
    
    if(.$width > 0) {
      trans_x <- function(x) {
        new_x <- offset_x(
          data$y, x,
          width=.$width, 
          var_width=.$var_width, 
          adjust=.$bandwidth)
        
        new_x + x
      }
        
    }

    transform_position(data, trans_x, trans_y)
  }

})

