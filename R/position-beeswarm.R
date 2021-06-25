#' An internal function to calculate new positions for geom_beeswarm
#' 
#' @family position adjustments
#' @param data A data.frame containing plotting data in columns x and y. 
#' Usually obtained from data processed by ggplot2.
#' @param yRange y axis limits
#' @param xRange.expand x axis limits plus small expansion
#' @param yRange.expand y axis limits plus small expansion
#' @param yLim.expand y data range plus small expansion
#' @param method Method for arranging points (see Details of \code{\link[beeswarm]{beeswarm}})
#' @param cex Scaling for adjusting point spacing (see \code{\link[beeswarm]{swarmx}})
#' @param side Direction to perform jittering: 0: both directions; 
#' 1: to the right or upwards; -1: to the left or downwards.
#' @param priority Method used to perform point layout (see \code{\link{swarmx}})
#' @keywords internal
#' @export
#' @importFrom beeswarm swarmx
#' @seealso \code{\link{geom_beeswarm}}, \code{\link{position_quasirandom}}, 
#' \code{\link[beeswarm]{swarmx}}
offset_beeswarm <- function(
  data, 
  yRange,
  xRange.expand,
  yRange.expand,
  yLim.expand,
  method = "swarm",
  cex = 1, 
  side = 0L,
  priority = "ascending"
) {
  if (method == "swarm") {
    # adjust par("usr") based on input data
    graphics::par("usr" = c(xRange.expand, yRange))
    
    x.offset <- beeswarm::swarmx(
      x = rep(0, length(data$y)), y = data$y,
      cex = cex, side = side, priority = priority
    )$x
  }
  
  data$x <- data$x + x.offset
  return(data)
}

position_beeswarm <- function(
  method = "swarm",
  cex = 1,
  side = 0L,
  priority = "ascending",
  groupOnX = NULL, 
  dodge.width = 0
) {
  if (!missing(groupOnX)) warning("The `groupOnX` argument of `position_beeswarm` is deprecated as of ggbeeswarm 0.7.0.9000.")
  
  ggproto(NULL, PositionBeeswarm, 
          method = method,
          cex = cex,
          side = side,
          priority = priority,
          dodge.width = dodge.width
  )
}

PositionBeeswarm <- ggplot2::ggproto("PositionBeeswarm", Position, 
                                     required_aes = c('x', 'y'),
                                     setup_params = function(self, data) {
                                       flipped_aes <- has_flipped_aes(data)
                                       data <- flip_data(data, flipped_aes)
                                       
                                       # get y range of data and extend it a little
                                       yLim.expand <- grDevices::extendrange(data$y, f = 0.01)
                                       
                                       list(
                                         # groupOnX = self$groupOnX, deprecated
                                         method = self$method,
                                         cex = self$cex,
                                         side = self$side,
                                         priority = self$priority,
                                         dodge.width = self$dodge.width,
                                         yLim.expand = yLim.expand,
                                         flipped_aes = flipped_aes 
                                       )
                                     },
                                     compute_panel = function(data, params, scales) {
                                       data <- flip_data(data, params$flipped_aes)
                                       
                                       # get plot limits
                                       if (params$flipped_aes) {
                                         yRange <- scales$x$get_limits()
                                         yRange.expand <- ggplot2:::expand_range4(scales$x$get_limits(), c(0.045, 0))
                                         xRange.expand <- ggplot2:::expand_range4(c(1, length(scales$y$get_limits())), c(0, 0.6))
                                       } else {
                                         yRange <- scales$y$get_limits()
                                         yRange.expand <- ggplot2:::expand_range4(scales$y$get_limits(), c(0.045, 0))
                                         xRange.expand <- ggplot2:::expand_range4(c(1, length(scales$x$get_limits())), c(0, 0.6))
                                       }
                                       
                                       # capture current par values
                                       current.par <- graphics::par("usr")
                                       # on exit return par("usr") to normal
                                       on.exit(graphics::par("usr" = current.par), add = TRUE)
                                       
                                       data <- ggplot2:::collide(
                                         data,
                                         params$dodge.width,
                                         name = "position_beeswarm",
                                         strategy = ggplot2:::pos_dodge,
                                         check.width = FALSE
                                       )
                                       
                                       # split data.frame into list of data.frames
                                       if(!is.null(params$dodge.width)) {
                                         data <- split(data, data$group)
                                       } else {
                                         data <- split(data, data$x)
                                       }
                                       
                                       # perform swarming separately for each data.frame
                                       data <- lapply(
                                         data,
                                         offset_beeswarm,
                                         yRange = yRange,
                                         xRange.expand = xRange.expand, 
                                         yRange.expand = yRange.expand,
                                         yLim.expand = params$yLim.expand,
                                         method = params$method,
                                         cex = params$cex,
                                         side = params$side,
                                         priority = params$priority
                                       )
                                       
                                       # recombine list of data.frames into one
                                       data <- Reduce(rbind, data)
                                       
                                       flip_data(data, params$flipped_aes)
                                     }
)
