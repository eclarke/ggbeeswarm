#' An internal function to calculate new positions for geom_beeswarm
#' 
#' @family position adjustments
#' @param data A data.frame containing plotting data in columns x and y. Usually obtained from data processed by ggplot2.
#' @param xRange span of the x axis e.g. yMax-yMin
#' @param yRange span of the y axis e.g. yMax-yMin
#' @param priority Method used to perform point layout (see \code{\link{swarmx}})
#' @param cex Scaling for adjusting point spacing (see \code{\link{swarmx}})
#' @param groupOnX if TRUE then jitter is added to the x axis and if FALSE jitter is added to the y axis. Prior to v0.6.0, the default NULL causes the function to guess which axis is the categorical one based on the number of unique entries in each. This could result in unexpected results when the x variable has few unique values and so in v0.6.0 the default was changed to always jitter on the x axis unless groupOnX=FALSE. Also consider \code{\link[ggplot2]{coord_flip}}.
#' @param dodge.width Amount by which points from different aesthetic groups will be dodged. This requires that one of the aesthetics is a factor.
#' @param beeswarmArgs A list of additional arguments to be passed to the \link{swarmx} function of beeswarm e.g. \code{list(side=1)} or \code{list(side=-1)} to only distribute points to the right/left side
#' @param oSize A 2 element vector giving the width and height of a cex=1 character "o" in user coordinates
#' @export
#' @importFrom beeswarm swarmx
#' @seealso \code{\link{geom_beeswarm}}, \code{\link{position_quasirandom}}, \code{\link[beeswarm]{swarmx}}
offset_beeswarm <- function(
  data, 
  plot.ylim.short,
  plot.xlim,
  plot.ylim,
  y.lim,
  method = "swarm",
  cex = 1, 
  side = 0L,
  priority = "ascending"
) {
  if (method == "swarm") {
    # adjust par("usr") based on input data
    graphics::par("usr" = c(plot.xlim, plot.ylim.short))
    
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
  ggproto(NULL, PositionBeeswarm, 
          method = method,
          cex = cex,
          side = side,
          priority = priority,
          # groupOnX = groupOnX, deprecated
          dodge.width = dodge.width
  )
}

PositionBeeswarm <- ggplot2::ggproto("PositionBeeswarm", Position, 
                                     required_aes = c('x', 'y'),
                                     setup_params = function(self, data) {
                                       flipped_aes <- has_flipped_aes(data)
                                       data <- flip_data(data, flipped_aes)
                                       
                                       # get y range of data and extend it a little
                                       y.lim <- grDevices::extendrange(data$y, f = 0.01)
                                       
                                       list(
                                         # groupOnX = self$groupOnX, deprecated
                                         method = self$method,
                                         cex = self$cex,
                                         side = self$side,
                                         priority = self$priority,
                                         dodge.width = self$dodge.width,
                                         y.lim = y.lim,
                                         flipped_aes = flipped_aes 
                                       )
                                     },
                                     compute_panel = function(data, params, scales) {
                                       data <- flip_data(data, params$flipped_aes)
                                       
                                       # get plot limits
                                       if (params$flipped_aes) {
                                         plot.ylim.short <- scales$x$get_limits()
                                         plot.ylim <- ggplot2:::expand_range4(scales$x$get_limits(), c(0.045, 0))
                                         plot.xlim <- ggplot2:::expand_range4(c(1, length(scales$y$get_limits())), c(0, 0.6))
                                       } else {
                                         plot.ylim.short <- scales$y$get_limits()
                                         plot.ylim <- ggplot2:::expand_range4(scales$y$get_limits(), c(0.045, 0))
                                         plot.xlim <- ggplot2:::expand_range4(c(1, length(scales$x$get_limits())), c(0, 0.6))
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
                                         plot.ylim.short = plot.ylim.short,
                                         plot.xlim = plot.xlim, plot.ylim = plot.ylim,
                                         y.lim = params$y.lim,
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
