#' An internal function to calculate new positions for geom_beeswarm
#' 
#' @family position adjustments
#' @param data A data.frame containing plotting data in columns x and y. 
#' Usually obtained from data processed by ggplot2.
#' @param xLim.expand x axis limits plus a small expansion using `ggplot2:::expand_range4`
#' @param yLim.expand y axis limits plus a small expansion using `grDevices::extendrange`
#' @param yLim y axis limits
#' @param xRange x axis range
#' @param yRange y axis range
#' @param method Method for arranging points (see Details below)
#' @param cex Scaling for adjusting point spacing (see \code{\link[beeswarm]{swarmx}})
#' @param side Direction to perform jittering: 0: both directions; 
#' 1: to the right or upwards; -1: to the left or downwards.
#' @param priority Method used to perform point layout (see Details below)
#' @param fast Use compiled version of swarm algorithm? This option is ignored 
#' for all methods expect `"swarm"`, `"swarm2"`, and `"compactswarm"`.
#' 
#' @details 
#' **method:** specifies the algorithm used to avoid overlapping points. The 
#' default `"swarm"` method places points in increasing order. If a point would
#' overlap with an existing point, it is shifted sideways (along the group axis)
#' by a minimal amount sufficient to avoid overlap. The `"swarm2"` method is very
#' similar to `"swarm"` but more closely follows the method used in the `beeswarm`
#' package.
#' 
#' While the `"swarm"` and `"swarm2"` method places points in a predetermined 
#' order, the `"compactswarm"` method uses a greedy strategy to determine which 
#' point will be placed next. This often leads to a more tightly-packed layout. 
#' The strategy is very simple: on each iteration, a point that can be placed as 
#' close as possible to the non-data axis is chosen and placed. If there are two 
#' or more equally good points, `priority` is used to break ties.
#' 
#' The other 3 methods first discretise the values along the data axis, in order
#' to create more efficient packing. The `"square"` method places points on a 
#' square grid, whereas `"hex"` uses a hexagonal grid. `"centre"`/`"center"` 
#' uses a square grid to produce a symmetric swarm. The number of break points 
#' for discretisation is determined by a combination of the available plotting 
#' area and the `spacing` argument.
#' 
#' **priority:** controls the order in which points are placed, which generally 
#' has a noticeable effect on the plot appearance. `"ascending"` gives the 
#' 'traditional' beeswarm plot. `"descending"` is the opposite. `"density"` 
#' prioritizes points with higher local density. `"random"` places points in a 
#' random order. `"none"` places points in the order provided.
#' 
#' @keywords internal
#' @export
#' @importFrom beeswarm swarmx
#' @seealso \code{\link{geom_beeswarm}}, \code{\link{position_quasirandom}}, 
#' \code{\link[beeswarm]{swarmx}}
offset_beeswarm <- function(
  data, 
  xLim.expand,
  yLim.expand,
  yLim,
  xRange,
  yRange,
  method = "swarm",
  cex = 1, 
  side = 0L,
  priority = "ascending",
  fast = TRUE
) {
  if (method %in% c("swarm", "swarm2", "compactswarm")) {
    ## SWARM METHODS
    
    if (method %in% c("swarm2", "compactswarm")) {
      # Determine point size as per `beeswarm` package
      
      # adjust par based on input data
      graphics::par("usr" = c(xLim.expand, yLim),
                    "mar" = c(1.9, 1.9 , 0.3, 0.3))
      # For explanation of why "mar" (base R plot margin) is altered
      # see https://github.com/csdaw/ggbeeswarm2/issues/1#issuecomment-888376988
      # Note this may cause issues if the user plays around with 
      # ggplot2::theme(plot.margin)
      
      x.size <- graphics::xinch(0.08, warn.log = FALSE)
      y.size <- graphics::yinch(0.08, warn.log = FALSE)
    } else {
      # Determine point size as per `ggbeeswarm` CRAN version 0.6.0
      
      # divisor is a magic number to get a reasonable baseline
      # better option would be to figure out point size in user coords
      x.size <- xRange / 100
      y.size <- yRange / 100
    }
    
    compact <- method == "compactswarm"
    
    x.offset <- beeswarm::swarmx(
      x = rep(0, length(data$y)), 
      y = data$y,
      xsize = x.size,
      ysize = y.size,
      cex = cex, side = side, priority = priority,
      fast = fast, compact = compact
    )$x
  } else {
    ## NON-SWARM METHODS
    
    # Determine point size as per `beeswarm` package
    
    # adjust par based on input data
    graphics::par("usr" = c(xLim.expand, yLim),
                  "mar" = c(1.9, 1.9 , 0.3, 0.3))
    # For explanation of why "mar" (base R plot margin) is altered
    # see https://github.com/csdaw/ggbeeswarm2/issues/1#issuecomment-888376988
    # Note this may cause issues if the user plays around with 
    # ggplot2::theme(plot.margin)
    
    x.size <- graphics::xinch(0.08, warn.log = FALSE)
    y.size <- graphics::yinch(0.08, warn.log = FALSE)
    
    # Hex method specific step
    if (method == "hex") y.size <- y.size * sqrt(3) / 2
    
    # Determine positions along the y axis
    breaks <- seq(yLim.expand[1], yLim.expand[2] + y.size, by = y.size)
    
    mids <- (utils::head(breaks, -1) + utils::tail(breaks, -1)) / 2
    y.index <- sapply(data$y, cut, breaks = breaks, labels = FALSE)
    
    y.pos <- sapply(y.index, function(a) mids[a])
    data$y <- y.pos
    
    # Determine positions along the x axis
    x.index <- determine_pos(y.index, method, side)
    
    x.offset <- x.index * x.size
  }
  
  data$x <- data$x + x.offset
  return(data)
}

position_beeswarm <- function(
  method = "swarm",
  cex = 1,
  side = 0L,
  priority = "ascending",
  fast = TRUE,
  groupOnX = NULL, 
  dodge.width = 0
) {
  if (!missing(groupOnX)) warning("The `groupOnX` argument of `position_beeswarm` is deprecated as of ggbeeswarm 0.7.0.9000.")
  
  match.arg(method, c("swarm", "swarm2", "compactswarm", 
                      "square", "hex", "centre", "center"))
  
  if (method == "centre") method <- "center"
  
  ggproto(NULL, PositionBeeswarm, 
          method = method,
          cex = cex,
          side = side,
          priority = priority,
          fast = fast,
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
                                         fast = self$fast,
                                         dodge.width = self$dodge.width,
                                         yLim.expand = yLim.expand,
                                         flipped_aes = flipped_aes 
                                       )
                                     },
                                     compute_panel = function(data, params, scales) {
                                       data <- flip_data(data, params$flipped_aes)
                                       
                                       # get plot limits
                                       if (params$flipped_aes) {
                                         xLim.expand <- ggplot2:::expand_range4(c(1, length(scales$y$get_limits())), c(0, 0.6))
                                         yLim <- scales$x$get_limits()
                                         
                                         xRange <- get_range(scales$y)
                                         yRange <- get_range(scales$x)
                                       } else {
                                         xLim.expand <- ggplot2:::expand_range4(c(1, length(scales$x$get_limits())), c(0, 0.6))
                                         yLim <- scales$y$get_limits()
                                        
                                         xRange <- get_range(scales$x)
                                         yRange <- get_range(scales$y)
                                       }
                                       
                                       # capture current par values
                                       current.usr <- graphics::par("usr")
                                       current.mar <- graphics::par("mar")
                                       # on exit return par("usr") to normal
                                       on.exit(graphics::par("usr" = current.usr, "mar" = current.mar), add = TRUE)
                                       
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
                                         xLim.expand = xLim.expand, 
                                         yLim.expand = params$yLim.expand,
                                         yLim = yLim,
                                         xRange = xRange,
                                         yRange = yRange,
                                         method = params$method,
                                         cex = params$cex,
                                         side = params$side,
                                         priority = params$priority,
                                         fast = params$fast
                                       )
                                       
                                       # recombine list of data.frames into one
                                       data <- Reduce(rbind, data)
                                       
                                       flip_data(data, params$flipped_aes)
                                     }
)

get_range <- function(scales) {
  if (is.null(scales$limits)) lim <- scales$range$range
  else lim <- scales$get_limits()
  
  if (inherits(scales, "ScaleContinuous")) {
    out <- diff(lim)
  } else if (inherits(scales, "ScaleDiscrete")) {
    out <- length(unique(lim))
  } else {
    stop("Unknown scale type")
  }
  
  if (out == 0) out <- 1
  out
}

determine_pos <- function(v, method, side) {
  # if(length(stats::na.omit(v)) == 0) 
  #   return(v)
  
  v.s <- lapply(split(v, v), seq_along)
  
  if(method %in% c("center", "square") && side == -1)
    v.s <- lapply(v.s, function(a) a - max(a))
  else if(method %in% c("center", "square") && side == 1)
    v.s <- lapply(v.s, function(a) a - 1)
  else if(method == "center")
    v.s <- lapply(v.s, function(a) a - mean(a))
  else if(method == "square")
    v.s <- lapply(v.s, function(a) a - floor(mean(a)))
  else if(method == "hex") {
    odd.row <- (as.numeric(names(v.s)) %% 2) == 1
    if(side == 0) {
      v.s[ odd.row] <- lapply(v.s[ odd.row], function(a) a - floor(mean(a)) - 0.25)
      v.s[!odd.row] <- lapply(v.s[!odd.row], function(a) a - ceiling(mean(a)) + 0.25)
    } else if(side == -1) {
      v.s[ odd.row] <- lapply(v.s[ odd.row], function(a) a - max(a))
      v.s[!odd.row] <- lapply(v.s[!odd.row], function(a) a - max(a) - 0.5)
    } else if(side ==  1) {
      v.s[ odd.row] <- lapply(v.s[ odd.row], function(a) a - 1)
      v.s[!odd.row] <- lapply(v.s[!odd.row], function(a) a - 0.5)
    }
  }
  unsplit(v.s, v)
}
