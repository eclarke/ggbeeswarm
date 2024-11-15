#' An internal function to calculate new positions for geom_beeswarm
#' 
#' @family position adjustments
#' @param data A data.frame containing plotting data in columns x and y. 
#' Usually obtained from data processed by ggplot2.
#' @param yLim.expand y data limits plus a small expansion using `grDevices::extendrange`
#' @param xRange x axis scale range
#' @param yRange y axis scale range
#' @param method Method for arranging points (see Details below)
#' @param cex Scaling for adjusting point spacing (see [beeswarm::swarmx()]).
#' Values between 1 (default) and 3 tend to work best.
#' @param side Direction to perform jittering: 0: both directions; 
#' 1: to the right or upwards; -1: to the left or downwards.
#' @param priority Method used to perform point layout (see Details below)
#' @param fast Use compiled version of swarm algorithm? This option is ignored 
#' for all methods expect `"swarm"` and `"compactswarm"`.
#' @param corral `string`. Method used to adjust points that would be placed to
#' wide horizontally, default is `"none"`. See details below.
#' @param corral.width `numeric`. Width of the corral, default is `0.9`.
#' 
#' @details 
#' **method:** specifies the algorithm used to avoid overlapping points. The 
#' default `"swarm"` method places points in increasing order. If a point would
#' overlap with an existing point, it is shifted sideways (along the group axis)
#' by a minimal amount sufficient to avoid overlap.
#' 
#' While the `"swarm"` method places points in a predetermined 
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
#' area and the `cex` argument.
#' 
#' **priority:** controls the order in which points are placed, which generally 
#' has a noticeable effect on the plot appearance. `"ascending"` gives the 
#' 'traditional' beeswarm plot. `"descending"` is the opposite. `"density"` 
#' prioritizes points with higher local density. `"random"` places points in a 
#' random order. `"none"` places points in the order provided.
#' 
#' **corral:** By default, swarms from different groups are not prevented from
#' overlapping, i.e. `"corral = "none"`. Thus, datasets that are very large or 
#' unevenly distributed may produce ugly overlapping beeswarms. To control 
#' runaway points one can use the following methods. `"gutter"` collects runaway
#' points along the boundary between groups. `"wrap"` implement periodic boundaries.
#' `"random"` places runaway points randomly in the region. `"omit"` omits runaway
#' points.
#' 
#' @keywords internal
#' @importFrom beeswarm swarmx
#' @seealso [geom_beeswarm()], [position_quasirandom()], 
#' [beeswarm::swarmx()]
offset_beeswarm <- function(
  data, 
  yLim.expand,
  xRange,
  yRange,
  method = "swarm",
  cex = 1, 
  side = 0L,
  priority = "ascending",
  fast = TRUE,
  corral = "none",
  corral.width = 0.2
) {
  if (method %in% c("swarm", "compactswarm")) {
    ## SWARM METHODS
    
    # Determine point size as per `ggbeeswarm` CRAN version 0.6.0
    
    # divisor is a magic number to get a reasonable baseline
    # better option would be to figure out point size in user coords
    x.size <- xRange / 100
    y.size <- yRange / 100
    
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
    # Determine point size as per `ggbeeswarm` CRAN version 0.6.0
    
    # divisor is a magic number to get a reasonable baseline
    # better option would be to figure out point size in user coords
    x.size <- xRange / 100 * cex
    y.size <- yRange / 100 * cex
    
    # Hex method specific step
    if (method == "hex") y.size <- y.size * sqrt(3) / 2

    # Determine positions along the y axis
    breaks <- seq(yLim.expand[1], yLim.expand[2] + y.size, by = y.size)
    
    mids <- (utils::head(breaks, -1) + utils::tail(breaks, -1)) / 2

    # include.lowest = T to account for cases where all y values are the same,
    # which otherwise would result in NAs. Fixes issue #85.
    y.index <- sapply(data$y, cut, breaks = breaks, include.lowest=T, labels = FALSE)
    y.pos <- sapply(y.index, function(a) mids[a])          
    
    

    if (any(data$y != y.pos)) {
      cli::cli_warn(c(
        "In `position_beeswarm`, method `{method}` discretizes the data axis (a.k.a the continuous or non-grouped axis).",
        "This may result in changes to the position of the points along that axis, proportional to the value of `cex`."
      ), .frequency = "once", .frequency_id = "beeswarm_method_data_axis_warn")
    }
    data$y <- y.pos
    
    # Determine positions along the x axis
    x.index <- determine_pos(y.index, method, side)
    
    x.offset <- x.index * x.size

  }
  
  ## CORRAL RUNAWAY POINTS
  if (corral != "none") {
    corral.low <- (side - 1) * corral.width / 2
    corral.high <- (side + 1) * corral.width / 2
    
    if (corral == "gutter") {
      x.offset <- sapply(
        x.offset, 
        function(zz) pmin(corral.high, pmax(corral.low, zz))
      )
    }
    if (corral == "wrap") {
      if (side == -1L) {
        # special case with side=-1: reverse the corral to avoid artefacts at zero
        x.offset <- sapply(
          x.offset, 
          function(zz) corral.high - ((corral.high - zz) %% corral.width)
        )
      } else {
        x.offset <- sapply(
          x.offset, 
          function(zz) ((zz - corral.low) %% corral.width) + corral.low
        )
      }
    }
    if (corral == 'random') {
      x.offset <- sapply(
        x.offset, 
        function(zz) ifelse(
          zz > corral.high | zz < corral.low, 
          yes = stats::runif(length(zz), corral.low, corral.high), 
          no = zz
        )
      )
    }
    if (corral == 'omit') {
      x.offset <- sapply(
        x.offset, 
        function(zz) ifelse(
          zz > corral.high | zz < corral.low, 
          yes = NA, 
          no = zz
        )
      )
    }
  }
  
  data$x <- data$x + x.offset
  return(data)
}

#' Arrange points using the `\link[beeswarm]` package.
#' 
#' @family position adjustments
#' @param method Method for arranging points (see Details below)
#' @param cex Scaling for adjusting point spacing (see [beeswarm::swarmx()]).
#' Values between 1 (default) and 3 tend to work best.
#' @param side Direction to perform jittering: 0: both directions; 
#' 1: to the right or upwards; -1: to the left or downwards.
#' @param priority Method used to perform point layout (see Details below)
#' @param fast Use compiled version of swarm algorithm? This option is ignored 
#' for all methods expect `"swarm"` and `"compactswarm"`.
#' @param dodge.width Amount by which points from different aesthetic groups 
#' will be dodged. This requires that one of the aesthetics is a factor.
#' @param corral `string`. Method used to adjust points that would be placed to
#' wide horizontally, default is `"none"`. See details below.
#' @param corral.width `numeric`. Width of the corral, default is `0.9`.
#' @param orientation The orientation (i.e., which axis to group on) is inferred from the data.
#' This can be overridden by setting `orientation` to either `"x"` or `"y"`.
#' @param groupOnX `r lifecycle::badge("superseded")` See `orientation`.

#' @details 
#' **method:** specifies the algorithm used to avoid overlapping points. The 
#' default `"swarm"` method places points in increasing order. If a point would
#' overlap with an existing point, it is shifted sideways (along the group axis)
#' by a minimal amount sufficient to avoid overlap.
#' 
#' While the `"swarm"` method places points in a predetermined 
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
#' area and the `cex` argument.
#' 
#' **priority:** controls the order in which points are placed, which generally 
#' has a noticeable effect on the plot appearance. `"ascending"` gives the 
#' 'traditional' beeswarm plot. `"descending"` is the opposite. `"density"` 
#' prioritizes points with higher local density. `"random"` places points in a 
#' random order. `"none"` places points in the order provided.
#' 
#' **corral:** By default, swarms from different groups are not prevented from
#' overlapping, i.e. `"corral = "none"`. Thus, datasets that are very large or 
#' unevenly distributed may produce ugly overlapping beeswarms. To control 
#' runaway points one can use the following methods. `"gutter"` collects runaway
#' points along the boundary between groups. `"wrap"` implement periodic boundaries.
#' `"random"` places runaway points randomly in the region. `"omit"` omits runaway
#' points.
#' 
#' 
#' @export
#' @importFrom beeswarm swarmx
#' @seealso [geom_beeswarm()], [position_quasirandom()], 
#' [beeswarm::swarmx()]
position_beeswarm <- function(
  method = "swarm",
  cex = 1,
  side = 0L,
  priority = "ascending",
  fast = TRUE,
  orientation = NULL,
  groupOnX = NULL,
  dodge.width = 0,
  corral = "none",
  corral.width = 0.2
) {
  
  if (!missing(groupOnX)) {
    lifecycle::deprecate_soft(
      when = "0.7.1", what = "position_beeswarm(groupOnX)", 
      details='The axis to group on is now guessed from the data. To override, specify orientation="x" or "y".'
    )
    if (groupOnX) {
      orientation = "x"
    } else {
      orientation = "y"
    }
  }
  if (!is.null(orientation) && !(orientation %in% c("x", "y"))) {
    cli::cli_abort("{.fn orientation} must be 'x', 'y', or NULL.")
  }
  if (method == "centre") method <- "center"
  
  ggproto(NULL, PositionBeeswarm, 
          method = method,
          cex = cex,
          side = side,
          priority = priority,
          fast = fast,
          orientation = orientation,
          dodge.width = dodge.width,
          corral = corral,
          corral.width = corral.width
  )
}

PositionBeeswarm <- ggplot2::ggproto("PositionBeeswarm", Position, 
                                     required_aes = c('x', 'y'),
                                     setup_params = function(self, data) {
                                       params <- list(
                                         method = self$method,
                                         cex = self$cex,
                                         side = self$side,
                                         priority = self$priority,
                                         fast = self$fast,
                                         dodge.width = self$dodge.width,
                                         corral = self$corral,
                                         corral.width = self$corral.width,
                                         orientation = self$orientation
                                       )
                                       if (!is.null(params$orientation)) {
                                         flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)                                                                                      
                                       } else {
                                         flipped_aes <- has_flipped_aes(data, group_has_equal = TRUE)
                                         if (flipped_aes) {
                                           cli::cli_inform("Orientation inferred to be along y-axis; override with `position_beeswarm(orientation = 'x')`")
                                         }
                                       }
                                       params$flipped_aes <- flipped_aes
                                       data <- flip_data(data, params$flipped_aes)
                                       # get y range of data and extend it a little
                                       params$yLim.expand <- grDevices::extendrange(data$y, f = 0.01)
                                       params
                                     },
                                     compute_panel = function(data, params, scales) {
                                       data <- flip_data(data, params$flipped_aes)
                                       
                                       # get plot limits
                                       if (params$flipped_aes) {
                                         xRange <- get_range(scales$y)
                                         yRange <- get_range(scales$x)
                                       } else {
                                         xRange <- get_range(scales$x)
                                         yRange <- get_range(scales$y)
                                       }
                                       
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
                                         yLim.expand = params$yLim.expand,
                                         xRange = xRange,
                                         yRange = yRange,
                                         method = params$method,
                                         cex = params$cex,
                                         side = params$side,
                                         priority = params$priority,
                                         fast = params$fast,
                                         corral = params$corral,
                                         corral.width = params$corral.width
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
