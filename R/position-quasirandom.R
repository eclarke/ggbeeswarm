#' Arrange points using quasirandom noise to avoid overplotting
#' 
#' @family position adjustments
#' @param method the method used for distributing points
#' (quasirandom, pseudorandom, smiley, maxout, frowney, minout, tukey, tukeyDense).
#' See [vipor::offsetSingleGroup()] for the details of each method.
#' @param width the maximum amount of spread (default: 0.4)
#' @param varwidth vary the width by the relative size of each group
#' @param bandwidth the bandwidth adjustment to use when calculating density
#' Smaller numbers (< 1) produce a tighter "fit". (default: 0.5)
#' @param nbins the number of bins used when calculating density
#' (has little effect with quasirandom/random distribution)
#' @param dodge.width Amount by which points from different aesthetic groups 
#' will be dodged. This requires that one of the aesthetics is a factor.
#' @param na.rm if FALSE, the default, missing values are removed with a warning.
#' If TRUE, missing values are silently removed.
#' @param orientation Normally the orientation is determined by which axis is grouped or distinct. 
#' This can be overridden by setting `orientation` to either `"x"` or `"y"`.
#' @param groupOnX `r lifecycle::badge("superseded")` See `orientation`.
#' @importFrom vipor offsetSingleGroup
#' @export
#' @seealso [vipor::offsetSingleGroup()], [geom_quasirandom()]
position_quasirandom <- function(
  method = 'quasirandom',
  width = NULL, 
  varwidth = FALSE, 
  bandwidth = .5,
  nbins = NULL,
  dodge.width = 0,
  orientation = NULL,
  groupOnX = NULL,
  na.rm = FALSE
) {
  
  
  if (!missing(groupOnX)) {
    lifecycle::deprecate_soft(
      when = "0.7.1", what = "position_quasirandom(groupOnX)", 
      details='Orientation is now guessed from the data. To override, specify orientation="x" or "y".'
    )
    if (groupOnX) {
      orientation = "x"
    } else {
      orientation = "y"
    }
  }
  
  if (!method %in% c("quasirandom", "pseudorandom", "smiley", "maxout", "frowney", "minout", "tukey", "tukeyDense")) {
    cli::cli_abort("{.fn method} must be one of: quasirandom, pseudorandom, smiley, maxout, frowney, minout, tukey, or tukeyDense.")
  }
  
  if (!is.null(orientation) && !(orientation %in% c("x", "y"))) {
    cli::cli_abort("{.fn orientation} must be 'x', 'y', or NULL.")
  }
  
  
  ggproto(NULL, PositionQuasirandom, 
          width = width, 
          varwidth = varwidth, 
          bandwidth = bandwidth,
          nbins = nbins,
          method = method,
          dodge.width = dodge.width,
          na.rm = na.rm,
          orientation = orientation
  )
}

PositionQuasirandom <- ggplot2::ggproto("PositionQuasirandom", Position,
                                        required_aes = c('x', 'y'),
                                        setup_params = function(self, data) {
                                          
                                          params <- list(
                                            width = self$width,
                                            varwidth = self$varwidth,
                                            bandwidth = self$bandwidth,
                                            nbins = self$nbins,
                                            method = self$method,
                                            dodge.width = self$dodge.width,
                                            na.rm = self$na.rm,
                                            orientation = self$orientation
                                          )
                                          
                                          if (!is.null(params$orientation)) {
                                            flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)                                                                                      
                                          } else {
                                            flipped_aes <- has_flipped_aes(data, group_has_equal = TRUE)
                                            if (flipped_aes) {
                                              cli::cli_inform("Orientation inferred to be along y-axis; override with `position_quasirandom(orientation = 'x')`")
                                            }
                                          }

                                          params$flipped_aes <- flipped_aes
                                          
                                          data <- flip_data(data, params$flipped_aes)
                                          
                                          # get the number of points in each x axis group
                                          # and find the largest group
                                          params$max_length <- max(data.frame(table(data$x))$Freq)
                                          
                                          params
                                        },
                                        
                                        setup_data = function(self, data, params) {
                                          data <- flip_data(data, params$flipped_aes)
#                                          data$x <- data$x %||% 0
                                          data <- remove_missing(
                                            data,
                                            na.rm = params$na.rm,
                                            vars = "y",
                                            name = "position_quasirandom"
                                          )
                                          flip_data(data, params$flipped_aes)
                                        },
                                        
                                        compute_panel = function(data, params, scales) {
                                          data <- flip_data(data, params$flipped_aes)

                                          # perform dodging if necessary
                                          data <- ggplot2:::collide(
                                            data,
                                            params$dodge.width,
                                            name = "position_quasirandom",
                                            strategy = ggplot2:::pos_dodge,
                                            check.width = FALSE
                                          )
                                          
                                          # ggplot2::resolution needs to be after dodge
                                          # set width if not specified
                                          if (is.null(params$width)) {
                                            params$width <- ggplot2::resolution(
                                              data$x, zero = FALSE) * 0.4
                                          }
                                          
                                          # split data.frame into list of data.frames
                                          if (!is.null(params$dodge.width)) {
                                            data <- split(data, data$group)
                                          } else {
                                            data <- split(data, data$x)
                                          }
                                          
                                          # perform transformation separately for each data.frame
                                          data <- lapply(
                                            data,
                                            offset_quasirandom,
                                            method = params$method,
                                            width = params$width,
                                            vary.width = params$varwidth,
                                            adjust = params$bandwidth,
                                            nbins = params$nbins,
                                            max.length = params$max.length
                                          )
                                          
                                          # recombine list of data.frame into one
                                          data <- Reduce(rbind, data)
                                          
                                          flip_data(data, params$flipped_aes)
                                        }
)

offset_quasirandom <- function(
  data,
  width = 0.4, 
  vary.width = FALSE,
  max.length = NULL,
  # na.rm = FALSE,
  ...
) {
  # if (any(is.na(data$y))) {
  #   if (na.rm) {
  #     
  #   }
  # }
  x.offset <- vipor::aveWithArgs(
    data$y, data$x,
    FUN = vipor::offsetSingleGroup,
    maxLength = if (vary.width) {max.length} else {NULL},
    ...
  )
  
  x.offset <- x.offset * width
  data$x <- data$x + x.offset
  data
}

