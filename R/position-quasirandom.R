#' Arrange points using quasirandom noise to avoid overplotting
#' 
#' @family position adjustments
#' @param method Method used for distributing points.
#' Options are `"quasirandom"` (default), `"pseudorandom"`, `"smiley"`, `"maxout"`, `"frowney"`, `"minout"`, `"tukey"`, `"tukeyDense"`.
#' See [vipor::offsetSingleGroup()] for the details of each method.
#' @param width Maximum amount of spread (default: 0.4)
#' @param varwidth Vary the width by the relative size of each group. (default: `FALSE`)
#' @param bandwidth the bandwidth adjustment to use when calculating density
#' Smaller numbers (< 1) produce a tighter "fit". (default: 0.5)
#' @param nbins the number of bins used when calculating density
#' (has little effect with quasirandom/random distribution)
#' @param dodge.width Amount by which points from different aesthetic groups 
#' will be dodged. This requires that one of the aesthetics is a factor. 
#' To disable dodging between groups, set this to NULL. (default: 0)
#' @param na.rm if `FALSE` (default), missing values are removed with a warning.
#' If `TRUE`, missing values are silently removed.
#' @param orientation The orientation (i.e., which axis to group on) is inferred from the data.
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
      details='The axis to group on is now guessed from the data. To override, specify orientation="x" or "y".'
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
                                          
                                          # check that the number of groups < number of data points
                                          if (!anyDuplicated(data$group)) {
                                            if (!is.null(params$dodge.width)) {
                                              # Warn if dodge.width was set to something besides default
                                              if (params$dodge.width != 0) {
                                                cli::cli_inform(
                                                  "Each group consists of only one observation; resetting dodge.width to NULL.",
                                                  "Disable this message by explicitly setting `dodge.width=NULL`, or by adjusting the group aesthetic."
                                                )
                                              }
                                              params$dodge.width = NULL
                                            }


                                          }
                                          
                                          params
                                        },
                                        
                                        setup_data = function(self, data, params) {

                                          data <- flip_data(data, params$flipped_aes)
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

  if ('xend' %in% colnames(data) && 'yend' %in% colnames(data)) {
      x.offset <- vipor::aveWithArgs(
        data$yend, data$xend,
        FUN = vipor::offsetSingleGroup,
        maxLength = if (vary.width) {max.length} else {NULL},
        ...
      )

      x.offset <- x.offset * width
      data$xend <- data$xend + x.offset
  }
  data
}

