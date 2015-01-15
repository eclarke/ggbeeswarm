#' Beeswarm-style plots to show overlapping points. x must be discrete.
#' 
#' @import proto
#' @name position-beeswarm
#' @family position adjustments
#' @param width degree of jitter in x direction. Defaults to 90\% of the
#'   resolution of the data.
#' @param nbins the number of divisions of the y-axis to use (default: \code{length(y)/5})
#' @param spacing the spacing between adjacent dots (generally between 0 and 1)
#' @export
#' @examples
#'
#' qplot(class, hwy, data = mpg, position="beeswarm")
#' # Generate fake data
#' distro <- melt(data.frame(list(runif=runif(100, min=-3, max=3), rnorm=rnorm(100))))
#' qplot(variable, value, data = distro, position = "beeswarm")
#' # Spacing and y-bin width can be adjusted
#' qplot(variable, value, data = distro, position = position_beeswarm(spacing = 1/20, nbins=35))
#' # Alternatively, can specify \code{width} parameter like \code{position_jitter}
#' qplot(variable, value, data = distro, position = position_beeswarm(width=0.4)

position_beeswarm <- function (width = NULL, nbins = NULL, spacing = NULL, sesh=NULL,
                               sesh2=NULL) {
  PositionBeeswarm$new(width = width, nbins = nbins, spacing = spacing,
                       sesh=sesh, sesh2=sesh2)
}

PositionBeeswarm <- proto(ggplot2:::Position, {
  width = NULL
  nbins = NULL
  spacing = NULL
  sesh = NULL
  sesh2 = NULL

  new <- function(.,
                  width = NULL,
                  nbins = NULL,
                  spacing = NULL,
                  sesh=NULL,
                  sesh2=NULL) {
    .$proto(width=width,
            nbins=nbins,
            spacing=spacing,
            sesh=sesh,
            sesh2=sesh2)
  }

  objname <- "beeswarm"

  adjust <- function(., data) {

    if (empty(data)) return(data.frame())
    check_required_aesthetics(c("x", "y"), names(data), "position_jitter")

    if (is.null(.$width)) .$width <- resolution(data$x, zero = FALSE) * 0.9
    if (is.null(.$nbins)) {
      n <- length(data$y)
#       .$nbins <- as.integer(length(data$y)/(2*density(data$y)$bw))
      .$nbins <- as.integer(length(data$y)/5)
      message("Default number of y-bins used (", .$nbins, ").")
    }
    trans_x <- NULL
    trans_y <- NULL

    if(.$width > 0) {




#       y_bins <- seq(min(data$y), max(data$y), length.out=.$nbins)


      trans_x <- function(x) {
#         dens <- density(data$y, adjust=.$sesh)
#         y_auc <- cumsum(((.$sesh2-dens$y)^(.$sesh))*diff(dens$x)[1])
#         y_cuts <- cut(y_auc, .$nbins)
#         y_bins <- sapply(split(dens$x, y_cuts), min)
        split_y <- split(data$y, x)
        max_len <- NULL #max(sapply(split_y, function(i) max(table(cut(i, y_bins)))))

        x_offsets <- lapply(split_y, function(x_class) {
          dens <- density(x_class, adjust=.$sesh)
          y_auc <- cumsum((((.$sesh2+max(dens$y))-dens$y)^(1/3))*diff(dens$x)[1])
          y_cuts <- cut(y_auc, .$nbins)
          y_bins <- sapply(split(dens$x, y_cuts), min)

          cuts <- cut(x_class, y_bins)
          shifts <- c(-1, 0)
          xy_bins <- split(x_class, cuts)
          even_bins <- sapply(xy_bins, function(i) {
            if (length(i) == 0) {
              return(NULL)
            } else {
              length(i)%%2 == 0
            }
          })
          even_bins <- unlist(even_bins)
          has_adj <- sapply(seq_along(even_bins), function(i) {
            if (i == length(even_bins)) return(0)
            even_bins[i] == even_bins[i+1]
          })

          if (length(has_adj) == 0) has_adj <- 0
          xy_offsets <- suppressWarnings(mapply(function(xy_bin, shift, adj) {
            len <- length(xy_bin)
            if (len == 0) {
              return(xy_bin)
            } else {
              w <- ifelse(is.null(.$spacing), .$width/max_len, .$spacing)
              offsets <- seq((-w)*((len-1)/2), w*((len-1)/2), by=w)
              # Place higher y values at end of "smile"
              offsets <- offsets[order(abs(offsets))][rank(xy_bin, ties="first")]
              # Offset if bin below also has even/odd items
#               if (adj) offsets <- offsets + shift * (w/2)
              return(offsets)
            }
          }, split(x_class, cuts), shifts, has_adj))

          unsplit(xy_offsets, cuts)
        })

        x_offsets <- unsplit(x_offsets, x)
        return(x + x_offsets)
      }
    }

    transform_position(data, trans_x, trans_y)
  }

})
