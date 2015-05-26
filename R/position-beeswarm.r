#' Beeswarm-style plots to show overlapping points. x must be discrete.
#' 
#' @import proto
#' @name position-beeswarm
#' @family position adjustments
#' @param width the maximum amount of spread (default: 0.4)
#' @param nbins number of points to use to estimate point density (default: 128)
#' @export
#' @examples
#' 
#' qplot(class, hwy, data = mpg, position="beeswarm")
#' # Generate fake data
#' distro <- melt(data.frame(list(runif=runif(100, min=-3, max=3), rnorm=rnorm(100))))
#' qplot(variable, value, data = distro, position = "beeswarm")
#' # Spacing and number of bins can be adjusted
#' qplot(variable, value, data = distro, position = position_beeswarm(width=0.05, nbins=35))
position_beeswarm <- function (width = NULL, nbins = NULL) {
  PositionBeeswarm$new(width = width, nbins = nbins)
}

PositionBeeswarm <- proto(ggplot2:::Position, {
  
  width=NULL
  nbins=NULL
  
  new <- function(., width=NULL, nbins=NULL) {
    .$proto(width=width, nbins=nbins)
  }
  
  objname <- "beeswarm"
  
  # Helper functions
  # From https://stat.ethz.ch/pipermail/r-help/2008-May/162911.html
  number2digits <- function(n, base){
    #first digit in output is the least significant
    digit <- n%%base
    if (n < base)
      return(digit)
    else
      return(c(digit, number2digits((n-digit)/base, base)))
  }
  
  digits2number <- function(digits, base){
    #first digit in input should be the most significant
    output <- 0
    for (digit in digits) output <- (base*output) + digit
    return(output)
  }
  
  vanDerCorput <- function(n, base=2){
    #generate n first digits of the van der Corput sequence
    output <- rep(NA,n)
    for(i in 1:n){
      digits <- number2digits(i, base)
      output[i] <- digits2number(digits, base)/base^length(digits)
    }
    return(output)
  }

  # Adjust function is used to calculate new positions (from ggplot2:::Position)
  adjust <- function(., data) {
    if (empty(data)) return(data.frame())
    check_required_aesthetics(c("x", "y"), names(data), "position_beeswarm")

    if (is.null(.$width)) .$width <- resolution(data$x, zero = FALSE) * 0.4
    if (is.null(.$nbins)) .$nbins <- 128
    
    trans_x <- NULL
    trans_y <- NULL
    
    if(.$width > 0) {
      trans_x <- function(x) {
        
        # Split the y-values by discrete values of x
        y_split <- split(data$y, x)
        
        # Apply the van der Corput noise to each x group to create offsets
        x_offsets <- lapply(y_split, function(y_subgroup) {
          
          # If there's only one value in this group, leave it alone
          if (length(y_split) == 1) return(0) 

          out <- rep(NA, length(y_subgroup))
          dens <- density(y_subgroup, n=.$nbins)
          dens$y <- dens$y / max(dens$y)
          offset <- vanDerCorput(length(y_subgroup))[rank(y_subgroup, ties.method="first")]
          for (i in 1:.$nbins) {
            selector <- y_subgroup <= dens$x[i] & is.na(out)
            out[selector] <- (offset[selector]*.$width*2-.$width) * dens$y[i]
          }
          return(out)
        })
        
        # Join the x-offsets together, then re-apply the old positions
        new_x <- unsplit(x_offsets, x)
        return(new_x + x)
      }
        
    }

    transform_position(data, trans_x, trans_y)
  }

})

