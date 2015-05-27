# Helper functions
# From https://stat.ethz.ch/pipermail/r-help/2008-May/162911.html

#' Offset data to avoid overplotting. 
#' 
#' Arranges data points using a van der Corput sequence to form "beeswarm" style
#' plots. Returns a vector of x-offsets of the same length that 
#' 
#' @param y vector of data points 
#' @param x a grouping factor for y (i.e. what will be the x axis)
#' @param width the maximum x-width of each group (0 = no width, 1 = max; default = 0.4)
#' @param var_width adjust the width of each group based on the number of points in the group
#' @param adjust bandwidth used to adjust the density
#' @param nbins the number of points used to calculate density
#' @export
#' @examples 
#' 
#' 
#' 
offset_x <- function(y, x, width=0.4, var_width=FALSE, adjust=0.5, nbins=1000) {
  
  if (missing(x)) x <- rep(1, length(y))
  
  # Split the y-values by discrete values of x
  y_split <- split(y, x)
  
  # Apply the van der Corput noise to each x group to create offsets
  x_offsets <- lapply(y_split, function(y_subgroup) {
    
    # If there's only one value in this group, leave it alone
    if (length(y_split) == 1) return(0) 
    
    out <- rep(NA, length(y_subgroup))
    
    subgroup_width <- 1
    if (var_width) subgroup_width <- length(y_subgroup)/max(table(x))
    
    dens <- density(y_subgroup, n = nbins, adjust = adjust)
    dens$y <- dens$y / max(dens$y)
    offset <- vanDerCorput(length(y_subgroup))[rank(y_subgroup, ties.method="first")]
    
    for (i in 1:nbins) {
      # Select the points in a bin
      selector <- y_subgroup <= dens$x[i] & is.na(out)
      out[selector] <- (offset[selector] * width * 2 - width) * dens$y[i] * subgroup_width
    }
    return(out)
  })
  
  # Join the x-offsets together
  new_x <- unsplit(x_offsets, x)
  return(new_x)
}

#' Generate van der Corput sequences
#' 
#' @param n the first n digits of the van der Corput sequence
#' @param base the base to use for calculating the van der Corput sequence
#' @export
vanDerCorput <- function(n, base=2){
  #generate n first digits of the van der Corput sequence
  output <- rep(NA,n)
  for(i in 1:n){
    digits <- number2digits(i, base)
    output[i] <- digits2number(digits, base)/base^length(digits)
  }
  return(output)
}

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
