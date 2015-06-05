#' Offset data to avoid overplotting. 
#' 
#' Arranges data points using a van der Corput sequence to form "beeswarm" style
#' plots. Returns a vector of x-offsets of the same length that 
#' 
#' @param y vector of data points 
#' @param x a grouping factor for y (optional)
#' @param width the maximum x-width of each group (0 = no width, 1 = max; default = 0.4)
#' @param var_width adjust the width of each group based on the number of points in the group
#' @param adjust bandwidth used to adjust the density
#' @param nbins the number of points used to calculate density
#' @export
#' @examples 
#' ## Generate fake data
#' dat <- list(rnorm(50), rnorm(500), c(rnorm(100), rnorm(100,5)), rcauchy(100))
#' labels <- c("Normal", "Dense Norm.", "Bimodal", "Extremes")
#' 
#' ## Plot each distribution with a variety of parameters
#' par(mfrow=c(4,1), mar=c(2,4, 0.5, 0.5))
#' mapply(function(y, label) {
#'   ids <- rep(1:4, each=length(y))
#'   
#'   offsets <- c(
#'     offset_x(y),  # Default
#'     offset_x(y, adjust=2),    # More smoothing
#'     offset_x(y, adjust=0.1),  # Tighter fit
#'     offset_x(y, width=0.1))   # Less wide
#'   
#'   plot(offsets + ids, rep(y, 4), ylab=label, xlab='', xaxt='n', pch=21, las=1)
#'   axis(1, 1:4, c("Default", "Adjust=2", "Adjust=0.1", "Width=10%"))
#' }, dat, labels)
#' 
offset_x <- function(y, x, width=0.4, var_width=FALSE, adjust=0.5, nbins=1000) {
  
  if (missing(x)) x <- rep(1, length(y))
  
  maxLength<-max(table(x))

  # Apply the van der Corput noise to each x group to create offsets
  new_x <- ave(y,x, FUN=function(y_subgroup) {
    
    # If there's only one value in this group, leave it alone
    if (length(y_subgroup) == 1) return(0) 
    
    out <- rep(NA, length(y_subgroup))
    
    subgroup_width <- 1
    if (var_width) subgroup_width <- length(y_subgroup)/maxLength
    
    dens <- stats::density(y_subgroup, n = nbins, adjust = adjust)
    dens$y <- dens$y / max(dens$y)
    offset <- vanDerCorput(length(y_subgroup))[rank(y_subgroup, ties.method="first")]

    pointDensities<-approx(dens$x,dens$y,y_subgroup)$y

    out<-(offset*width*2-width)*pointDensities*subgroup_width
    
    return(out)
  })
  
  return(new_x)
}

#' Generate van der Corput sequences
#' 
#' @param n the first n elements of the van der Corput sequence
#' @param base the base to use for calculating the van der Corput sequence
#' @export
vanDerCorput <- function(n, base=2){
  #generate n first digits of the van der Corput sequence
  out<-sapply(1:n,function(ii)digits2number(number2digits(ii,base),base,TRUE))
  return(out)
}

#first digit in output is the least significant
number2digits <- function(n, base){
  nDigits<-ceiling(log(n+1,base))
  powers<-base^(0:nDigits)
  out<-diff(n %% powers)/powers[-length(powers)]
  return(out)
}

#first digit in input should be the most significant
digits2number<-function(digits,base,fractional=FALSE){
  if(length(digits)==0)return(0)
  powers<-(length(digits)-1):0
  out<-sum(digits*base^powers)
  if(fractional)out<-out/base^(length(digits))
  return(out)
}
