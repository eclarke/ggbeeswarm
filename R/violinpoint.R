#' Offset data to avoid overplotting. 
#' 
#' Arranges data points using a van der Corput sequence to form "beeswarm" style
#' plots. Returns a vector of the offsets to be used in plotting.
#' 
#' @param y vector of data points 
#' @param x a grouping factor for y (optional)
#' @param width the maximum spacing away from center for each group of points. Since points are spaced to left and right, the maximum width of the cluster will be approximately width*2 (0 = no offset, default = 0.4)
#' @param varwidth adjust the width of each group based on the number of points in the group
#' @param adjust bandwidth used to adjust the density
#' @param nbins the number of points used to calculate density
#' @return a vector with of x-offsets of the same length as y
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
offset_x <- function(y, x, width=0.4, varwidth=FALSE, adjust=0.5, nbins=1000) {
  
  if (missing(x)) x <- rep(1, length(y))
  if (length(x)!=length(y)) stop(simpleError('x and y not the same length in offset_x'))
  
  maxLength<-max(table(x))

  # Apply the van der Corput noise to each x group to create offsets
  new_x <- ave(y,x, FUN=function(y_subgroup) {
    
    # If there's only one value in this group, leave it alone
    if (length(y_subgroup) == 1) return(0) 
    
    subgroup_width <- 1
	 #sqrt to match boxplot (allows comparison of order of magnitude different ns, scale with standard error)
    if (varwidth) subgroup_width <- sqrt(length(y_subgroup)/maxLength)
    
    dens <- stats::density(y_subgroup, n = nbins, adjust = adjust)
    dens$y <- dens$y / max(dens$y)
    offset <- vanDerCorput(length(y_subgroup))[rank(y_subgroup, ties.method="first")]

    pointDensities<-stats::approx(dens$x,dens$y,y_subgroup)$y

    out<-(offset-.5)*2*width*pointDensities*subgroup_width
    
    return(out)
  })
  
  return(new_x)
}

#' Generate van der Corput sequences
#' 
#' Generates the first (or an arbitrary offset) n elements of the van der Corput low-discrepancy sequence for a given base
#' 
#' @param n the first n elements of the van der Corput sequence
#' @param base the base to use for calculating the van der Corput sequence
#' @param start start at this position in the sequence
#' @return a vector of length n with values ranging between 0 and 1
#' @references \url{https://en.wikipedia.org/wiki/Van_der_Corput_sequence}
#' @export
#' @examples
#' vanDerCorput(100)
vanDerCorput <- function(n, base=2,start=1){
  #generate n first digits of the van der Corput sequence
  out<-sapply(1:n+start-1,function(ii)digits2number(rev(number2digits(ii,base)),base,TRUE))
  return(out)
}

#' Convert an integer to an arbitrary base
#' 
#' Takes an integer and converts it into an arbitrary base e.g. binary or octal. Note that the first digit in the output is the least significant.
#' 
#' @param n the integer to be converted
#' @param base the base for the numeral system (e.g. 2 for binary or 8 for octal)
#' @return a vector of length ceiling(log(n+1,base)) respresenting each digit for that numeral system
#' @references \url{https://en.wikipedia.org/wiki/Radix}
#' @export
#' @examples
#' number2digits(100)
#' number2digits(100,8)
number2digits <- function(n, base=2){
  nDigits<-ceiling(log(n+1,base))
  powers<-base^(0:nDigits)
  out<-diff(n %% powers)/powers[-length(powers)]
  return(out)
}

#' Convert a vector of integers representing digits in an arbitrary base to an integer
#' 
#' Takes a vector of integers representing digits in an arbitrary base e.g. binary or octal and converts it into an integer (or the integer divided by base^length(digits) for the number of digits if fractional is TRUE). Note that the first digit in the input is the least significant.
#' 
#' @param digits a vector of integers representing digits in an arbitrary base
#' @param base the base for the numeral system (e.g. 2 for binary or 8 for octal)
#' @param fractional divide the 
#' @return an integer
#' @references \url{https://en.wikipedia.org/wiki/Radix}
#' @export
#' @examples
#' digits2number(c(4,4,1),8)
#' digits2number(number2digits(100))
digits2number<-function(digits,base=2,fractional=FALSE){
  if(length(digits)==0)return(0)
  powers<-0:(length(digits)-1)
  out<-sum(digits*base^powers)
  if(fractional)out<-out/base^(length(digits))
  return(out)
}
