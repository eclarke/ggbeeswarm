#' Violin point-style plots to show overlapping points. x must be discrete.
#' 
#' @family position adjustments
#' @param width the maximum amount of spread (default: 0.4)
#' @param varwidth vary the width by the relative size of each group
#' @param bandwidth the bandwidth adjustment to use when calculating density 
#' Smaller numbers (< 1) produce a tighter "fit". (default: 0.5)
#' @param nbins the number of bins used when calculating density (has little effect with quasirandom/random distribution)
#' @param method the method used for distributing points (quasirandom, pseudorandom, smiley or frowney)
#' @param groupOnX should jitter be added to the x axis if TRUE or y axis if FALSE (the default NULL causes the function to guess which axis is the categorical one based on the number of unique entries in each)
#' @param dodge.width Amount by which points from different aesthetic groups will be dodged. This requires that one of the aesthetics is a factor.
#' @export
#' @importFrom vipor offsetX
#' @seealso \code{\link[vipor]{offsetX}}
#' @examples
#' 
#'   ggplot2::qplot(class, hwy, data = ggplot2::mpg, geom='quasirandom')
#'   # Generate fake data
#'   distro <- data.frame(
#'     'variable'=rep(c('runif','rnorm'),each=100),
#'     'value'=c(runif(100, min=-3, max=3), rnorm(100))
#'   )
#'   ggplot2::qplot(variable, value, data = distro, geom = 'quasirandom')
#'   ggplot2::qplot(variable, value, data = distro) + geom_quasirandom(width=0.1)
#'
position_quasirandom <- function (width = NULL, varwidth = FALSE, bandwidth=.5,nbins=1000,method='quasirandom',groupOnX=NULL,dodge.width=0) {
  ggplot2::ggproto(NULL,PositionQuasirandom,width = width, varwidth = varwidth, bandwidth=bandwidth,nbins=nbins,method=method,groupOnX=groupOnX,dodge.width=dodge.width)
}

PositionQuasirandom <- ggplot2::ggproto("PositionQuasirandom",ggplot2:::Position,required_aes=c('x','y'),
  setup_params=function(self,data){
    list(width=self$width,varwidth=self$varwidth,bandwidth=self$bandwidth,nbins=self$nbins,method=self$method,groupOnX=self$groupOnX,dodge.width=self$dodge.width)
  },
  compute_layer= function(data,params,panel) {
    data <- remove_missing(data, vars = c("x","y"), name = "position_quasirandom")
    if (nrow(data)==0) return(data.frame())
    
  # dodge
    data <- ggplot2:::collide(data,
    params$dodge.width,
    "position_dodge", 
    ggplot2:::pos_dodge,
    check.width = FALSE)
  
  # then quasirandom transform
    trans_x <- NULL
    trans_y <- NULL
    
    if(is.null(params$groupOnX)) params$groupOnX <- length(unique(data$y)) > length(unique(data$x))
    if (is.null(params$width)) params$width <- ggplot2::resolution(data[,ifelse(params$groupOnX,'x','y')], zero = FALSE) * 0.4

    trans_xy <- function(x) {
      new_x <- vipor::offsetX( 
        data[,ifelse(params$groupOnX,'y','x')],
        x,
        width=params$width, 
        varwidth=params$varwidth, 
        adjust=params$bandwidth,
        method=params$method
      )
      new_x + x
    }

    if(params$width > 0) {
      if(params$groupOnX) trans_x<-trans_xy
      else trans_y<-trans_xy
    }

    transform_position(data, trans_x, trans_y)
  }
)

