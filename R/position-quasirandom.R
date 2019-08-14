#' Violin point-style plots to show overlapping points. x must be discrete.
#' 
#' @family position adjustments
#' @param width the maximum amount of spread (default: 0.4)
#' @param varwidth vary the width by the relative size of each group
#' @param bandwidth the bandwidth adjustment to use when calculating density
#' Smaller numbers (< 1) produce a tighter "fit". (default: 0.5)
#' @param nbins the number of bins used when calculating density (has little effect with quasirandom/random distribution)
#' @param method the method used for distributing points (quasirandom, pseudorandom, smiley or frowney)
#' @param groupOnX if TRUE then jitter is added to the x axis and if FALSE jitter is added to the y axis. Prior to v0.6.0, the default NULL causes the function to guess which axis is the categorical one based on the number of unique entries in each. This could result in unexpected results when the x variable has few unique values and so in v0.6.0 the default was changed to always jitter on the x axis unless groupOnX=FALSE. Also consider \code{\link[ggplot2]{coord_flip}}.
#' @param dodge.width Amount by which points from different aesthetic groups will be dodged. This requires that one of the aesthetics is a factor.
#' @export
#' @importFrom vipor offsetX
#' @seealso \code{\link[vipor]{offsetX}}, \code{\link{geom_quasirandom}}
#'
position_quasirandom <- function (width = NULL, varwidth = FALSE, bandwidth=.5,nbins=NULL,method='quasirandom',groupOnX=NULL,dodge.width=0){
  ggplot2::ggproto(NULL,PositionQuasirandom,width = width, varwidth = varwidth, bandwidth=bandwidth,nbins=nbins,method=method,groupOnX=groupOnX,dodge.width=dodge.width)
}

PositionQuasirandom <- ggplot2::ggproto("PositionQuasirandom",ggplot2:::Position,required_aes=c('x','y'),
  setup_params=function(self,data){
    list(width=self$width,varwidth=self$varwidth,bandwidth=self$bandwidth,nbins=self$nbins,method=self$method,groupOnX=self$groupOnX,dodge.width=self$dodge.width)
  },
  compute_panel= function(data,params,scales){
    data <- remove_missing(data, vars = c("x","y"), name = "position_quasirandom")
    if (nrow(data)==0) return(data.frame())

    if(is.null(params$groupOnX)){
      params$groupOnX<-TRUE
      if(length(unique(data$y)) <= length(unique(data$x))) warning('The default behavior of beeswarm has changed in version 0.6.0. In versions <0.6.0, this plot would have been dodged on the y-axis.  In versions >=0.6.0, groupOnX=FALSE must be explicitly set to group on y-axis. Please set groupOnX=TRUE/FALSE to avoid this warning and ensure proper axis choice.')
    }

    # dodge
    if(!params$groupOnX){
      data[,c('x','y')]<-data[,c('y','x')]
      origCols<-colnames(data)
    }
    data <- ggplot2:::collide(
      data,
      params$dodge.width,
      "position_dodge",
      ggplot2:::pos_dodge,
      check.width = FALSE
    )
    if(!params$groupOnX){
      data[,c('x','y')]<-data[,c('y','x')]
      #remove x/y min/max created by collide
      data<-data[,origCols]
    }

    #resolution needs to be after the dodge
    if (is.null(params$width)) params$width <- ggplot2::resolution(data[,ifelse(params$groupOnX,'x','y')], zero = FALSE) * 0.4
  
    # then quasirandom transform
    trans_x <- NULL
    trans_y <- NULL
    
    trans_xy <- function(xx){
      new_x <- vipor::offsetX(
        data[,ifelse(params$groupOnX,'y','x')],
        xx,
        width=params$width,
        varwidth=params$varwidth,
        adjust=params$bandwidth,
        method=params$method,
        nbins=params$nbins
      )
      return(new_x + xx)
    }

    if(params$width > 0){
      if(params$groupOnX) trans_x<-trans_xy
      else trans_y<-trans_xy
    }

    transform_position(data, trans_x, trans_y)
  }
)

