#' An internal function to calculate new positions for geom_beeswarm
#' 
#' @family position adjustments
#' @param data A data.frame containing plotting data in columns x and y. Usually obtained from data processed by ggplot2.
#' @param xRange span of the x axis e.g. yMax-yMin
#' @param yRange span of the y axis e.g. yMax-yMin
#' @param priority Method used to perform point layout (see \code{\link{swarmx}})
#' @param cex Scaling for adjusting point spacing (see \code{\link{swarmx}})
#' @param groupOnX if TRUE then jitter is added to the x axis and if FALSE jitter is added to the y axis. Prior to v0.6.0, the default NULL causes the function to guess which axis is the categorical one based on the number of unique entries in each. This could result in unexpected results when the x variable has few unique values and so in v0.6.0 the default was changed to always jitter on the x axis unless groupOnX=FALSE. Also consider \code{\link[ggplot2]{coord_flip}}.
#' @param dodge.width Amount by which points from different aesthetic groups will be dodged. This requires that one of the aesthetics is a factor.
#' @param beeswarmArgs A list of additional arguments to be passed to the \link{swarmx} function of beeswarm e.g. \code{list(side=1)} or \code{list(side=-1)} to only distribute points to the right/left side
#' @param oSize A 2 element vector giving the width and height of a cex=1 character "o" in user coordinates
#' @export
#' @importFrom beeswarm swarmx
#' @seealso \code{\link{geom_beeswarm}}, \code{\link{position_quasirandom}}, \code{\link[beeswarm]{swarmx}}


offset_beeswarm= function(data,xRange=1,yRange=1,priority = c("ascending", "descending", "density", "random", "none"), cex=1, groupOnX=NULL, dodge.width=0, beeswarmArgs=list(),oSize=c(1/200,1/200)){
  # Adjust function is used to calculate new positions (from ggplot2:::Position)
  data <- remove_missing(data, vars = c("x","y"), name = "position_beeswarm")
  if (nrow(data)==0) return(data.frame())

  if(is.null(groupOnX)){
    groupOnX<-TRUE
    if(length(unique(data$y)) <= length(unique(data$x))) warning('The default behavior of beeswarm has changed in version 0.6.0. In versions <0.6.0, this plot would have been dodged on the y-axis.  In versions >=0.6.0, groupOnX=FALSE must be explicitly set to group on y-axis. Please set groupOnX=TRUE/FALSE to avoid this warning and ensure proper axis choice.')
  }

  #divisors are magic numbers to get a reasonable base spacing
  #note that the base beeswarm package is not 100% precise with differing plotting sizes resulting in variation in points overlap/spacing
  xSize<-xRange*oSize[1]/.71
  ySize<-yRange*oSize[2]/.92
  offset<-stats::ave(
    data[,ifelse(groupOnX,'y','x')],
    data[,ifelse(groupOnX,'x','y')],
    FUN=function(yy){
      if (length(yy) == 1) return(0)
      else do.call(beeswarm::swarmx,c(list(
        0,
        yy,
        cex=1,
        priority=priority,
        xsize=ifelse(groupOnX,xSize,ySize),
        ysize=ifelse(groupOnX,ySize,xSize)
      ),beeswarmArgs)
    )$x
    }
  )
  data[,ifelse(groupOnX,'x','y')]<-data[,ifelse(groupOnX,'x','y')]+offset

  return(data)
}
    
position_beeswarm <- function (groupOnX=NULL,dodge.width=0){
  ggplot2::ggproto(NULL,PositionBeeswarm,groupOnX=groupOnX,dodge.width=dodge.width)
}

PositionBeeswarm <- ggplot2::ggproto("PositionBeeswarm",ggplot2:::Position,required_aes=c('x','y'),
  setup_params=function(self,data){
    list(groupOnX=self$groupOnX,dodge.width=self$dodge.width)
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
    return(data)
  }
)
