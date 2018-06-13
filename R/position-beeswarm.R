#' Violin point-style plots to show overlapping points. x must be discrete.
#' 
#' @family position adjustments
#' @param priority Method used to perform point layout (see \code{\link{swarmx}})
#' @param cex Scaling for adjusting point spacing (see \code{\link{swarmx}})
#' @param groupOnX if TRUE then jitter is added to the x axis and if FALSE jitter is added to the y axis. Prior to v0.6.0, the default NULL causes the function to guess which axis is the categorical one based on the number of unique entries in each. This could result in unexpected results when the x variable has few unique values and so in v0.6.0 the default was changed to always jitter on the x axis unless groupOnX=FALSE. Also consider \code{\link[ggplot2]{coord_flip}}.
#' @param dodge.width Amount by which points from different aesthetic groups will be dodged. This requires that one of the aesthetics is a factor.
#' @export
#' @importFrom beeswarm swarmx
#' @seealso \code{\link{position_quasirandom}}, \code{\link[beeswarm]{swarmx}} 
#' @examples
#' 
#'   ggplot2::qplot(class, hwy, data = ggplot2::mpg, position=position_beeswarm())
#'
position_beeswarm <- function (priority = c("ascending", "descending", "density", "random", "none"),cex=1,groupOnX=NULL,dodge.width=0){
  ggplot2::ggproto(NULL,PositionBeeswarm,priority = priority,cex=cex,groupOnX=groupOnX,dodge.width=dodge.width)
}

PositionBeeswarm <- ggplot2::ggproto("PositionBeeswarm",ggplot2:::Position, required_aes=c('x','y'),
  setup_params=function(self,data){
    list(priority=self$priority,
    cex=self$cex,
    groupOnX=self$groupOnX,
    dodge.width=self$dodge.width)
  },
  compute_panel= function(data,params,scales){
    # Adjust function is used to calculate new positions (from ggplot2:::Position)
    data <- remove_missing(data, vars = c("x","y"), name = "position_beeswarm")
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

    # then beeswarm
    trans_x<-NULL
    trans_y<-NULL

    getScaleDiff<-function(scales){
      if(is.null(scales$limits))lims<-scales$range$range
      else lims<-scales$get_limits()
      if(inherits(scales,'ScaleContinuous')){
        limDiff<-diff(lims)
      }else if(inherits(scales,'ScaleDiscrete')){
        limDiff<-length(unique(lims))
      }else{
        stop('Unknown scale type')
      }
      if(limDiff==0)limDiff<-1
      return(limDiff)
    }

    trans_xy <- function(xx){
      xRange<-getScaleDiff(scales$x)
      yRange<-getScaleDiff(scales$y)
      newX<-ave(
        data[,ifelse(params$groupOnX,'y','x')],
        data[,ifelse(params$groupOnX,'x','y')],
        FUN=function(yy){
          if (length(yy) == 1) return(0)
          else beeswarm::swarmx(
            0,
            yy,
            cex=params$cex,
            priority=params$priority,
            #divisor is a magic number to get a reasonable baseline
            #better option would be to figure out point size in user coords
            xsize=ifelse(params$groupOnX,xRange,yRange)/100,
            ysize=ifelse(params$groupOnX,yRange,xRange)/100
          )$x
        }
      )
      return(newX+xx)
    }

    if(params$groupOnX) trans_x<-trans_xy
    else trans_y<-trans_xy

    transform_position(data, trans_x, trans_y)
  }
)

