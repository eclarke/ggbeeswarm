#' Violin point-style plots to show overlapping points. x must be discrete.
#' 
#' @family position adjustments
#' @param priority Method used to perform point layout (see \code{\link{swarmx}})
#' @param cex Scaling for adjusting point spacing (see \code{\link{swarmx}})
#' @param groupOnX should jitter be added to the x axis if TRUE or y axis if FALSE (the default NULL causes the function to guess which axis is the categorical one based on the number of unique entries in each)
#' @param dodge.width Amount by which points from different aesthetic groups will be dodged. This requires that one of the aesthetics is a factor.
#' @export
#' @importFrom beeswarm swarmx
#' @seealso \code{\link{position_quasirandom}}, \code{\link[beeswarm]{swarmx}} 
#' @examples
#' 
#'   ggplot2::qplot(class, hwy, data = ggplot2::mpg, geom='beeswarm')
#'   # Generate fake data
#'   distro <- data.frame(
#'     'variable'=rep(c('runif','rnorm'),each=100),
#'     'value'=c(runif(100, min=-3, max=3), rnorm(100))
#'   )
#'   ggplot2::qplot(variable, value, data = distro, geom='beeswarm')
#'   ggplot2::qplot(variable, value, data = distro) +
#'     geom_beeswarm(priority='density',cex=2.5)
#'
position_beeswarm <- function (priority = c("ascending", "descending", "density", "random", "none"),cex=2,groupOnX=NULL,dodge.width=0) {
  ggplot2::ggproto(NULL,PositionBeeswarm,priority = priority,cex=cex,groupOnX=NULL,dodge.width=dodge.width)
}

PositionBeeswarm <- ggplot2::ggproto("PositionBeeswarm",ggplot2:::Position, required_aes=c('x','y'),
  setup_params=function(self,data){
    list(priority=self$priority,
    cex=self$cex,
    groupOnX=self$groupOnX,
    dodge.width=self$dodge.width)
  },
  compute_panel=function(data,params,scales){
	# Adjust function is used to calculate new positions (from ggplot2:::Position)
		data <- remove_missing(data, vars = c("x","y"), name = "position_beeswarm")
		if (nrow(data)==0) return(data.frame())

		# more unique entries in x than y suggests y (not x) is categorical
    if(is.null(params$groupOnX)) params$groupOnX <- length(unique(data$y)) > length(unique(data$x))
    
    # dodge
    data <- ggplot2:::collide(data,
    	params$dodge.width,
    	"position_dodge", 
    	ggplot2:::pos_dodge,
    	check.width = FALSE)
    	
    # then beeswarm
    trans_x<-NULL
    trans_y<-NULL

    trans_xy <- function(x) {
      newX<-ave(data[,ifelse(params$groupOnX,'y','x')],data[,ifelse(params$groupOnX,'x','y')],
          FUN=function(xx) {
            if (length(xx) < 2) {
              return(0)
            } else {
              beeswarm::swarmx(0,xx,cex=params$cex,priority=params$priority)$x            
            }})
      return(newX+x)
    }

    if(params$groupOnX) trans_x<-trans_xy
    else trans_y<-trans_xy

		transform_position(data, trans_x, trans_y)
	}
)

