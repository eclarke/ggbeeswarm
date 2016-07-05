### R code from vignette source 'usageExamples.Rnw'

###################################################
### code chunk number 1: package
###################################################
options(keep.source = TRUE, width = 60)
packageInfo <- packageDescription("ggbeeswarm")
library(ggbeeswarm)
packageKeywords<-"visualization, display, one dimensional, grouped, groups, violin, scatter, points, quasirandom, beeswarm, van der Corput, beeswarm, ggplot, ggplot2"


###################################################
### code chunk number 2: ggPlot (eval = FALSE)
###################################################
##   library(ggbeeswarm)
##   set.seed(12345)
##   n<-100
##   dat<-rnorm(n*2)
##   labs<-rep(c('a','b'),n)
##   ggplot(mapping=aes(labs, dat)) + geom_quasirandom()


###################################################
### code chunk number 3: showGgPlot
###################################################
  library(ggbeeswarm)
  set.seed(12345)
  n<-100
  dat<-rnorm(n*2)
  labs<-rep(c('a','b'),n)
  ggplot(mapping=aes(labs, dat)) + geom_quasirandom()


###################################################
### code chunk number 4: ggOpts (eval = FALSE)
###################################################
##   ggplot(mapping=aes(labs, dat)) + geom_quasirandom(aes(color=labs))


###################################################
### code chunk number 5: showGgOpts
###################################################
  ggplot(mapping=aes(labs, dat)) + geom_quasirandom(aes(color=labs))


###################################################
### code chunk number 6: ggFactors (eval = FALSE)
###################################################
##   labs2<-factor(labs,levels=c('b','a'))
##   ggplot(mapping=aes(labs2, dat)) + geom_quasirandom(aes(color=labs))


###################################################
### code chunk number 7: showGgFactors
###################################################
  labs2<-factor(labs,levels=c('b','a'))
  ggplot(mapping=aes(labs2, dat)) + geom_quasirandom(aes(color=labs))


###################################################
### code chunk number 8: distAdjust (eval = FALSE)
###################################################
##   dat <- list(
##     'Normal'=rnorm(50),
##     'Dense normal'= rnorm(500),
##     'Bimodal'=c(rnorm(100), rnorm(100,5)),
##     'Extremes'=rcauchy(100)
##   )
##   labs<-rep(names(dat),sapply(dat,length),levels=names(dat))
##   dat<-unlist(dat)
##   ggplot(mapping=aes(labs, dat)) + geom_quasirandom(bandwidth=2)
##   ggplot(mapping=aes(labs, dat)) + geom_quasirandom(bandwidth=.1)
##   ggplot(mapping=aes(labs, dat)) + geom_quasirandom(width=.1)
##   ggplot(mapping=aes(labs, dat)) + geom_quasirandom(nbins=100)


###################################################
### code chunk number 9: showDistAdjust
###################################################
  dat <- list(
    'Normal'=rnorm(50),
    'Dense normal'= rnorm(500),
    'Bimodal'=c(rnorm(100), rnorm(100,5)),
    'Extremes'=rcauchy(100)
  )
  labs<-rep(names(dat),sapply(dat,length),levels=names(dat))
  dat<-unlist(dat)
  ggplot(mapping=aes(labs, dat)) + geom_quasirandom(bandwidth=2)
  ggplot(mapping=aes(labs, dat)) + geom_quasirandom(bandwidth=.1)
  ggplot(mapping=aes(labs, dat)) + geom_quasirandom(width=.1)
  ggplot(mapping=aes(labs, dat)) + geom_quasirandom(nbins=100)


###################################################
### code chunk number 10: varwidth (eval = FALSE)
###################################################
##   dat <- list(
##     '10 points'=rnorm(10),
##     '50 points'=rnorm(50,2),
##     '200 points'=c(rnorm(400), rnorm(100,5)),
##     '5000 points'= rnorm(5000,1)
##   )
##   labs<-rep(names(dat),sapply(dat,length))
##   labs<-factor(labs,levels=unique(labs))
##   dat<-unlist(dat)
##   ggplot(mapping=aes(labs, dat)) + geom_quasirandom(varwidth=TRUE)


###################################################
### code chunk number 11: showVarwidth
###################################################
  dat <- list(
    '10 points'=rnorm(10),
    '50 points'=rnorm(50,2),
    '200 points'=c(rnorm(400), rnorm(100,5)),
    '5000 points'= rnorm(5000,1)
  )
  labs<-rep(names(dat),sapply(dat,length))
  labs<-factor(labs,levels=unique(labs))
  dat<-unlist(dat)
  ggplot(mapping=aes(labs, dat)) + geom_quasirandom(varwidth=TRUE)


