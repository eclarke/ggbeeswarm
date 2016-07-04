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
##   library(vipor)
##   set.seed(12345)
##   n<-100
##   dat<-rnorm(n*2)
##   labs<-rep(c('a','b'),n)
##   ggplot(mapping=aes(labs, dat)) + geom_quasirandom()


###################################################
### code chunk number 3: showGgPlot
###################################################
  library(vipor)
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
##   ggplot(mapping=aes(labs, dat)) + geom_quasirandom()


###################################################
### code chunk number 7: showGgFactors
###################################################
  labs2<-factor(labs,levels=c('b','a'))
  ggplot(mapping=aes(labs, dat)) + geom_quasirandom()


