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
### code chunk number 8: methods (eval = FALSE)
###################################################
##   library(gridExtra)
##   dat <- list(
##     'Normal'=rnorm(50),
##     'Dense normal'= rnorm(500),
##     'Bimodal'=c(rnorm(100), rnorm(100,5)),
##     'Trimodal'=c(rnorm(100), rnorm(100,5),rnorm(100,-3))
##   )
##   labs<-rep(names(dat),sapply(dat,length))
##   labs<-factor(labs,levels=unique(labs))
##   dat<-unlist(dat)
##   p1<-ggplot(mapping=aes(labs, dat)) +
##     geom_quasirandom(alpha=.2) +
##     ggtitle('quasirandom') + labs(x='') +
##     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
##   p2<-ggplot(mapping=aes(labs, dat)) +
##     geom_quasirandom(method='pseudorandom',alpha=.2) +
##     ggtitle('pseudorandom') + labs(x='') +
##     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
##   p3<-ggplot(mapping=aes(labs, dat)) +
##     geom_quasirandom(method='smiley',alpha=.2) +
##     ggtitle('smiley') + labs(x='') +
##     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
##   p4<-ggplot(mapping=aes(labs, dat)) +
##     geom_quasirandom(method='frowney',alpha=.2) +
##     ggtitle('smiley') + labs(x='') +
##     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
##   p5<-ggplot(mapping=aes(labs, dat)) +
##     geom_quasirandom(method='tukey',alpha=.2) +
##     ggtitle('tukey') + labs(x='') +
##     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
##   p6<-ggplot(mapping=aes(labs, dat)) +
##     geom_beeswarm(alpha=.2,cex=8,size=.75) +
##     ggtitle('geom_beeswarm') + labs(x='') +
##     theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
##   grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)


###################################################
### code chunk number 9: showMethods
###################################################
  library(gridExtra)
  dat <- list(
    'Normal'=rnorm(50),
    'Dense normal'= rnorm(500),
    'Bimodal'=c(rnorm(100), rnorm(100,5)),
    'Trimodal'=c(rnorm(100), rnorm(100,5),rnorm(100,-3))
  )
  labs<-rep(names(dat),sapply(dat,length))
  labs<-factor(labs,levels=unique(labs))
  dat<-unlist(dat)
  p1<-ggplot(mapping=aes(labs, dat)) +
    geom_quasirandom(alpha=.2) +
    ggtitle('quasirandom') + labs(x='') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  p2<-ggplot(mapping=aes(labs, dat)) +
    geom_quasirandom(method='pseudorandom',alpha=.2) +
    ggtitle('pseudorandom') + labs(x='') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  p3<-ggplot(mapping=aes(labs, dat)) +
    geom_quasirandom(method='smiley',alpha=.2) +
    ggtitle('smiley') + labs(x='') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  p4<-ggplot(mapping=aes(labs, dat)) +
    geom_quasirandom(method='frowney',alpha=.2) +
    ggtitle('smiley') + labs(x='') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  p5<-ggplot(mapping=aes(labs, dat)) +
    geom_quasirandom(method='tukey',alpha=.2) +
    ggtitle('tukey') + labs(x='') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  p6<-ggplot(mapping=aes(labs, dat)) +
    geom_beeswarm(alpha=.2,cex=8,size=.75) +
    ggtitle('geom_beeswarm') + labs(x='') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)


###################################################
### code chunk number 10: distAdjust (eval = FALSE)
###################################################
##   library(gridExtra)
##   p1<-ggplot(mapping=aes(labs, dat)) +
##     geom_quasirandom(bandwidth=2,alpha=.2) +
##     ggtitle('bandwidth=2') + labs(x='')
##   p2<-ggplot(mapping=aes(labs, dat)) +
##     geom_quasirandom(bandwidth=.1,alpha=.2) +
##     ggtitle('bandwidth=.1') + labs(x='')
##   p3<-ggplot(mapping=aes(labs, dat)) +
##     geom_quasirandom(width=.1,alpha=.2) +
##     ggtitle('width=.1') + labs(x='')
##   p4<-ggplot(mapping=aes(labs, dat)) +
##     geom_quasirandom(nbins=100,alpha=.2) +
##     ggtitle('nbins=100') + labs(x='')
##   grid.arrange(p1, p2, p3, p4, ncol=1)


###################################################
### code chunk number 11: showDistAdjust
###################################################
  library(gridExtra)
  p1<-ggplot(mapping=aes(labs, dat)) +
    geom_quasirandom(bandwidth=2,alpha=.2) +
    ggtitle('bandwidth=2') + labs(x='')
  p2<-ggplot(mapping=aes(labs, dat)) +
    geom_quasirandom(bandwidth=.1,alpha=.2) +
    ggtitle('bandwidth=.1') + labs(x='')
  p3<-ggplot(mapping=aes(labs, dat)) +
    geom_quasirandom(width=.1,alpha=.2) +
    ggtitle('width=.1') + labs(x='')
  p4<-ggplot(mapping=aes(labs, dat)) +
    geom_quasirandom(nbins=100,alpha=.2) +
    ggtitle('nbins=100') + labs(x='')
  grid.arrange(p1, p2, p3, p4, ncol=1)


###################################################
### code chunk number 12: varwidth (eval = FALSE)
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
### code chunk number 13: showVarwidth
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


###################################################
### code chunk number 14: vpBeaver (eval = FALSE)
###################################################
##   beaver<-data.frame(
##     'Temperature'=c(beaver1$temp,beaver2$temp),
##     'Beaver'=rep(
##       c('Beaver 1','Beaver 2'),
##       c(nrow(beaver1),nrow(beaver2))
##     )
##   )
##   ggplot(beaver,mapping=aes(Beaver, Temperature)) + geom_quasirandom()


###################################################
### code chunk number 15: showBeaver
###################################################
  beaver<-data.frame(
    'Temperature'=c(beaver1$temp,beaver2$temp),
    'Beaver'=rep(
      c('Beaver 1','Beaver 2'),
      c(nrow(beaver1),nrow(beaver2))
    )
  )
  ggplot(beaver,mapping=aes(Beaver, Temperature)) + geom_quasirandom()


###################################################
### code chunk number 16: vpGene (eval = FALSE)
###################################################
##   library(vipor)
##   ints<-integrations[integrations$nearestGene>0,]
##   ints$logGeneDist<-log(ints$nearestGene)
##   ggplot(ints,mapping=aes(study, logGeneDist,color=latent,alpha=.2)) +
##   geom_quasirandom(dodge.width=1)


###################################################
### code chunk number 17: showGene
###################################################
  library(vipor)
  ints<-integrations[integrations$nearestGene>0,]
  ints$logGeneDist<-log(ints$nearestGene)
  ggplot(ints,mapping=aes(study, logGeneDist,color=latent,alpha=.2)) +
  geom_quasirandom(dodge.width=1)


