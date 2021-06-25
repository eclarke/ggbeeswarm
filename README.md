Beeswarm-style plots with ggplot2
================

[![Build
Status](https://travis-ci.org/eclarke/ggbeeswarm.svg?branch=master)](https://travis-ci.org/eclarke/ggbeeswarm)
[![CRAN
status](https://www.r-pkg.org/badges/version/ggbeeswarm)](https://cran.r-project.org/package=ggbeeswarm)

## Introduction

Beeswarm plots (aka column scatter plots or violin scatter plots) are a
way of plotting points that would ordinarily overlap so that they fall
next to each other instead. In addition to reducing overplotting, it
helps visualize the density of the data at each point (similar to a
violin plot), while still showing each data point individually.

`ggbeeswarm` provides two different methods to create beeswarm-style
plots using [ggplot2](http://ggplot2.org). It does this by adding two
new ggplot geom objects:

-   `geom_quasirandom`: Uses a [van der Corput
    sequence](http://en.wikipedia.org/wiki/Van_der_Corput_sequence) or
    Tukey texturing (Tukey and Tukey “Strips displaying empirical
    distributions: I. textured dot strips”) to space the dots to avoid
    overplotting. This uses
    [sherrillmix/vipor](https://github.com/sherrillmix/vipor).

-   `geom_beeswarm`: Uses the
    [beeswarm](https://cran.r-project.org/web/packages/beeswarm/index.html)
    library to do point-size based offset.

Features:

-   Can handle categorical variables on the y-axis (thanks @smsaladi,
    @koncina)
-   Automatically dodges if a grouping variable is categorical and
    `dodge.width` is specified (thanks @josesho)

See the examples below.

## Installation

This package is on CRAN so install should be a simple:

``` r
install.packages('ggbeeswarm')
```

If you want the development version from GitHub, you can do:

``` r
devtools::install_github("eclarke/ggbeeswarm")
```

## Examples

Here is a comparison between `geom_jitter` and `geom_quasirandom` on the
`iris` dataset:

``` r
set.seed(12345)
library(ggplot2)
library(ggbeeswarm)
#compare to jitter
ggplot(iris,aes(Species, Sepal.Length)) + geom_jitter()
```

<img src="README_files/figure-gfm/ggplot2-compare-1.png" width="576" />

``` r
ggplot(iris,aes(Species, Sepal.Length)) + geom_quasirandom()
```

<img src="README_files/figure-gfm/ggplot2-compare-2.png" width="576" />

### geom\_quasirandom()

Using `geom_quasirandom`:

``` r
#default geom_quasirandom
ggplot(mpg,aes(class, hwy)) + geom_quasirandom()
```

<img src="README_files/figure-gfm/ggplot2-examples-1.png" width="576" />

``` r
# With categorical y-axis
ggplot(mpg,aes(hwy, class)) + geom_quasirandom(groupOnX=FALSE)
```

<img src="README_files/figure-gfm/ggplot2-examples-2.png" width="576" />

``` r
# Some groups may have only a few points. Use `varwidth=TRUE` to adjust width dynamically.
ggplot(mpg,aes(class, hwy)) + geom_quasirandom(varwidth = TRUE)
```

<img src="README_files/figure-gfm/ggplot2-examples-3.png" width="576" />

``` r
# Automatic dodging
sub_mpg <- mpg[mpg$class %in% c("midsize", "pickup", "suv"),]
ggplot(sub_mpg, aes(class, displ, color=factor(cyl))) + geom_quasirandom(dodge.width=1)
```

<img src="README_files/figure-gfm/ggplot2-examples-4.png" width="576" />

#### Alternative methods

`geom_quasirandom` can also use several other methods to distribute
points. For example:

``` r
ggplot(iris, aes(Species, Sepal.Length)) + geom_quasirandom(method = "tukey") + ggtitle("Tukey texture")
```

<img src="README_files/figure-gfm/ggplot2-methods-1.png" width="576" />

``` r
ggplot(iris, aes(Species, Sepal.Length)) + geom_quasirandom(method = "tukeyDense") +
    ggtitle("Tukey + density")
```

<img src="README_files/figure-gfm/ggplot2-methods-2.png" width="576" />

``` r
ggplot(iris, aes(Species, Sepal.Length)) + geom_quasirandom(method = "frowney") +
    ggtitle("Banded frowns")
```

<img src="README_files/figure-gfm/ggplot2-methods-3.png" width="576" />

``` r
ggplot(iris, aes(Species, Sepal.Length)) + geom_quasirandom(method = "smiley") +
    ggtitle("Banded smiles")
```

<img src="README_files/figure-gfm/ggplot2-methods-4.png" width="576" />

``` r
ggplot(iris, aes(Species, Sepal.Length)) + geom_quasirandom(method = "pseudorandom") +
    ggtitle("Jittered density")
```

<img src="README_files/figure-gfm/ggplot2-methods-5.png" width="576" />

``` r
ggplot(iris, aes(Species, Sepal.Length)) + geom_beeswarm() + ggtitle("Beeswarm")
```

<img src="README_files/figure-gfm/ggplot2-methods-6.png" width="576" />

### geom\_beeswarm()

Using `geom_beeswarm`:

``` r
ggplot(iris,aes(Species, Sepal.Length)) + geom_beeswarm()
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-1.png" width="576" />

``` r
ggplot(iris,aes(Species, Sepal.Length)) + geom_beeswarm(side = 1L)
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-2.png" width="576" />

``` r
ggplot(mpg,aes(class, hwy)) + geom_beeswarm(size=.5)
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-3.png" width="576" />

``` r
# With categorical y-axis
ggplot(mpg,aes(hwy, class)) + geom_beeswarm(size=.5)
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-4.png" width="576" />

``` r
# Also watch out for points escaping from the plot with geom_beeswarm
ggplot(mpg,aes(hwy, class)) + geom_beeswarm(size=.5) + scale_y_discrete(expand=expansion(add=c(0.5,1)))
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-5.png" width="576" />

``` r
ggplot(mpg,aes(class, hwy)) + geom_beeswarm(size=1.1)
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-6.png" width="576" />

``` r
# With automatic dodging
ggplot(sub_mpg, aes(class, displ, color=factor(cyl))) + geom_beeswarm(dodge.width=0.5)
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-7.png" width="576" />

``` r
#With different beeswarm point distribution priority
dat<-data.frame(x=rep(1:3,c(20,40,80)))
dat$y<-rnorm(nrow(dat),dat$x)
ggplot(dat,aes(x,y)) + geom_beeswarm(size=2) + ggtitle('Default (ascending)') + scale_x_continuous(expand=expansion(add=c(0.5,.5)))
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-8.png" width="576" />

``` r
ggplot(dat,aes(x,y)) + geom_beeswarm(size=2,priority='descending') + ggtitle('Descending') + scale_x_continuous(expand=expansion(add=c(0.5,.5)))
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-9.png" width="576" />

``` r
ggplot(dat,aes(x,y)) + geom_beeswarm(size=2,priority='density') + ggtitle('Density') + scale_x_continuous(expand=expansion(add=c(0.5,.5)))
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-10.png" width="576" />

``` r
ggplot(dat,aes(x,y)) + geom_beeswarm(size=2,priority='random') + ggtitle('Random') + scale_x_continuous(expand=expansion(add=c(0.5,.5)))
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-11.png" width="576" />

------------------------------------------------------------------------

Authors: Erik Clarke and Scott Sherrill-Mix
