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

### geom_quasirandom()

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

### geom_beeswarm()

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

#### Alternative methods

``` r
df <- data.frame(
  x = "A",
  y = sample(1:100, 200, replace = TRUE)
)
ggplot(df, aes(x = x, y = y)) + geom_beeswarm(cex = 2.5, method = "swarm") + ggtitle('method = "swarm" (default)')
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-alt-1.png" width="576" />

``` r
ggplot(df, aes(x = x, y = y)) + geom_beeswarm(cex = 2.5, method = "compactswarm") + ggtitle('method = "compactswarm"')
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-alt-2.png" width="576" />

``` r
ggplot(df, aes(x = x, y = y)) + geom_beeswarm(cex = 2.5, method = "hex") + ggtitle('method = "hex"')
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-alt-3.png" width="576" />

``` r
ggplot(df, aes(x = x, y = y)) + geom_beeswarm(cex = 2.5, method = "square") + ggtitle('method = "square"')
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-alt-4.png" width="576" />

``` r
ggplot(df, aes(x = x, y = y)) + geom_beeswarm(cex = 2.5, method = "center") + ggtitle('method = "center"')
```

<img src="README_files/figure-gfm/ggplot2-beeswarm-alt-5.png" width="576" />

#### Different point distribution priority

``` r
#With different beeswarm point distribution priority
dat<-data.frame(x=rep(1:3,c(20,40,80)))
dat$y<-rnorm(nrow(dat),dat$x)
ggplot(dat,aes(x,y)) + geom_beeswarm(cex=2) + ggtitle('Default (ascending)') + scale_x_continuous(expand=expansion(add=c(0.5,.5)))
```

<img src="README_files/figure-gfm/ggplot2-priority-1.png" width="576" />

``` r
ggplot(dat,aes(x,y)) + geom_beeswarm(cex=2,priority='descending') + ggtitle('Descending') + scale_x_continuous(expand=expansion(add=c(0.5,.5)))
```

<img src="README_files/figure-gfm/ggplot2-priority-2.png" width="576" />

``` r
ggplot(dat,aes(x,y)) + geom_beeswarm(cex=2,priority='density') + ggtitle('Density') + scale_x_continuous(expand=expansion(add=c(0.5,.5)))
```

<img src="README_files/figure-gfm/ggplot2-priority-3.png" width="576" />

``` r
ggplot(dat,aes(x,y)) + geom_beeswarm(cex=2,priority='random') + ggtitle('Random') + scale_x_continuous(expand=expansion(add=c(0.5,.5)))
```

<img src="README_files/figure-gfm/ggplot2-priority-4.png" width="576" />

#### Corral runaway points

``` r
set.seed(1995)
df2 <- data.frame(
  y = rnorm(1000),
  id = sample(c("G1", "G2", "G3"), size = 1000, replace = TRUE)
)
p <- ggplot(df2, aes(x = id, y = y, colour = id))

# use corral.width to control corral width
p + geom_beeswarm(cex = 2.5, corral = "none", corral.width = 0.9) + ggtitle('corral = "none" (default)')
```

<img src="README_files/figure-gfm/ggplot2-corral-1.png" width="576" />

``` r
p + geom_beeswarm(cex = 2.5, corral = "gutter", corral.width = 0.9) + ggtitle('corral = "gutter"')
```

<img src="README_files/figure-gfm/ggplot2-corral-2.png" width="576" />

``` r
p + geom_beeswarm(cex = 2.5, corral = "wrap", corral.width = 0.9) + ggtitle('corral = "wrap"')
```

<img src="README_files/figure-gfm/ggplot2-corral-3.png" width="576" />

``` r
p + geom_beeswarm(cex = 2.5, corral = "random", corral.width = 0.9) + ggtitle('corral = "random"')
```

<img src="README_files/figure-gfm/ggplot2-corral-4.png" width="576" />

``` r
p + geom_beeswarm(cex = 2.5, corral = "omit", corral.width = 0.9) + ggtitle('corral = "omit"')
```

    ## Warning: Removed 303 rows containing missing values (geom_point).

<img src="README_files/figure-gfm/ggplot2-corral-5.png" width="576" />

------------------------------------------------------------------------

Authors: Erik Clarke, Scott Sherrill-Mix, and Charlotte Dawson
