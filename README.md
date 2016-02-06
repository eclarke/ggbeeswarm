

# Beeswarm-style plots with ggplot2

## Introduction
Beeswarm plots (aka column scatter plots or violin scatter plots) are a way of plotting points that would ordinarily overlap so that they fall next to each other instead. In addition to reducing overplotting, it helps visualize the density of the data at each point (similar to a violin plot), while still showing each data point individually.

`ggbeeswarm` provides two different methods to create beeswarm-style plots using [ggplot2](http://ggplot2.org). It does this by adding two new ggplot geom objects:

- `geom_quasirandom`: Uses a [van der Corput sequence](http://en.wikipedia.org/wiki/Van_der_Corput_sequence) to space the dots to avoid overplotting. This uses [sherrillmix/vipor](https://github.com/sherrillmix/vipor).

- `geom_beeswarm`: Uses the [beeswarm](https://cran.r-project.org/web/packages/beeswarm/index.html) library to do point-size based offset. 

Features: 

- Can handle categorical variables on the y-axis (thanks @smsaladi)
- Automatically dodges if a grouping variable is categorical and `dodge.width` is specified (thanks @josesho)

See the examples below.


## Installation


```r
# Until this package is on CRAN:
devtools::install_github("eclarke/ggbeeswarm")
```

## Examples

### geom_quasirandom()

Using `geom_quasirandom`:

```r
set.seed(12345)
library(ggplot2)
library(ggbeeswarm)
qplot(Species, Sepal.Length, data=iris) + geom_quasirandom()
```

<img src="README_files/figure-html/ggplot2-examples-1.png" title="" alt="" width="576" />

```r
qplot(class, hwy, data=mpg, geom="quasirandom")
```

<img src="README_files/figure-html/ggplot2-examples-2.png" title="" alt="" width="576" />

```r
# With categorical y-axis
qplot(hwy, class, data=mpg, geom='quasirandom')
```

<img src="README_files/figure-html/ggplot2-examples-3.png" title="" alt="" width="576" />

```r
# Some groups may have only a few points. Use `varwidth=TRUE` to adjust width dynamically.
qplot(class, hwy, data=mpg) + geom_quasirandom(varwidth = TRUE)
```

<img src="README_files/figure-html/ggplot2-examples-4.png" title="" alt="" width="576" />

```r
# Automatic dodging
sub_mpg <- mpg[mpg$class %in% c("midsize", "pickup", "suv"),]
ggplot(sub_mpg, aes(class, displ, color=factor(cyl))) + geom_quasirandom(dodge.width=1)
```

<img src="README_files/figure-html/ggplot2-examples-5.png" title="" alt="" width="576" />

### geom_beeswarm()

Using `geom_beeswarm`:

```r
qplot(Species, Sepal.Length, data=iris) + geom_beeswarm()
```

<img src="README_files/figure-html/ggplot2-beeswarm-1.png" title="" alt="" width="576" />

```r
qplot(class, hwy, data=mpg, geom='beeswarm')
```

<img src="README_files/figure-html/ggplot2-beeswarm-2.png" title="" alt="" width="576" />

```r
# With categorical y-axis
qplot(hwy, class, data=mpg, geom='beeswarm')
```

<img src="README_files/figure-html/ggplot2-beeswarm-3.png" title="" alt="" width="576" />

```r
# ggplot doesn't pass any information about the actual device size of the points
# to the underlying layout code, so it's important to manually adjust the `cex` 
# parameter for best results
qplot(class, hwy, data=mpg) + geom_beeswarm(cex=5)
```

<img src="README_files/figure-html/ggplot2-beeswarm-4.png" title="" alt="" width="576" />

```r
qplot(Species, Sepal.Length, data=iris) + geom_beeswarm(cex=4,priority='density')
```

<img src="README_files/figure-html/ggplot2-beeswarm-5.png" title="" alt="" width="576" />

```r
# With automatic dodging
ggplot(sub_mpg, aes(class, displ, color=factor(cyl))) + geom_beeswarm(dodge.width=0.7, cex=4)
```

<img src="README_files/figure-html/ggplot2-beeswarm-6.png" title="" alt="" width="576" />


------
Authors: Erik Clarke and Scott Sherrill-Mix

