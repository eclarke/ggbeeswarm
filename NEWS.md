---
editor_options: 
  markdown: 
    wrap: 80
---

# ggbeeswarm

## Development version

**Bugfixes:**

- `position_quasirandom()` default `dodge.width` is now `NULL` instead of 0 (#79)
- A few stray references to the deprecated `groupOnX` argument are removed.

## v0.7.1

This release incorporates all the incredible work done by @csdaw to refactor the
various geom and position functions, fix outstanding issues, and remove
dependencies on deprecated ggplot2 functions like `qplot`.

This version will be on CRAN.

**New features:**

-   Can now use new features from the `beeswarm` package, including compact
    swarms and 'corralling' points in overlapping groups.
-   Updated examples and documentation.

**Changes:**

-   The `groupOnX` argument is deprecated. `ggplot2` now has logic to determine
    when a plot is 'flipped' automatically so this argument is no longer
    necessary.
-   The `beeswarmArgs` argument is deprecated as its arguments are now called in
    `geom_beeswarm` directly.
-   `position_beeswarm` is once again an exported function.
-   Examples and documentation no longer use `ggplot2::qplot` as it has been
    hard- deprecated in `ggplot2`
-   The license has been clarified to be GPL-3, not GPL \>= 2.
-   Minimum R and `ggplot2` versions have changed to 3.5.0 and 3.3.0,
    respectively.

**Bugfixes:**

Too many to summarize here.
