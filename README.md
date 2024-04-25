
<!-- README.md is generated from README.Rmd. Please edit that file -->

# citree

This is a wrapper of
[partykit::ctree()](https://rdrr.io/cran/partykit/man/ctree.html) with
addition functions:

- `partykit::ctree()` only produces the best separation at each node,
  i.e. one tree. By setting `recursive = T`, all trees meeting p-val
  cutoff are produced and saved. Recursion is done by removing the 1st
  splitting variable from the input data.frame and running `runCtree()`
  inside `runCtree()`.
- The info and stats of each node of each tree are collected and
  summarized in an excel file, which also contains ULRs to each tree.
- Before running `partykit::ctree()`, low-informative columns and rows
  are removed to reduce computation and adjustment on association p-vals

Note:

- ctree uses `coin::independence_test()` to test the association of two
  variables of any data type. See
  [here](https://cran.r-project.org/web/packages/coin/vignettes/LegoCondInf.pdf)
  for theory behind the test, and
  [here](https://stats.stackexchange.com/questions/404589/ctree-in-r-how-optimal-is-the-optimal-split-point)
  for an explanation of the algorithm.
- see
  [here](https://stats.stackexchange.com/questions/12140/conditional-inference-trees-vs-traditional-decision-trees)
  for discussions on the pros and cons of ctree in comparison to other
  trees, e.g. rpart.

See [manual](https://blueskypie.github.io/citree/reference/index.html)
and
[examples](https://blueskypie.github.io/cHeatmap/articles/citree-vignette.html)

## Installation

Since this is just a toy, I have no plan to submit it to CRAN. So please
install from github directly:

``` r
devtools::install_github("blueskypie/citree")
```

## Example

``` r
library(citree)
data('mtcars')
re=runCtree(mtcars,'mtcars',oDir='tmp',yi=1,pCut=0.05,naCut=0.3,recursive=T)
```

check the `tmp` directory for output.
