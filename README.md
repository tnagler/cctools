# cctools
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

cctools is an R package implementing the uniform scaled beta distribution, a 
generic function for continuous convolution, and the continuous convolution
kernel density estimator, see Nagler (2017).

### How to install

``` r
devtools::install_github("tnagler/cctools")
```


### Functions

* `dusb()`, `rusb()`: Density and simulation functions for the uniform scaled
  beta distribution.
* `cont_conv`: Expands all factor variables in a data frame and applies the 
  continuous convolution tricks to all `ordered()` variables.
* `cckde`, `dcckde`, `predict.cckde`: fit and evaluate the continuous 
  convolution kernel density estimator.


### References 

Nagler, T. (2017). *A generic approach to nonparametric function estimation
with mixed data.*
Unpublished manuscript.
