#' Continuous convolution
#'
#' Applies the continuous convolution trick, i.e. adding continuous noise to
#' all discrete variables. If a variable should be treated as discrete, they must
#' be declared as [ordered()].
#'
#' @param x data; numeric matrix or data frame.
#' @param b scale parameter of the UPSB distribution (see, [dupsb()]).
#' @param ell smoothness parameter of the UPSB distribution (see, [dupsb()]).
#' The estimator uses the Epanechnikov kernel for smoothing and the UPSB for
#' continuous convolution (default parameters correspond to the
#' \eqn{U[-0.5, 0.5]} distribution).
#'
#' @return Continuosly convoluted random vector.
#'
#' The UPSB distribution ([dupsb()]) is used as the noise distribution. Discrete
#' variables are assumed to be integer-valued.
#'
#' @references
#' Nagler, T. (2017).
#' Nonparametric estimation of probability densities when some variables are
#' discrete.
#' Unpublished manuscript.
#'
#' @examples
#' Z <- rbinom(100, 6, 0.3)  # discrete variable
#' X <- rexp(1000, 5)        # continuous variable
#' dat <- cbind(Z, X)
#'
#' # plot original and continuously convoluted data
#' plot(dat)
#' points(cont_conv(dat), col = 2)
#'
#' @export
cont_conv <- function(x, b = 0, ell = 5) {
    if (is.numeric(x))
        return(x)
    if (!inherits(x, "data.frame"))
        x <- as.data.frame(x)
    as.data.frame(sapply(x, cont_conv_one, b = b, ell = ell))
}

cont_conv_one <- function(x, b, ell) {
    if (is.numeric(x)) {
        return(x)
    } else if (is.ordered(x)) {
        return(as.numeric(x) + rupsb(length(x), b, ell))
    } else if (is.factor(x)) {
        stop("x must be either numeric or ordered; \n",
             "redefine the variables as ordered and/or ",
             "call again with 'model.frame(~ x)' for dummy coding.")
    } else {
        stop("x has unsupported type ", class(x))
    }
}

