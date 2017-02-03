#' Continuous convolution
#'
#' Applies the continuous convolution trick, i.e. adding continuous noise to
#' all discrete variables.
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
    x <- as.matrix(x)

    # find out which variables are discrete
    ints <- which(apply(x, 2, function(y) all(y == round(y))))

    if (length(ints) > 0) {
        # simulate continuous noise
        E <- matrix(rupsb(nrow(x) * length(ints), b, ell),
                    nrow = nrow(x),
                    ncol = length(ints))
        # add noise to discrete variables
        x[, ints] <- x[, ints, drop = FALSE] + E
    }

    # return convoluted vector
    x
}

