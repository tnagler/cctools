#' UPSB distribution
#'
#' The uniform plus scaled beta (UPSB) distribution describes the distribution of
#' the random variable
#' \deqn{U_{b, \ell} = U + b(B - 0.5),}
#' where \eqn{U} is a \eqn{U[-0.5, 0.5]} random variable, \eqn{B} is a
#' \eqn{Beta(\ell, \ell)} random variable, and \eqn{b > 0, \ell >= 1}.
#'
#' @param x vector of quantiles.
#' @param b scale parameter of the UPSB distribution.
#' @param ell smoothness parameter of the UPSB distribution.
#' @param n number of observations.
#'
#' @references
#' Nagler, T. (2017).
#' Nonparametric estimation of probability densities when some variables are
#' discrete.
#' Unpublished manuscript.
#'
#' @examples
#' # plot distribution
#' sq <- seq(-0.8, 0.8, by = 0.01)
#' plot(sq, dupsb(sq), type = "l")
#' lines(sq, dupsb(sq, b = 0.25), col = 2)
#' lines(sq, dupsb(sq, b = 0.25, ell = 10), col = 3)
#'
#' # simulate from the distribution
#' x <- rupsb(100, b = 0.3, ell = 0)
#'
#' @export
dupsb <- function(x, b = 0, ell = 5) {
    stopifnot(b >= 0)
    a <- 0.5
    out <- numeric(length(x))

    # first component
    ind1 <- (x > -a - b) & (x < -a + b)
    out[ind1] <- pbeta((x[ind1] + a + b) / (2 * b), ell, ell) / (2 * a)

    # second component
    ind2 <- (abs(x) <= a - b)
    out[ind2] <- 1 / (2 * a)

    # third component
    ind3 <- (x > a - b) & (x < a + b)
    out[ind3] <- pbeta((a + b - x[ind3]) / (2 * b), ell, ell) / (2 * a)

    out
}

#' @rdname dupsb
#' @export
rupsb <- function(n, b = 0, ell = 5) {
    stopifnot(b >= 0)
    a <- 0.5
    (runif(n) - 0.5) * 2 * a + 2 * b * (rbeta(n, ell, ell) - 0.5)
}
