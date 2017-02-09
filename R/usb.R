#' Uniform scaled beta distribution
#'
#' The uniform scaled beta (USB) distribution describes the distribution of
#' the random variable
#' \deqn{U_{b, \nu} = U + \theta(B - 0.5),}
#' where \eqn{U} is a \eqn{U[-0.5, 0.5]} random variable, \eqn{B} is a
#' \eqn{Beta(\nu, \nu)} random variable, and \eqn{theta > 0, \nu >= 1}.
#'
#' @param x vector of quantiles.
#' @param theta scale parameter of the USB distribution.
#' @param nu smoothness parameter of the USB distribution.
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
#' plot(sq, dusb(sq), type = "l")
#' lines(sq, dusb(sq, theta = 0.25), col = 2)
#' lines(sq, dusb(sq, theta = 0.25, nu = 10), col = 3)
#'
#' # simulate from the distribution
#' x <- rusb(100, theta = 0.3, nu = 0)
#'
#' @importFrom stats pbeta
#' @export
dusb <- function(x, theta = 0, nu = 5) {
    stopifnot(theta >= 0)
    stopifnot(theta <= 0.5)
    a <- 0.5
    out <- numeric(length(x))

    # first component
    ind1 <- (x > -a - theta) & (x < -a + theta)
    out[ind1] <- pbeta((x[ind1] + a + theta) / (2 * theta), nu, nu) / (2 * a)

    # second component
    ind2 <- (abs(x) <= a - theta)
    out[ind2] <- 1 / (2 * a)

    # third component
    ind3 <- (x > a - theta) & (x < a + theta)
    out[ind3] <- pbeta((a + theta - x[ind3]) / (2 * theta), nu, nu) / (2 * a)

    out
}

#' @rdname dusb
#' @param n number of observations.
#' @param quasi logical indicating whether quasi random numbers sholuld be used
#'   ([qrng::ghalton()]); only works for `theta = 0`.
#' @importFrom qrng ghalton
#' @importFrom stats qbeta rbeta
#' @export
rusb <- function(n, theta = 0, nu = 5, quasi = FALSE) {
    stopifnot(theta >= 0)
    stopifnot(theta <= 0.5)
    a <- 0.5
    if (!quasi) {
        x <- (runif(n) - 0.5) * 2 * a
        if (theta > 0)
            x <- x + 2 * theta * (rbeta(n, nu, nu) - 0.5)
    } else {
        if (theta == 0) {
            x <- qrng::ghalton(n, d = 1) * 2 * a
        } else {
            u <- qrng::ghalton(2 * n, d = 1) * 2 * a
            u <- u[sample(2 * n)]
            x <- u[seq.int(n)] + 2 * theta * (qbeta(u[-seq.int(n)], nu, nu) - 0.5)
        }
    }

    x
}
