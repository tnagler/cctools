#' Continuous convolution density estimator
#'
#' The continuous convolution kernel density estimator is defined as the
#' classical kernel density estimator based on continuously convoluted data (see
#' [cont_conv()]). [cckde()] fits the estimator (including bandwidth selection),
#' [dcckde()] and [predict.cckde()] can be used to evaluate the estimator.
#'
#' @param x a matrix or data frame containing the data (or evaluation points).
#' @param bw vector of bandwidth parameter; if `NULL`, the bandwidths are
#'   selected automatically by likelihood cross validation.
#' @param mult bandwidth multiplier; either a positive number or a vector of
#'   such. Each bandwidth parameter is multiplied with the corresponding
#'   multiplier.
#' @param theta scale parameter of the USB distribution (see, [dusb()]).
#' @param nu smoothness parameter of the USB distribution (see, [dusb()]).
#'   The estimator uses the Epanechnikov kernel for smoothing and the UPSB for
#'   continuous convolution (default parameters correspond to the
#' @param object `cckde` object.
#' @param newdata matrix or data frame containing evaluation points
#'
#' @details If a variable should be treated as ordered discrete, declare it as
#'   [ordered()], factors are expanded into discrete dummy codings.
#'
#' @references Nagler, T. (2017). Nonparametric estimation of probability
#'   densities when some variables are discrete. Unpublished manuscript.
#'
#' @examples
#' Z <- rbinom(100, 6, 0.3)  # discrete variable
#' X <- rexp(100, 5)         # continuous variable
#' dat <- cbind(Z, X)
#'
#' fit <- cckde(dat)  # continuous convolution estimate
#' sum(log(dcckde(dat, fit)))  # log likelihood
#'
#' @export
#' @useDynLib cctools
cckde <- function(x, bw = NULL, mult = 1, theta = 0, nu = 0.5) {
    # continuous convolution of the data
    x_cc <- cont_conv(x, theta = theta, nu = nu)
    x_eval <- cont_conv(x, theta = theta, nu = nu, for_eval = TRUE)
    # find optimal bandwidths using likelihood cross-validation
    if (is.null(bw)) {
        bw <- select_bw(x_eval, x_cc, attr(x_cc, "i_disc"), bw_min = 0.5 - nu)
    } else {
        stopifnot(length(bw) == ncol(x_cc))
        stopifnot(all(bw > 0))
    }

    # adjust bandwidth parameters
    stopifnot(all(mult > 0))
    stopifnot(length(mult) %in% c(1, ncol(x_cc)))
    bw <- mult * bw

    # create and return cckde object
    structure(
        list(x_cc = x_cc, bw = bw, theta = theta, ell = nu),
        class = "cckde"
    )
}

#' @rdname cckde
#' @export
dcckde <- function(x, object) {
    stopifnot(inherits(object, "cckde"))
    x <- if (is.numeric(x)) x else expand_as_numeric(x)
    c(eval_mvkde(x, as.matrix(object$x_cc), object$bw))
}

#' @rdname cckde
#' @param ... unused.
#' @export
predict.cckde <- function(object, newdata, ...)
    dcckde(newdata, object)

#' @importFrom stats IQR optim pbeta rbeta runif sd
#' @importFrom Rcpp evalCpp
#' @noRd
select_bw <- function(x, x_cc, i_ord = integer(0), bw_min = 0) {
    n <- nrow(x)
    d <- ncol(x)

    ## set lower bounds for the bandwidth of each variable
    bw_lower <- numeric(d)
    bw_lower[i_ord] <- bw_min

    ## set starting values by normal reference rule
    bw_start_fun <- function(y)
        2.34 * min(sd(y), IQR(y) / 1.34) * n^(-1/(4 + d))
    bw_start <- apply(x_cc, 2, bw_start_fun)
    bw_start <- pmax(bw_start, bw_lower)  # adjust with lower bounds

    ## find optimal bandwidth by likelihood cross-validation
    opt <- optim(
        bw_start,
        function(bw) lcv_mvkde_disc(x, x_cc, bw),
        lower = bw_lower,
        method = "L-BFGS-B",
        control = list(fnscale = -1)  # for maximization
    )

    ## return optimal bandwidths
    opt$par
}
