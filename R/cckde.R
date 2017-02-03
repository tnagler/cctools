#' Continuous convolution density estimator
#'
#' The continuous convolution kernel density estimator is defined as the
#' classical kernel density estimator based on continuously convoluted data (see
#' [cont_conv()]). [dcckde()] evaluates the density.
#'
#' @param x a matrix or data frame containing the data.
#' @param bw vector of bandwidth parameter; if `NULL`, the bandwidths are
#' selected automatically by likelihood cross validation.
#' @param mult bandwidth multiplier; either a positive number or a vector of
#' such. Each bandwidth parameter is multiplied with the corresponding
#' multiplier.
#' @param b scale parameter of the UPSB distribution (see, [dupsb()]).
#' @param ell smoothness parameter of the UPSB distribution (see, [dupsb()]).
#' The estimator uses the Epanechnikov kernel for smoothing and the UPSB for
#' continuous convolution (default parameters correspond to the
#' @param obj `cckde` object.
#'
#' @references
#' Nagler, T. (2017).
#' Nonparametric estimation of probability densities when some variables are
#' discrete.
#' Unpublished manuscript.
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
cckde <- function(x, bw = NULL, mult = 1, b = 0, ell = 0.5) {
    x <- as.matrix(x)

    # find the discrete variabels
    int_vars <- which(apply(x, 2, function(y) all(y == round(y))))

    # continuous convolution of the data
    x_cc <- cont_conv(x, b = b, ell = ell)

    # find optimal bandwidths using likelihood cross-validation
    if (is.null(bw)) {
        bw <- select_bw(x, x_cc, int_vars, bw_min = 0.5 - b)
    } else {
        stopifnot(ncol(x) == length(bw))
        stopifnot(all(bw > 0))
    }

    # adjust bandwidth parameters
    stopifnot(all(mult > 0))
    stopifnot(length(mult) %in% c(1, ncol(x)))
    bw <- mult * bw

    # create and return cckde object
    structure(
        list(x = x, x_cc = x_cc, int_vars = int_vars, bw = bw, b = b, ell = ell),
        class = "cckde"
    )
}

#' @rdname cckde
#' @export
dcckde <- function(x, obj) {
    stopifnot(inherits(obj, "cckde"))
    x <- as.matrix(x)
    if (ncol(x) == 1)
        x <- t(x)
    stopifnot(ncol(x) == ncol(obj$x))

    c(eval_mvkde(x, obj$x_cc, obj$bw))
}

#' @importFrom stats IQR optim pbeta rbeta runif sd
#' @importFrom Rcpp evalCpp
select_bw <- function(x, x_cc, int_vars = integer(0), bw_min = 0) {
    n <- nrow(x)
    d <- ncol(x)

    ## set lower bounds for the bandwidth of each variable
    bw_lower <- numeric(d)
    bw_lower[int_vars] <- bw_min

    ## set starting values by normal reference rule
    bw_start <- apply(
        x_cc,
        2,
        function(y)
            2.34 * min(sd(y), IQR(y) / 1.34) * n^(-1/(4 + d - length(int_vars)))
    )
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
