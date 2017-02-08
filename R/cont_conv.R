#' Continuous convolution
#'
#' Applies the continuous convolution trick, i.e. adding continuous noise to all
#' discrete variables. If a variable should be treated as discrete, declare it
#' as [ordered()]. Factors are expanded into binary dummy codes.
#'
#' @param x data; numeric matrix or data frame.
#' @param theta scale parameter of the USB distribution (see, [dusb()]).
#' @param nu smoothness parameter of the USB distribution (see, [dusb()]). The
#'   estimator uses the Epanechnikov kernel for smoothing and the USB for
#'   continuous convolution (default parameters correspond to the \eqn{U[-0.5,
#'   0.5]} distribution).
#' @param eval logical; if `TRUE`, no noise is added (useful for evaluating an
#'   estimator; adding noise is only useful for estimation. The output is a
#'   matrix rather
#'
#' @return A data frame with noise added to each discrete variable (ordered
#'   columns).
#'
#' @details The UPSB distribution ([dusb()]) is used as the noise distribution.
#' Discrete variables are assumed to be integer-valued.
#'
#' @references Nagler, T. (2017). Nonparametric estimation of probability
#'   densities when some variables are discrete. Unpublished manuscript.
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
cont_conv <- function(x, theta = 0, nu = 5, eval = FALSE, ...) {
    if (is.numeric(x))
        return(as.data.frame(x))
    x <- expand_as_numeric(x)
    if (eval) cc_add_noise(x) else x
}

#' Numeric model matrix for continuous convolution
#'
#' Turns ordered variables into integers and expands factors as binary dummy
#' codes. [cc()] additionally adds noise to discrete variables, but this is only
#' useful for estimation. `[cc_prepare()]` can be used to evaluate an already
#' fitted estimate.
#'
#' @param x a vector or data frame with numeric, ordered, or factor columns.
#'
#' @return A numeric matrix containing the expanded variables. It has additional
#'   type `expanded_as_numeric` and `attr(, "i_disc")` cntains the indices of
#'   discrete variables.
#'
#' @noRd
expand_as_numeric <- function(x) {
    if (!inherits(x, "data.frame"))
        x <- as.data.frame(x)
    # which variables will be discrete in the output data frame?
    i_disc <- get_i_disc(x)

    # ordered -> integer, factors -> dummy coding
    x <- do.call(cbind, lapply(x, cc_prepare_one))

    # indicate which variables are discrete
    attr(x, "i_disc") <- i_disc
    class(x) <- c("matrix", "expanded_as_numeric")

    x
}

cc_prepare_one <- function(x) {
    if (is.numeric(x)) {
        # nothing to do
    } else if (is.ordered(x)) {
        x <- as.numeric(x)
    } else if (is.factor(x)) {
        # expand factors, first column is intercept
        x <- model.matrix(~ x)[, -1, drop = FALSE]
        colnames(x) <- paste0(seq.int(ncol(x)))
    } else if (is.character(x)) {
        stop("Don't know how to treat character variables; ",
             "use either numeric, ordered, or factor.")
    } else {
        stop("x has unsupported type (", class(x), "); ",
             "use either numeric, ordered, or factor.")
    }
    x
}

get_i_disc <- function(x)
    which(unlist(lapply(x, is_disc)))

is_disc <- function(x) {
    if (is.numeric(x)) {
        return(FALSE)
    } else if (is.ordered(x)) {
        return(TRUE)
    } else {
        return(rep(TRUE, length(levels(x)) - 1))
    }
}

#' Add noise to discrete variables
#'
#' @param x data matrix created by `expand_as_numeric()` (this is important,
#'   because we need to know which variables are discrete).
#' @noRd
cc_add_noise <- function(x, theta = 0, nu = 5) {
    stopifnot(inherits(x, "cc_prepared"))
    i_disc <- attr(x, "i_disc")
    n_disc <- length(i_disc)
    if (n_disc > 1)
        x[, i_disc] <- x[, i_disc] + rusb(n_disc * nrow(x), nrow(x), n_disc)

    x
}

