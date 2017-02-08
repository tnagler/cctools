#' Continuous convolution
#'
#' Applies the continuous convolution trick, i.e. adding continuous noise to all
#' discrete variables. If a variable should be treated as discrete, declare it
#' as [ordered()] (passed to [expand_as_numeric()]).
#'
#' @param x data; numeric matrix or data frame.
#' @param theta scale parameter of the USB distribution (see, [dusb()]).
#' @param nu smoothness parameter of the USB distribution (see, [dusb()]). The
#'   estimator uses the Epanechnikov kernel for smoothing and the USB for
#'   continuous convolution (default parameters correspond to the \eqn{U[-0.5,
#'   0.5]} distribution).
#'
#' @return A data frame with noise added to each discrete variable (ordered
#'   columns).
#'
#' @details The UPSB distribution ([dusb()]) is used as the noise distribution.
#'   Discrete variables are assumed to be integer-valued.
#'
#' @references Nagler, T. (2017). Nonparametric estimation of probability
#'   densities when some variables are discrete. Unpublished manuscript.
#'
#' @examples
#' # dummy data with discrete variables
#' dat <- data.frame(
#'     F1 = factor(rbinom(100, 4, 0.1), 0:4),
#'     Z1 = as.ordered(rbinom(100, 5, 0.5)),
#'     Z2 = as.ordered(rpois(100, 1)),
#'     X1 = rnorm(100),
#'     X2 = rexp(100)
#' )
#'
#' pairs(dat)
#' pairs(expand_as_numeric(dat))  # expanded variables without noise
#' pairs(cont_conv(dat))          # continuously convoluted data
#'
#' @export
cont_conv <- function(x, theta = 0, nu = 5) {
    if (is.numeric(x))
        return(x)
    cc_add_noise(expand_as_numeric(x))
}

#' Numeric model matrix for continuous convolution
#'
#' Turns ordered variables into integers and expands factors as binary dummy
#' codes. [cont_conv()] additionally adds noise to discrete variables, but this is only
#' useful for estimation. `[cc_prepare()]` can be used to evaluate an already
#' fitted estimate.
#'
#' @param x a vector or data frame with numeric, ordered, or factor columns.
#'
#' @return A numeric matrix containing the expanded variables. It has additional
#'   type `expanded_as_numeric` and `attr(, "i_disc")` cntains the indices of
#'   discrete variables.
#'
#' @examples
#' # dummy data with discrete variables
#' dat <- data.frame(
#'     F1 = factor(rbinom(100, 4, 0.1), 0:4),
#'     Z1 = as.ordered(rbinom(100, 5, 0.5)),
#'     Z2 = as.ordered(rpois(100, 1)),
#'     X1 = rnorm(100),
#'     X2 = rexp(100)
#' )
#'
#' pairs(dat)
#' pairs(expand_as_numeric(dat))  # expanded variables without noise
#' pairs(cont_conv(dat))          # continuously convoluted data
#'
#' @export
expand_as_numeric <- function(x) {
    if (!inherits(x, "data.frame"))
        x <- as.data.frame(x)
    # which variables will be discrete in the output data frame?
    i_disc <- get_i_disc(x)

    # ordered -> integer, factors -> dummy coding
    new_names <- expand_names(x)
    x <- do.call(cbind, lapply(x, cc_prepare_one))
    colnames(x) <- new_names

    # indicate which variables are discrete
    attr(x, "i_disc") <- i_disc
    class(x) <- c("matrix", "expanded_as_numeric")

    x
}

expand_names <- function(x) {
    nms <- sapply(seq_along(colnames(x)), function(i) {
        if (is.factor(x[, i]) & !is.ordered(x[, i])) {
            paste0(colnames(x)[i], seq_len(length(levels(x[, i])) - 1))
        } else {
            colnames(x)[i]
        }
    })
    unlist(nms)
}

#' @importFrom stats model.matrix
#' @noRd
cc_prepare_one <- function(x) {
    if (is.numeric(x)) {
        # nothing to do
    } else if (is.ordered(x)) {
        x <- as.numeric(x)
    } else if (is.factor(x)) {
        # expand factors, first column is intercept
        x <- model.matrix(~ x)[, -1, drop = FALSE]
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
    stopifnot(inherits(x, "expanded_as_numeric"))
    i_disc <- attr(x, "i_disc")
    n_disc <- length(i_disc)
    if (n_disc > 1)
        x[, i_disc] <- x[, i_disc] + rusb(n_disc * nrow(x), nrow(x), n_disc)
    x
}

