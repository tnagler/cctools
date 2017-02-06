context("Continuous convolution kernel density estimator")

# dummy data
dat <- data.frame(
    Z1 = as.ordered(rbinom(10, 5, 0.5)),
    X1 = rnorm(10),
    Z2 = as.ordered(rpois(10, 1)),
    X2 = rexp(10)
)
set.seed(1)  # because continuously convoluted data is random
fit <- cckde(dat)

test_that("Recognizes discrete variables", {
    expect_equal(length(fit$i_ord), 2)
})

test_that("Works with numeric and data.frame input", {
    expect_error(cckde(sapply(dat, as.numeric), bw = 0))
})

test_that("bw parameter works", {
    expect_error(cckde(dat, bw = 0))
    expect_error(cckde(dat, bw = rep(0, 4)))
    new_fit <- cckde(dat, bw = rep(0.5, 4))
    expect_equal(new_fit$bw, rep(0.5, 4))
})

test_that("mult parameter works", {
    expect_error(cckde(dat, mult = 0))
    set.seed(1)
    new_fit <- cckde(dat, mult = 2)
    expect_equal(2 * fit$bw, new_fit$bw)
    set.seed(1)
    new_fit <- cckde(dat, mult = 1:4)
    expect_equal(1:4 * fit$bw, new_fit$bw)
})

test_that("Density works", {
    expect_error(dcckde(dat, 1))
    expect_is(dcckde(dat[1, ], fit), "numeric")
    expect_is(dcckde(dat, fit), "numeric")
})
