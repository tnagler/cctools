context("Continuous convolution kernel density estimator")

# dummy data
dat <- cbind(
    Z1 = rbinom(10, 5, 0.5),
    X1 = rnorm(10),
    Z2 = rpois(10, 1),
    X2 = rexp(10)
)
set.seed(1)  # because continuously convoluted data is random
fit <- cckde(dat)

test_that("Recognizes discrete variables", {
    expect_equal(length(fit$int_vars), 2)
})

test_that("bw parameter works", {
    expect_error(cckde(dat, bw = 0), info = "bw = 0")
    expect_error(cckde(dat, bw = rep(0, 4)), info = "bw = rep(0, 4)")
    new_fit <- cckde(dat, bw = rep(0.5, 4))
    expect_equal(new_fit$bw, rep(0.5, 4), info = "bw = rep(0.5, 4)")
})

test_that("mult parameter works", {
    expect_error(cckde(dat, mult = 0), info = "mult = 0")
    set.seed(1)
    new_fit <- cckde(dat, mult = 2)
    expect_equal(2 * fit$bw, new_fit$bw, info = "mult = 2")
    set.seed(1)
    new_fit <- cckde(dat, mult = 1:4)
    expect_equal(1:4 * fit$bw, new_fit$bw, info = "mult = 1:4")
})

test_that("Density works", {
    expect_error(dcckde(dat, 1), info = "non-cckde object")
    expect_is(dcckde(dat[1, ], fit), "numeric")
    expect_is(dcckde(dat, fit), "numeric")
})
