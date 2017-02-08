context("Continuous convolution")

# dummy data
dat <- data.frame(
    F1 = factor(rbinom(10, 4, 0.1), 0:4),
    Z1 = as.ordered(rbinom(10, 5, 0.5)),
    Z2 = as.ordered(rpois(10, 1)),
    X1 = rnorm(10),
    X2 = rexp(10)
)

test_that("expands (only) factors", {
    expect_equal(ncol(cont_conv(dat)), 8)
})

test_that("cont_cov adds noise to the right variables", {
    dat_cc <- cont_conv(dat)
    i_disc <- attr(dat_cc, "i_disc")
    i_cnt <- setdiff(seq.int(ncol(dat)), i_disc)
    sapply(i_cnt, function(i) expect_equal(dat[, i], dat_cc[ , i]))
    sapply(i_cnt, function(i) expect_false(all(dat[, i] == dat_cc[ , i])))
})

test_that("throws error for characters or other", {
    dat$char <- rep("A", 10)
    expect_error(cont_conv(dat))
    dat$char <- complex(1:10)
    expect_error(cont_conv(dat))
})
