context("Continuous convolution (cont_cov)")

# dummy data
dat <- data.frame(
    Z1 = as.ordered(rbinom(10, 5, 0.5)),
    X1 = rnorm(10),
    Z2 = as.ordered(rpois(10, 1)),
    X2 = rexp(10)
)

test_that("does not add noise to continuous variables", {
    dat_cc <- cont_conv(dat)
    i_cnt <- which(sapply(dat, is.numeric))
    sapply(i_cnt, function(i) expect_equal(dat[, i], dat_cc[ , i]))
})

test_that("does add noise to ordered variables", {
    dat_cc <- cont_conv(dat)
    i_cnt <- which(sapply(dat, is.ordered))
    sapply(i_cnt, function(i) expect_false(all(dat[, i] == dat_cc[ , i])))
})

test_that("preserves input type", {
    expect_equal(class(dat), class(cont_conv(dat)))
    dat_num <- sapply(dat, as.numeric)
    expect_equal(class(dat), class(cont_conv(dat)))
})

test_that("throws an error message for factors", {
    dat_with_fct <- dat
    dat_with_fct$F1 <- as.factor(1:10)
    expect_error(cont_conv(dat_with_fct))
})
