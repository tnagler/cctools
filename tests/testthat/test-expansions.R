context("Expansion functions for categorical variables")

# dummy data
dat <- data.frame(
    F1 = factor(rbinom(10, 4, 0.1), 0:4),
    Z1 = as.ordered(rbinom(10, 5, 0.5)),
    Z2 = as.ordered(rpois(10, 1)),
    X1 = rnorm(10),
    X2 = rexp(10)
)
exp_len <- 8

test_that("returns right size and type of appropriate size", {
    expect_is(expand_as_numeric(dat), "numeric")
    expect_equal(dim(expand_as_numeric(dat)), c(nrow(dat), exp_len))

    expect_is(expand_vec(rep(0.5, 5), dat), "numeric")
    expect_is(expand_vec(0.5, dat), "numeric")
})

test_that("throw error for characters or other", {
    dat$char <- rep("A", 10)
    expect_error(cont_conv(dat))
    dat$char <- complex(1:10)
    expect_error(cont_conv(dat))
})
