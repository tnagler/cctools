context("Continuous convolution")

# dummy data
dat <- cbind(
    Z1 = rbinom(10, 5, 0.5),
    X1 = rnorm(10),
    Z2 = rpois(10, 1),
    X2 = rexp(10)
)
# parameter scenarios
bs <- c(0, 0.2)
ells <- c(0, 5, 500)

for (bb in bs) {
    for (ll in ells) {
        par_msg <- paste0("b = ", bb, ", ell = ", ll)
        cc_dat <- cont_conv(dat, b = bb, ell = ll)
        test_that(
            "Only affects discrete variables",
            expect_identical(cc_dat[, c(2, 4)], dat[, c(2, 4)], info = par_msg)
        )
        test_that(
            "Catches all discrete variables",
            expect_equal(sum(cc_dat[, c(1, 3)] == dat[, c(1, 3)]), 0, info = par_msg)
        )
    }
}
