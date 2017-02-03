context("Uniform plus scaled beta (UPSB) distribution")

sq <- seq(-1, 1, l = 0.1)

test_that("Density integrates to one", {
    expect_equal(integrate(dupsb, -1, 1)$value, 1)
    expect_equal(integrate(dupsb, -1, 1, b = 0.4)$value, 1)
    expect_equal(integrate(dupsb, -1, 1, b = 0.4, ell = 10)$value, 1)
    expect_equal(integrate(dupsb, -1, 1, b = 0.4, ell = 0)$value, 1)
})

test_that("Simulation works", {
    expect_is(rupsb(10), "numeric")
    expect_is(rupsb(10, b = 0.4), "numeric")
    expect_is(rupsb(10, b = 0.4, ell = 10), "numeric")
    expect_is(rupsb(10, b = 0.4, ell = 0), "numeric")
})

