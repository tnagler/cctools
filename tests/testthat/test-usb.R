context("Uniform scaled beta (USB) distribution")

sq <- seq(-1, 1, l = 0.1)

test_that("Density integrates to one", {
    expect_equal(integrate(dusb, -1, 1)$value, 1)
    expect_equal(integrate(dusb, -1, 1, theta = 0.4)$value, 1)
    expect_equal(integrate(dusb, -1, 1, theta = 0.4, nu = 10)$value, 1)
    expect_equal(integrate(dusb, -1, 1, theta = 0.4, nu = 0)$value, 1)
})

test_that("Simulation works", {
    expect_lt(max(abs(rusb(100))), 0.5)
    expect_is(rusb(100), "numeric")
    expect_lt(max(abs(rusb(100, theta = 0.4))), 0.9)
    expect_is(rusb(100, theta = 0.4, nu = 10), "numeric")
})

test_that("qrng works", {
    expect_lt(max(abs(rusb(100, quasi = TRUE))), 0.5)
    expect_is(rusb(100, quasi = TRUE), "numeric")
    expect_lt(max(abs(rusb(100, theta = 0.4, quasi = TRUE))), 0.9)
    expect_is(rusb(100, theta = 0.4, nu = 10, quasi = TRUE), "numeric")
})
