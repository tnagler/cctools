context("Uniform scaled beta (USB) distribution")

sq <- seq(-1, 1, l = 0.1)

test_that("Density integrates to one", {
    expect_equal(integrate(dusb, -1, 1)$value, 1)
    expect_equal(integrate(dusb, -1, 1, theta = 0.4)$value, 1)
    expect_equal(integrate(dusb, -1, 1, theta = 0.4, nu = 10)$value, 1)
    expect_equal(integrate(dusb, -1, 1, theta = 0.4, nu = 0)$value, 1)
})

test_that("Simulation works", {
    expect_is(rusb(10), "numeric")
    expect_is(rusb(10, theta = 0.4), "numeric")
    expect_is(rusb(10, theta = 0.4, nu = 10), "numeric")
    expect_is(rusb(10, theta = 0.4, nu = 0), "numeric")
})

