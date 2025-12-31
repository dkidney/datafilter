x = c(1, 1:3)

test_that("calc_n_mode works", {
  expect_equal(calc_n_mode(x), 2)
})

test_that("calc_n_mode2 works", {
  expect_equal(calc_n_mode2(x), 2)
})

test_that("calc_n_mode3 works", {
  expect_equal(calc_n_mode3(x), 2)
})

test_that("calc_n_mode3 works", {
  expect_equal(calc_n_mode4(x)$n_mode, 2)
})
