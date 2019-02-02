context("eud")

test_that("Whether eud gives a correct output",{
  expect_equal(eud(14000,777),    7)
  expect_equal(eud(100,1000), 100)
  expect_equal(eud(-100,1000),100)
  expect_error(eud("100",1000))
  expect_error(eud(100,"1000"))
  expect_error(eud(T,"1000"))
})


