context('Classes load') # see https://github.com/OHI-Science/ohicore/issues/80

test_that("classes exist", {
  expect_that(exists("Conf"), is_true())
  expect_that(exists("Layers"), is_true())
})