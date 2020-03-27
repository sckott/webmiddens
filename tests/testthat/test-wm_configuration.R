test_that("wm_configuration", {
  expect_is(wm_configuration, "function")
  expect_error(wm_configuration())
})
