test_that("webmiddens utils", {
  expect_null(midden_kill())
  expect_error(midden_current())
  expect_error(midden_path())
})
