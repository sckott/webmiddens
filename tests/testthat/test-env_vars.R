test_that("env vars", {
  # env var not set
  expect_equal(Sys.getenv("WEBMIDDENS_TURN_OFF"), "")

  # env var set
  Sys.setenv("WEBMIDDENS_TURN_OFF" = FALSE)
  expect_equal(Sys.getenv("WEBMIDDENS_TURN_OFF"), "FALSE")
  Sys.setenv("WEBMIDDENS_TURN_OFF" = TRUE)
  expect_equal(Sys.getenv("WEBMIDDENS_TURN_OFF"), "TRUE")

  # reset to default
  Sys.setenv("WEBMIDDENS_TURN_OFF" = "")
})

test_that("env vars with midden class", {
  library(crul)
  con <- crul::HttpClient$new("https://httpbin.org")
  x <- midden$new()
  x$init(path = "forest1", type = 'tempdir')
  
  # no files in cache
  expect_equal(length(x$cache$list()), 0)

  # make a real request & now 1 file in cache
  invisible(x$r(con$get("get", query = list(fruit = "apples"))))
  expect_equal(length(x$cache$list()), 1)

  # delete that 1 file
  x$cache$delete_all()

  # set WEBMIDDENS_TURN_OFF=TRUE
  # & now a real request will be made and will not be cached
  Sys.setenv("WEBMIDDENS_TURN_OFF" = TRUE)
  invisible(x$r(con$get("get", query = list(fruit = "apples"))))
  expect_equal(length(x$cache$list()), 0)

  # cleanup
  x$destroy()

  # reset to default
  Sys.setenv("WEBMIDDENS_TURN_OFF" = "")
})

test_that("env vars fail as expected: WEBMIDDENS_TURN_OFF", {
  x <- midden$new()
  x$init(path = "forest31", type = 'tempdir')
  library(crul)
  con <- crul::HttpClient$new("https://httpbin.org")

  # env var not set
  Sys.setenv("WEBMIDDENS_TURN_OFF" = "")
  expect_is(x$r(con$get("get", query = list(fruit = "apples"))),
    "HttpResponse")

  # env var set to something not allowed
  Sys.setenv("WEBMIDDENS_TURN_OFF" = 5)
  expect_error(x$r(con$get("get", query = list(fruit = "apples"))))

  Sys.setenv("WEBMIDDENS_TURN_OFF" = "asdfasdfs")
  expect_error(x$r(con$get("get", query = list(fruit = "apples"))))

  # back to an allowed value
  Sys.setenv("WEBMIDDENS_TURN_OFF" = "true")
  expect_is(x$r(con$get("get", query = list(fruit = "apples"))),
    "HttpResponse")

  # reset to default
  Sys.setenv("WEBMIDDENS_TURN_OFF" = "")
})

test_that("env vars fail as expected: WEBMIDDENS_EXPIRY_SEC", {
  x <- midden$new()
  x$init(path = "forest33", type = 'tempdir')
  library(crul)
  con <- crul::HttpClient$new("https://httpbin.org")

  # env var not set
  Sys.setenv("WEBMIDDENS_EXPIRY_SEC" = "")
  expect_is(x$r(con$get("get", query = list(fruit = "apples"))),
    "HttpResponse")

  # env var set to something not allowed
  # Sys.setenv("WEBMIDDENS_EXPIRY_SEC" = "foobar")
  # expect_error(x$r(con$get("get", query = list(fruit = "apples"))))

  # Sys.setenv("WEBMIDDENS_EXPIRY_SEC" = TRUE)
  # expect_error(x$r(con$get("get", query = list(fruit = "apples"))))

  # back to an allowed value
  Sys.setenv("WEBMIDDENS_EXPIRY_SEC" = 3)
  expect_is(x$r(con$get("get", query = list(fruit = "apples"))),
    "HttpResponse")

  # reset to default
  Sys.unsetenv("WEBMIDDENS_EXPIRY_SEC")
})
