context('midden: basic structure')
test_that("midden - basic structure, before calling init", {
  expect_is(midden, "R6ClassGenerator")
  mid <- midden$new()
  expect_is(mid, "midden")
  expect_is(mid, "R6")
  expect_null(mid$cach_path)
  expect_null(mid$cache)
  expect_false(mid$verbose)
  expect_is(mid$expire, "function")
  expect_is(mid$destroy, "function")
  expect_is(mid$init, "function")
  expect_is(mid$r, "function")
  expect_output(print(mid), "<midden>")
})
test_that("midden - structure after calling init", {
  mid <- midden$new(verbose = TRUE)
  mid$init(path = "rainforest10", type = 'tempdir')
  expect_is(mid, "midden")
  expect_is(mid, "R6")
  expect_null(mid$cach_path)
  expect_is(mid$cache, "HoardClient")
  expect_true(mid$verbose)
})

context('midden: real requests')
test_that("midden - real request", {
  library(crul)
  x <- midden$new()
  x$init(path = "rainforest11", type = 'tempdir')
  con <- crul::HttpClient$new("https://httpbin.org")
  # real HTTP request, no webmidden interference
  req_no_midden <- con$get("get")
  # real HTTP request, using webmiddens
  req_midden1 <- x$r(con$get("get"))
  # cached HTTP request, using webmiddens
  req_midden2 <- x$r(con$get("get"))

  expect_identical(req_no_midden$request, req_midden1$request)
  # set times to be the same, webmockr doesn't fill this in
  req_midden2$times <- req_midden1$times
  # expect_equal(req_midden1, req_midden2)

  # timing - 2nd request should be faster
  first <- system.time(x$r(con$get("get", query = list(foo = "bar"))))
  second <- system.time(x$r(con$get("get", query = list(foo = "bar"))))
  expect_gt(first[[3]], second[[3]])
})

context('midden: destroy method')
test_that("midden - destroy files", {
  library(crul)
  x <- midden$new()
  x$init(path = "rainforest12", type = 'tempdir')
  con <- crul::HttpClient$new("https://httpbin.org")
  x$r(con$get("get", query = list(a = 1)))
  x$r(con$get("get", query = list(a = 2)))
  x$r(con$get("get", query = list(a = 3)))

  # should be 3 files in the midden
  expect_equal(length(x$cache$list()), 3)

  # destroy: delete the 3 files
  x$destroy()

  # should now be 0 files in the midden & dir is gone too
  expect_equal(length(x$cache$list()), 0)
  expect_false(dir.exists(x$cache$cache_path_get()))
})

context('midden: expire method')
test_that("midden - expire function", {
  library(crul)
  x <- midden$new()
  expect_null(x$expire())
  x$expire(5)
  expect_equal(x$expire(), 5)
})

context('midden: fails well')
test_that("midden fails well", {
  expect_error(midden$new(a = 5))
  expect_error(midden$new(verbose = 5))
})
