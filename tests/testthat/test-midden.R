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
  mid$init(path = paste0("rainforest", randnums()), type = 'tempdir')
  expect_is(mid, "midden")
  expect_is(mid, "R6")
  expect_null(mid$cach_path)
  expect_is(mid$cache, "HoardClient")
  expect_true(mid$verbose)

  # cleanup
  mid$destroy()
})

context('midden: real requests')
test_that("midden - real request", {
  library(crul)
  x <- midden$new()
  x$init(path = paste0("rainforest", randnums()))
  # x$init(path = paste0("rainforest", randnums()), type = 'tempdir')
  con <- crul::HttpClient$new("https://httpbin.org")
  # real HTTP request, no webmidden interference
  req_no_midden <- con$get("get")
  # real HTTP request, using webmiddens
  req_midden1 <- x$r(con$get("get"))
  Sys.sleep(3)
  # cached HTTP request, using webmiddens
  req_midden2 <- x$r(con$get("get"))

  expect_identical(req_no_midden$request, req_midden1$request)
  # set times to be the same, webmockr doesn't fill this in
  req_midden2$times <- req_midden1$times
  expect_equal(req_midden1, req_midden2)

  # cleanup
  x$destroy()

  # FIXME: too sensitive to internet speed, etc.
  # timing - 2nd request should be faster
  # first <- system.time(x$r(con$get("get", query = list(foo = "bar"))))
  # second <- system.time(x$r(con$get("get", query = list(foo = "bar"))))
  # expect_gt(first[[3]], second[[3]])
})

context('midden: destroy method')
test_that("midden - destroy files", {
  library(crul)
  w <- midden$new()
  w$init(path = paste0("rainforest", randnums()), type = 'tempdir')
  con <- crul::HttpClient$new("https://httpbin.org")
  # Sys.sleep(3)
  w$r(con$get("get", query = list(a = 1)))
  # Sys.sleep(3)
  w$r(con$get("get", query = list(b = 2)))
  # Sys.sleep(3)
  w$r(con$get("get", query = list(c = 3)))

  Sys.sleep(3)
  
  # should be 3 files in the midden
  expect_gt(length(w$cache$list()), 0)

  # destroy: delete the 3 files
  w$destroy()

  # should now be 0 files in the midden & dir is gone too
  expect_equal(length(w$cache$list()), 0)
  expect_false(dir.exists(w$cache$cache_path_get()))
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
