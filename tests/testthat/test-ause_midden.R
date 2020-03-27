context("use_midden")
test_that("use_midden", {
  expect_is(use_midden, "function")

  skip_on_cran()
  skip_if_not_installed("crul")

  library("crul")

  some_http_request <- function(...) {
    x <- crul::HttpClient$new("https://httpbin.org",
      opts = list(...),
      headers = list(`Content-Type` = "application/json"))
    x$post("anything",
      body = jsonlite::toJSON(do.call("rbind", replicate(3, iris, FALSE))),
      encode = "json")
  }
  some_fxn <- function(...) {
    res <- some_http_request(...)
    jsonlite::fromJSON(res$parse("UTF-8"))
  }

  # real HTTP requests are allowed when webmiddens is loaded but
  # not wrapped in use_midden
  strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))
  time_pattern <- "[0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}"
  now <- as.POSIXct(format(Sys.time(), tz="GMT"), tz="GMT")
  nowtime <- strextract(now, time_pattern)
  nowtime <- strptime(nowtime, "%H:%M:%S", tz = "GMT")
  Sys.sleep(1) # sleep just a tiny bit to make sure the http request time is >
  z <- some_http_request()
  ztime <- strextract(z$response_headers$date, time_pattern)
  resptime <- strptime(ztime, "%H:%M:%S", tz = "GMT")
  expect_gt(resptime, nowtime)

  # midden path not set yet
  expect_error(use_midden(some_fxn()), "path not set")
  
  # FIXME: ideally test that this is a real HTTP request, not sure how
  # real http request
  # some_fxn()
  
  # 1st request is a real one, 2nd is cached
  wm_configuration("rainforest46")
  res1 <- use_midden(some_fxn())
  expect_false(webmockr::enabled())# webmockr should be disabled after each run
  res2 <- use_midden(some_fxn())
  expect_false(webmockr::enabled())# webmockr should be disabled after each run
  expect_identical(res1, res2)

  # you can cleanup cached responses to do a real request
  x <- midden_current()
  x
  x$cleanup()
  expect_equal(length(x$cache$list()), 0)
})
