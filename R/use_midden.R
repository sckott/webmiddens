#' @title use_midden
#' @description use a midden: cache http requests
#' @export
#' @param ... a function
#' @examples \dontrun{
#' wm_configuration("rainforest9")
#' 
#' if (requireNamespace("crul")) {
#' some_http_request <- function(...) {
#'   x <- crul::HttpClient$new("https://httpbin.org", opts = list(...))
#'   x$get("get")
#' }
#' some_fxn <- function(...) {
#'   res <- some_http_request(...)
#'   jsonlite::fromJSON(res$parse("UTF-8"))
#' }
#'
#' # real http request
#' some_fxn(verbose = TRUE)
#'
#' # use webmiddens: 1st request is a real one, 2nd is cached
#' (res1 <- use_midden(some_fxn(verbose = TRUE)))
#' (res2 <- use_midden(some_fxn(verbose = TRUE)))
#' 
#' # you can cleanup cached responses to do a real request
#' x <- midden_current()
#' x
#' x$cleanup()
#' use_midden(some_fxn(verbose = TRUE))
#' 
#' # set an expiration time - 30 seconds
#' wm_configuration("rainforest9", expire = 30)
#' self <- midden_current()
#' self
#' self$.__enclos_env__$private$expiry
#' (res1 <- use_midden(some_fxn(verbose = TRUE)))
#' (res2 <- use_midden(some_fxn(verbose = TRUE)))
#' }}
use_midden <- function(...) {
  if (!wm_enabled()) {
    force(...)
  } else {
    mid <- midden$new()
    mid$init(path = midden_path())
    res <- mid$r(...)
    return(res)
  }
}
