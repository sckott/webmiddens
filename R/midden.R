#' web middens
#'
#' @export
#' @details
#' **Methods**
#'   \describe{
#'     \item{`init(path)`}{
#'       a directory path
#'     }
#'     \item{`call(..., expire = NULL)`}{
#'       an http request code block
#'       - ...: a http request block
#'       - expire: (integer) number of seconds until expiry. after this time, 
#'         we force a real HTTP reqeuest even if a matching stub exists.
#'         times are recorded in UTC.
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @examples \dontrun{
#' library(crul)
#' 
#' # without middens
#' con <- crul::HttpClient$new("https://httpbin.org")
#' con$get(query = list(stuff = "bananas"))
#' 
#' # with middens
#' x <- midden$new()
#' x
#' x$init(path = "rainforest")
#' x
#' # first request is a real HTTP request
#' x$call(con$get("get", query = list(stuff = "bananas")))
#' # following requests use the cached response
#' x$call(con$get("get", query = list(stuff = "bananas")))
#' 
#' # verbose output
#' x <- midden$new(verbose = TRUE)
#' x$init(path = "rainforest")
#' x$call(con$get("get", query = list(stuff = "bananas")))
#' 
#' # set expiration time
#' x <- midden$new()
#' x$init(path = "grass")
#' x
#' # set expiry
#' x$call(con$get("get", query = list(grass = "tall")), expire = 10)
#' ## before expiry, get mocked response
#' x$call(con$get("get", query = list(grass = "tall")), expire = 10)
#' Sys.sleep(10)
#' ## after expiry, get real response
#' x$call(con$get("get", query = list(grass = "tall")), expire = 10)
#' }
midden <- R6::R6Class(
  'midden',
  public = list(
    cache = NULL,
    cache_path = NULL,
    verbose = FALSE,
    expiry = NULL,

    initialize = function(verbose = FALSE) self$verbose <- verbose,
    print = function(x, ...) {
      cat("<midden> ", sep = "\n")
      cat(paste0("  path: ", self$cache_path), sep = "\n")
    },
    call = function(..., expire = NULL) {
      if (is.null(self$cache)) stop("run $init first")
      if (!dir.exists(self$cache$cache_path_get())) self$cache$mkdir()
      private$webmock_init()
      private$load_stubs()
      res <- force(...)
      stub <- private$make_stub(res$method, res$url, res$content)
      checked_stub <- private$in_stored_stubs(stub, expire)
      if (!checked_stub$found || (checked_stub$found && checked_stub$rerun)) {
        if (checked_stub$rerun) {
          res <- force(...)
          stub <- private$make_stub(res$method, res$url, res$content)
        }
        private$cache_stub(stub)
      }
      private$webmock_cleanup()
      return(res)
    },
    init = function(path) {
      cache_obj <- hoardr::hoard()
      cache_obj$cache_path_set(path)
      cache_obj$mkdir()
      self$cache_path <- path
      self$cache <- cache_obj
    },
    destroy = function() {
      if (is.null(self$cache)) return(NULL)
      unlink(self$cache$cache_path_get(), TRUE, TRUE)
    },
    expire = function(time) {
      self$expiry <- time
    }
  ),

  private = list(
    webmock_init = function() {
      private$m(webmockr::enable())
      private$m(webmockr::webmockr_allow_net_connect())
    },
    webmock_cleanup = function() {
      on.exit(private$m(webmockr::webmockr_disable_net_connect()),
        add = TRUE)
      on.exit(private$m(webmockr::disable()), add = TRUE)
    },
    m = function(x) if (!self$verbose) suppressMessages(x) else x,
    cleave_q = function(x) sub("\\?.+", "", x),
    make_stub = function(method, url, body) {
      stub <- webmockr::stub_request(method, url)
      # FIXME: probably set the real headers from the first real request
      # stub <- wi_th(stub, query = query)
      stub <- webmockr::to_return(stub, body = body, status = 200)
      stub
    },
    cache_file = function() {
      file.path(self$cache$cache_path_get(), basename(tempfile("_middens")))
    },
    cache_stub = function(stub, file = private$cache_file()) {
      saveRDS(list(recorded = private$time(), stub = stub), file = file,
        compress = TRUE)
    },
    load_stubs = function() {
      stubs <- lapply(self$cache$list(), readRDS)
      if (length(stubs) > 0) {
        sr <- webmockr::stub_registry()
        invisible(lapply(stubs, function(w) sr$register_stub(w$stub)))
        private$m(message(length(stubs), " stubs loaded"))
      }
    },
    in_stored_stubs = function(stub, expire = NULL) {
      rerun <- FALSE
      ff <- self$cache$list()
      if (length(ff) == 0) return(list(found = FALSE, rerun = FALSE))
      ss <- lapply(ff, readRDS)
      stub_matches <- vapply(ss, function(w) 
        identical(w$stub$to_s(), stub$to_s()), logical(1))

      if (!is.null(expire)) {
        expiry_matches <- vector(length = length(ss))
        for (i in seq_along(ss)) {
          stub_expired <- as.POSIXct(private$time(), tz = "UTC") >=
            (as.POSIXct(ss[[i]]$recorded, tz = "UTC") + expire)
          if (stub_expired) {
            rerun <- TRUE
            unlink(ff[i], force = TRUE)
          }
          expiry_matches[i] <- stub_expired
        }
      } else {
        expiry_matches <- rep(TRUE, length(ss))
      }

      list(found = any(stub_matches & expiry_matches), rerun = rerun)
    },
    time = function() format(as.POSIXct(Sys.time()), tz = "UTC", usetz = TRUE)
  )
)
