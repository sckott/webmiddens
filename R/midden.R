#' @title midden
#' @description web middens class
#' @export
#' @examples \dontrun{
#' library(crul)
#'
#' # without middens
#' con <- crul::HttpClient$new("https://httpbin.org")
#' con2 <- crul::HttpClient$new("https://google.com")
#' con$get("get", query = list(stuff = "bananas"))
#' con2$get(query = list(q = "stuff"))
#'
#' # with middens
#' x <- midden$new()
#' x
#' x$init(path = "rainforest3")
#' x
#' x$cache
#' x$expire()
#' x$expire(5)
#' x$expire()
#' x$expire(reset = TRUE)
#' x$expire()
#' Sys.setenv(WEBMIDDENS_EXPIRY_SEC = 35)
#' x$expire()
#' x$expire(reset = TRUE)
#' x$expire()
#' # first request is a real HTTP request
#' x$r(con$get("get", query = list(stuff = "bananas")))
#' # following requests use the cached response
#' x$r(con$get("get", query = list(stuff = "bananas")))
#'
#' # verbose output
#' x <- midden$new(verbose = TRUE)
#' x$init(path = "rainforest")
#' x$r(con$get("get", query = list(stuff = "bananas")))
#'
#' # set expiration time
#' x <- midden$new()
#' x$init(path = "grass")
#' x$expire(3)
#' x
#' # real request
#' x$r(con$get("get", query = list(grass = "tall")))
#' ## before expiry, get mocked response
#' x$r(con$get("get", query = list(grass = "tall")))
#' Sys.sleep(5)
#' ## after expiry, get real response
#' x$r(con$get("get", query = list(grass = "tall")))
#' }
midden <- R6::R6Class(
  'midden',
  public = list(
    #' @field cache - an `HoardClient` class, see [hoardr::hoard()] for help
    cache = NULL,
    #' @field cache_path (character) the cache path
    cache_path = NULL,
    #' @field verbose (logical) verbose or not
    verbose = FALSE,

    #' @description Create a new `midden` object
    #' @param verbose (logical) get messages about whats going on.
    #' default: `FALSE`
    #' @return A new `midden` object
    initialize = function(verbose = FALSE) {
      self$verbose <- assert(verbose, "logical")
      mdenv$current_midden <- self
    },
    #' @description print method for `midden` objects
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) {
      cat("<midden> ", sep = "\n")
      pth <- if (inherits(self$cache, "HoardClient"))
        self$cache$cache_path_get()
      else
        self$cache_path
      cat(paste0("  path: ", pth), sep = "\n")
      cat(paste0("  expiry (sec): ", private$expiry %||% "not set"),
        sep = "\n")
    },
    #' @description execute an http request code block
    #' @param ... any function that makes an http request
    #' @return http response
    r = function(...) {
      on.exit(webmock_cleanup(), add = TRUE)
      if (private$middens_turned_off()) return(force(...))
      if (is.null(self$cache)) stop("run $init first")
      if (!dir.exists(self$cache$cache_path_get())) self$cache$mkdir()
      private$webmock_init()
      private$clear_expired_stubs()
      private$load_stubs()
      force(...)
    },
    #' @description initialize the class with a path for where to cache data
    #' @param path (character) the path to be appended to the cache path set
    #' by `type`
    #' @param type (character) the type of cache, see \pkg{rappdirs}
    #' @param prefix (character) prefix to the `path` value. Default: "R"
    #' @param full_path (character) instead of using `path`, `type`, and `prefix`
    #' just set the full path with this parameter
    #' @return NULL
    init = function(path = NULL, type = "user_cache_dir", prefix = "R", 
      full_path = NULL) {
      cache_obj <- hoardr::hoard()
      cache_obj$cache_path_set(path, type, prefix, full_path)
      cache_obj$mkdir()
      self$cache_path <- path
      self$cache <- cache_obj
      # set expiry
      self$expire()
      invisible(NULL)
    },
    #' @description remove all cached files in the midden, including directory
    #' @return NULL
    destroy = function() {
      if (is.null(self$cache)) return(NULL)
      unlink(self$cache$cache_path_get(), TRUE, TRUE)
    },
    #' @description remove all cached files in the midden, but leave the
    #' directory
    #' @return NULL
    cleanup = function() {
      if (is.null(self$cache)) return(NULL)
      unlink(self$cache$delete_all(), TRUE, TRUE)
    },
    #' @description set an expiration time
    #' @param expire (integer) seconds to expire - OR, set via the
    #' environment variable `WEBMIDDENS_EXPIRY_SEC`
    #' @param reset (logical) reset to `NULL`? default: `FALSE`
    #' @return NULL
    #' @examples
    #' z <- midden$new()
    #' z$expire(35) # set to expire all requests in 35 seconds
    #' # or set by env var
    #' Sys.setenv(WEBMIDDENS_EXPIRY_SEC = 35)
    expire = function(expire = NULL, reset = FALSE) {
      assert(reset, "logical")
      if (reset) {
        private$expiry <- NULL
        Sys.setenv("WEBMIDDENS_EXPIRY_SEC" = "")
        return(NULL)
      }
      private$set_expiry(expire)
    },

    #' @description cache response
    #' @param x a crul HttpResponse
    #' @param exp (integer) seconds to expire
    cache_response = function(x, exp) {
      stub <- private$make_stub(x$method, x$url,
        x$content, x$request$headers, x$response_headers)
      private$cache_stub(stub, exp)
    }
  ),

  private = list(
    expiry = NULL,
    webmock_init = function() {
      private$m(webmockr::enable())
      private$m(webmockr::webmockr_allow_net_connect())
      Sys.setenv(VCR_TURN_OFF = TRUE)
      if ("package:vcr" %in% search()) unloadNamespace("vcr")
    },
    m = function(x) if (!self$verbose) suppressMessages(x) else x,
    cleave_q = function(x) sub("\\?.+", "", x),
    make_stub = function(method, url, body, request_headers, response_headers) {
      stub <- webmockr::stub_request(method, url)
      stub <- webmockr::wi_th(stub, headers = request_headers)
      stub <- webmockr::to_return(stub, body = body, status = 200,
        headers = response_headers)
      stub
    },
    cache_file = function() {
      file.path(self$cache$cache_path_get(), basename(tempfile("_middens")))
    },
    cache_stub = function(stub, expire = NULL, file = private$cache_file()) {
      private$m(paste0("in cache_stub - going to save to: ", file))
      saveRDS(list(recorded = private$time(),
        ttl = expire, stub = stub), file = file,
        compress = TRUE)
    },
    # clear stubs from webmockr stub registry
    clear_stubs = function() {
      webmockr::stub_registry_clear()
    },
    # clear stubs on disk, does not touch webmockr stub registry
    clear_expired_stubs = function() {
      if (is.null(private$expiry)) return(NULL)
      ff <- self$cache$list()
      if (length(ff) == 0) {
        private$m("no stubs expired")
        return(NULL)
      }
      ss <- lapply(ff, readRDS)

      if (!is.null(private$expiry)) {
        expiry_matches <- vector(length = length(ss))
        for (i in seq_along(ss)) {
          stub_expired <- as.POSIXct(private$time(), tz = "UTC") >=
            (as.POSIXct(ss[[i]]$recorded, tz = "UTC") + private$expiry)
          if (stub_expired) {
            private$m("stub_expired: TRUE")
            private$m(paste0("deleting file: ", ff[i]))
            unlink(ff[i], force = TRUE)
          }
        }
      }
    },
    load_stubs = function() {
      stubs <- lapply(self$cache$list(), readRDS)
      if (length(stubs) > 0) {
        sr <- webmockr::stub_registry()
        invisible(lapply(stubs, function(w) sr$register_stub(w$stub)))
        private$m(message(length(stubs), " stubs loaded"))
      }
    },
    time = function() format(as.POSIXct(Sys.time()), tz = "UTC", usetz = TRUE),
    middens_turned_off = function() {
      x <- Sys.getenv("WEBMIDDENS_TURN_OFF", FALSE)
      x <- if (x == "") FALSE else as.logical(x)
      if (is.na(x))
        stop("WEBMIDDENS_TURN_OFF must be logical",
          call. = FALSE)
      assert(x, "logical")
    },
    set_expiry = function(expire = NULL) {
      expire <-
        expire %||% private$expiry %||%
        Sys.getenv("WEBMIDDENS_EXPIRY_SEC") %||% NULL
      if (!is.null(expire))
        expire <- tryCatch(as.numeric(expire), warning = function(w) w)
      assert(expire, c("numeric", "integer"))
      private$expiry <- expire
      return(private$expiry)
    }
  )
)
