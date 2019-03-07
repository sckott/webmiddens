#' web middens
#'
#' @export
#' @details
#' **Methods**
#'   \describe{
#'     \item{`init(path)`}{
#'       a directory path
#'     }
#'     \item{`call(...)`}{
#'       an http request code block
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
#' x$call(con$get("get", query = list(stuff = "bananas")))
#' 
#' x <- midden$new(verbose = TRUE)
#' x$init(path = "rainforest")
#' x$call(con$get("get", query = list(stuff = "bananas")))
#' }
midden <- R6::R6Class(
  'midden',
  public = list(
    cache = NULL,
    cache_path = NULL,
    verbose = FALSE,

    initialize = function(verbose = FALSE) self$verbose <- verbose,
    print = function(x, ...) {
      cat("<midden> ", sep = "\n")
      cat(paste0("  path: ", self$cache_path), sep = "\n")
    },
    call = function(...) {
      if (is.null(self$cache)) stop("run $init first")
      if (!dir.exists(self$cache$cache_path_get())) self$cache$mkdir()
      private$webmock_init()
      private$load_stubs()
      res <- force(...)
      stub <- private$make_stub(res$method, res$url, res$content)
      if (!private$in_stored_stubs(stub)) private$cache_stub(stub)
      on.exit(private$m(webmockr::webmockr_disable_net_connect()),
        add = TRUE)
      on.exit(private$m(webmockr::disable()), add = TRUE)
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
    }
  ),

  private = list(
    webmock_init = function() {
      private$m(webmockr::enable())
      private$m(webmockr::webmockr_allow_net_connect())
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
      saveRDS(stub, file = file, compress = TRUE)
    },
    load_stubs = function() {
      stubs <- lapply(self$cache$list(), readRDS)
      if (length(stubs) > 0) {
        sr <- webmockr::stub_registry()
        invisible(lapply(stubs, sr$register_stub))
        private$m(message(length(stubs), " stubs loaded"))
      }
    },
    in_stored_stubs = function(stub) {
      ff <- self$cache$list()
      if (length(ff) == 0) return(FALSE)
      ss <- lapply(ff, readRDS)
      any(vapply(ss, function(w) identical(w$to_s(), stub$to_s()), logical(1)))
    }
  )
)
