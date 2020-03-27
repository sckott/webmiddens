#' @title MiddenHandler
#' @description midden handler
#' @export
#' @details
#' - `MiddenHandler`: default handler
#' - `MiddenHandlerCrul`: `crul` handler
MiddenHandler <- R6::R6Class(
  'MiddenHandler',
  public = list(
    #' @field request an http request
    request = NULL,
    #' @field response an http response
    response = NULL,

    #' @description Create a new `MiddenHandler` object
    #' @param request an http request
    #' @param response an http response
    #' @return A new `MiddenHandler` object
    initialize = function(request, response = NULL) {
      if (!missing(request)) self$request <- request
      if (!is.null(response)) self$response <- response
    },

    #' @description Handle the request (`request` given in `$initialize()`)
    #' @param webmockr_stub_found (logical) whether a webmockr stub was found
    #' or not. default: `FALSE`
    #' @return handles a request, outcomes vary
    handle = function(webmockr_stub_found = FALSE) {
      if (webmockr_stub_found) {
        private$on_stubbed_by_webmiddens_request()
      } else {
        private$on_recordable_request(NULL)
      }
    }
  ),

  private = list(
    # reassign each per adapter
    on_stubbed_by_webmiddens_request = function() {},
    on_recordable_request = function(expire = NULL) {}
  )
)

#' @rdname MiddenHandler
#' @export
MiddenHandlerCrul <- R6::R6Class(
  'MiddenHandlerCrul',
  inherit = MiddenHandler,
  private = list(
    on_stubbed_by_webmiddens_request = function() self$response,
    on_recordable_request = function(expire = NULL) {
      tmp2 <- webmockr::webmockr_crul_fetch(self$request)
      response <- webmockr::build_crul_response(self$request, tmp2)
      z <- tryCatch(midden_current(), error = function(e) e)
      if (inherits(z, "error")) stop("no midden in use")
      z$cache_response(response, expire)
      return(response)
    }
  )
)
