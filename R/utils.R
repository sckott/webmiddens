#' @title midden_tools
#' @description get the current midden
#' @name midden_tools
#' @section Methods:
#' 
#' - `midden_current()`: get the current midden
#' - `midden_path()`: gethe current midden path
#' - `midden_kill()`: deletes the current midden
#' - `wm_enable()`: enable webmiddens
#' - `wm_disable()`: disable webmiddens
#' - `wm_enabled()`: is webmiddens endabled?
NULL

#' @export
#' @rdname midden_tools
midden_current <- function() {
  if (!is.null(mdenv$current_midden)) return(mdenv$current_midden)
  if (is.null(mdenv$current_midden)) {
    if (!is.null(mdenv$path)) {
      message("configuring midden from $path")
      wm_configuration(mdenv$path)
      mid <- midden$new()
      mid$init(path = midden_path())
      return(mdenv$current_midden)
    } else {
      stop("no midden currently set", call. = FALSE)
    }
  }
}

#' @export
#' @rdname midden_tools
midden_kill <- function() {
  mdenv$path <- mdenv$current_midden <- NULL
}

#' @export
#' @rdname midden_tools
midden_path <- function() {
  if (is.null(mdenv$path))
    stop("midden storage path not set; see ?wm_configuration")
  return(mdenv$path)
}

#' @export
#' @rdname midden_tools
wm_enable <- function() {
  mdenv$turned_on <- TRUE
}
#' @export
#' @rdname midden_tools
wm_disable <- function() {
  mdenv$turned_on <- FALSE
}
#' @export
#' @rdname midden_tools
wm_enabled <- function() {
  mdenv$turned_on
}

webmock_cleanup = function() {
  suppressMessages(webmockr::webmockr_disable_net_connect())
  suppressMessages(webmockr::disable())
  Sys.setenv(VCR_TURN_OFF = FALSE)
}
