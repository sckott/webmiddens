#' @title wm_configuration
#' @description webmiddens settings
#' @export
#' @param path (character) path to set for the current midden
#' @param expire (numeric/integer) an expiration time (seconds)
#' @return nothing, sets path and expire settings
#' @examples \dontrun{
#' wm_configuration("foobar")
#' wm_configuration("foobar", expire = 6)
#' }
wm_configuration <- function(path, expire = NULL) {
  assert(path, "character")
  assert(expire, c("numeric", "integer"))
  mdenv$path <- path
  if (is.null(expire)) {
    expire <- Sys.getenv("WEBMIDDENS_EXPIRY_SEC")
    mdenv$expire <- expire
  } else {
    Sys.setenv(WEBMIDDENS_EXPIRY_SEC = expire)
  }
}
