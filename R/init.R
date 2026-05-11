#' @include AAA.R
NULL


#' @title Initialize a logger with nice defaults
#' @details
#' All arguments must be named.
#'
#' This function sets the threshold and this package's layout/timestamp formats on the specified Logger, which be default is {lgr}'s root Logger (i.e. `lgr::lgr`).
#'
#' Like other functions in this package, this function cannot fail.
#' If invalid arguments are given, this function will warn but not err.
#' @param lgr the `lgr::Logger` to initialize (i.e. modify in-place); {lgr}'s root logger i.e. `lgr::lgr` by default.
#' @param threshold the threshold to use for both the Logger _and_ all attached appenders; typically one of `all`, `trace`, `debug`, `warn`, `info`, or `fatal`.
#' @param layout_fmt the layout format string; by default `%L [%t] [%g] %m %f`.
#' @param timestamp_fmt the timestamp format string; by default `%FT%T%z` (ISO8601 datetime).
#' @export
init <- function(..., lgr = lgr::lgr, threshold = "all", layout_fmt = LAYOUT_FMT, timestamp_fmt = TIMESTAMP_FMT) {
  try({
    lgr$set_threshold(threshold)
		for(a in lgr$appenders) a$set_threshold(threshold)
    lgr$appenders[[1]]$set_layout(lgr::LayoutFormat$new(fmt = layout_fmt, timestamp_fmt = timestamp_fmt))
  })
}
