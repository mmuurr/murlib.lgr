#' @include AAA.R
NULL


logfn_classname <- "murlib.lgr.logfn"


is_logger <- function(x) {
  isTRUE(R6::is.R6(x) && inherits(x, "Logger") && checkmate::test_string(x$name))
}


is_logfn <- function(x) {
  isTRUE(is.function(x) && inherits(x, logfn_classname) && is_logger(environment(x)$logger))
}


#' @title Build a 'clean' logger name
#' @details
#' Given a vector `x` of length `n`, this function:
#' 1. coerces to `chr(n)` (and throws an error if non-coercible),
#' 2. whitespace-trims the new vector's elements, and
#' 3. removes from the vector any empty or `NA` elements.
#'
#' ## Motivation
#' `lgr::get_logger(x)` will happily create loggers with names like `/foo/  /bar/NA//baz` if the input is `c("foo", "  ", "bar", NA_character_, "", "baz")`.
#' This function applies some sanity to that process and converts that example input into `c("foo", "bar", "baz")`, which can then be forwarded-on to `lgr::get_logger()`.
#' 
#' @param x `chr(n)`
#' @returns `chr(n* <= n)`
clean_logger_name <- function(x) {
	x |>
		as.character() |>
		stringr::str_trim() |>
		stringr::str_subset(".")  ## remove empty strings
}


#' @title Converts input to a log-friendly message string
#' @details
#' If `msg` is a non-NA string (`chr(1)`), it is returned as-is. Notably, the empty string (`""`) is happily accepted here.
#' If `msg` is a condition, the condition message is extracted and returned.
#' Otherwise `fail_msg` is returned.
#' @param `msg` the source message (or wrapper).
#' @param `fail_msg` the log message to use if `msg` can't be coerced into a log-friendly message (string).
#' @returns a log-friendly message (string).
logfn_msg <- function(msg, fail_msg = "<bad log msg>") {
	tryCatch({
		if (checkmate::test_string(msg)) {
			msg
		} else if (inherits(msg, "condition")) {
			conditionMessage(msg)
		} else {
			stop()
		}
	}, error = \(e) {
		"<bad log msg>"
	})
}


#' @title Create a new, scoped, logging function.
#' @param x `chr(n >= 0) | NULL | Logger | logfn`
#' @param context a (typically-named) list providing key/val context spliced into `...` in the underlying logging call.
#' @details
#' If `x` is `NULL`, character vector, or a Logger object, we assume we're creating and/or wrapping a Logger.
#' If `x` is a single `logfn`, we're wrapping (or 'layering deeper within') an existing `logfn` wrapper.
#' In both cases, we may be adding context.
#'
#' This logging function will always take the sgnature: `f(level, msg = "", ...)`, meaning:
#' * `level` **must** be specified (and exist for that Logger --- in the rare case where new log levels are created).
#' * `...` hold the spliced-in `context`, to be used when interpolating into `msg`.
#'
#' The logging function will **never fail**.
#' If it's called errantly, it will warn and swallow any error encountered during the call.
#' This helps enforce the principle that adding logging should never introduce errors into the surrounding program.
#' 
#' @returns a (possibly-new) scoped logging function.
#' @export
logfn <- function(x = NULL, ..., context = list()) {
	rlang::check_dots_empty()
	logger <-
		if (is_logger(x)) {
			x
		} else if (is.null(x)) {
			lgr::get_logger()
		} else if (is_logfn(x)) {
			logfn_logger(x)
		} else if (is.character(x)) {
			lgr::get_logger(clean_logger_name(x))
		} else {
			stop("x must be one of: NULL | Logger | logfn | chr(0+)")
		}
	context <-
		if (is_logfn(x)) {
			c(as.list(context), environment(x)$context)
		} else {
			as.list(context)
		}
	fn <- function(level, msg = "", ...) {
		tryCatch({
			if (missing(level)) rlang::abort("missing log level")
			dots <- rlang::dots_list(..., !!!context, .homonyms = "first")
			rlang::inject(logger$log(level, logfn_msg(msg), !!!dots))
		}, error = \(e) {
			rlang::warn(sprintf("failed logfn exec: %s", conditionMessage(e)))
		})
	}
	fn <- prepend_class(fn, logfn_classname)
	fn
}


#' @title Get a logfn's underlying Logger
#' @param logfn the logging function.
#' @returns `logfn`'s underlying (delegated-to) Logger.
#' @export
logfn_logger <- function(logfn) {
	tryCatch({
		environment(logfn)$logger
	}, error = \(e) NULL)
}


#' @title Generic (fallback) log message pretty-printer for R6 objects.
#' @details
#' To override, add `string_repr(x, width, ...)` method to an R6's `public` method list.
#' @exportS3Method lgr::string_repr
string_repr.RisioR6 <- function(x, width = 32L, ...) {
	x1 <- lgr:::string_repr.default(x, width, ...)
	x2 <- rlang::obj_address(x)
	sprintf("%s<%s>", x1, x2)
}
