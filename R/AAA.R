#' @import murlib.core
NULL


## see: https://s-fleck.github.io/lgr/reference/LayoutFormat.html
LAYOUT_FMT <- "%L [%t] [%g] %m %f"
TIMESTAMP_FMT <- "%FT%T%z"  ## %t above, follows the base::format.POSIXct() formatting rules
