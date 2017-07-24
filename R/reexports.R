# This allows us to use these functions without attaching the packages they come from.

#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @importFrom mosaicCore makeFun df_stats
#' @export
mosaicCore::makeFun

#' @export
mosaicCore::df_stats