#' Deprecated horizontal plotting functions
#'
#' These functions were wrappers around functions from `ggstance` from an era
#' before `ggplot2` supported horizonally oriented geoms.  `ggstance` has not
#' been updated to comply with the current version of `ggplot2`, and since the
#' functionalilty is now available by other means, these functions have been
#' deprecated.
#'
#' @rdname ggstance
#' @aliases ggstance
#' @param ... additional arguments
#' @export
#' @examples
#' gf_violin(carat ~ color, data = diamonds)
#' gf_violin(carat ~ color, data = diamonds) |>
#'   gf_refine(coord_flip())
#' gf_violin(color ~ carat, data = diamonds)
#' gf_density(~ carat, data = diamonds)
#' gf_density(carat ~ ., data = diamonds)

gf_barh <-
  function(...) {
    .Defunct(msg = "This function has been removed from the ggformula.  Use gf_bar() instead.  See `?ggstance'.")
    gf_bar(...)
  }
#' @rdname ggstance
#' @aliases ggstance
#' @export
gf_countsh <-
  function(...) {
    .Defunct(msg = "This function has been removed from the ggformula.  Use gf_counts() instead.  See `?ggstance'.")
    gf_counts(...)
  }

#' @rdname ggstance
#' @aliases ggstance
#' @export
gf_colh <-
  function(...) {
    .Defunct(msg = "This function has been removed from the ggformula.  Use gf_col() instead.  See `?ggstance'.")
    gf_col(...)
  }
#' @rdname ggstance
#' @aliases ggstance
#' @export
gf_propsh <-
  function(...) {
    .Defunct(msg = "This function has been removed from the ggformula.  Use gf_props() instead.  See `?ggstance'.")
    gf_props(...)
  }
#' @rdname ggstance
#' @aliases ggstance
#' @export
gf_percentsh <-
  function(...) {
    .Defunct(msg = "This function has been removed from the ggformula.  Use gf_percents() instead.  See `?ggstance'.")
    gf_percents(...)
  }
#' @rdname ggstance
#' @aliases ggstance
#' @export
gf_boxploth <-
  function(...) {
    .Defunct(msg = "This function has been removed from the ggformula.  Use gf_boxplot() instead.  See `?ggstance'.")
    gf_boxplot(...)
  }
#
# #' @rdname ggstance
# #' @aliases ggstance
# #' @export
# gf_histogramh <-
#   function(...) {
#     .Defunct(msg = "This function has been removed from the ggformula.  Use gf_histogram() instead.  See `?ggstance'.")
#   }

#' @rdname ggstance
#' @aliases ggstance
#' @export
gf_linerangeh <-
  function(...) {
    .Defunct(msg = "This function has been removed from the ggformula.  Use gf_linerange() instead.  See `?ggstance'.")
    gf_linerange(...)
  }
#' @rdname ggstance
#' @aliases ggstance
#' @export
gf_pointrangeh <-
  function(...) {
    .Defunct(msg = "This function has been removed from the ggformula.  Use gf_pointrange() instead.  See `?ggstance'.")
    gf_pointrange(...)
  }
#' @rdname ggstance
#' @aliases ggstance
#' @export
gf_crossbarh <-
  function(...) {
    .Defunct(msg = "This function has been removed from the ggformula.  Use gf_crossbar() instead.  See `?ggstance'.")
    gf_crossbar(...)
  }
#' @rdname ggstance
#' @aliases ggstance
#' @export
gf_violinh <-
  function(...) {
    .Defunct(msg = "This function has been removed from the ggformula.  Use gf_violin() instead.  See `?ggstance'.")
    gf_violin(...)
  }
#' @rdname ggstance
#' @aliases ggstance
#' @export
gf_errorbarh <-
  function(...) {
    .Defunct(msg = "This function has been removed from the ggformula.  Use gf_errorbar() instead.  See `?ggstance'.")
    gf_errorbar(...)
  }
