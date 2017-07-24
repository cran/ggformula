#' Non-layer functions for gf plots
#'
#' These functions modify things like labels, limits, scales, etc. for plots
#' ggplot2 plots. They are wrappers around functions in ggplot2 that allow for
#' chaining syntax.
#'
#' \code{gf_refine()} provides a mechanism to replace \code{+} with the
#' chaining operator from \pkg{magrittr}.
#' Each of its \code{\dots} arguments is added in turn to the
#' base plot in \code{object}.  The other functions are thin wrappers around
#' specific \code{ggplot2} refinement functions and pass their \code{\dots}
#' arguments through to the similarly named \code{ggplot2} functions.
#'
#' @param object a gg object
#' @param ... additional arguments passed through to the similarly named function in
#' \pkg{ggplot2}.
#' @return a modified gg object
#'
#' @rdname gf_aux
#' @export
#' @examples
#' if (require(mosaicData)) {
#' gf_dens( ~ cesd, color = ~ substance, size = 1.5, data = HELPrct) %>%
#' gf_labs(
#'   title = "Center for Epidemiologic Studies Depression measure",
#'   subtitle = "(at baseline)",
#'   color = "Abused substance: ",
#'   x = "CESD score",
#'   y = "",
#'   caption = "Source: HELPrct"
#' ) %>%
#'   gf_theme(theme_classic()) %>%
#'   gf_theme(
#'     axis.text.y = element_blank(),
#'     legend.position = "top",
#'     plot.title = element_text(hjust = 0.5, color = "navy"),
#'     plot.subtitle = element_text(hjust = 0.5, color = "navy", size = 12))
#' }
#' gf_point(eruptions ~ waiting, data = faithful, alpha = 0.5)
#' gf_point(eruptions ~ waiting, data = faithful, alpha = 0.5) %>%
#'   gf_lims(x = c(65, NA), y = c(3, NA))
#'
#' # modify scales using gf_refine()
#' gf_jitter(Sepal.Length ~ Sepal.Width, color = ~ Species, data = iris) %>%
#'   gf_refine(scale_color_brewer(type = "qual", palette = 3)) %>%
#'   gf_theme(theme_bw())
#'
#' gf_jitter(Sepal.Length ~ Sepal.Width, color = ~ Species, data = iris) %>%
#'   gf_refine(scale_color_manual(values = c("red", "navy", "limegreen"))) %>%
#'   gf_theme(theme_bw())
#'
gf_labs <- function(object, ...) {
  object + ggplot2::labs(...)
}

#' @rdname gf_aux
#' @export
gf_lims <- function(object, ...) {
  object + ggplot2::lims(...)
}

#' Themes for ggformula
#'
#' Themes for ggformula
#'
#' @rdname gf_theme
#' @param object a gg object
#' @param theme a ggplot2 theme function like \code{\link{theme_minimal}}.
#' @param ... If \code{theme} is missing, then these additional arguments are theme elements
#'   of the sort handled by \code{\link[ggplot2]{theme}()}.
#' @return a modified gg object
#' @export
gf_theme <- function(object, theme, ...) {
  if (missing(theme)) {
    object + ggplot2::theme(...)
  } else {
    if (is.function(theme)) {
      object + do.call(theme, list(...))
    } else {
      object + theme
    }
  }
}

#' @rdname gf_aux
#' @export
gf_facet_wrap <- function(object, ...) {
  object + ggplot2::facet_wrap(...)
}

#' @rdname gf_aux
#' @export
gf_facet_grid <- function(object, ...) {
  object + ggplot2::facet_grid(...)
}

#' @rdname gf_aux
#' @export
gf_refine <- function(object, ...) {
  Reduce(`+`, list(...), init = object)
}

