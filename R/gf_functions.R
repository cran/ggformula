#' @importFrom mosaicCore makeFun
NA

#' Formula interface to ggplot2
#'
#' The functions in \pkg{ggformula} provide a formula interface to \pkg{ggplot2} layer
#' functions and a system for working with pipes to create multi-layer
#' plots and to refine plots.
#' For plots with just one layer, the formula interface
#' is more compact than native \pkg{ggplot2} code and is consistent with modeling
#' functions like [lm()] that use a formula interface and with the
#' numerical summary functions in the \pkg{mosaic} package.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#'
#' @rdname ggformula
#' @name ggformula
#' @examples
#' apropos("gf_")
#' gf_point()

NA

#' Formula interface to geom_point()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`,
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`, or
#'   (d) arguments for the geom, stat, or position function.
#'   Available attributes include
#'   `alpha`, `color`, `size`, `shape`, `fill`, `group`, `stroke`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_point()]
#' @export
#' @examples
#' gf_point()
#' gf_point(mpg ~ hp, color = ~ cyl, size = ~wt, data = mtcars)
#' # faceting -- two ways
#' gf_point(mpg ~ hp, data = mtcars) %>%
#'   gf_facet_wrap(~ am)
#' gf_point(mpg ~ hp | am, group = ~ cyl, data = mtcars)
#' gf_point(mpg ~ hp | ~ am, group = ~ cyl, data = mtcars)
#' gf_point(mpg ~ hp | am ~ ., group = ~ cyl,  data = mtcars)
#'
#' # Chaining in the data
#' mtcars %>% gf_point(mpg ~ wt)
#'

gf_point <-
  layer_factory(
    geom = "point",
    extras = alist(alpha = ,  color = , size = , shape = , fill = , group = , stroke = )
  )

#' Formula interface to geom_jitter()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `size`, `shape`, `fill`, `group`,
#'   `stroke`, `width`, `height`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_jitter()]
#' @export
#' @examples
#' gf_jitter()
#' if (require(mosaicData)) {
#'   # without jitter
#'   gf_point(age ~ sex, alpha = 0.25, data = HELPrct)
#'   # jitter only horizontally
#'   gf_jitter(age ~ sex, alpha = 0.25, data = HELPrct, width = 0.2, height = 0)
#'   # alternative way to get jitter
#'   gf_point(age ~ sex, alpha = 0.25, data = HELPrct,
#'     position = "jitter", width = 0.2, height = 0)
#' }
gf_jitter <-
  layer_factory(
    geom = "point",
    position = "jitter",
    extras = alist(alpha = ,  color = , size = , shape = , fill = , group = , stroke = )
    )

#' Formula interface to geom_line()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`, `lineend`, `linejoin`, `linemitre`, `arrow`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_line()]
#' @export
#' @examples
#' gf_line()
#' if (require(mosaicData)) {
#'   gf_point(age ~ sex, alpha = 0.25, data = HELPrct)
#'   gf_point(births ~ date, color = ~wday, data = Births78)
#'   # lines make the exceptions stand out more prominently
#'   gf_line(births ~ date, color = ~wday, data = Births78)
#'   }
gf_line <-
  layer_factory(
    geom = "line",
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = ,
                   lineend = , linejoin = , linemitre = , arrow = )
    )

#' Formula interface to geom_path()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `lineend`, `linejoin`, `linemitre`, `arrow`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_path()]
#' @export
#' @examples
#' gf_path()
#' if (require(dplyr)) {
#'   data.frame(t = seq(1, 10 * pi, length.out = 400)) %>%
#'   mutate( x = t * cos(t), y = t * sin(t)) %>%
#'   gf_path(y ~ x, color = ~t)
#'   }

gf_path <-
  layer_factory(
    geom = "path",
    extras = alist(alpha = , color = , group = , linetype = , size = ,
                   lineend = "butt", linejoin = "round", linemitre = 1, arrow = NULL)
  )

#' Formula interface to geom_smooth()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `method`, `formula`, `se`, `method.args`, `n`, `span`, `fullrange`, `level`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_smooth()]
#' @export
#' @examples
#' gf_smooth()
#' gf_lm()
#' if (require(mosaicData)) {
#'   gf_smooth(births ~ date, color = ~wday, data = Births78)
#'   gf_smooth(births ~ date, color = ~wday, data = Births78, fullrange = TRUE)
#'   gf_smooth(births ~ date, color = ~wday, data = Births78, show.legend = FALSE, se = FALSE)
#'   gf_lm(length ~ width, data = KidsFeet, color = ~biggerfoot, alpha = 0.2) %>%
#'     gf_point()
#'   gf_lm(length ~ width, data = KidsFeet, color = ~biggerfoot, fullrange = FALSE, alpha = 0.2)
#'     gf_point()
#'   gf_lm(length ~ width, color = ~ sex, data = KidsFeet,
#'         formula = y ~ poly(x,2), linetype = "dashed") %>%
#'     gf_point()
#'   gf_lm(length ~ width, color = ~ sex, data = KidsFeet,
#'         formula = log(y) ~ x, backtrans = exp) %>%
#'     gf_point()
#' }
#' gf_lm(hwy ~ displ, data = mpg,
#'       formula = log(y) ~ poly(x,3), backtrans = exp,
#'      interval = "prediction", fill = "skyblue") %>%
#'   gf_lm(
#'      formula = log(y) ~ poly(x,3), backtrans = exp,
#'      interval = "confidence", color = "red") %>%
#'   gf_point()
#'
gf_smooth <-
  layer_factory(
    geom = "smooth",
    stat = "smooth",
    extras = alist(method = "auto", formula = y ~ x, se = TRUE, method.args = ,
                   n = 80 , span = 0.75 , fullrange = FALSE, level = 0.95)
    )

#' @rdname gf_smooth
#' @export

gf_lm <-
  layer_factory(
    geom = "lm",
    stat = "lm",
    aes_form = y ~ x,
    extras = alist(alpha = 0.3, lm.args = list(), interval = "none", level = 0.95, fullrange = TRUE)
  )


#' Formula interface to geom_spline()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `weight`, `df`, `spar`, `tol`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_spline()]
#' @export
#' @examples
#' if (require(mosaicData)) {
#'   gf_spline(births ~ date, color = ~wday, data = Births78)
#'   gf_spline(births ~ date, color = ~wday, data = Births78, df = 20)
#'   gf_spline(births ~ date, color = ~wday, data = Births78, df = 4)
#' }

gf_spline <-
  layer_factory(
    geom = "line",
    stat = "spline",
    extras = alist(alpha = , color = , group = , linetype = , size = ,
                   weight = , df = , spar = , tol = )
    )

#' Formula interface to geom_raster()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`, `hjust`, `vjust`, `interpolate`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_raster()]
#' @export
#' @examples
#' # Justification controls where the cells are anchored
#' D <- expand.grid(x = 0:5, y = 0:5)
#' D$z <- runif(nrow(D))
#' # centered squares
#' gf_raster(z ~ x + y, data = D)
#' gf_raster(y ~ x, fill = ~ z, data = D)
#' # zero padding
#' gf_raster(z ~ x + y, data = D, hjust = 0, vjust = 0)

gf_raster <-
  layer_factory(
    geom = "raster",
    aes_form = list(y ~ x, fill ~ x + y),
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = ,
                   hjust = 0.5, vjust = 0.5, interpolate = FALSE)
  )

#' Formula interface to geom_quantile()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `weight`, `lineend`, `linejoin`, `linemitre`, `quantiles`, `formula`, `method`, `method.args`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_quantile()]
#' @export
#' @examples
#' gf_point((1/hwy) ~ displ, data = mpg) %>%
#'   gf_quantile((1/hwy) ~ displ)

gf_quantile <-
  layer_factory(
    geom = "quantile",
    stat = "quantile",
    extras = alist(alpha = , color = , group = , linetype = , size = , weight =,
                   lineend = "butt", linejoin = "round", linemitre = 1, quantiles = ,
                   formula = , method = ,  method.args =  )
  )

#' Formula interface to geom_density_2d()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `contour`, `n`, `h`, `lineend`, `linejoin`, `linemitre`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_density_2d()]
#' @export
#' @examples
#' if (require(mosaicData)) {
#'   gf_jitter(i1 ~ age, alpha = 0.2, data = HELPrct, width = 0.4, height = 0.4) %>%
#'   gf_density_2d(i1 ~ age, data = HELPrct)
#' }

gf_density_2d <-
  layer_factory(
    geom = "density_2d",
    stat = "density_2d",
    extras = alist(alpha = , color = , group = , linetype = , size = ,
                   contour = TRUE, n = 100 , h = NULL , lineend = "butt", linejoin = "round",
                   linemitre = 1 )
  )

#' Formula interface to geom_density2d()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `contour`, `n`, `h`, `lineend`, `linejoin`, `linemitre`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_density2d()]
#' @export
#' @examples
#' if (require(mosaicData)) {
#'   gf_jitter(i1 ~ age, alpha = 0.2, data = HELPrct, width = 0.4, height = 0.4) %>%
#'   gf_density2d(i1 ~ age, data = HELPrct)
#' }

gf_density2d <-
  layer_factory(
    geom = "density2d",
    stat = "density2d",
    extras = alist(alpha = , color = , group = , linetype = , size = ,
                   contour = TRUE, n = 100 , h = NULL , lineend = "butt", linejoin = "round",
                   linemitre = 1 )
  )

#' Formula interface to geom_hex()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `bins`, `binwidth`, `alpha`, `color`, `fill`, `group`, `size`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_hex()]
#' @export
#' @examples
#' if (require(mosaicData)) {
#'   gf_hex(i1 ~ age, data = HELPrct, bins = 15) %>%
#'   gf_density2d(i1 ~ age, data = HELPrct, color = "red", alpha = 0.5)
#' }
gf_hex <-
  layer_factory(
    geom = "hex",
    stat = "binhex",
    extras = alist(bins = , binwidth = , alpha = , color = , fill = , group = , size = )
  )

#' Formula interface to geom_boxplot()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `shape`, `size`, `weight`, `coef`, `outlier.color`, `outlier.fill`, `outlier.shape`, `outlier.size`, `outlier.stroke`, `outlier.alpha`, `notch`, `notchwidth`, `varwidth`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#'
#' @seealso [geom_boxplot()]
#' @export
#' @examples
#' if (require(mosaicData)) {
#'   gf_boxplot(age ~ substance, data = HELPrct)
#'   gf_boxplot(age ~ substance, data = HELPrct, varwidth = TRUE)
#'   gf_boxplot(age ~ substance, data = HELPrct, color = ~sex)
#'   gf_boxplot(age ~ substance, data = HELPrct, color = ~sex, outlier.color = "gray50")
#'   # longer whiskers
#'   gf_boxplot(age ~ substance, data = HELPrct, color = ~sex, coef = 2)
#'   gf_boxplot(age ~ substance, data = HELPrct, color = ~sex, position = position_dodge(width = 0.9))
#' }
gf_boxplot <-
  layer_factory(
    geom = "boxplot",
    stat = "boxplot",
    position = "dodge",
    extras = alist(
      alpha = , color = , fill = , group = , linetype = , shape = , size = ,
      weight =, coef = ,
      outlier.color = NULL, outlier.fill = NULL,
      outlier.shape = 19, outlier.size = 1.5, outlier.stroke = 0.5,
      outlier.alpha = NULL, notch = FALSE, notchwidth = 0.5, varwidth = FALSE)
  )

#' Formula interface to geom_text()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `label`, `alpha`, `angle`, `color`, `family`, `fontface`, `group`, `hjust`, `lineheight`, `size`, `vjust`, `parse`, `nudge_x`, `nudge_y`, `check_overlap`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_text()]
#' @export
#' @examples
#' gf_text(Sepal.Length ~ Sepal.Width, data = iris,
#'   label = ~Species, color = ~Species, size = 2, angle = 30)
#'
gf_text <-
  layer_factory(
    geom = "text",
    extras = alist(
      label =, alpha = , angle = , color = , family = , fontface = , group = , hjust = ,
      lineheight = , size = , vjust = , parse = FALSE, nudge_x = 0, nudge_y = 0,
      check_overlap = FALSE
      )
  )

#' Formula interface to geom_label()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `label`, `alpha`, `angle`, `color`, `family`, `fontface`, `group`, `hjust`, `lineheight`, `size`, `vjust`, `parse`, `nudge_x`, `nudge_y`, `lparse`, `nudge_x`, `nudge_y`, `label.padding`, `label.r`, `label.size`, `check_overlap`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_label()]
#' @export
#' @examples
#' if (require(dplyr)) {
#'   iris_means <-
#'     iris %>%
#'     group_by(Species) %>%
#'     summarise(Sepal.Length = mean(Sepal.Length), Sepal.Width = mean(Sepal.Width))
#'   gf_point(Sepal.Length ~ Sepal.Width, data = iris, color = ~ Species) %>%
#'   gf_label(Sepal.Length ~ Sepal.Width, data = iris_means,
#'     label = ~Species, color = ~Species, size = 2, alpha = 0.7)
#' }

gf_label <-
  layer_factory(
    geom = "label",
    extras = alist(
      label =, alpha = , angle = , color = , family = , fontface = , group = , hjust = ,
      lineheight = , size = , vjust = ,
      parse = , nudge_x = , nudge_y = ,
      nudge_x = 0, nudge_y = 0,
      label.padding = unit(0.25, "lines"), label.r = unit(0.15, "lines"),
      label.size = 0.25)
  )

#' Formula interface to geom_area()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_area()]
#' @export
#' @examples
#' if (require(weatherData) && require(dplyr)) {
#'   Temps <- NewYork2013 %>%
#'     mutate(date = lubridate::date(Time),
#'          month = lubridate::month(Time)) %>%
#'     filter(month <= 4) %>%
#'     group_by(date) %>%
#'     summarise(
#'       hi = max(Temperature, na.rm = TRUE),
#'       lo = min(Temperature, na.rm = TRUE)
#'     )
#'   gf_linerange(lo + hi  ~ date, color = ~hi, data = Temps)
#'   gf_ribbon(lo + hi ~ date, data = Temps, color = "navy", alpha = 0.3)
#'   gf_area(hi ~ date, data = Temps, color = "navy", alpha = 0.3)
#'
#'   Temps2 <- NewYork2013 %>% mutate(city = "NYC") %>%
#'     bind_rows(Mumbai2013 %>% mutate(city = "Mumbai")) %>%
#'     bind_rows(London2013 %>% mutate(city = "London")) %>%
#'     mutate(date = lubridate::date(Time),
#'            month = lubridate::month(Time)) %>%
#'     group_by(city, date) %>%
#'     summarise(
#'       hi = max(Temperature, na.rm = TRUE),
#'       lo = min(Temperature, na.rm = TRUE),
#'       mid = (hi + lo)/2
#'     )
#'   gf_ribbon(lo + hi ~ date, data = Temps2, alpha = 0.3) %>%
#'     gf_facet_grid(city ~ .)
#'
#'   gf_linerange(lo + hi ~ date, color = ~ mid, data = Temps2) %>%
#'     gf_facet_grid(city ~ .) %>%
#'     gf_refine(scale_colour_gradientn(colors = rev(rainbow(5))))
#' }
#'
gf_area <-
  layer_factory(
    geom = "area",
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = )
    )

#' Formula interface to geom_violin()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`, \code{}, `draw_quatiles`, `trim`, `scale`, `bw`, `adjust`, `kernel`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_violin()]
#' @export
#' @examples
#' if (require(mosaicData)) {
#'   gf_violin(age ~ substance, data = HELPrct)
#'   gf_violin(age ~ substance, data = HELPrct, fill = ~sex)
#' }
#'
gf_violin <-
  layer_factory(
    geom = "violin",
    stat = "ydensity",
    position = "dodge",
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = , weight = ,
      draw_quantiles = NULL, trim = TRUE, scale = "area", bw = , adjust = 1,
      kernel = "gaussian")
  )

#' Formula interface to geom_spoke()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `angle`, `radius`, `alpha`, `color`, `group`, `linetype`, `size`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @section Note `angle` and `radius` must be set or mapped.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_spoke()]
#' @export
#' @examples
#' D <- expand.grid(x = 1:10, y=1:10)
#' D$angle <- runif(100, 0, 2*pi)
#' D$speed <- runif(100, 0, sqrt(0.1 * D$x))
#'
#' gf_point(y ~ x, data = D) %>%
#'   gf_spoke(y ~ x, angle = ~angle, radius = 0.5)
#'
#' gf_point(y ~ x, data = D) %>%
#'   gf_spoke(y ~ x, angle = ~angle, radius = ~speed)

gf_spoke <-
  layer_factory(
    geom = "spoke",
    extras = alist(
      angle = , radius = ,
      alpha = , color = , group = , linetype = , size = ),
    note = "Note: angle and radius must be set or mapped."
  )


#' Formula interface to geom_step()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `direction`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_step()]
#' @export
#' @examples
#' if (require(mosaicData)) {
#'   gf_step( births ~ date, data = Births78, color = ~wday)
#' }

gf_step <-
  layer_factory(
    geom = "step",
    extras = alist(alpha = , color = , group = , linetype = , size = , direction = "hv" )
    )

#' Formula interface to geom_tile()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_tile()]
#' @export
#' @examples
#' D <- expand.grid(x = 0:5, y = 0:5)
#' D$z <- runif(nrow(D))
#' gf_tile(y ~ x, fill = ~ z, data = D)
#' gf_tile(z ~ x + y, data = D)

gf_tile <-
  layer_factory(
    geom = "tile",
    aes_form = list(y ~ x, fill ~ x + y),
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = )
  )

#' Formula interface to geom_count()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `shape`, `size`, `stroke`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_count()]
#' @export
#' @examples
#' # Best used in conjunction with scale_size_area which ensures that
#' # counts of zero would be given size 0. Doesn't make much difference
#' # here because the smallest count is already close to 0.
#'
#' gf_count(hwy ~ cty, data = mpg, alpha = 0.5) %>%
#'   gf_refine(scale_size_area())
#'
gf_count <-
  layer_factory(
    geom = "point",
    stat = "sum",
    extras = alist(
      alpha = , color = , fill = , group = , shape = , size = , stroke =
    )
  )

#' Formula interface to geom_col()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_col()]
#' @export
#' @examples
#' D <- data.frame(
#'   group = LETTERS[1:3],
#'   count = c(20, 25, 18)
#' )
#' gf_col(count ~ group, data = D)

gf_col <-
  layer_factory(
    geom = "col",
    position = "stack",
    extras = alist(
      alpha = , color = , fill = , group = , linetype = , size =
    )
  )
#' Formula interface to geom_blank()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   \code{}
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_blank()]
#' @export
#' @examples
#'
#'
#' gf_point((c(0,1)) ~ (c(0,5)))
#' gf_frame((c(0,1)) ~ (c(0,5)))
#' gf_blank((c(0,1)) ~ (c(0,5)))

gf_frame <-
  layer_factory(geom = "blank")

#' @export
#' @rdname gf_frame
gf_blank <-
  layer_factory(geom = "blank")

#' Formula interface to geom_histogram()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `~x` or `y ~ x`.
#'   `y` may be `..density..` or `..count..` or `..ndensity..` or `..ncount..`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_histogram()]
#' @export
#' @examples
#' x <- rnorm(1000)
#' gf_histogram(  ~ x, bins = 30)
#' gf_histogram( ..density.. ~ x, bins = 30)
#' gf_histogram(~ Sepal.Length | Species, data = iris, binwidth = 0.25)
#' if (require(mosaicData)) {
#'   gf_histogram(~age, data = HELPrct, binwidth = 5, fill = "skyblue", color = "black")
#'   # bins can be adjusted left/right using center or boundary
#'   gf_histogram(~age, data = HELPrct, binwidth = 5, fill = "skyblue", color = "black", center = 42.5)
#'   gf_histogram(~age, data = HELPrct, binwidth = 5, fill = "skyblue", color = "black", boundary = 40)
#' }


gf_histogram <-
  layer_factory(
    geom = "bar", stat = "bin", position = "stack",
    aes_form = list(~x, y ~ x),
    extras = alist(bins = 25, binwidth = , alpha = , color = , fill = , group = , linetype = , size = ),
    note = "y may be ..density.. or ..count.. or ..ndensity.. or ..ncount.."
  )

#' @rdname gf_histogram
#' @export
gf_dhistogram <-
  layer_factory(
    geom = "bar", stat = "bin", position = "stack",
    aes_form = list(~x, y ~ x),
    extras = alist(bins = 25, binwidth = , alpha = , color = , fill = , group = , linetype = , size = ),
    note = "y may be ..density.. or ..count.. or ..ndensity.. or ..ncount..",
    aesthetics = aes(y = ..density..)
  )

#' Formula interface to geom_density()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `~x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`, `weight`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_density()]
#' @export
#' @examples
#' gf_dens()
#' gf_density(~ Sepal.Length,  color = ~Species, data = iris)
#' gf_dens(~ Sepal.Length, color = ~Species, data = iris)
#' gf_freqpoly(~ Sepal.Length, color = ~Species, data = iris)
#' # Chaining in the data
#' iris %>% gf_dens(~ Sepal.Length, color = ~Species)
gf_density <-
  layer_factory(
    geom = "area", stat = "density",
    aes_form = ~ x,
    extras = alist(alpha = 0.5 , color = , fill = ,
                   group = , linetype = , size = , weight = ,
                   kernel = "gaussian", n = 512, trim = FALSE),
    aesthetics = aes(y = ..density..)
  )

# modified version of density plot without line along bottom and sides

#' Formula interface to geom_line() and stat_density()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `~x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `stat`, `alpha`, `color`, `fill`, `group`, `linetype`, `size`, `weight`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_line()]
#' @export
#' @examples
#' gf_dens()
#' gf_density(~ Sepal.Length,  color = ~Species, data = iris)
#' gf_dens(~ Sepal.Length, color = ~Species, data = iris)
#' gf_freqpoly(~ Sepal.Length, color = ~Species, data = iris)
#' # Chaining in the data
#' iris %>% gf_dens(~ Sepal.Length, color = ~Species)
gf_dens <-
  layer_factory(
    geom = "line", stat = "density",
    aes_form = ~ x,
    extras = alist(alpha = 0.5 , color = ,
                   group = , linetype = , size = , weight = ,
                   kernel = "gaussian", n = 512, trim = FALSE),
    aesthetics = aes(y = ..density..)
  )

#' Formula interface to geom_dotplot()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `~x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `binwidth`, `binaxis`, `method`, `binpositions`, `stackdir`, `stackratio`, `dotsize`, `stackgroups`, `origin`, `right`, `width`, `drop`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_dotplot()]
#' @export
#' @examples
#' gf_dotplot(~ Sepal.Length, fill = ~Species, data = iris)

gf_dotplot <-
  layer_factory(
    geom = "dotplot", stat = "bindot",
    aes_form = ~x,
    extras = alist(
      alpha = , color = , fill =, group = ,
      binwidth = NULL, binaxis = "x", method = "dotdensity",
      binpositions = "bygroup", stackdir = "up", stackratio = 1,
      dotsize = 1, stackgroups = FALSE, origin = NULL, right = TRUE,
      width = 0.9, drop = FALSE)
  )

#' Formula interface to geom_bar()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `~x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`, `width`, `binwidth`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_bar()]
#' @export
#' @examples
#' if (require(mosaicData)) {
#'   gf_bar( ~ substance, data = HELPrct)
#'   gf_bar( ~ substance, data = HELPrct, fill = ~sex)
#'   gf_bar( ~ substance, data = HELPrct, fill = ~sex, position = position_dodge())
#'   # gf_counts() is another name for gf_bar()
#'   gf_counts( ~ substance, data = HELPrct, fill = ~sex, position = position_dodge())
#' }

gf_bar <-
  layer_factory(
    geom = "bar", stat = "count", position = "stack",
    aes_form = ~ x,
    extras = alist(
      alpha = , color = , fill = , group = , linetype = , size = ,
      width = NULL, binwidth = NULL )
  )

#' @rdname gf_bar
#' @export

gf_counts <-
  layer_factory(
    geom = "bar", stat = "count", position = "stack",
    aes_form = ~ x,
    extras = alist(
      alpha = , color = , fill = , group = , linetype = , size = ,
      width = NULL, binwidth = NULL)
  )

#' Formula interface to geom_freqpoly()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `~x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `binwidth`, `bins`, `center`, `boundary`, \code{}
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_freqpoly()]
#' @export
#' @examples
#' gf_histogram(~ Sepal.Length | Species, alpha = 0.2, data = iris, bins = 20) %>%
#'   gf_freqpoly(~ Sepal.Length, data = iris, color = ~Species, bins = 20)
#' gf_freqpoly(~ Sepal.Length, color = ~Species, data = iris, bins = 20)
#' gf_dens(~ Sepal.Length, data = iris, color = "navy") %>%
#' gf_freqpoly(~ Sepal.Length, y = ~..density.., data = iris, color = "red", bins = 20)

gf_freqpoly <-
  layer_factory(
    geom = "path", stat = "bin",
    aes_form = ~ x,
    extras = alist(
      alpha = , color = , group = , linetype = , size =,
      binwidth =, bins = , center = , boundary = ,
    )
  )

#' Formula interface to geom_qq()
#'
#' `gf_qq()` an `gf_qqstep()` both create quantile-quantile plots. They
#' differ in how they display the qq-plot.
#' `gf_qq()` uses points and `gf_qqstep()` plots a step function
#' through these points.
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `~sample`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `group`, `x`, `y`, `distribution`, `dparams`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_qq()]
#' @export
#' @examples
#' gf_qq(~rnorm(100))
#' gf_qq(~Sepal.Length | Species, data = iris) %>% gf_qqline()
#' gf_qq(~Sepal.Length | Species, data = iris) %>% gf_qqline(tail = 0.10)
#' gf_qq(~Sepal.Length, color = ~Species, data = iris) %>%
#' gf_qqstep(~Sepal.Length, color = ~Species, data = iris)
gf_qq <-
  layer_factory(
    geom = "point", stat = "qq",
    aes_form = ~ sample,
    extras = alist(group = , x = , y =, distribution = stats::qnorm , dparams = list())
  )
#' @rdname gf_qq
#' @export

gf_qqline <-
  layer_factory(
    geom = "line", stat = "qqline",
    aes_form = ~ sample,
    extras = alist(group = , x = , y =, distribution = stats::qnorm , dparams = list(),
                   linetype = "dashed", alpha = 0.7)
  )

#' @export
#' @rdname gf_qq

gf_qqstep <-
  layer_factory(
    geom = "step", stat = "qq", position = "identity",
    aes_form = ~ sample,
    extras = alist(group = , x = , y =, distribution = stats::qnorm , dparams = list())
  )


#' Formula interface to geom_rug()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `~x` or `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `sides`, `alpha`, `color`, `group`, `linetype`, `size`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_rug()]
#' @export
#' @examples
#' gf_histogram(~eruptions, data = faithful) %>%
#' gf_rug(~eruptions, data = faithful, color = "red", sides = "bl") %>%
#' gf_rug(~eruptions, data = faithful, color = "navy", sides = "tr")
#' gf_point(Sepal.Length ~ Sepal.Width, data = iris) %>%
#' gf_rug(Sepal.Length ~ Sepal.Width)
#' gf_point(Sepal.Length ~ Sepal.Width, data = iris) %>%
#' gf_rug( x = ~ Sepal.Width, data = iris, color = "navy") %>%
#' gf_rug( y = ~ Sepal.Length, data = iris, color = "red")
gf_rug <-
  layer_factory(
    geom = "rug",
    aes_form = list(~ x, y ~ x, NULL),
    extras = alist(sides = "bl", alpha = , color = , group = , linetype = , size = )
    )



#' Formula interface to geom_contour()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `z ~ x + y`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   \code{}
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_contour()]
#' @export
#' @examples
#' gf_density_2d(eruptions ~ waiting, data = faithful, alpha = 0.5, color = "navy") %>%
#'   gf_contour(density ~ waiting + eruptions, data = faithfuld, bins = 10, color = "red")

gf_contour <-
  layer_factory(
    geom = "contour", stat = "contour",
    aes_form = z ~ x + y)

#' Formula interface to geom_ribbon()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `ymin + ymax ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_ribbon()]
#' @export
#' @examples
#' gf_ribbon()
#' if (require(weatherData) & require(dplyr)) {
#' Temps <- NewYork2013 %>% mutate(city = "NYC") %>%
#' bind_rows(Mumbai2013 %>% mutate(city = "Mumbai")) %>%
#' bind_rows(London2013 %>% mutate(city = "London")) %>%
#'   mutate(date = lubridate::date(Time),
#'          month = lubridate::month(Time)) %>%
#'   group_by(city, date) %>%
#'   summarise(
#'     hi = max(Temperature, na.rm = TRUE),
#'     lo = min(Temperature, na.rm = TRUE),
#'     mid = (hi + lo)/2
#'   )
#'
#' gf_ribbon(lo + hi ~ date, data = Temps, fill = ~city, alpha = 0.4) %>%
#'    gf_theme(theme = theme_minimal())
#' gf_linerange(lo + hi ~ date | city ~ ., color = ~mid, data = Temps) %>%
#'   gf_refine(scale_colour_gradientn(colors = rev(rainbow(5))))
#' gf_ribbon(lo + hi ~ date | city ~ ., data = Temps)
#' # Chaining in the data
#' Temps %>% gf_ribbon(lo + hi ~ date, alpha = 0.4) %>%
#'   gf_facet_grid(city ~ .)
#' }

gf_ribbon <-
  layer_factory(
    geom = "ribbon", aes_form = ymin + ymax ~ x,
    extras = list(alpha = 0.3))

#' Formula interface to geom_curve()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y + yend ~ x + xend`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `curvature`, `angle`, `ncp`, `arrow`, `lineend`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_curve()]
#' @export
#' @examples
#' D <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
#' gf_point(mpg ~ wt, data = mtcars) %>%
#'   gf_curve(y1 + y2 ~ x1 + x2, data = D, color = "navy") %>%
#'   gf_segment(y1 + y2 ~ x1 + x2, data = D, color = "red")

gf_curve <-
  layer_factory(
    geom = "curve", aes_form = y + yend ~ x + xend,
    extras = alist(
      alpha = , color = , group = , linetype = , size = ,
      curvature = 0.5, angle = 90, ncp = 5, arrow = NULL, lineend = "butt")
  )

#' Formula interface to geom_segment()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y + yend ~ x + xend`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `arrow`, `lineend`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_segment()]
#' @export
#' @examples
#' D <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
#' gf_point(mpg ~ wt, data = mtcars) %>%
#'   gf_curve(y1 + y2 ~ x1 + x2, data = D, color = "navy") %>%
#'   gf_segment(y1 + y2 ~ x1 + x2, data = D, color = "red")

gf_segment <-
  layer_factory(
    geom = "segment",
    aes_form = y + yend ~ x + xend,
    extras = alist(
      alpha = , color = , group = , linetype = , size = ,
      arrow = NULL, lineend = "butt"
      )
  )

#' Formula interface to geom_linerange()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `ymin + ymax ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_linerange()]
#' @export
#' @examples
#' gf_linerange()
#' if (require(weatherData) & require(dplyr)) {
#' Temps <- NewYork2013 %>% mutate(city = "NYC") %>%
#' bind_rows(Mumbai2013 %>% mutate(city = "Mumbai")) %>%
#' bind_rows(London2013 %>% mutate(city = "London")) %>%
#'   mutate(date = lubridate::date(Time),
#'          month = lubridate::month(Time)) %>%
#'   group_by(city, date) %>%
#'   summarise(
#'     hi = max(Temperature, na.rm = TRUE),
#'     lo = min(Temperature, na.rm = TRUE),
#'     mid = (hi + lo)/2
#'   )
#'
#' gf_ribbon(lo + hi ~ date, data = Temps, fill = ~city, alpha = 0.4) %>%
#'    gf_theme(theme = theme_minimal())
#' gf_linerange(lo + hi ~ date | city ~ ., color = ~mid, data = Temps) %>%
#'   gf_refine(scale_colour_gradientn(colors = rev(rainbow(5))))
#' gf_ribbon(lo + hi ~ date | city ~ ., data = Temps)
#' # Chaining in the data
#' Temps %>% gf_ribbon(lo + hi ~ date, alpha = 0.4) %>%
#'   gf_facet_grid(city ~ .)
#' }
#'
gf_linerange <-
  layer_factory(
    geom = "linerange",
    aes_form = ymin + ymax ~ x,
    extras = alist( alpha = , color = , group = , linetype = , size = )
  )

#' Formula interface to geom_pointrange()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y + ymin + ymax ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `fatten`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_pointrange()]
#' @export
#' @examples
#' if (require(mosaicData) && require(dplyr)) {
#' HELP2 <- HELPrct %>%
#'   group_by(substance, sex) %>%
#'   summarise(
#'     mean.age = mean(age),
#'     median.age = median(age),
#'     max.age = max(age),
#'     min.age = min(age),
#'     sd.age = sd(age),
#'     lo = mean.age - sd.age,
#'     hi = mean.age + sd.age
#'     )
#'
#'   gf_jitter(age ~ substance, data = HELPrct,
#'       alpha = 0.5, width = 0.2, height = 0, color = "skyblue") %>%
#'     gf_pointrange( mean.age + lo + hi ~ substance,  data = HELP2) %>%
#'     gf_facet_grid( ~ sex)
#'   gf_jitter(age ~ substance, data = HELPrct,
#'       alpha = 0.5, width = 0.2, height = 0, color = "skyblue") %>%
#'     gf_errorbar( lo + hi ~ substance,  data = HELP2) %>%
#'     gf_facet_grid( ~ sex)
#' }

gf_pointrange <-
  layer_factory(
    geom = "pointrange",
    aes_form = y + ymin + ymax ~ x,
    extras = alist(
      alpha = , color = , group = , linetype = , size = ,
      fatten = 2 )
  )

#' Formula interface to geom_crossbar()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y + ymin + ymax ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `fatten`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_crossbar()]
#' @export
#' @examples
#' if (require(mosaicData) && require(dplyr)) {
#' HELP2 <- HELPrct %>%
#'   group_by(substance, sex) %>%
#'   summarise(
#'     mean.age = mean(age),
#'     median.age = median(age),
#'     max.age = max(age),
#'     min.age = min(age),
#'     sd.age = sd(age),
#'     lo = mean.age - sd.age,
#'     hi = mean.age + sd.age
#'     )
#'
#'   gf_jitter(age ~ substance, data = HELPrct,
#'       alpha = 0.5, width = 0.2, height = 0, color = "skyblue") %>%
#'     gf_pointrange( mean.age + lo + hi ~ substance,  data = HELP2) %>%
#'     gf_facet_grid( ~ sex)
#'   gf_jitter(age ~ substance, data = HELPrct,
#'       alpha = 0.5, width = 0.2, height = 0, color = "skyblue") %>%
#'     gf_errorbar( lo + hi ~ substance,  data = HELP2) %>%
#'     gf_facet_grid( ~ sex)
#'   gf_jitter(age ~ substance, data = HELPrct,
#'       alpha = 0.5, width = 0.2, height = 0, color = "skyblue") %>%
#'     gf_boxplot( age ~ substance,  data = HELPrct, color = "red") %>%
#'     gf_crossbar( mean.age + lo + hi ~ substance,  data = HELP2) %>%
#'     gf_facet_grid( ~ sex)
#' }

gf_crossbar <-
  layer_factory(
    geom = "crossbar",
    aes_form = y + ymin + ymax ~ x,
    extras = alist(
      alpha = , color = , group = , linetype = , size = , fatten = 2.5
    )
  )

#' Formula interface to geom_errorbar()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `ymin + ymax ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @seealso [geom_errorbar()]
#' @export
#' @examples
#' if (require(mosaicData) && require(dplyr)) {
#' HELP2 <- HELPrct %>%
#'   group_by(substance, sex) %>%
#'   summarise(
#'     mean.age = mean(age),
#'     median.age = median(age),
#'     max.age = max(age),
#'     min.age = min(age),
#'     sd.age = sd(age),
#'     lo = mean.age - sd.age,
#'     hi = mean.age + sd.age
#'     )
#'
#'   gf_jitter(age ~ substance, data = HELPrct,
#'       alpha = 0.5, width = 0.2, height = 0, color = "skyblue") %>%
#'     gf_pointrange( mean.age + lo + hi ~ substance,  data = HELP2) %>%
#'     gf_facet_grid( ~ sex)
#'   gf_jitter(age ~ substance, data = HELPrct,
#'       alpha = 0.5, width = 0.2, height = 0, color = "skyblue") %>%
#'     gf_errorbar( lo + hi ~ substance,  data = HELP2) %>%
#'     gf_facet_grid( ~ sex)
#'   gf_jitter(age ~ substance, data = HELPrct,
#'       alpha = 0.5, width = 0.2, height = 0, color = "skyblue") %>%
#'     gf_boxplot( age ~ substance,  data = HELPrct, color = "red") %>%
#'     gf_crossbar( mean.age + lo + hi ~ substance,  data = HELP2) %>%
#'     gf_facet_grid( ~ sex)
#' }
gf_errorbar <-
  layer_factory(
    geom = "errorbar",
    aes_form = ymin + ymax ~ x,
    inherit.aes = FALSE,
    extras = alist(
      alpha = , color = , group = , linetype = , size =
      )
    )

#' Formula interface to geom_errorbarh()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x + xmin + xmax`.
#'   Faceting can be achieved by including `|` in the formula.
#'   Note: The odd shape for this is due to a quirk in \pkg{ggplot2} which has
#'   been changed on github, but not yet on CRAN.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#'
#' @seealso [geom_errorbarh()]
#' @export
#' @examples
#' if (require(mosaicData) && require(dplyr)) {
#' HELP2 <- HELPrct %>%
#'   group_by(substance, sex) %>%
#'   summarise(
#'     mean.age = mean(age),
#'     median.age = median(age),
#'     max.age = max(age),
#'     min.age = min(age),
#'     sd.age = sd(age),
#'     lo = mean.age - sd.age,
#'     hi = mean.age + sd.age
#'     )
#'
#'   gf_jitter(substance ~ age, data = HELPrct,
#'       alpha = 0.5, height = 0.2, width = 0, color = "skyblue") %>%
#'     gf_errorbarh( substance ~ mean.age + lo + hi,  data = HELP2) %>%
#'     gf_facet_grid( ~ sex)
#'   gf_jitter(age ~ substance, data = HELPrct,
#'       alpha = 0.5, width = 0.2, height = 0, color = "skyblue") %>%
#'     gf_errorbar( lo + hi ~ substance,  data = HELP2) %>%
#'     gf_facet_grid( ~ sex)
#' }
gf_errorbarh <-
  layer_factory(
    geom = "errorbarh",
    aes_form = y ~ x + xmin + xmax,
    extras = alist(
      alpha = , color = , group = , linetype = , size =
      )
    )

#' Formula interface to geom_rect()
#'
#' \pkg{ggformula} functions provide a formula interface to \pkg{ggplot2} layer
#' functions.
#' For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and \pkg{mosaic} notation.
#'
#' Positional aesthetics are typically specified using a formula (see the `gformula` argument).
#' Setting and mapping of additional attributes can be done through the use of additional arguments.
#' Attributes can be set can be set using arguments of the form `attribute = value` or
#' mapped using arguments of the form `attribute = ~ expression`.
#'
#' In formulas of the form `A | B`, `B` will be used to form facets using
#' [facet_wrap()] or [facet_grid()].
#' This provides an alternative to
#' [gf_facet_wrap()] and
#' [gf_facet_grid()] that is terser and may feel more familiar to users
#' of \pkg{lattice}.
#'
#' Evaluation of the \pkg{ggplot2} code occurs in the environment of `gformula`.
#' This will typically do the right thing when formulas are created on the fly, but might not
#' be the right thing if formulas created in one environment are used to create plots
#' in another.
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `ymin + ymax ~ xmin + xmax`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#'
#' @seealso [geom_rect()]
#' @export
#' @examples
#' gf_rect( 1 + 2 ~ 3 + 4, alpha = 0.3, color = "red")
#'
gf_rect <-
  layer_factory(
    geom = "rect",
    aes_form = ymin + ymax ~ xmin + xmax,
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = )
  )


#' Reference lines -- horizontal, vertical, and diagonal.
#'
#' These functions create layers that display lines described i various ways.  Unlike most
#' of the plotting functions in `ggformula`, these functions do not take a formula
#' as input for describing positional attributes of the plot.
#'
#'
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula Must be `NULL`.
#' @param data A data frame with the variables to be plotted.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `slope`, `intercept`
#' @param geom A character string naming the geom used to make the layer.
#' @param stat A character string naming the stat used to make the layer.
#' @param position Either a character string naming the position function used
#'   for the layer or a position object returned from a call to a position function.
#' @param show.legend A logical indicating whether this layer should be included in
#'   the legends.  `NA`, the default, includes layer in the legends if any
#'   of the attributes of the layer are mapped.
#' @param show.help If `TRUE`, display some minimal help.
#' @param coef A numeric vector of length at least 2, treated as intercept and slope.
#' Additional components, if any, are ignored (with a warning).
#' @param model An object with a method for `coef()` that returns a
#' numeric vector, the first two elements of which are intercept and slope.
#' This is equivalent to `coef = coef(model)`.
#' @param inherit A logical indicating whether default attributes are inherited.
#' @return a gg object
#' @rdname gf_lines
#' @seealso [geom_abline()],
#'   [geom_vline()],
#'   [geom_hline()]
#' @export
#' @examples
#' mtcars2 <- df_stats( wt ~ cyl, data = mtcars)
#' gf_point(wt ~ hp, size = ~wt, color = ~cyl, data = mtcars) %>%
#'   gf_abline(slope = 0, intercept = ~median, color = ~cyl, data = mtcars2)
#' gf_point(wt ~ hp, size = ~wt, color = ~cyl, data = mtcars) %>%
#'   gf_hline(slope = 0, yintercept = ~median, color = ~cyl, data = mtcars2)
#'
#' gf_point(mpg ~ hp, color = ~cyl, size = ~wt, data = mtcars) %>%
#'   gf_abline(color="red", slope = -0.10, intercept = 35)
#' gf_point(mpg ~ hp, color = ~cyl, size = ~wt, data = mtcars) %>%
#'   gf_abline(color = "red", slope = ~slope, intercept = ~intercept,
#'   data = data.frame(slope = -0.10, intercept = 33:35))
#' gf_point(mpg ~ hp, color = ~cyl, size = ~wt, data = mtcars) %>%
#'   gf_abline(intercept = ~ c(10, 20, 30), slope = ~c(1, 0, -1)/100,
#'     color = c("red", "green", "blue"))
#'
#' # We can set the color of the guidelines while mapping color in other
#' # layers
#' gf_point(mpg ~ hp, color = ~cyl, size = ~wt, data = mtcars) %>%
#'   gf_hline(color = "navy", yintercept = ~c(20, 25)) %>%
#'   gf_vline(color = "brown", xintercept = ~c(200, 300))
#'
#' # If we want to map the color of the guidelines, it must work with the
#' # scale of the other colors in the plot.
#' gf_point(mpg ~ hp, size = ~wt, data = mtcars, alpha = 0.3) %>%
#'   gf_hline(color = ~"horizontal", yintercept = ~c(20, 25)) %>%
#'   gf_vline(color = ~"vertical", xintercept = ~c(100, 200, 300), data = NA)
#' gf_point(mpg ~ hp, size = ~wt, color = ~ factor(cyl), data = mtcars, alpha = 0.3) %>%
#'   gf_hline(color = "orange", yintercept = 20, data = NA) %>%
#'   gf_vline(color = ~c("4", "6", "8"), xintercept = c(80, 120, 250), data = NA) %>%
#' # reversing the layers requires using inherit = FALSE
#' gf_hline(color = "orange", yintercept = 20, data = NA) %>%
#'   gf_vline(color = ~c("4", "6", "8"), xintercept = c(80, 120, 250), data = NA) %>%
#'   gf_point(mpg ~ hp, size = ~wt, color = ~ factor(cyl), data = mtcars, alpha = 0.3,
#'     inherit = FALSE)
#'
#'
gf_abline <-
  layer_factory(
    geom = "abline", aes_form = NULL,
    extras = alist( slope =, intercept = ),
    inherit.aes = FALSE,
    data = NA
  )

#' @rdname gf_lines
#' @export
gf_hline <-
  layer_factory(
    geom = "hline", aes_form = NULL,
    extras = alist(yintercept = ),
    inherit.aes = FALSE,
    data = NA
  )

#' @rdname gf_lines
#' @export
gf_vline <-
  layer_factory(
    geom = "vline", aes_form = NULL,
    extras = alist(xintercept = ),
    inherit.aes = FALSE,
    data = NA
    )

#' @rdname gf_lines
#' @export


gf_coefline <- function(object = NULL, coef = NULL, model = NULL, ...) {
  if (is.null(coef) + is.null(model) != 1) stop("must specify exactly one of coef or model")
  if (is.null(coef)) coef <- coef(model)
  if (length(coef) > 2) warning("Ignoring all but first two values of coef.")
  if (length(coef) < 2) stop("coef must be of length at least 2.")
  gf_abline(object = object, intercept = coef[1], slope = coef[2], ..., inherit.aes = FALSE)
}

utils::globalVariables(c("x"))

#' Layers displaying graphs of functions
#'
#' These functions provide two different
#' interfaces for creating a layer that contains the graph of a function.
#'
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#' @param ... Other arguments such as `position="dodge"`.
#' @param fun A function.
#' @param inherit A logical indicating whether attributes should be inherited.
#' @rdname gf_functions
#' @export
#' @examples
#' gf_function(fun = sqrt, xlim = c(0, 10))
#' if (require(mosaicData)) {
#'   gf_histogram(..density.. ~ age, data = HELPrct, binwidth = 3, alpha = 0.6) %>%
#'     gf_function(fun = dnorm,
#'       args = list(mean = mean(HELPrct$age), sd = sd(HELPrct$age)),
#'       color = "red")
#' }


gf_function <- function(object = NULL, fun, xlim, ..., inherit = FALSE) {
  if (rlang::is_function(object) || rlang::is_character(object)) {
    fun <- object
    object <- NULL
  }
  if (is.null(object)) {
    object <- ggplot(data = data.frame(x = xlim), aes(x))
    inherit <- TRUE
  }
  qdots <- rlang::quos(...)
  afq <- aes_from_qdots(qdots)
  object +
    do.call(
      ggplot2::layer,
      list(geom = "path", stat = "function", position = "identity",
           mapping = afq$mapping,
           inherit.aes = inherit,
           data = if (missing(xlim)) NULL else data.frame(x = xlim),
           params = c(list(fun = fun), lapply(afq$qdots, rlang::f_rhs))
      )
    )
}

#' @rdname gf_functions
#' @param formula A formula describing a function.  See examples and [mosaicCore::makeFun()].
#' @param xlim A numeric vector providing the extent of the x-axis when creating
#'   the first layer in a plot.  Ignored when creating a subsequent layer.
#' @export
#' @examples
#' gf_fun(5 + 3 * cos(10 * x) ~ x, xlim = c(0,2))
#' # Utility bill is quadratic in month?
#' f <- makeFun(lm(totalbill ~ poly(month, 2), data = Utilities))
#' gf_point(totalbill ~ month, data = Utilities, alpha = 0.6) %>%
#'   gf_fun(f(m) ~ m, color = "red")

gf_fun <- function(object = NULL, formula, xlim, ..., inherit = FALSE) {
  if (rlang::is_formula(object) && missing(formula)) {
    formula <- object
    object <- NULL
  }

  if (is.null(object)) {
    object <- ggplot(data = data.frame(x = xlim), aes(x))
    inherit <- TRUE
  }
  qdots <- rlang::quos(...)
  fun <- function(x, ...) mosaicCore::makeFun(formula)(x, ...)
  afq <- aes_from_qdots(qdots)
  object +
    do.call(
      ggplot2::layer,
        list(geom = "path", stat = "function", position = "identity",
             mapping = afq$mapping,
             inherit.aes = inherit,
             data = if (missing(xlim)) NULL else data.frame(x = xlim),
             params = c(list(fun = fun), lapply(afq$qdots, rlang::f_rhs))
      )
    )
}

