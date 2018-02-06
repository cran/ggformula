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
#' A (sometimes partial) list of available attributes can be obtained by executing
#' plotting functions with no arguments.
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
#' Scatterplots in `ggformula`.
#'
#' Positional aesthetics are specified using the formula in `gformula`.
#' Setting and mapping of additional attributes can be done through the
#' use of additional arguments.
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
#'
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument.
#' See details and examples.
#'
#' @param gformula A formula with shape `y ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param data A data frame with the variables to be plotted.
#' @param environment An environment in which to look for variables not found in `data`.
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
#' @param xlab Label for x-axis. See also [`gf_labs()`].
#' @param ylab Label for y-axis. See also [`gf_labs()`].
#' @param title,subtitle,caption Title, sub-title, and caption for the plot.
#'   See also [`gf_labs()`].
#'
#' @return a gg object
#' @seealso [ggplot2::geom_point()], [gf_line()], [gf_jitter()]
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
#' # Chaining in the data
#' mtcars %>% gf_point(mpg ~ wt)
#'
#' # short cuts for main labels in the plot
#' if (require(mosaicData)) {
#'   gf_point(births ~ date, color = ~ wday, data = Births78,
#'     xlab = "Date", ylab = "Number of Live Births",
#'     title = "Interesting Patterns in the Number of Births",
#'     subtitle = "(United States, 1978)",
#'     caption = "Source: mosaicData::Births78")
#' }
#'

gf_point <-
  layer_factory(
    geom = "point",
    extras = alist(alpha = ,  color = , size = , shape = , fill = , group = , stroke = )
  )

#' Formula interface to geom_jitter()
#'
#' Jittered scatter plots in `ggformula`.
#'
#' @inherit gf_point
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `size`, `shape`, `fill`, `group`,
#'   `stroke`, `width`, `height`
#' @seealso [ggplot2::geom_jitter()], [gf_point()]
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

#' Formula interface to geom_line() and geom_path()
#'
#' Line plots in `ggformula`.  `gf_path()` differs from `gf_line()` in that points
#' are connected in the order in which they appear in `data`.
#'
#' @inherit gf_point
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`, `lineend`, `linejoin`, `linemitre`, `arrow`
#' @seealso [ggplot2::geom_line()], [gf_point()]
#' @export
#' @examples
#' gf_line()
#' if (require(mosaicData)) {
#'   gf_point(age ~ sex, alpha = 0.25, data = HELPrct)
#'   gf_point(births ~ date, color = ~wday, data = Births78)
#'   # lines make the exceptions stand out more prominently
#'   gf_line(births ~ date, color = ~wday, data = Births78)
#'   }
#'
gf_line <-
  layer_factory(
    geom = "line",
    extras = alist(alpha = , color = , fill = , group = , linetype = , size = ,
                   lineend = , linejoin = , linemitre = , arrow = )
    )

#' @rdname gf_line
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
#' LOESS and linear model smoothers in `ggformula`.
#'
#' @inherit gf_point
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `method`, `formula`, `se`, `method.args`, `n`, `span`, `fullrange`, `level`
#' @seealso [ggplot2::geom_smooth()], [gf_spline()]
#'
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
#' Fitting splines in `ggformula`.
#'
#' @inherit gf_point
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `weight`, `df`, `spar`, `tol`
#' @seealso [geom_spline()], [gf_smooth()], [gf_lm()]
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
#' Formula interface to geom_raster()
#'
#' @inherit gf_point
#' @param gformula A formula with shape  `y ~ x` or `fill ~ x + y`
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`, `hjust`, `vjust`, `interpolate`
#' @seealso [ggplot2::geom_raster()]
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
#' Quantile-Quantile plots in `ggformula`.
#'
#' @inherit gf_point
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `weight`, `lineend`, `linejoin`, `linemitre`, `quantiles`, `formula`, `method`, `method.args`
#' @seealso [ggplot2::geom_quantile()]
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
#' @inherit gf_point
#' @inherit ggplot2::geom_density_2d description
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `contour`, `n`, `h`, `lineend`, `linejoin`, `linemitre`
#' @seealso [ggplot2::geom_density_2d()]
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

#' @rdname gf_density_2d
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
#' @inherit gf_point
#' @inherit ggplot2::geom_hex details
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `bins`, `binwidth`, `alpha`, `color`, `fill`, `group`, `size`
#' @seealso [ggplot2::geom_hex()]
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
#' @inherit gf_point
#' @inherit ggplot2::geom_boxplot description references
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `shape`, `size`, `weight`, `coef`, `outlier.color`, `outlier.fill`, `outlier.shape`, `outlier.size`, `outlier.stroke`, `outlier.alpha`, `notch`, `notchwidth`, `varwidth`
#'
#' @seealso [ggplot2::geom_boxplot()], [fivenum()], [df_stats()]
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

#' Formula interface to geom_text() and geom_label()
#'
#' @inherit gf_point
#' @inherit ggplot2::geom_text references description
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `label`, `alpha`, `angle`, `color`, `family`, `fontface`, `group`, `hjust`, `lineheight`, `size`, `vjust`, `parse`, `nudge_x`, `nudge_y`, `check_overlap`
#' @seealso [ggplot2::geom_text()]
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

#' @rdname gf_text
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
#' @inherit gf_point
#' @inherit ggplot2::geom_area description
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`
#' @seealso [ggplot2::geom_area()]
#' @export
#' @examples
#' if (require(dplyr) && require(mosaicData)) {
#'   Temps <- Weather %>%
#'     filter(city == "Chicago", year == 2016, month <= 4)
#'   gf_linerange(low_temp + high_temp  ~ date, color = ~ high_temp, data = Temps)
#'   gf_ribbon(low_temp + high_temp ~ date, data = Temps, color = "navy", alpha = 0.3)
#'   gf_area(high_temp ~ date, data = Temps, color = "navy", alpha = 0.3)
#'
#'   gf_ribbon(low_temp + high_temp ~ date, data = Weather, alpha = 0.3) %>%
#'     gf_facet_grid(city ~ .)
#'
#'   gf_linerange(low_temp + high_temp ~ date, color = ~ high_temp, data = Weather) %>%
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
#' @inherit gf_point
#' @inherit ggplot2::geom_violin references description
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`, \code{}, `draw_quatiles`, `trim`, `scale`, `bw`, `adjust`, `kernel`
#' @seealso [ggplot2::geom_violin()]
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
#' @inherit gf_point
#' @inherit ggplot2::geom_spoke description references
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `angle`, `radius`, `alpha`, `color`, `group`, `linetype`, `size`
#' @seealso [ggplot2::geom_spoke()]
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
#' @inherit gf_point
#' @inherit ggplot2::geom_step description references
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `direction`
#' @seealso [ggplot2::geom_step()]
#' @export
#' @examples
#' if (require(mosaicData)) {
#'   gf_step( births ~ date, data = Births78, color = ~wday)
#' }
#'
#' # Roll your own Kaplan-Meier plot
#'
#' if (require(survival) && require(broom)) {
#'   # fit a survival model
#'   surv_fit <- survfit(coxph(Surv(time, status) ~ age + sex, lung))
#'   surv_fit
#'   # use broom::tidy() to create a tidy data frame for plotting
#'   surv_df <- tidy(surv_fit)
#'   head(surv_df)
#'   # now create a plot
#'   surv_df %>%
#'     gf_step(estimate ~ time) %>%
#'     gf_ribbon(conf.low + conf.high ~ time, alpha = 0.2)
#' }

gf_step <-
  layer_factory(
    geom = "step",
    extras = alist(alpha = , color = , group = , linetype = , size = , direction = "hv" )
    )

#' Formula interface to geom_tile()
#'
#' @inherit gf_point
#' @inherit ggplot2::geom_tile description references
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`
#' @seealso [ggplot2::geom_tile()]
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
#' @inherit gf_point
#' @inherit ggplot2::geom_count description references
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `shape`, `size`, `stroke`
#' @seealso [ggplot2::geom_count()]
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
#' @inherit gf_point
#' @inherit ggplot2::geom_col description references
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`
#' @seealso [ggplot2::geom_col()]
#' @export
#' @examples
#' D <- data.frame(
#'   group = LETTERS[1:3],
#'   count = c(20, 25, 18)
#' )
#' gf_col(count ~ group, data = D)
#'
#' # A Pareto chart
#'
#' if(require(dplyr) && require(mosaicData)) {
#'   HELPrct %>%
#'     group_by(substance) %>%
#'     summarise(count = n()) %>%
#'     ungroup() %>%
#'     arrange(-count) %>%
#'     mutate(
#'       cumcount = cumsum(count),
#'       substance = reorder(substance, - count)
#'     ) %>%
#'     gf_col(count ~ substance, fill = "skyblue") %>%
#'     gf_point(cumcount ~ substance) %>%
#'     gf_line(cumcount ~ substance, group = 1) %>%
#'     gf_refine(
#'       scale_y_continuous(sec.axis = sec_axis(~ . /nrow(HELPrct)))
#'     )
#'   }

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
#' @inherit gf_point
#' @inherit ggplot2::geom_blank description references
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   \code{}
#' @seealso [ggplot2::geom_blank()]
#' @export
#' @examples
#'
#' gf_point((c(0,1)) ~ (c(0,5)))
#' gf_frame((c(0,1)) ~ (c(0,5)))
#' gf_blank((c(0,1)) ~ (c(0,5)))

gf_blank <-
  layer_factory(geom = "blank")

#' @rdname gf_blank
#' @export
#'
gf_frame <-
  layer_factory(geom = "blank")


#' Formula interface to geom_histogram()
#'
#' Count and density histograms in `ggformula`.
#' @inherit gf_point
#' @inherit ggplot2::geom_histogram description references
#' @param gformula A formula with shape `~ x` (or `y ~ x`, but this shape is not
#'   generally needed).
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`
#' @seealso [ggplot2::geom_histogram()]
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

#' Formula interface to stat_density()
#'
#' @inherit gf_point
#' @inherit ggplot2::geom_density description references
#'
#' @param gformula A formula with shape `~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`, `weight`
#' @seealso [ggplot2::geom_density()]
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

#' @rdname gf_density
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
#' @inherit gf_point
#' @inherit ggplot2::geom_dotplot description
#' @param gformula A formula with shape `~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `binwidth`, `binaxis`, `method`, `binpositions`, `stackdir`, `stackratio`, `dotsize`, `stackgroups`, `origin`, `right`, `width`, `drop`
#' @seealso [ggplot2::geom_dotplot()]
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
#' @inherit gf_point
#' @inherit ggplot2::geom_bar description references
#'
#' @param gformula A formula with shape `~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`, `width`, `binwidth`
#' @seealso [ggplot2::geom_bar()]
#' @export
#' @examples
#' if (require(mosaicData)) {
#'   gf_bar( ~ substance, data = HELPrct)
#'   gf_bar( ~ substance, data = HELPrct, fill = ~sex)
#'   gf_bar( ~ substance, data = HELPrct, fill = ~sex, position = position_dodge())
#'   # gf_counts() is another name for gf_bar()
#'   gf_counts( ~ substance, data = HELPrct, fill = ~sex, position = position_dodge())
#'   # gf_props() and gf_percents() use proportions or percentages instead of counts
#'   gf_props( ~ substance, data = HELPrct, fill = ~sex, position = position_dodge())
#'   gf_percents( ~ substance, data = HELPrct, fill = ~sex, position = position_dodge())
#'   if (require(scales)) {
#'     gf_props( ~ substance, data = HELPrct, fill = ~sex, position = position_dodge()) %>%
#'       gf_refine(scale_y_continuous(labels = scales::percent))
#'   }
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

#' @rdname gf_bar
#' @export
gf_props <-
  layer_factory(
    geom = "bar", stat = "count", position = "stack",
    aes_form = list(~x),
    extras = alist(bins = 25, binwidth = , alpha = , color = ,
                   fill = , group = , linetype = , size = , ylab = "proportion"),
    aesthetics = aes(y = ..count.. / sum(..count..))
  )

#' @rdname gf_bar
#' @export
gf_percents <-
  layer_factory(
    geom = "bar", stat = "count", position = "stack",
    aes_form = list(~x),
    extras = alist(bins = 25, binwidth = , alpha = , color = ,
                   fill = , group = , linetype = , size = , ylab = "percent"),
    aesthetics = aes(y = 100 * ..count.. / sum(..count..))
  )

#' Formula interface to geom_freqpoly()
#'
#' @inherit gf_point
#' @inherit ggplot2::geom_freqpoly description references
#' @param gformula A formula with shape `~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `binwidth`, `bins`, `center`, `boundary`, \code{}
#' @seealso [ggplot2::geom_freqpoly()]
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
#' @inherit gf_freqpoly
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `group`, `x`, `y`, `distribution`, `dparams`
#' @seealso [ggplot2::geom_qq()]
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
#' @inherit gf_point
#' @inherit ggplot2::geom_rug
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `sides`, `alpha`, `color`, `group`, `linetype`, `size`
#' @seealso [ggplot2::geom_rug()]
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
#' @inherit gf_point
#' @inherit ggplot2::geom_contour description references
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   \code{}
#' @seealso [ggplot2::geom_contour()], [gf_density_2d()]
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
#' @inherit gf_point
#' @inherit ggplot2::geom_ribbon description references
#' @param gformula A formula with shape `ymin + ymax ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`
#' @seealso [ggplot2::geom_ribbon()]
#' @export
#' @examples
#' gf_ribbon()
#'
#' if (require(mosaicData)) {
#' gf_ribbon(low_temp + high_temp ~ date, data = Weather, fill = ~ city, alpha = 0.4) %>%
#'    gf_theme(theme = theme_minimal())
#' gf_linerange(
#'     low_temp + high_temp ~ date | city ~ ., color = ~ high_temp,
#'     data = Weather) %>%
#'   gf_refine(scale_colour_gradientn(colors = rev(rainbow(5))))
#' gf_ribbon(low_temp + high_temp ~ date | city ~ ., data = Weather)
#' # Chaining in the data
#' Weather %>% gf_ribbon(low_temp + high_temp ~ date, alpha = 0.4) %>%
#'   gf_facet_grid(city ~ .)
#' }

gf_ribbon <-
  layer_factory(
    geom = "ribbon", aes_form = ymin + ymax ~ x,
    extras = list(alpha = 0.3))

#' Formula interface to geom_curve()
#'
#' @inherit gf_point
#' @inherit ggplot2::geom_curve description references
#'
#' @param gformula A formula with shape `y + yend ~ x + xend`.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `curvature`, `angle`, `ncp`, `arrow`, `lineend`
#' @seealso [ggplot2::geom_curve()]
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
#' @inherit gf_curve
#' @inherit ggplot2::geom_segment description references
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `arrow`, `lineend`
#' @seealso [ggplot2::geom_segment()]
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

#' Formula interface to geom_linerange() and geom_pointrange()
#'
#' @inherit gf_ribbon
#' @inherit ggplot2::geom_linerange description references
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`
#' @seealso [ggplot2::geom_linerange()]
#' @export
#' @examples
#' gf_linerange()
#'
#' if (require(mosaicData)) {
#' gf_ribbon(low_temp + high_temp ~ date, data = Weather,
#'           fill = ~ city, alpha = 0.4) %>%
#'    gf_theme(theme = theme_minimal())
#' gf_linerange(
#'   low_temp + high_temp ~ date | city ~ ., data = Weather,
#'   color = ~ ((low_temp + high_temp) / 2) ) %>%
#'   gf_refine(scale_colour_gradientn(colors = rev(rainbow(5)))) %>%
#'   gf_labs(color = "mid-temp")
#'
#' gf_ribbon(low_temp + high_temp ~ date | city ~ ., data = Weather)
#'
#' # Chaining in the data
#' Weather %>%
#'   gf_ribbon(low_temp + high_temp ~ date, alpha = 0.4) %>%
#'   gf_facet_grid(city ~ .)
#' }
#'
gf_linerange <-
  layer_factory(
    geom = "linerange",
    aes_form = ymin + ymax ~ x,
    extras = alist( alpha = , color = , group = , linetype = , size = )
  )

#' @rdname gf_linerange
#' @seealso [ggplot2::geom_pointrange()]
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
#'  # width is defined differently for gf_boxplot() and gf_jitter()
#'  #   * for gf_boxplot() it is the full width of the box.
#'  #   * for gf_jitter() it is half that -- the maximum amount added or subtracted.
#'  gf_boxplot(age ~ substance, data = HELPrct, width = 0.4) %>%
#'    gf_jitter(width = 0.4, height = 0, color = "skyblue", alpha = 0.5)
#'  gf_boxplot(age ~ substance, data = HELPrct, width = 0.4) %>%
#'    gf_jitter(width = 0.2, height = 0, color = "skyblue", alpha = 0.5)
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
#' @inherit gf_point
#' @inherit ggplot2::geom_crossbar description references
#'
#' @param gformula A formula with shape `y + ymin + ymax ~ x`.
#'   Faceting can be achieved by including `|` in the formula.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`, `fatten`
#' @seealso [ggplot2::geom_crossbar()]
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
#' @inherit gf_ribbon
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`
#' @seealso [ggplot2::geom_errorbar()]
#' @section Note:
#'   There is discrepancy between the information required for `gf_errorbar()`
#'   and `gf_errobarh()`.  It expected that this will change in a future release
#'   of `ggplot2`.
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
#' @inherit gf_point
#' @inherit ggplot2::geom_errorbarh description references
#' @param gformula A formula with shape `y ~ x + xmin + xmax`.
#'   Faceting can be achieved by including `|` in the formula.
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `group`, `linetype`, `size`
#' @section Note:
#'   There is discrepancy between the information required for `gf_errorbar()`
#'   and `gf_errobarh()`.  It expected that this will change in a future release
#'   of `ggplot2`.
#'
#' @seealso [ggplot2::geom_errorbarh()]
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
#' @inherit gf_point
#' @inherit ggplot2::geom_rect description references
#'
#' @param gformula A formula with shape `ymin + ymax ~ xmin + xmax`.
#'   Faceting can be achieved by including `|` in the formula.
#'
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `alpha`, `color`, `fill`, `group`, `linetype`, `size`
#' @seealso [ggplot2::geom_rect()]
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
#' @inheritParams gf_point
#'
#' @param gformula Must be `NULL`.
#' @param ... Additional arguments.  Typically these are
#'   (a) ggplot2 aesthetics to be set with `attribute = value`,
#'   (b) ggplot2 aesthetics to be mapped with `attribute = ~expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'   Available attributes include
#'   `slope`, `intercept`
#' @rdname gf_lines
#' @seealso [ggplot2::geom_abline()],
#'   [ggplot2::geom_vline()],
#'   [ggplot2::geom_hline()]
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
#'
#' # We can set the color of the guidelines while mapping color in other layers
#' gf_point(mpg ~ hp, color = ~cyl, size = ~wt, data = mtcars) %>%
#'   gf_hline(color = "navy", yintercept = c(20, 25), data = NA) %>%
#'   gf_vline(color = "brown", xintercept = c(200, 300), data = NA)
#'
#' # If we want to map the color of the guidelines, it must work with the
#' # scale of the other colors in the plot.
#' gf_point(mpg ~ hp, size = ~wt, data = mtcars, alpha = 0.3) %>%
#'   gf_hline(color = ~"horizontal", yintercept = ~c(20, 25), data = NA) %>%
#'   gf_vline(color = ~"vertical", xintercept = ~c(100, 200, 300), data = NA)
#' gf_point(mpg ~ hp, size = ~wt, color = ~ factor(cyl), data = mtcars, alpha = 0.3) %>%
#'   gf_hline(color = "orange", yintercept = 20, data = NA) %>%
#'   gf_vline(color = ~c("4", "6", "8"), xintercept = c(80, 120, 250), data = NA) %>%
#' # reversing the layers requires using inherit = FALSE
#' gf_hline(color = "orange", yintercept = ~20, data = NA) %>%
#'   gf_vline(color = ~c("4", "6", "8"), xintercept = c(80, 120, 250), data = NA) %>%
#'   gf_point(mpg ~ hp, size = ~wt, color = ~ factor(cyl), data = mtcars, alpha = 0.3,
#'     inherit = FALSE)
#'
gf_abline <-
  layer_factory(
    geom = "abline",
    aes_form = NULL,
    extras = alist( slope =, intercept = ),
    inherit.aes = FALSE,
    data = NA,
    layer_fun = ggplot2::geom_abline
  )

#' @rdname gf_lines
#' @export
gf_hline <-
  layer_factory(
    geom = "hline",
    aes_form = NULL,
    extras = alist(yintercept = ),
    inherit.aes = FALSE,
    data = NA,
    layer_fun = ggplot2::geom_hline
  )

#' @rdname gf_lines
#' @export
gf_vline <-
  layer_factory(
    geom = "vline",
    aes_form = NULL,
    extras = alist(xintercept = ),
    inherit.aes = FALSE,
    data = NA,
    layer_fun = ggplot2::geom_vline
    )

#' @rdname gf_lines
#' @param coef A numeric vector of coefficients.
#' @param model A model from which to extract coefficients.
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
#' @inheritParams gf_point
#' @param ... Other arguments such as `position="dodge"`.
#' @param fun A function.
#' @rdname gf_function
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

#' @rdname gf_function
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

#' Plot density function based on fit to data
#'
#' `MASS::fitdistr()` is used to fit coefficients of a specified family of
#' distributions and the resulting density curve is displayed.
#'
#' @inherit gf_point
#' @param object When chaining, this holds an object produced in the earlier portions
#' of the chain.  Most users can safely ignore this argument. See examples.
#' @param gformula A formula with shape ` ~ x` used to specify the data
#'   to be fit to a family of distributions.
#' @param data A data frame containing the variable to be fitted.
#' @param environment An environment in which to look for variables not found in `data`.
#' @param ... Additional arguments
####  to [MASS::fitdistr()].
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
#' @seealso [mosaicCore::fit_distr_fun()]
#' @export
#' @examples
#' if (require(mosaicData)) {
#'   gf_fitdistr( ~ length, data = KidsFeet, inherit = FALSE) %>%
#'     gf_dhistogram( ~ length, data = KidsFeet, binwidth = 0.5, alpha = 0.25)
#'
#'   gf_dhistogram( ~ length, data = KidsFeet, binwidth = 0.5, alpha = 0.25) %>%
#'     gf_fitdistr()
#' }

gf_fitdistr <-
  layer_factory(
    geom = "path", stat = "fitdistr", position = "identity",
    aes_form = list(~ x), inherit.aes = "x",
    extras = alist(dist = dnorm, start = NULL, alpha = , color = , fill = , group = , linetype = , size = ),
    note = "dist should be a density function like dnorm or dgamma"
  )
