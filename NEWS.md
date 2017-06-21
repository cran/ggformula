# ggformula 0.4.0

Version 0.4.0 constitutes a stable beta release.  Changes to the API are still possible, but more 
likely future changes will focus on expansion of the suite of functions supplied, changes to
the internal implementation, and improved documentation.

  * Separated `ggformula` from `statisticalModeling`.
  * Added support for many more geoms.
  * Improved parsing of formulas.  This is still a bit clunky since the order of operations in
  R formulas does not match what we would prefer in this package.
  * `data` may now be an expression (like `data = KidsFeet %>% filter(sex == "G")`)
  * Added support for geoms that have different required aesthetics.
  * Added support for functions that allow more than one formula shape.  Example: `gf_histogram()`
  accepts formulas with shape ` ~ x` or `y ~ x`.  This makes it possible to create density 
  histograms with `gf_histogram()`.
  * Parntheses now halt parsing of formulas.  This allows for on-the-fly computations in formulas.  Typically these computed expressions must be within parentheses to avoid formula expansion.
  * Use `::` to indicate mapping aesthetics.  (`:` will autodetect, but only if the value
is the name of a variable in the data set.)  This should be considered experimental.
  * Added wrappers `gf_lims()`, `gf_labs()`, `gf_theme()`, `gf_facet_grid()`, `gf_facet_wrap()`
  * Added `gf_refine()` which can be used to pass by chaining anything that would have been "added" in ``ggplot2`
  * Expanded and improved vignette describing use of the package.
  * Added two tutorials.
  * Added "quick help" for plotting functions.
  * Added `gf_lm()`, which is `gf_smooth()` with `method = "lm"`
  * Added `gf_dens()` which is `gf_line()` with `stat = "density"`.



