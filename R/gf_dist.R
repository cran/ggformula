#' @importFrom mosaicCore named unnamed named_among
NA

#' Plot distributions
#'
#' Create a layer displaying a probability distribution.
#'
#' @importFrom stats approxfun ppoints
#' @importFrom rlang set_env
#' @param object a gg object.
#' @param dist A character string providing the name of a distribution.  Any
#'   distribution for which the functions with names formed by prepending
#'   "d", "p", or "q" to `dist` exist can be used.
#' @param xlim A numeric vector of length 2 providing lower and upper bounds for the
#'   portion of the distribution that will be displayed.  The default is to attempt
#'   to determine reasonable bounds using quantiles of the distribution.
#' @param ... additional arguments passed both to the distribution functions and
#'   to the layer.  Note: avoid possible ambiguities using `params`.
#' @param kind One of `"density"`, `"cdf"`, `"qq"`, `"qqstep"`, or `"histogram"`
#'   describing what kind of plot to create.
#' @param resolution An integer specifying the number of points to use for creating
#'  the plot.
#' @param params a list of parameters for the distribution.
#' @export
#' @examples
#' gf_histogram( ..density.. ~ rnorm(100), bins = 20) %>%
#'   gf_dist("norm", color = "red")
#'
#' gf_dist(dist = "norm", color = "red")
#'
#' gf_dist("norm", color = "red")
#' gf_dist("norm", color = "red", kind = "cdf")
#' gf_dist("norm", fill = "red", kind = "histogram")
#' gf_dist("norm", color = "red", kind = "qqstep", resolution = 25) %>%
#' gf_dist("norm", color = "black", kind = "qq", resolution = 25)
#' # This doesn't work well because size has two meanings
#' gf_dist("binom", size = 20, prob = 0.25)
#' # This is better
#' gf_dist("binom", params = list(size = 20, prob = 0.25))

gf_dist <- function(
  object = geom_blank(),
  dist, ...,
  xlim = NULL,
#  xmin = NULL, xmax = NULL,
  kind = c("density", "cdf", "qq", "qqstep", "histogram"),
  resolution = 5000L, params = NULL )
{

  if (is.character(object) && missing(dist)) {
    dist <- object
  }
  kind = match.arg(kind)
  ddist = paste('d', dist, sep='')
  qdist = paste('q', dist, sep='')
  pdist = paste('p', dist, sep='')

  original_call <- match.call()
  dots <- original_call
  dots[[1]] <- NULL
  unnamed_dots <- original_call
  named_dots <- original_call
  unnamed_dots[[1]] <- NULL
  named_dots[[1]] <- NULL
  groupless_dots <- original_call
  groupless_dots[[1]] <- NULL
  for (i in length(unnamed_dots):1) {
    if (names(unnamed_dots)[i] != "") {
      unnamed_dots[i] <- NULL
    } else {
      named_dots[i] <- NULL
    }
  }
  if (is.null(params)) {
    params <- original_call
    params[[1]] <- NULL
    for (item in names(formals()) ) {
      if (item %in% names(params)) params[[item]] <- NULL
    }
    dparams <- c(unnamed(params), named_among( params, names(formals(ddist))) )
    pparams <- c(unnamed(params), named_among( params, names(formals(pdist))) )
    qparams <- c(unnamed(params), named_among( params, names(formals(qdist))) )
  } else {
    dparams <- params
    pparams <- params
    qparams <- params
  }
  # attempting to make evaluation of these arguments more intuitive
  env <- parent.frame()
  dparams <- lapply(dparams, function(x) eval(x, env))
  pparams <- lapply(pparams, function(x) eval(x, env))
  qparams <- lapply(qparams, function(x) eval(x, env))

  sample_values = do.call(qdist, c(p=list(ppoints(resolution)), qparams))

  unique_values <- unique(sample_values)
  discrete <- length(unique_values) < length(sample_values)

  if (is.null(xlim)) {
    xlim_opts <- do.call(qdist, c(list(p = c(0, 0.001, 0.999, 1)), qparams))
    dxlim_opts <- diff(xlim_opts)
    xlim <- xlim_opts[2:3]
    if (dxlim_opts[1] < dxlim_opts[2]){xlim[1] <- xlim_opts[1]}
    if (dxlim_opts[3] < dxlim_opts[2]){xlim[2] <- xlim_opts[4]}
  }
  plim <- do.call(pdist, c(list(q = xlim), pparams))
  #  dpqrdist(dist, type = "p", q = xlim)

  if (! discrete) {
    unif_values = seq(
      do.call(qdist, c(list(p = plim[1]), qparams)),
      do.call(qdist, c(list(p = plim[2]), qparams)),
      length.out = resolution
    )
    fewer_values <- unif_values
  } else {
    fewer_values <- unique_values
  }

  if (kind=='cdf') {
    if (discrete) {
      step = min(diff(fewer_values))
      cdfx <- seq( min(fewer_values) -1.5 * step , max(fewer_values) + 1.5 * step, length.out = resolution)
      cdfx <- sort(unique( c(fewer_values, cdfx) ) )
      cdfy <- approxfun( fewer_values, do.call(pdist, c(list(q=fewer_values),pparams)), method='constant',
                         f=0, yleft=0, yright=1 ) (cdfx)
      PlotData <- data.frame(y = cdfy, x = cdfx)
    } else {
      cdfx <- unif_values
      cdfy <- do.call( pdist, c(list(q=unif_values), pparams) )
      PlotData <- data.frame(y = cdfy, x = cdfx)
    }
  }

  ydata <-
    switch(kind,
           density = do.call(ddist, c(list(x = fewer_values), dparams)),
           cdf = cdfy,
           qq = NULL,
           qqstep = NULL,
           histogram = do.call(ddist, c(list(x = sample_values), dparams))
     )

  # print(length(fewer_values))

  if (discrete) {
    switch(kind,
           density =
             gf_point(
               gf_segment(object, rlang::set_env(density + 0 ~ x + x, parent.frame()),
                          data = data.frame(density = ydata, x = fewer_values), ...),
               rlang::set_env(y ~ x, parent.frame()),
               data = data.frame(y = ydata, x = fewer_values), ...),
           cdf =
             gf_step(object, rlang::set_env(cumulative_density ~ x, parent.frame()),
                     data = data.frame(cumulative_density = ydata, x = cdfx), ...),
           qq =
             gf_qq(object, rlang::set_env(~ x, parent.frame()), data = data.frame(x = sample_values), ...),
           qqstep =
             gf_qqstep(object, rlang::set_env(~ x, parent.frame()), data = data.frame(x = sample_values), ...),
           histogram =
             gf_histogram(object, rlang::set_env(..density.. ~ x, parent.frame()),
                          data = data.frame(x = sample_values), ...)
    )
  } else {
    switch(kind,
           density =
             gf_line(object, rlang::set_env(density ~ x, parent.frame()),
                     data = data.frame(density = ydata, x = fewer_values), ...),
           cdf =
             gf_line(object, rlang::set_env(cumulative_density ~ x, parent.frame()),
                     data = data.frame(cumulative_density = ydata, x = cdfx), ...),
           qq =
             gf_qq(object, rlang::set_env(~ x, parent.frame()),
                   data = data.frame(x = sample_values), ...),
           qqstep =
             gf_qqstep(object, rlang::set_env(~ x, parent.frame()),
                       data = data.frame(x = sample_values), ...),
           histogram =
             gf_histogram(object, rlang::set_env(..density.. ~ x, parent.frame()),
                          data = data.frame(x = sample_values), ...)
    )
  }
}


