#' Calculate basic statistics on a quantitative variable
#'
#' Creates a data frame of statistics calculated on one variable, possibly for each
#' group formed by combinations of additional variables.
#' The resulting data frame has one column
#' for each of the statistics requested as well as columns for any grouping variables.
#'
#' @param formula A formula indicating which variables are to be used. See details.
#' @param data A data frame or list containing the variables.
#' @param ... Functions used to compute the statistics.  If this is empty,
#'   \code{\link[mosaic]{favstats}()} is used.  Functions used must accept a vector
#'   of values and return either a (possibly named) single value,
#'   a (possibly named) vector of values, or a data frame with one row.
#' @param drop A logical indicating whether combinations of the grouping
#'   variables that do not occur in \code{data} should be dropped from the
#'   result.
#' @param fargs Arguments passed to the functions in \code{...}.
#' @param long_names A logical indciting whether the default names should include the name
#'   of the variable being summarized as well as the summarizing function name in the default
#'   case when names are not derived from the names of the returned object or
#'   an argument name.
#' @param nice_names A logical indicating whether \code{\link{make.names}()} should be
#'   used to force names of the returned data frame to by syntactically valid.
#'
#' @details
#' Use a one-sided formula to compute summary statistics for the left hand side
#' expression over the entire data.
#' Use a two-sided formula to compute summary statistics for the left hand expression
#' for each combination of levels of the expressions ocurring on the right hand side.
#' This is most useful when the left hand side is quantitative and each expression
#' on the right hand side has relatively few unique values.  A function like
#' \code{\link{ntiles}()} is often useful to create a few groups of roughly equal size
#' determined by ranges of a quantitative variable.  See the examples.
#'
#' Note that unlike \code{dplyr::\link[dplyr]{summarise}()}, `df_stats()` ignores
#' any grouping defined in \code{data} if \code{data} is a grouped \code{tibble}.
#'
#' Names of columns in the resulting data frame are determined as follows.  For named
#' arguments in \code{...}, the argument name is used.  For unnamed arguments, if the
#' statistic function returns a result with names, those names are used.  Else, a name is
#' computed from the expression in \code{...} and the name of the variable being summarized.
#' For functions that produce multiple
#' outputs without names, consecutive integers are appended to the names.
#' See the examples.
#' @return A data frame.
#'
#' @examples
#' df_stats( ~ hp, data = mtcars)
#' df_stats( ~ hp, data = mtcars, mean, median)
#' df_stats( hp ~ cyl, data = mtcars, mean, median, range)
#' # magrittr style piping is also supported
#' mtcars %>% df_stats(hp ~ cyl)
#' gf_violin(hp ~ cyl, data = mtcars, group = ~ cyl) %>%
#'   gf_point(mean_hp ~ cyl, data = df_stats(hp ~ cyl, data = mtcars, mean))
#'
#' @export
#' @importFrom rlang eval_tidy exprs expr
#' @importFrom mosaic favstats
#' @importFrom stats model.frame aggregate
#'
df_stats <- function(formula, data, ..., drop = TRUE, fargs = list(), long_names = TRUE,
                     nice_names = FALSE) {
  # dots <- lazyeval::lazy_dots(...)
  dots <- rlang::exprs(...)

  if (length(dots) < 1) {
    dots <- list(rlang::expr(mosaic::favstats))
    names(dots) <- ""
  }

  if (inherits(formula, "data.frame") && inherits(data, "formula")) {
    # switched at birth. Likely because input is piped in
    tmp <- data
    data <- formula
    formula <- tmp
  }
  if ( ! inherits(formula, "formula")) stop("first arg must be a formula")
  if ( ! inherits(data, "data.frame")) stop("second arg must be a data.frame")
  MF <- model.frame(formula, data)

  res <-
    lapply(dots, function(f)
      aggregate(MF[, 1], by = MF[, -1, drop = FALSE],
                FUN = function(x) do.call(rlang::eval_tidy(f), c(list(x), fargs)),
                drop = drop
      )
    )
  d <- ncol(MF) - 1
  groups <- res[[1]][, 1:d, drop = FALSE]

  res <- lapply(res, function(x) as.matrix(x$x))
  dims <- sapply(res, ncol)

  res_names <- lapply(res, colnames)
  arg_names <- names(res)
  fun_names <- sapply(dots, function(x) deparse(x))
  if (long_names) {
    fun_names <- paste0(fun_names, "_", deparse(formula[[2]]))
  }
  final_names <-
    lapply(
      1:length(res),
      function(i) {
        if (! is.null(res_names[[i]]) ) return (res_names[[i]])
        if (arg_names[i] != "") {
          if (dims[i] < 2) {
            return(arg_names[i])
          }
          return(paste0(arg_names[i], 1:dims[i]))
        }
        if (dims[i] < 2) {
          return(fun_names[i])
        }
        return(paste0(fun_names[i], 1:dims[i]))
      }
    )

  res <- do.call(cbind, c(list(groups), res))
  names(res) <- c(names(res)[1:d], unlist(final_names))
  if (nice_names) names(res) <- base::make.names(names(res), unique = TRUE)
  return(res)
}

