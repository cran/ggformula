utils::globalVariables("role")

#' @importFrom utils head tail
#' @importFrom tibble data_frame
#' @importFrom stringr str_split str_match
#' @importFrom stats as.formula
#' @importFrom utils modifyList
#' @importFrom rlang is_character exprs f_rhs is_formula is_null enquo
#' @import ggplot2

# The actual graphing functions are created dynamically.
#  See the functions at the bottom of this file

# These are unexported helper functions to create the gf_ functions. The gf_ functions
# themselves are at the end of this file....

# traverse a formula and return a nested list of "nodes"
# stop traversal if we encouter a binary operator in stop_binops
formula_slots <- function(x, stop_binops = c(":", "::")) {
  if (length(x) == 2L && deparse(x[[1]]) == "~") {
    formula_slots(x[[2]])
  } else if (length(x) == 3L && deparse(x[[1]]) %in% stop_binops) {
    list(x)
  } else if (length(x) <= 2L) {
    list(x)
  } else {
    list(formula_slots(x[[2]]), formula_slots(x[[3]]))
  }
}

# add quotes to character elements of list x and returns a vector of character
.quotify <- function(x) {
  if(is_null(x)) return(x)
  x <- if (rlang::is_character(x)) paste0('"', x, '"') else x
  x <- if (is.name(x)) as.character(x) else x
  x <- if (rlang::is_character(x)) x else format(x)
  x
}

.default_value <- function(x) {
  sapply(x,
         function(x) ifelse (is.symbol(x), "", paste0(" = ", lapply(x, .quotify)))
  )
}

.add_arg_list_to_function_string <- function(S, extras) {
  empty <- grepl("\\(\\s?\\)$", S)
  res <- if (length(extras) == 0 ) {
    S
  } else {
    more <- paste0(names(extras), " = ", extras, collapse = ", ")
    S <- gsub("\\)$", "", S)
    paste0(S, ifelse(empty, "", ", "), more, ")")
  }
  res
}

# produces a gf_ function wrapping a particular geom.
# use gf_roxy to create boilerplate roxygen documentation to match (and then edit by hand as needed).

gf_factory <- function(
  type,
  aes_form = y ~ x,
  extras = alist(),
  note = NULL,
  function_name = paste0("gf_", type)
) {
  function(object = NULL, gformula = NULL,
           data = NULL, geom = type, verbose = FALSE,
           add = inherits(object, c("gg", "ggplot")),
           ..., position = NULL, # stat = NULL,
           show.help = NULL) {

    qdots <- rlang::quos(...)
    # dots <- list(...)
    if (is.null(show.help)) {
      show.help <- is.null(object) && is.null(gformula) && length(qdots) == 0L
    }

    if (!is.list(aes_form)) aes_form <- list(aes_form)
    if (show.help) {
      fun <- match.call()[1]
      if (any(sapply(aes_form, is.null))) {
        message(fun, " does not require a formula.")
      } else {
        message(fun, " uses a formula with shape ", paste(sapply(aes_form, format), collapse = " or "), ".")
      }
      if(length(extras) > 0) {
        message("Additional attributes include: ",
                strwrap(
                  paste(names(extras), .default_value(extras), collapse = ", ", sep = ""),
                  prefix = "\n    "
                  )
        )
      }
      if (!is.null(note)) message(note)
      message("For more information, try ?", function_name)
      return(invisible(NULL))
    }

    aesthetics <- list()
    data_name <- deparse(substitute(data))
    object_name <- deparse(substitute(object))

    if (inherits(object, "formula")) {
      gformula <- object
      object <- NULL
    }

    fmatches <-  formula_match(gformula, aes_form = aes_form)
    if (! any(fmatches)) {
      stop("Invalid formula type for ", function_name, ".", call. = FALSE)
    }
    aes_form <- aes_form[[which.max(fmatches)]]

    if (length(qdots) > 0) {
      # proceed backwards through list so that removing items doesn't mess up indexing
      for (i in length(qdots):1L) {
        if (is_formula(f_rhs(qdots[[i]])) && length(f_rhs(qdots[[i]])) == 2L) {
          aesthetics[[names(qdots)[i]]] <- f_rhs(qdots[[i]])[[2]]
          qdots[[i]] <- NULL
        }
      }
    }

    qposition <- rlang::enquo(position)
    if (!is.null(position)) {
      qdots[["position"]] <- qposition
    }

    # qstat <- rlang::enquo(stat)
    # if (!is.null(stat)) {
    #   qdots[["stat"]] <- qstat
    # }


    if (length(extras) > 0) {
      extras <- extras[sapply(extras, function(x) !is.symbol(x))]
    }

    if (length(qdots) > 0) {
      extras <- modifyList(extras, lapply(qdots, rlang::f_rhs))
    }

    extras <- lapply(extras, .quotify)

    dot_eval <- FALSE
    if (inherits(object, "data.frame")) {
      data <- object
      data_name <- object_name
      object <- NULL
      dot_eval <- TRUE
    }

    if (!inherits(object, c("gg", "ggplot"))) {
      add <- FALSE  # can't add if we don't have a plot to add to
    }

    gg_string <- gf_master(formula = gformula, data = data,
                           geom = geom, gg_object = object,
                           add = add, extras = extras,
                           aes_form = aes_form,
                           data_name = data_name,
                           aesthetics = aesthetics)

    if (verbose) cat(gsub("geom", "\n  geom", gg_string$text, fixed = TRUE), "\n")

    if (dot_eval) {
      gg_string <- gf_master(formula = gformula, data = data,
                             geom = geom, gg_object = object,
                             add = TRUE, extras = extras,
                             aes_form = aes_form,
                             data_name = NULL,
                             aesthetics = aesthetics)
      # if (verbose) cat(gsub("geom", "\n  geom", gg_string$text, fixed = TRUE), "\n")
    }

    if (dot_eval) {
      P <- ggplot(data) + eval(parse(text = gg_string$main))
      if (length(gg_string$facet) > 0) {
        P <- P + eval(parse(text = gg_string$facet))
      }
    } else {
      P <- eval(parse(text = gg_string$text), environment(gformula))
    }
    if (add)  #  don't need this part: && inherits(object, c("gg", "ggplot")))
      return(object + P)
    else
      return(P)
  }
}

formula_split <- function(formula) {
  # split A | B into formula <- A; condition <- B
  fs <-
    stringr::str_split(deparse(formula), "\\|")[[1]]
  # try to split, else leave formula unchanged and set condition to NULL
  if ( (length(fs) != 2) ||
       ! tryCatch({
         formula_string <- fs[1]
         condition_string <- fs[2]
         if (! grepl("~", condition_string)) {
           condition_string <- paste0("~", condition_string)
           condition <- as.formula(condition_string, env = environment(formula))
           facet_type <- "facet_wrap"
         } else {
           condition <- as.formula(condition_string, env = environment(formula))
           facet_type <- "facet_grid"
         }
         formula <- as.formula(formula_string, env = environment(formula))
         TRUE
       }, error = function(e) {warning(e); FALSE}
       )
  ) {
    condition <- NULL
    facet_type <- "none"
  }
  list(formula = formula, condition = condition, facet_type = facet_type)
}

gf_master <- function(formula = NULL, data = NULL,
                      add = FALSE,
                      data_name = NULL,
                      geom = "geom_point", extras = list(),
                      gg_object = NULL,
                      aes_form = y ~ x,
                      aesthetics = list()) {

  data_string <-
    if (is.null(data)) ""
  else {
    if (is.null(data_name)) "" else paste("data =", data_name)
  }

  # if ( (! add) && is.null(data) )
  #   warning("No data supplied.  Was that intentional?")

  # split A | B into formula <- A; condition <- B
  fs <- formula_split(formula)
  formula <- fs[["formula"]]
  condition <- fs[["condition"]]
  facet_type <- fs[["facet_type"]]

  var_names <-
    if (is.null(data)) {
      if (is.null(gg_object)) {
        character(0)
      } else {
        names(gg_object$data)
      }
    } else {
      names(data)
    }

  # arguments for the frame or, if add == TRUE, for the geom
  # main_arguments <-
  #   formula_to_aesthetics(formula, var_names,
  #                         prefix = data_string,
  #                         aes_form = aes_form)

  from_formula <-
    rbind(
      formula_to_df(formula, var_names, aes_form = aes_form),
      data.frame(
        role = names(aesthetics),
        expr = sapply(aesthetics, deparse),
        map  = rep(TRUE, length(aesthetics))
      )
    )

  main_arguments <-
    df_to_aesthetics(
      from_formula, var_names,
      prefix = if (add) data_string else "")

  gg_string <-
    .add_arg_list_to_function_string(
      paste0("geom_", geom, main_arguments),
      extras)

  if (! add) gg_string <-   # need ggplot() call, too
    paste0("ggplot(", data_string, ") + ", gg_string)

  res <-
  list(
    main = gg_string,
    facet =
      if (is.null(condition)) ""  else
        paste0(facet_type, "(", deparse(condition), ")")
    )
  res$text <-
    if (is.null(condition)) gg_string  else
      paste0(gg_string, " + ", res$facet)
  res
}

formula_shape <- function(x) {
  if (length(x) < 2) return(0)
  arity <- length(x) - 1
  if (as.character(x[[1]]) %in% c("(")){
    return(0)
  }
  if (as.character(x[[1]]) %in% c(":", "(")){
    return(-1)
  }

  # if (as.character(x[[1]]) %in% c("|")){
  #   return(formula_shape(x[[2]]))
  # }

  if (arity == 1L) {
    right_shape <- formula_shape(x[[2]])
    arity <- arity - (right_shape[1] < 0)
    if (arity == 0) return(arity)
    return( right_shape )
  }
  if (arity == 2L) {
    right_shape <- formula_shape(x[[3]])
    left_shape <- formula_shape(x[[2]])
    if (left_shape[1] < 0 && right_shape < 0) { return(0) }
    if (left_shape[1] < 0) {
      if (right_shape[1] == 1L) return(right_shape[-1])
      return(right_shape)
    }
    if (right_shape[1] < 0) {
      if (left_shape[1] == 1L) return(left_shape[-1])
      return(left_shape)
    }
    return( c(2, left_shape, right_shape) )
  }
  stop("problems here.")

  c(length(x) - 1, unlist(sapply(x[-1], formula_shape)))
  # list(length(x) - 1, lapply(x[-1], formula_shape))
}

formula_match <- function(formula, aes_form = y ~ x) {
  if (!is.list(aes_form)) {
    aes_form <- list(aes_form)
  }
  user_shape <- formula_shape(formula_split(formula)$formula)
  shapes <- lapply(aes_form, formula_shape)
  sapply(shapes, function(s) identical(s, user_shape))
}

formula_to_df <- function(formula = NULL, data_names = character(0),
                          aes_form = y ~ x) {
  if (is.null(formula))
    return(data.frame(role = character(0),
                      expr = character(0),
                      map = logical(0)))
  parts <- formula_slots(formula) %>% rapply(deparse, how = "replace") %>% unlist()
  aes_names <- formula_slots(aes_form) %>% rapply(deparse, how = "replace") %>% unlist()

  # trim leading blanks
  parts <- gsub("^\\s+|\\s+$", "", parts)

  # split into pairs/nonpairs
  pairs <- parts[grepl(":+", parts)]
  nonpairs <- parts[ ! grepl(":+", parts)]

  pair_list <- list()
  mapped_pairs <- character(0)
  for (pair in pairs) {
    this_pair <- stringr::str_split(pair, ":+", n = 2)[[1]]
    pair_list[this_pair[1]] <- this_pair[2]
    if (stringr::str_match(pair, ":+") == "::")
      mapped_pairs <- c(mapped_pairs, this_pair[1])
  }

  nonpair_list <- nonpairs
  # remove items specified explicitly
  aes_names <- setdiff(all.vars(aes_form), names(pair_list))
  names(nonpair_list) <- head(aes_names, length(nonpair_list))

  if (length(nonpair_list) > length(aes_names)) {
    stop("Formula too large.  I'm looking for ", format(aes_form),
         call. = FALSE)
  }
  if (length(nonpair_list) < length(aes_names)) {
    stop("Formula too small.  I'm looking for ", format(aes_form),
         call. = FALSE)
  }

  res <- c(nonpair_list, pair_list)

  res <-
    tibble::data_frame(
      role = names(res),
      expr = unlist(res),
      map = unlist(res) %in% c(data_names) | role %in% aes_names | role %in% mapped_pairs)
  row.names(res) <- NULL
  res
}

df_to_aesthetics <- function(formula_df, data_names = NULL, prefix = "") {
  aes_substr <-
    if (is.null(data_names) || nrow(formula_df) == 0) {
      ""
    } else {
      paste0("aes(",
             with(subset(formula_df, formula_df$map),
                  paste(role, expr, sep = " = ", collapse = ", ")),
             ")",
             ifelse(any( ! formula_df$map), ", ", "") # prepare for more args
      )
    }
  S <- paste0("(", prefix,
              ifelse(nchar(prefix) > 0, ", ", ""),
              aes_substr,
              with(subset(formula_df, ! formula_df$map),
                   paste(role, expr, sep = " = ", collapse = ", ")),
              ")")
  S
}


formula_to_aesthetics <- function(formula,
                                  data_names = NULL,
                                  prefix = "",
                                  aes_form = y ~ x) {
  df <- formula_to_df(formula, data_names, aes_form = aes_form)
  df_to_aesthetics(df, data_names = data_names, prefix = prefix)
}

# pull out the pairs from a formula like color::red + alpha:0.5
# return them as a named list
pairs_in_formula <- function(formula) {
  fc <- as.character(formula)
  parts <- unlist(strsplit(fc, "+", fixed = TRUE))
  # trim leading and trailing blanks
  parts <- gsub("^\\s+|\\s+$", "", parts)
  # identify the pairs
  pairs <- parts[grep(":+", parts)]
  xy <- parts[ ! grepl(":", parts)][-1] # logic for x:, y: explicit
  res <- list()
  for (pair in pairs) {
    this_pair <- stringr::str_split(pair, ":+", n = 2)
    res[this_pair[1] ] <- this_pair[2]
  }
  # more logic for x:, y: explicit.
  stop("Haven't yet updated logic in frame_string. See comment.")
  # BUT ... not yet replaced explicit "x" and "y" arguments in
  # frame_string()
  if (length(xy) == 2) {
    if ("y" %in% names(res))
      warning("duplicate specification of y aesthetic")
    else res["y"] <- xy[1]

    if ("x" %in% names(res))
      warning("duplicate specification of x aesthetic")
    else res["x"] <- xy[2]
  } else if (length(xy) == 1) {
    if ("y" %in% names(res)) {
      if ("x" %in% names(res))
        warning("duplicate specification of x aesthetic")
      else res["x"] <- xy
    } else if ("x" %in% names(res)) {
      if ("y" %in% names(res))
        warning("duplicate specification of y aesthetic")
      else res["y"] <- xy
    }
  }

  res
}
