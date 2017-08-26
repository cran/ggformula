utils::globalVariables("role")

#' @importFrom utils head tail
#' @importFrom tibble data_frame
#' @importFrom stringr str_split str_match
#' @importFrom stats as.formula
#' @importFrom utils modifyList
#' @importFrom rlang is_character exprs f_rhs is_formula is_null enquo
#' @import ggplot2

# covert y ~ 1 into ~ 7
# convert y ~ 1 | a into ~ y | a
# convert y ~ 1 | a ~ b into ~ y | a ~ b
# convert y ~ 1 | ~ a into ~ y | ~ a

# This is clunky because | doen't have the right precedence for the intended interpretation of
# the formula.

standard_formula <- function(formula) {
  if (length(formula) == 3L && formula[[3]] == 1) {
    formula[[3]] <- formula[[2]]
    # can remove either slot 2 or slot 3 here to get 1-sided formula
    formula[[2]] <- NULL
  } else if (length(formula) == 3L &&
             length(formula[[3]])  == 3L &&
             formula[[3]][[1]] == as.name("|") &&
             formula[[3]][[2]] == 1L) {
    formula[[3]][[2]] <- formula[[2]]
    formula[[2]] <- NULL
  } else if (length(formula) == 3L && rlang::is_formula(formula[[2]])) {
    formula[[2]] <- standard_formula(formula[[2]])
  }
  formula
}

# The actual graphing functions are created dynamically.
#  See the functions at the bottom of this file

# These are unexported helper functions to create the gf_ functions. The gf_ functions
# themselves are at the end of this file....

# traverse a formula and return a nested list of "nodes"
# stop traversal if we encouter a binary operator in stop_binops
formula_slots <- function(x, stop_binops = c(":", "::")) {
  if (length(x) == 2L && deparse(x[[1]]) == "~") {
    formula_slots(x[[2]])
  } else if (length(x) == 3L && deparse(x[[1]]) == "~") {
    list(formula_slots(x[[2]]), formula_slots(x[[3]]))
  } else if (length(x) > 1 && is.name(x[[1]]) && !deparse(x[[1]]) %in% c("+", "|")) {
    list(x)
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
  if(is_null(x)) return("NULL")
  x <- if (rlang::is_character(x)) paste0('"', x, '"') else x
  x <- if (is.name(x)) as.character(x) else x
  x <- if (rlang::is_character(x)) x else format(x)
  x
}


.default_value <- function(x) {
  sapply(
    x,
    function(x) if (is.symbol(x))  "" else paste0(" = ", .quotify(x))
  )
}

aes_from_qdots <- function(qdots, mapping = aes()) {
  if (length(qdots) > 0) {
    # proceed backwards through list so that removing items doesn't mess up indexing
    for (i in length(qdots):1L) {
      if (rlang::is_formula(f_rhs(qdots[[i]])) && length(rlang::f_rhs(qdots[[i]])) == 2L) {
        mapping[[names(qdots)[i]]] <- f_rhs(qdots[[i]])[[2]]
        qdots[[i]] <- NULL
      }
    }
  }
  list(
    mapping = do.call(aes, mapping),
    qdots = qdots
  )
}

emit_help <- function(function_name, aes_form, extras = list(), note = NULL,
                      geom, stat = "identity", position = "identity"){
  message_text <- ""
  if (any(sapply(aes_form, is.null))) {
    message_text <-
      paste(message_text, function_name, "() does not require a formula.")
  } else {
    message_text <-
      paste(message_text, function_name, "() uses \n    * a formula with shape ",
            paste(sapply(aes_form, format), collapse = " or "), ".")
  }
  if (is.character(geom))
    message_text <- paste(message_text, "\n    * geom: ", geom)
  if (is.character(stat) && stat != "identity")
    message_text <- paste(message_text, "\n    * stat: ", stat)
  if (is.character(position) && position != "identity")
    message_text <- paste(message_text, "\n    * stat: ", position)

  if(length(extras) > 0) {
    message_text <-
      paste(
        message_text,
        "\n    * attributes: ",
        paste(
          strwrap(
            width = options("width")[[1]] - 20, simplify = TRUE,
            paste(
              names(extras), .default_value(extras),
              collapse = ", ", sep = ""),
            initial = "",
            prefix = "\n                   "
          ),
        collapse = "", sep = ""
        )
      )
  }
  if (!is.null(note))
    message_text <- paste(message_text, "\nNote: ", note)
  message_text <- paste0(message_text, "\n\nFor more information, try ?", function_name)

  message(message_text)

  return(invisible(NULL))
}

# produces a gf_ function wrapping a particular geom.
# use gf_roxy to create boilerplate roxygen documentation to match (and then edit by hand as needed).

layer_factory <- function(
  geom = "point",
  position = "identity",
  stat = "identity",
  aes_form = y ~ x,
  extras = alist(),
  note = NULL,
  aesthetics = aes(),
  inherit.aes = TRUE,
  data = NULL
) {
  # the formals of this will be modified below
  # the formals included here help avoid CRAN warnings
  res <- function(show.legend , function_name, inherit, ...) {

    dots <- list(...)
    function_name <- as.character(match.call()[1])

    if (is.null(show.help)) {
      show.help <- is.null(object) && is.null(gformula) && length(dots) == 0L
    }

    # make sure we have a list of formulas here
    if (!is.list(aes_form)) aes_form <- list(aes_form)

    if (show.help) {
      emit_help(function_name = function_name,
                aes_form, extras, note,
                geom = geom, stat = stat, position = position)
      return(invisible(NULL))
    }

    if (is.character(position)) {
      position_fun <- paste0("position_", position)
      pdots <- dots[intersect(names(dots), names(formals(position_fun)))]
      position <- do.call(position_fun, pdots)
    }

    if (inherits(object, "formula")) {
      gformula <- object
      object <- NULL
    }

    if (inherits(object, "data.frame")) {
      data <- object
      object <- NULL
    }

    # convert y ~ 1 into ~ y if a 1-sided formula is an option
    if (any(sapply(aes_form, function(f) length(f) == 2L))) {
      gformula <- standard_formula(gformula)
    }

    # find matching formula shape
    fmatches <- formula_match(gformula, aes_form = aes_form)


    if (! any(fmatches)) {
      if (inherits(object, "gg") && inherit) {
        aes_form = NULL
      } else {
        stop("Invalid formula type for ", function_name, ".", call. = FALSE)
      }
    } else {
      aes_form <- aes_form[[which.max(fmatches)]]
    }

    if (length(dots) > 0) {
      # proceed backwards through list so that removing items doesn't mess up indexing
      for (i in length(dots):1L) {
        if (is_formula(dots[[i]]) && length(dots[[i]]) == 2L) {
          aesthetics[[names(dots)[i]]] <- dots[[i]][[2]]
          dots[[i]] <- NULL
        }
      }
    }

    if (length(extras) > 0) {
      extras <- extras[sapply(extras, function(x) !is.symbol(x))]
    }

    if (length(dots) > 0) {
      extras <- modifyList(extras, dots) # lapply(qdots, rlang::f_rhs))
    }

    add <- inherits(object, c("gg", "ggplot"))

    ingredients <-
      gf_ingredients(
        formula = gformula, data = data,
        gg_object = object,
        extras = extras,
        aes_form = aes_form,
        aesthetics = aesthetics)

    layer_args <-
      list(
        geom = geom, stat = stat,
        data = ingredients[["data"]],
        mapping = ingredients[["mapping"]],
        position = position,
        params = ingredients[["params"]],
        check.aes = TRUE, check.param = FALSE,
        show.legend = show.legend,
        inherit.aes = inherit
      )
    # print(layer_args[c("mapping", "setting", "params", "data")])
    new_layer <- do.call(ggplot2::layer, layer_args)
    # message(
    #   do.call(call, c(list("layer"), layer_args))
    # )
    if (is.null(ingredients[["facet"]])) {
      if (add)
        return(object + new_layer)
      else
        return(ggplot(data = ingredients$data, mapping = ingredients[["mapping"]]) + new_layer)
    } else {
      if (add)
        return(object + new_layer + ingredients[["facet"]])
      else
        return(ggplot(data = ingredients$data, mapping = ingredients[["mapping"]]) +
                 new_layer + ingredients[["facet"]])
    }
  }
  formals(res) <-
    c(
      list(
        object = NULL, gformula = NULL, data = data,
        geom = geom, stat = stat, position = position,
        show.legend = NA,
        show.help = NULL,
        inherit = inherit.aes
      ),
      alist(... = )
    )
  # assign("inherit.aes", inherit.aes, environment(res))
  res
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

gf_ingredients <-
  function(formula = NULL, data = NULL,
           extras = list(),
           aes_form = y ~ x,
           aesthetics = aes(),
           gg_object = NULL) {

  # split A | B into formula <- A; condition <- B
  fs <- formula_split(formula)

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

  aes_df <-
    rbind(
      formula_to_df(fs[["formula"]], var_names, aes_form = aes_form),
      data.frame(
        role = names(aesthetics),
        expr = sapply(aesthetics, deparse),
        map  = rep(TRUE, length(aesthetics)),
        stringsAsFactors = FALSE
      )
    )


  mapped_list <- as.list(aes_df[["expr"]][aes_df$map])
  names(mapped_list) <- aes_df[["role"]][aes_df$map]

  set_list <- as.list(aes_df[["expr"]][!aes_df$map])
  names(set_list) <- aes_df[["role"]][!aes_df$map]
  set_list <- modifyList(extras, set_list)

  res <-
    list(
      data = data,
      mapping = modifyList(do.call(aes, aesthetics), do.call(aes_string, mapped_list)),
      setting = set_list,
      facet =
        if (is.null(fs[["condition"]])){
          NULL
        } else {
          do.call(fs[["facet_type"]], list(facets = fs[["condition"]]))
        },
      params = modifyList(set_list, extras)
    )
  if (identical(data, NA)) {
    res$data <-
      do.call(
        data.frame,
        c(res[["mapping"]], res[["setting"]], list(stringsAsFactors = FALSE)))
    res$params[names(res$mapping)] <- NULL  # remove mapped attributes
    aes_list <- as.list(intersect(names(res$data), names(res$mapping)))
    names(aes_list) <- aes_list
    res$mapping <- do.call(aes_string, aes_list)
    res$setting <- as.list(res$data)[names(res$setting)]
    res$params[names(res$setting)] <- res$setting
  }
  res
}


formula_shape <- function(x) {
  if (length(x) < 2) return(0)
  arity <- length(x) - 1
  if (as.character(x[[1]]) %in% c("(")){
    return(0)
  }
  if (as.character(x[[1]]) %in% c(":", "(")){
    return(0)   # was -1 when supporting attribute:value
  }
  # stop if we hit a name that isn't + or ~
  if (is.name(x[[1]]) && ! as.character(x[[1]]) %in% c("+", "~")) {
    return(0)
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
  stop("Bug: problems determining formula shape.")

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

  # trim leading/trailing blanks
  parts <- gsub("^\\s+|\\s+$", "", parts)

  # split into pairs/nonpairs
  pairs <- parts[grepl(":+", parts)]
  nonpairs <- parts[ ! grepl(":+", parts)]

  ## !! turning off support for attribute:value !!
  pairs <- parts[FALSE]
  nonpairs <- parts[TRUE]

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
  S <- paste0("", prefix,
              ifelse(nchar(prefix) > 0, ", ", ""),
              aes_substr,
              with(subset(formula_df, ! formula_df$map),
                   paste(role, expr, sep = " = ", collapse = ", ")),
              "")
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
