# This allows us to use these functions without attaching the packages they come from.

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom mosaicCore makeFun
#' @export
mosaicCore::makeFun

# utils::globalVariables(c("stat", "value"))

#' @importFrom mosaicCore df_stats
# @importFrom tidyr gather
# @importFrom dplyr %>% bind_rows
# @importFrom rlang is_character f_rhs eval_tidy quos
# @importFrom stats as.formula na.exclude
#' @export
mosaicCore::df_stats

#' @importFrom mosaicCore na.warn
#' @export
mosaicCore::na.warn
