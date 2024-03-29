# to satisfy CRAN checks about conditional use of suggested packages
# code suggested by Lionel Henry

# Check for vdiffr before using it
#

wrapped_expect_doppelganger <- function(title, fig, path = NULL, ...) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    title, fig,
    # path = path,
    ...)
}
