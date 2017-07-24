
## Formula interface to ggplot2

`ggformula` introduces a family of graphics functions, `gf_point()`,
`gf_density()`, and so on, bring the formula interface to `ggplot()`. This
captures and extends the excellent simplicity of the `lattice`-graphics formula
interface, while providing the intuitive "add this component" capabilities of
`ggplot2`.

### Installation

Installations from CRAN are done in the usual way. The development version of
the package is here on GitHub. To install it, use the following commands in your
R system.

```r
# Install devtools if necessary
install.packages("devtools")

# Install ggformula
devtools::install_github("projectMOSAIC/ggformula")
```

### Tutorials

Interactive tutorials demonstrating the package functionality can be run with

```r
learnr::run_tutorial("introduction", package = "ggformula")
learnr::run_tutorial("refining", package = "ggformula")
```

### Package Vignette

The package vignette is available at [https://cran.r-project.org/package=ggformula/vignettes/ggformula.html](https://cran.r-project.org/package=ggformula/vignettes/ggformula.html).
