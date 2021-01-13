## ----setup, include = FALSE---------------------------------------------------
have_packages <-
  require(ggformula) &&
  require(dplyr) &&
  require(ggplot2) &&
  require(mosaicData) &&
  require(maps) &&
  require(palmerpenguins) &&
  requireNamespace("mosaic") 

have_extra_packages <- 
  require(maps) && require(sf) && require(purrr)  

knitr::opts_chunk$set(
  fig.show = "asis",
  fig.align = "center",
  fig.width = 6, fig.height = 4,
  out.width = "60%",
  eval = have_packages
)
theme_set(theme_light())

## ---- eval = ! have_packages, echo = FALSE, results = 'asis'------------------
#  cat(
#  "
#  ## Warning: Missing packages
#  
#  Because one or more of
#  `ggformula`, `ggplot2`, `dplyr`, `mosaic`, `mosaicData`, `palmerpenguins`,
#  and `maps`, appears to be missing, this vignette is compiling
#  without executing any code.
#  
#  "
#  )

## ---- plottype, eval = FALSE--------------------------------------------------
#  gf_plottype(formula, data = mydata)

## ---- plottype2, eval = FALSE-------------------------------------------------
#  mydata %>% gf_plottype(formula)

## ---- simple-example----------------------------------------------------------
library(ggformula)
gf_point(mpg ~ hp, data = mtcars)
mtcars %>% gf_point(mpg ~ hp)

## ---- mapping-setting---------------------------------------------------------
gf_point(mpg ~ hp, color = ~ cyl, size = ~ carb, alpha = 0.50, data = mtcars) 

## ---- on-the-fly--------------------------------------------------------------
library(dplyr)
gf_point(mpg ~ hp,  color = ~ factor(cyl), size = ~ carb, alpha = 0.75, data = mtcars)
mtcars %>% 
  mutate(cylinders = factor(cyl)) %>% 
  gf_point(mpg ~ hp,  color = ~ cylinders, size = ~ carb, alpha = 0.75)

## ----penguins, warning=FALSE--------------------------------------------------
data(penguins, package = "palmerpenguins")   
gf_density( ~ bill_length_mm, data = penguins)
gf_density( ~ bill_length_mm,  fill = ~ species,  alpha = 0.5, data = penguins)
# gf_dens() is similar, but there is no line at bottom/sides and the plot is not fillable
gf_dens( ~ bill_length_mm, color = ~ species,  alpha = 0.7, data = penguins)
# gf_dens2() is like gf_dens() but is fillable
gf_dens2( ~ bill_length_mm, fill = ~ species, data = penguins,
          color = "gray50", alpha = 0.4) 

## ---- dens--------------------------------------------------------------------
# less smoothing
penguins %>% gf_dens( ~ bill_length_mm, color = ~ species, alpha = 0.7, adjust = 0.25)  
# more smoothing
penguins %>% gf_dens( ~ bill_length_mm, color = ~ species, alpha = 0.7, adjust = 4)     

