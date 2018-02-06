## ----setup, include = FALSE----------------------------------------------
have_packages <-
  require(ggformula) &&
  require(dplyr) &&
  require(ggplot2) &&
  require(mosaicData) &&
  require(mosaicModel)

knitr::opts_chunk$set(
  fig.show = "hold",
  out.width = "45%",
  eval = have_packages
)
theme_set(theme_light())

## ---- eval = !have_packages, echo = FALSE, results = 'asis'--------------
#  cat(
#  "
#  ## Warning: Missing packages
#  
#  Because one or more of
#  `ggformula`, `ggplot2`, `dplyr`, `mosaicModel` and `mosaicData`
#  appears to be missing, this vignette is compiling
#  without executing any code.
#  
#  "
#  )

## ---- plottype, eval = FALSE---------------------------------------------
#  gf_plottype(formula, data = mydata)

## ---- simple-example-----------------------------------------------------
library(ggformula)
gf_point(mpg ~ hp, data = mtcars)

## ---- mapping-setting----------------------------------------------------
gf_point(mpg ~ hp, color = ~ cyl, size = ~ carb, alpha = 0.50, data = mtcars) 

## ---- on-the-fly---------------------------------------------------------
library(dplyr)
gf_point(mpg ~ hp,  color = ~ factor(cyl), size = ~ carb, alpha = 0.75, data = mtcars)
gf_point(mpg ~ hp,  color = ~ cylinders, size = ~ carb, alpha = 0.75, 
         data = mtcars %>% mutate(cylinders = factor(cyl)))

## ----Runners, fig.show = "hold", out.width = "30%", warning=FALSE--------
data(Runners, package = "mosaicModel")
Runners <- Runners %>% filter( ! is.na(net))
gf_density( ~ net, data = Runners)
gf_density( ~ net,  fill = ~ sex,  alpha = 0.5, data = Runners)
# gf_dens() is similar, but there is no line at bottom/sides, and it is not "fillable"
gf_dens( ~ net, color = ~ sex, alpha = 0.7, data = Runners)    

## ---- dens---------------------------------------------------------------
# less smoothing
gf_dens( ~ net, color = ~ sex, alpha = 0.7, data = Runners, adjust = 0.25)  
# more smoothing
gf_dens( ~ net, color = ~ sex, alpha = 0.7, data = Runners, adjust = 4)     

## ----position, fig.show = "hold", warning=FALSE--------------------------
gf_density( ~ net, fill = ~ sex, color = NA, position = "stack", data = Runners)
gf_density( ~ net, fill = ~ sex, color = NA, position = "fill", data = Runners, adjust = 2)

## ---- jitter-------------------------------------------------------------
gf_point(age ~ sex, alpha = 0.05, data = Runners)
gf_jitter(age ~ sex, alpha = 0.05, data = Runners)

## ----boxplot, fig.show = "hold", warning = FALSE-------------------------
gf_boxplot(net ~ sex, color = "red", data = Runners)
gf_boxplot(net ~ sex, color = ~ start_position, data = Runners)

## ---- boxplot02----------------------------------------------------------
gf_boxplot(net ~ year, data = Runners)

## ---- boxplot03----------------------------------------------------------
gf_boxplot(net ~ year, group = ~ year, data = Runners)

## ---- factors, fig.keep = 1, fig.width = 5-------------------------------
# add a new variable to the data
Runners$the_year <- as.character(Runners$year)               # in base R
Runners <- Runners %>% mutate(the_year = as.character(year)) # in dplyr
gf_boxplot(net ~ the_year, color = ~ sex, data = Runners)

# or do it on the fly
gf_boxplot(net ~ factor(year), color = ~ sex, data = Runners)

## ----density2d-hex, fig.show = "hold"------------------------------------
gf_density_2d(net ~ age, data = Runners)
gf_hex(net ~ age, data = Runners)

## ----paths, fig.show = "hold", out.width = "30%"-------------------------
# create a categorical variable
mtcars <- mtcars %>% mutate(n_cylinders = as.character(cyl)) 
gf_line(mpg ~ hp, data = mtcars)
gf_path(mpg ~ hp, data = mtcars)
gf_line(mpg ~ hp, color = ~ n_cylinders, data = mtcars)

## ---- births01-----------------------------------------------------------
library(mosaicData)
gf_point(births ~ date, data = Births78)

## ---- births02-----------------------------------------------------------
gf_point(births ~ date, color = ~ wday, data = Births78)

## ---- births03-----------------------------------------------------------
gf_line(births ~ date, color = ~ wday, data = Births78)

## ---- weather------------------------------------------------------------
Temps <-
  Weather %>%
  filter(month <= 4, year <= 2016, city == "Chicago")

gf_pointrange(avg_temp + low_temp + high_temp  ~ date, color = ~ avg_temp, data = Temps) %>%
    gf_refine(scale_color_gradientn(colors = c("blue", "green", "orange", "red")))

gf_ribbon(low_temp + high_temp  ~ date, color = "navy", alpha = 0.3, data = Temps)

## ---- KidsFeet-----------------------------------------------------------
gf_point(length ~ sex, color = ~ domhand, data = KidsFeet,
         position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.4))

## ---- stats--------------------------------------------------------------
gf_bar( ~ age, data = HELPrct, stat = "bin")

## ---- stats02------------------------------------------------------------
gf_jitter(length ~ sex, color = ~ domhand, data = KidsFeet,
          width = 0.1, height = 0) %>%
  gf_line(length ~ sex, color = ~ domhand, data = KidsFeet,
          group = ~ domhand, stat="summary")

## ---- stats03------------------------------------------------------------
gf_jitter(length ~ sex, color = ~ domhand, data = KidsFeet,
          width = 0.1, height = 0, alpha = 0.3) %>%
  gf_pointrange(length + ..ymin.. + ..ymax.. ~ sex, 
                color = ~ domhand, data = KidsFeet, 
                group = ~ domhand, stat="summary")

## ---- stats04------------------------------------------------------------
gf_point(length ~ sex, color = ~ domhand, data = KidsFeet,
          width = 0.1, height = 0, alpha = 0.5,
          position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.3)) %>%
  gf_pointrange(length + ..ymin.. + ..ymax.. ~ sex, 
                color = ~ domhand, data = KidsFeet, 
                group = ~ domhand, stat="summary",
                fun.y = median, fun.ymin = min, fun.ymax = max,
                position = position_dodge(width = 0.6))

## ---- layers-------------------------------------------------------------
gf_histogram( ~ age, data = Runners, alpha = 0.3, fill = "navy") %>%
  gf_freqpoly( ~ age)

## ---- layers02-----------------------------------------------------------
gf_density_2d(net ~ age, data = Runners) %>%
  gf_point(net ~ age, alpha = 0.01) 

## ----facets, fig.show = "hold", warning=FALSE----------------------------
gf_density_2d(net ~ age, data = Runners) %>% gf_facet_grid( ~ sex)
# the dot here is a bit strange, but required to make a valid formula
gf_density_2d(net ~ age, data = Runners) %>% gf_facet_grid( sex ~ .)
gf_density_2d(net ~ age, data = Runners) %>% gf_facet_wrap( ~ the_year)
gf_density_2d(net ~ age, data = Runners) %>% gf_facet_grid(start_position ~ sex)

## ---- facets02, out.width = "95%", fig.width = 6, fig.height = 3.5-------

gf_ribbon(low_temp + high_temp ~ date | city ~ year, data = Weather, alpha = 0.3) 

gf_linerange(low_temp + high_temp ~ date | city ~ year, color = ~ avg_temp, data = Weather) %>%
  gf_refine(scale_colour_gradientn(colors = rev(rainbow(5))))

## ---- facets03, out.width = "95%", fig.width = 6, fig.height = 3.5-------

gf_ribbon(low_temp + high_temp ~ date | city ~ ., data = Weather, alpha = 0.3) 

gf_linerange(low_temp + high_temp ~ date, color = ~ avg_temp, data = Weather) %>%
  gf_refine(scale_colour_gradientn(colors = rev(rainbow(5)))) %>%
  gf_facet_grid(city ~ year, scales = "free_x")

## ---- themes-------------------------------------------------------------
gf_histogram( ~ age, data = Runners, alpha = 0.2, fill = "navy",
              binwidth = 5) %>%
  gf_freqpoly( ~ age, binwidth = 5) %>%
  gf_labs(x = "age (years)", title = "Age of runners") %>%
  gf_lims(x = c(20, 80)) %>%
  gf_theme(theme = theme_minimal())

gf_histogram( ~ age, data = Runners, alpha = 0.5, fill = "white",
              binwidth = 5) %>%
  gf_freqpoly( ~ age, color = "skyblue", binwidth = 5, size = 1.5) %>%
  gf_labs(x = "age (years)", title = "Age of runners") %>%
  gf_lims(x = c(20, 80)) %>%
  gf_theme(theme = theme_dark())

