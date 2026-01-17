## -----------------------------------------------------------------------------
#| label: setup
#| include: false
set.seed(1234)
library(ggformula)
library(dplyr)
library(patchwork)


## -----------------------------------------------------------------------------
#| label: load-ggformula
library(ggformula)
theme_set(theme_bw())


## -----------------------------------------------------------------------------
#| label: cars-scatter
data(mtcars)
mtcars2 <- mtcars |>
  tibble::rownames_to_column(var = "carname")

cars_scatter <-
  mtcars2 |>
  gf_point_interactive(
    wt ~ drat,
    color = ~mpg,
    tooltip = ~carname, # show carname when hovering on a point
    data_id = ~carname, # unique identifier -- selection is a single point
    hover_nearest = TRUE,
    size = 3
  )

# to display the graph with interactive components enabled, use
# gf_girafe() to convert it to an HTML widget.

gf_girafe(cars_scatter)



## -----------------------------------------------------------------------------
#| label: cars-scatter-tooltip
cars_scatter_tooltip <-
  mtcars2 |>
  gf_point_interactive(
    qsec ~ disp,
    tooltip = ~ glue::glue("{carname} ({cyl} cylinders)"),
    data_id = ~cyl,
    size = 3,
    hover_nearest = TRUE
  )

gf_girafe(cars_scatter_tooltip)



## -----------------------------------------------------------------------------
#| label: diamonds-bargraph
data(diamonds)
diamonds_bargraph <-
  diamonds |>
  gf_bar_interactive(
    ~color,
    fill = ~cut,
    tooltip = ~ after_stat(count),
    data_id = ~ as.character(cut)
  )

diamonds_bargraph |> gf_girafe()


## -----------------------------------------------------------------------------
#| label: layer_data

diamonds_bargraph |> layer_data() |> head(3)


## -----------------------------------------------------------------------------
#| label: diamonds-colgraph-1
library(dplyr)
diamonds |>
  group_by(color, cut) |>
  summarise(count = n()) |>
  gf_col_interactive(
    count ~ color,
    fill = ~cut,
    tooltip = ~ glue::glue("color: {color}, cut: {cut}, count: {count}"),
    data_id = ~ glue::glue("{cut} - {color}")
  ) |>
  gf_girafe()


## -----------------------------------------------------------------------------
#| label: diamonds-col-graph-2
diamonds |>
  group_by(color, cut) |>
  summarise(count = n()) |>
  gf_col_interactive(
    count ~ color,
    fill = ~cut,
    tooltip = ~ glue::glue("color: {color}, cut: {cut}, count: {count}")
  ) |>
  gf_girafe()


## -----------------------------------------------------------------------------
#| label: diamonds-bargraph2
diamonds_bargraph_2 <-
  diamonds |>
  gf_bar_interactive(
    ~color,
    fill = ~cut,
    tooltip = ~ stage(
      start = glue::glue("color: {color}; cut: {cut}"),
      after_stat = glue::glue("{tooltip}; count = {count}")
    ),
    data_id = ~ glue::glue("{cut} -- {color}"),
    size = 3
  )
diamonds_bargraph_2 |>
  gf_girafe()


## -----------------------------------------------------------------------------
#| label: interactive-scales
diamonds_bargraph_3 <-
  diamonds_bargraph |>
  gf_refine(
    scale_fill_viridis_d_interactive(
      begin = 0.1,
      end = 0.7,
      option = "D",
      data_id = function(breaks) as.character(breaks),
      tooltip = function(breaks) glue::glue("break: {as.character(breaks)}")
    )
  )

diamonds_bargraph_3 |>
  gf_girafe()


## -----------------------------------------------------------------------------
#| label: faceting
diamonds_bargraph_4 <-
  diamonds_bargraph_3 |>
  gf_theme(
    strip.text = element_text_interactive(),
    strip.background = element_rect_interactive()
  ) |>
  gf_facet_wrap_interactive(
    ~clarity, # or vars(clarity)
    interactive_on = "both",
    ncol = 2,
    labeller = gf_labeller_interactive(
      tooltip = ~ paste("this is clarity", clarity),
      data_id = ~clarity
    )
  ) 

  diamonds_bargraph_4 |> gf_girafe()


## -----------------------------------------------------------------------------
#| label: diamonds-bargraph4
diamonds_bargraph_4 |> layer_data() |> slice_sample(n=4)


## -----------------------------------------------------------------------------
#| label: theme-facets-interactive
diamonds_bargraph_3 |>
  gf_theme(theme_facets_interactive(theme_minimal())) |>
  gf_facet_wrap_interactive(
    ~clarity, # or vars(clarity)
    interactive_on = "both",
    ncol = 2,
    labeller = gf_labeller_interactive(
      tooltip = ~ paste("this is clarity", clarity),
      data_id = ~clarity
    )
  ) |>
  gf_girafe()


## -----------------------------------------------------------------------------
#| label: patchwork
library(patchwork)


cars_scatter_2 <-
  mtcars2 |>
  gf_point_interactive(
    disp ~ qsec,
    color = ~mpg,
    tooltip = ~carname,
    data_id = ~carname,
    hover_nearest = TRUE,
    size = 3
  )

gf_girafe(cars_scatter / cars_scatter_2)


## -----------------------------------------------------------------------------
#| label: js-alert
  mtcars2 |>
  gf_point_interactive(
    wt ~ drat,
    color = ~mpg,
    data_id = ~carname, 
    onclick = ~glue::glue('alert("Here is some info for {carname} ...")'),
    size = 3
  ) |>
    gf_girafe()


## -----------------------------------------------------------------------------
#| label: js-window-open
  mtcars2 |>
  gf_point_interactive(
    wt ~ drat,
    color = ~ mpg,
    data_id = ~ carname, 
    tooltip = ~ carname,
    onclick = ~ glue::glue('window.open("https://en.wikipedia.org/w/index.php?search={carname}")'),
    size = 3
  ) |>
    gf_girafe()


## -----------------------------------------------------------------------------
#| label: hover-css
diamonds_bargraph_3 |>
  gf_theme(theme_facets_interactive(theme_minimal())) |>
  gf_facet_wrap_interactive(
    ~clarity, # or vars(clarity)
    interactive_on = "both",
    ncol = 2,
    labeller = gf_labeller_interactive(
      tooltip = ~ paste("this is clarity", clarity),
      data_id = ~clarity
    )
  ) |>
  gf_girafe(
    options = list(
      opts_hover("fill:red; opacity: 0.5")
    )
  )


## -----------------------------------------------------------------------------
#| label: girafe-css
diamonds_bargraph_3 |>
  gf_theme(theme_facets_interactive(theme_minimal())) |>
  gf_facet_wrap_interactive(
    ~clarity, # or vars(clarity)
    interactive_on = "both",
    ncol = 2,
    labeller = gf_labeller_interactive(
      tooltip = ~ paste("this is clarity", clarity),
      data_id = ~clarity
    )
  ) |>
  gf_girafe(
    options = list(
      opts_hover(
        css = girafe_css(
          css = "fill:red; opacity:0.7; stroke:black; stroke-width:3px;",
          text = "stroke:none; fill:white; opacity:0.9;"
        )
      )
    )
  )


## -----------------------------------------------------------------------------
#| label: label-interactive
mtcars2[1:6, ] |>
  gf_label_interactive(qsec ~ disp, label = ~carname, data_id = ~carname) |>
  gf_girafe(
    options = list(
      opts_hover(
        css = girafe_css(
          css = "fill:yellow;",
          text = "stroke:none; fill:red;"
        )
      )
    )
  )


## -----------------------------------------------------------------------------
#| label: weather-hover-inv
mosaicData::Weather |>
  gf_line_interactive(
    high_temp ~ date,
    color = ~city,
    show.legend = FALSE,
    tooltip = ~city,
    data_id = ~city
  ) |>
  gf_facet_wrap_interactive(
    ~year,
    ncol = 1,
    scales = "free_x",
    labeller = gf_labeller_interactive(
      data_id = ~year,
      tooltip = ~ glue::glue("This is the year {year}")
    )
  ) |>
  gf_theme(theme_facets_interactive()) |>
  gf_girafe(
    options = list(
      opts_hover_inv(css = "opacity:0.2;"),
      opts_hover(css = "stroke-width:2;", nearest_distance = 40),
      opts_tooltip(use_cursor_pos = FALSE, offx = 0, offy = -30)
    )
  )



## -----------------------------------------------------------------------------
#| label: tooltip-position
cars_scatter |>
  gf_girafe(
    options = list(
      opts_tooltip(offx = 0, offy = -30, use_cursor_pos = FALSE)
    )
  )


## -----------------------------------------------------------------------------
#| label: autocoloring
diamonds_bargraph_3 |>
  gf_girafe(
    options = list(
      opts_tooltip(
        use_fill = TRUE,
        offx = 0,
        offy = 0,
        use_cursor_pos = FALSE,
        css = "border: 2px solid black; color: aliceblue; border-radius: 4px; padding: 6px;"
      )
    )
  )


## -----------------------------------------------------------------------------
#| label: zoom
cars_scatter |>
  gf_girafe(
    options = list(opts_zoom(max = 5))
  )


## -----------------------------------------------------------------------------
#| label: girafe-defaults

set_girafe_defaults(
  # set colors for
  opts_hover = opts_hover(
    css = "fill:yellow;stroke:black;stroke-width:3px;r:10px;"
  ),
  opts_hover_inv = opts_hover_inv(css = "opacity:0.5"),
  # allow zooming/panning up to 4x size
  opts_zoom = opts_zoom(min = 1, max = 4),
  opts_tooltip = opts_tooltip(
    css = "padding: 2px; border: 4px solid navy; background-color: steelblue; color: white; border-radius: 8px"
  ),
  opts_sizing = opts_sizing(rescale = TRUE),
  opts_toolbar = opts_toolbar(
    saveaspng = FALSE,
    position = "bottom",
    delay_mouseout = 5000
  )
)

cars_scatter |>
  gf_girafe()

cars_scatter |>
  gf_girafe(
    options = list(
      opts_tooltip(offx = 0, offy = -25, use_cursor_pos = FALSE)
    )
  )

