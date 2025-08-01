% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gf_functions.R
\name{gf_step}
\alias{gf_step}
\title{Formula interface to geom_step()}
\usage{
gf_step(
  object = NULL,
  gformula = NULL,
  data = NULL,
  ...,
  alpha,
  color,
  group,
  linetype,
  linewidth,
  direction = "hv",
  xlab,
  ylab,
  title,
  subtitle,
  caption,
  geom = "step",
  stat = "identity",
  position = "identity",
  show.legend = NA,
  show.help = NULL,
  inherit = TRUE,
  environment = parent.frame()
)
}
\arguments{
\item{object}{When chaining, this holds an object produced in the earlier portions
of the chain.  Most users can safely ignore this argument.
See details and examples.}

\item{gformula}{A formula with shape \code{y ~ x}.
Faceting can be achieved by including \code{|} in the formula.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{...}{Additional arguments.  Typically these are
(a) ggplot2 aesthetics to be set with \code{attribute = value},
(b) ggplot2 aesthetics to be mapped with \code{attribute = ~ expression}, or
(c) attributes of the layer as a whole, which are set with \code{attribute = value}.}

\item{alpha}{Opacity (0 = invisible, 1 = opaque).}

\item{color}{A color or a formula used for mapping color.}

\item{group}{Used for grouping.}

\item{linetype}{A linetype (numeric or "dashed", "dotted", etc.) or a formula used
for mapping linetype.}

\item{linewidth}{A numerical line width or a formula used for mapping linewidth.}

\item{direction}{direction of stairs: 'vh' for vertical then horizontal,
'hv' for horizontal then vertical, or 'mid' for step half-way between
adjacent x-values.}

\item{xlab}{Label for x-axis. See also \code{\link[=gf_labs]{gf_labs()}}.}

\item{ylab}{Label for y-axis. See also \code{\link[=gf_labs]{gf_labs()}}.}

\item{title, subtitle, caption}{Title, sub-title, and caption for the plot.
See also \code{\link[=gf_labs]{gf_labs()}}.}

\item{geom}{A character string naming the geom used to make the layer.}

\item{stat}{The statistical transformation to use on the data for this layer.
When using a \verb{geom_*()} function to construct a layer, the \code{stat}
argument can be used the override the default coupling between geoms and
stats. The \code{stat} argument accepts the following:
\itemize{
\item A \code{Stat} ggproto subclass, for example \code{StatCount}.
\item A string naming the stat. To give the stat as a string, strip the
function name of the \code{stat_} prefix. For example, to use \code{stat_count()},
give the stat as \code{"count"}.
\item For more information and other ways to specify the stat, see the
\link[ggplot2:layer_stats]{layer stat} documentation.
}}

\item{position}{A position adjustment to use on the data for this layer. This
can be used in various ways, including to prevent overplotting and
improving the display. The \code{position} argument accepts the following:
\itemize{
\item The result of calling a position function, such as \code{position_jitter()}.
This method allows for passing extra arguments to the position.
\item A string naming the position adjustment. To give the position as a
string, strip the function name of the \code{position_} prefix. For example,
to use \code{position_jitter()}, give the position as \code{"jitter"}.
\item For more information and other ways to specify the position, see the
\link[ggplot2:layer_positions]{layer position} documentation.
}}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{show.help}{If \code{TRUE}, display some minimal help.}

\item{inherit}{A logical indicating whether default attributes are inherited.}

\item{environment}{An environment in which to look for variables not found in \code{data}.}
}
\value{
a gg object
}
\description{
\code{geom_path()} connects the observations in the order in which they appear
in the data. \code{geom_line()} connects them in order of the variable on the
x axis. \code{geom_step()} creates a stairstep plot, highlighting exactly
when changes occur. The \code{group} aesthetic determines which cases are
connected together.
}
\section{Specifying plot attributes}{


Positional attributes (a.k.a, aesthetics) are specified using the formula in \code{gformula}.
Setting and mapping of additional attributes can be done through the
use of additional arguments.
Attributes can be set can be set using arguments of the form \code{attribute = value} or
mapped using arguments of the form \code{attribute = ~ expression}.

In formulas of the form \code{A | B}, \code{B} will be used to form facets using
\code{\link[ggplot2:facet_wrap]{ggplot2::facet_wrap()}} or \code{\link[ggplot2:facet_grid]{ggplot2::facet_grid()}}.
This provides an alternative to
\code{\link[=gf_facet_wrap]{gf_facet_wrap()}} and
\code{\link[=gf_facet_grid]{gf_facet_grid()}} that is terser and may feel more familiar to users
of \pkg{lattice}.
}

\section{Evaluation}{


Evaluation of the \pkg{ggplot2} code occurs in the environment of \code{gformula}.
This will typically do the right thing when formulas are created on the fly, but might not
be the right thing if formulas created in one environment are used to create plots
in another.
}

\examples{
gf_step(births ~ date, data = mosaicData::Births78, color = ~wday)

# Roll your own Kaplan-Meier plot

if (require(survival) && require(broom)) {
  # fit a survival model
  surv_fit <- survfit(coxph(Surv(time, status) ~ age + sex, lung))
  surv_fit
  # use broom::tidy() to create a tidy data frame for plotting
  surv_df <- tidy(surv_fit)
  head(surv_df)
  # now create a plot
  surv_df |>
    gf_step(estimate ~ time) |>
    gf_ribbon(conf.low + conf.high ~ time, alpha = 0.2)
}
}
\seealso{
\code{\link[ggplot2:geom_path]{ggplot2::geom_step()}}
}
