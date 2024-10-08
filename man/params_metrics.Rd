% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_params.R
\name{params_metrics}
\alias{params_metrics}
\title{Get Metric parameters}
\usage{
params_metrics()
}
\value{
A table of all metrics and corresponding info
}
\description{
This function returns a nested list of metrics, their names and titles used for plots as well as their description.
Can for example be used for the creation of a selectInput of metrics.
}
\examples{
metric_choices <- params_metrics()

}
