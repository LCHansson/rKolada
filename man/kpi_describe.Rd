% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kpi.R
\name{kpi_describe}
\alias{kpi_describe}
\title{Describe the KPIs in a Kolada KPI metadata table}
\usage{
kpi_describe(
  kpi_df,
  max_n = 5,
  format = "inline",
  heading_level = 2,
  sub_heading_level = heading_level + 1
)
}
\arguments{
\item{kpi_df}{A Kolada KPI metadata table}

\item{max_n}{The maximum number of KPIs to describe.}

\item{format}{Output format. Can be one of "inline" (default) or "md", i.e.
markdown.}

\item{heading_level}{The top heading level output format is "md".}

\item{sub_heading_level}{The sub heading level output format is "md".}
}
\value{
Returns the object passed to the function, invisibly, to be re-used
in a pipe.
}
\description{
Print a human-readable description of each entity of a KPI metadata table (up
to a maximum number of rows). Can be printed either directly to the R console
or used to populate a R markdown document, which can be useful for
documentation purposes.
}
