% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kpi_groups.R
\name{kpi_grp_extract_ids}
\alias{kpi_grp_extract_ids}
\title{Extract KPI ID strings from a Kolada KPI Group metadata table}
\usage{
kpi_grp_extract_ids(kpi_grp_df)
}
\arguments{
\item{kpi_grp_df}{A Kolada KPI Group metadata table, as created by e.g.
\code{get_kpi_groups}.}
}
\value{
A vector of KPI IDs.
}
\description{
This function is primarily intended as a convenient way to pass a (filtered)
Kolada KPI Group metadata table to \code{\link{get_values}}. All IDs of the
KPIs contained in each group in the table are extracted.
}
