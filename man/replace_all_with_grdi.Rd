% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grdi.R
\name{replace_all_with_grdi}
\alias{replace_all_with_grdi}
\title{Attempts to replace all values in a vector with GRDI terms}
\usage{
replace_all_with_grdi(
  x,
  grdi_col,
  term_query_dist = 0,
  ignore.case = TRUE,
  setNA = NA,
  ...
)
}
\arguments{
\item{x}{vector of strings to replace over}

\item{grdi_col}{The name of the grdi column with the ontology terms}

\item{term_query_dist}{For fuzzy searching.}

\item{ignore.case}{ignore case, passed onto agrep}

\item{...}{passed onto agrep}
}
\value{
A vector with replaced terms, when found
}
\description{
Attempts to replace all values in a vector with GRDI terms
}
