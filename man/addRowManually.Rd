% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eposmol.R
\name{addRowManually}
\alias{addRowManually}
\title{Add a result row manually to the lipidScoresTableData tibble.}
\usage{
addRowManually(
  lipidScoresTableData,
  lipidName,
  lipidCategoryOrClass,
  ionMode,
  evidenceClassification,
  evidenceScore
)
}
\arguments{
\item{lipidScoresTableData}{The lipidScoresTableData tibble.}

\item{lipidName}{The lipid name.}

\item{lipidCategoryOrClass}{The lipid category or class.}

\item{ionMode}{The ion mode.}

\item{evidenceClassification}{The evidence classification.}

\item{evidenceScore}{The evidence score.}
}
\value{
A tibble with the added row.
}
\description{
Add a result row manually to the lipidScoresTableData tibble.
}
