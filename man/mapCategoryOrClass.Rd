% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eposmol.R
\name{mapCategoryOrClass}
\alias{mapCategoryOrClass}
\title{Map the lipid category or class to the Eposmol category or class.}
\usage{
mapCategoryOrClass(lipidCategoryOrClass, lipidCategoryAndClassMap)
}
\arguments{
\item{lipidCategoryOrClass}{The lipid category or class. This can be a category or class from Goslin and/or LIPID MAPS.}

\item{lipidCategoryAndClassMap}{The lipid category and class map tibble with columns LipidMapsCategoryOrClass and EposMolCategoryOrClass. By default, we suggest using the class_map.xlsx table in inst/extdata/.}
}
\value{
The Eposmol category or class. If 'Unknown', the lipid category or class is not found in the map.
}
\description{
Map the lipid category or class to the Eposmol category or class.
}
