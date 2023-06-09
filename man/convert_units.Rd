% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_units.R
\name{convert_units}
\alias{convert_units}
\title{unit conversion for tsp, tbsp, fl oz, cup, pt, qt, gal}
\usage{
convert_units(amount, from_unit, to_unit)
}
\arguments{
\item{amount}{numeric quantity for ingredient}

\item{from_unit}{string of original unit of measurement. Must be one of following: "tsp", "tbsp", "fl oz", "cup", "pt", "qt", "gal"}

\item{to_unit}{string of desired unit of measurement. Must be one of following: "tsp", "tbsp", "fl oz", "cup", "pt", "qt", "gal"}
}
\value{
string with new amount and unit of measurement.
}
\description{
unit conversion for tsp, tbsp, fl oz, cup, pt, qt, gal
}
