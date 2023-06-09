% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_serving_size.R
\name{convert_serving_size}
\alias{convert_serving_size}
\title{Converts serving size provided by a recipe to desired serving size}
\usage{
convert_serving_size(old, new, ingredients, amounts, units = NULL)
}
\arguments{
\item{old}{numeric value for serving size from original recipe (required)}

\item{new}{numeric value for serving size user wants (required)}

\item{ingredients}{vector with list of ingredients (required)}

\item{amounts}{vector with quantity recipe calls for (required)}

\item{units}{vector with unit names for ingredients (optional)}
}
\value{
a dataframe with new ingredient quantities for recipe
}
\description{
Converts serving size provided by a recipe to desired serving size
}
