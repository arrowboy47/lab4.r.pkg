% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sideorder.R
\name{get_sidedish}
\alias{get_sidedish}
\title{Gives you an appropriate side dish to accompany a main course}
\usage{
get_sidedish(
  main_course_id,
  q = NULL,
  app_id = "6f567a3a",
  app_key = "aec97451eec00326ae7fedab93b7c250"
)
}
\arguments{
\item{q}{A string of the side dish to search for (optional)}

\item{app_id}{A string of your application id for edamam (required)}

\item{app_key}{A string of app key for edamam (required)}

\item{maincourse}{edamam url to maincourse meal (required)}
}
\value{
A dataframe with recipe names and urls
}
\description{
Gives you an appropriate side dish to accompany a main course
}
