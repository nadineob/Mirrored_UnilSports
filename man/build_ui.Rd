% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{build_ui}
\alias{build_ui}
\title{User Interface function for UnilSports Shiny Application}
\usage{
build_ui(clean_sport_schedule)
}
\arguments{
\item{clean_sport_schedule}{The data frame output from the get_cleanschedule_met. The output of this function should not be modified, so this function can apply the integer optimization technique properly.}
}
\value{
a shiny web application dashboard that builds a user interface for the UnilSports shiny application. This function is then used as one of  the parameters of the  UnilSports_gui function.
}
\description{
This is a function that builds a user interface for the UnilSports shiny application. This function is then used as the first parameter of the UnilSports_gui function.
}
\examples{
build_ui(clean_sport_schedule)
}
\author{
Arturo Garcia Luna Beltran Manunpat Sirijaturaporn Nadine Obeid Katti Irastorza
}
