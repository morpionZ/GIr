\name{GIr-package}
\alias{GIr-package}
\alias{GIr}
\docType{package}
\title{Package overview}
\description{
This package contains functions to search and retrieve pictures from Google 
Images based on keywords provided by the user. The images are automatically can 
be downloaded to the computer and stored in a folder in the working directory 
for later use.
}
\details{
\tabular{ll}{
Package: \tab GIr\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2014-02-13\cr
License: \tab GPL-2\cr
}
}
\author{
Simon Garnier <garnier@njit.edu>

Maintainer: Simon Garnier <garnier@njit.edu>
}
\references{
This package uses Google Images (deprecated) JSON API. More information about 
the API at: \url{https://developers.google.com/image-search/v1/jsondevguide}.
}
\examples{
require("EBImage")
imgs <- getIMGs(keywords = c("R", "is", "fantastic"),
                number = 5,
                WRITE = FALSE,
                RETURN = TRUE,
                imgtype = "all")
display(imgs[[1]])
}
