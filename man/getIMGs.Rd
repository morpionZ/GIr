\name{getIMGs}
\alias{getIMGs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve and save pictures from a Google Images search.
}
\description{
This functions uses user-provided keywords to perform a Google Images search. It 
can return a list of objects of EBImage class Image and/or save the images in a 
local folder for later use. 
}
\usage{
getIMGs(keywords, number, WRITE, RETURN, imgtype)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{keywords}{
A vector containing the keywords for the Google Images search.
}
\item{number}{
The number of results to return (default: 5).
}
\item{WRITE}{
Logical. Should the function save pictures to hard drive (default: TRUE). If 
TRUE, the pictures will be saved in the working directory in a folder named 
after the provided keywords.
}
\item{RETURN}{
Logical. Should the function return a list of the images (default: FALSE). Users
should use this argument carefully, especially if they request a large number of 
images, to avoid running out of memory. 
  }
\item{imgtype}{
A character string indicating the type of images returned by the Google Images 
search (default: "all"). Possible values are "all" (all types of picture), 
"photo" (only photographs), "face" (only pictures containing faces), "clipart" 
(only cliparts) and "lineart" (only line art).
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
If RETURN is TRUE, the function returns a list of objects of EBImage class 
Image. If FALSE, the function does not return anything
}
\references{
This function uses Google Images (deprecated) JSON API. More information about 
the API at: \url{https://developers.google.com/image-search/v1/jsondevguide}.
}
\author{
Simon Garnier <garnier@njit.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See the \code{\link{EBImage}} package for more information on manipulating 
\code{\link{Image}} objects. 
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Google Images }
