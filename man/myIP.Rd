\name{myIP}
\alias{myIP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Uses jsonip.com to retrieve computer's public IP address.
}
\description{
This function connects to jsonip.com to retrieve the public IP address of the 
computer. The user IP address is requested by the Google Images JSON API: "You 
should include the userip parameter, which supplies the IP address of the 
end-user who made the request and validates that you are not making automated 
requests in violation of the Terms of Service."
}
\usage{
myIP()
}
\arguments{
This function has no argument.
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The function returns a character string corresponding to the public IP of the 
computer.
}
\references{
For more information about jsonip.com, visit: \url{http://jsonip.com/about}.
}
\author{
Simon Garnier <garnier@njit.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
myIP()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IP address }
