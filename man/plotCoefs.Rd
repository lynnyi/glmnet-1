\name{plotCoefs}
\alias{plotCoefs}
\title{plot coefficients from a "cv.glmnet2" object}
\description{
    Uses ggplot2 to plot coefficients as a bar graph
}
\usage{
plotCoefs(model, nonzero, ...)
}
\arguments{
  \item{model}{fitted \code{"cv.glmnet2"} model}
  \item{term}{logical.  If TRUE, only plot nonzero coefficients}
  \item{\dots}{Currently not used}
}
\details{
}
\references{Friedman, J., Hastie, T. and Tibshirani, R. (2008)
  \emph{Regularization Paths for Generalized Linear Models via Coordinate
  Descent}}
\author{Jerome Friedman, Trevor Hastie and Rob Tibshirani\cr
  Maintainer: Trevor Hastie <hastie@stanford.edu>}
\seealso{\code{cv.glmnet2}, \code{plotFactorCoefs}}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}
