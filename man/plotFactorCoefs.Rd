\name{plotFactorCoefs}
\alias{plotFactorCoefs}
\title{plot factor coefficients from a "cv.glmnet2" object}
\description{
    Uses ggplot2 to plot coefficients as a bar graph
}
\usage{
plotFactorCoefs(model, term, ...)
}
\arguments{
  \item{model}{fitted \code{"cv.glmnet2"} model}
  \item{term}{a single term as in \code{formula} specifying the factor to plot.  term
              should be an R expression, not a character vector}
  \item{\dots}{Currently not used}
}
\details{
}
\references{Friedman, J., Hastie, T. and Tibshirani, R. (2008)
  \emph{Regularization Paths for Generalized Linear Models via Coordinate
  Descent}}
\author{Jerome Friedman, Trevor Hastie and Rob Tibshirani\cr
  Maintainer: Trevor Hastie <hastie@stanford.edu>}
\seealso{\code{cv.glmnet2}, \code{plotCoefs}}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}
