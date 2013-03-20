\name{plotCoefs}
\alias{plotCoefs}
\title{plot coefficients from a "cv.glmnet2" object}
\description{
    Uses ggplot2 to plot coefficients as a bar graph
}
\usage{
plotCoefs(model, nonzero, subset.condition, ...)
}
\arguments{
  \item{model}{fitted \code{"cv.glmnet2"} model}
  \item{term}{logical.  If TRUE, only plot nonzero coefficients}
  \item{subset.condition}{an expression as in \code{expression} which can be passed
    into the function \code{subset}.  The expression should be a function of the
    variables "label" and "value"}
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
