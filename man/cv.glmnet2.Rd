\name{cv.glmnet2}
\alias{cv.glmnet2}
\title{Cross-validation for glmnet Improved}
\description{Does k-fold cross-validation for glmnet, produces a plot,
  and returns a value for \code{lambda}}
\usage{
cv.glmnet2(formula, data, ...)
}
\arguments{
  \item{formula}{formula expression specifying the response variable and features}
  \item{data}{a dataframe containing the response and feature variables}
  \item{\dots}{Other arguments that can be passed to \code{cv.glmnet}}
}
\details{A wrapper around cv.glmnet that allows the user to use formulas instead of
  passing in the model matrix} 
\value{an object of class \code{"cv.glmnet"} and \code{"cv.glmnet2"} is returned,
  containing the same items as \code{"cv.glmnet"}, but with the addition of
  \item{formula}{the formula used to generate the model matrix}
  \item{data}{the original data frame}
}
\references{Friedman, J., Hastie, T. and Tibshirani, R. (2008)
  \emph{Regularization Paths for Generalized Linear Models via Coordinate
    Descent},   \url{http://www.stanford.edu/~hastie/Papers/glmnet.pdf}\cr
  \emph{Journal of Statistical Software, Vol. 33(1), 1-22 Feb 2010}\cr
  \url{http://www.jstatsoft.org/v33/i01/}\cr
  Simon, N., Friedman, J., Hastie, T., Tibshirani, R. (2011)
  \emph{Regularization Paths for Cox's Proportional Hazards Model via
    Coordinate Descent, Journal of Statistical Software, Vol. 39(5)
    1-13}\cr
  \url{http://www.jstatsoft.org/v39/i05/}
}
\author{Jerome Friedman, Trevor Hastie and Rob Tibshirani\cr 
Noah Simon helped develop the 'coxnet' function.\cr
Maintainer: Trevor Hastie \email{hastie@stanford.edu}}
\seealso{\code{cv.glmnet}, \code{plotCoefs}}
\examples{
}
\keyword{models}
\keyword{regression}

