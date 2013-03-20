glmnet
======

[glmnet on CRAN](http://cran.r-project.org/web/packages/glmnet/index.html)

Extremely efficient procedures for fitting the entire lasso or elastic-net regularization path for linear regression, logistic
and multinomial regression models, poisson regression and the Cox model. Two recent additions are the multiresponse gaussian,
and the grouped multinomial. The algorithm uses cyclical coordinate descent in a pathwise fashion, as described in the paper listed below.

This repository is intended to be a mirror of the source code on CRAN.  The source code was last pulled on 2013-03-19

##Installation

```
./configure
R CMD INSTALL ./
```

##Dev Enhancements
I am developing a dev version which builds on top of glmnet.
Currently, the dev version allows parallel cross validation and
the use of formulas instead of user created model matrices.

The dev version can be installed with

```
require(devtools)
install_github("glmnet", "jeffwong", "dev")
```
