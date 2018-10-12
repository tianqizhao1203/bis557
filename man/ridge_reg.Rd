\name{ridge_reg}
\alias{ridge_reg}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ridge Regression via SVD
}
\description{
Use SVD to estimate the coefficients in ridge regression.
}
\usage{
ridge_reg(formula, lambda, data)
}
\value{
\item{coefficients }{Estimated betas for ridge regression}
\item{lambda }{Lambda used for calculating betas}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\dontrun{ridge_reg(Sepal.Length ~., 1.12, iris)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
