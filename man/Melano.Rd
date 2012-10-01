\name{Melano}
\alias{Melano}
\docType{data}
\title{
  Malignant melanoma data
}
\description{
In the period 1962-77, 205 patients with malignant melanoma (skin cancer) had a radical operation performed at Odense University
Hospital, Denmark. All patients were followed until the end of 1977 by 
which time 134 were still alive while 71 had died (of out whom 57 had
died from cancer and 14 from other causes).

The object of the study was to assess the effect of risk factors on
survival. 
}

\usage{data(Melano)}
\format{
  A data frame with 205 observations on the following 4 variables.
  \describe{
    \item{\code{time}}{ : time in days from operation.}
    \item{\code{status}}{ : the status indicator :  \code{0} = censored, \code{1} = death from malignant melanoma and \code{2} = death from other causes. }
    \item{\code{thick}}{ : tumour thickness (in 1/100 mm).}
    \item{\code{age}}{ : age at operation (years).}
  }
}
\source{
\url{http://192.38.117.59/~linearpredictors/?page=datasets&dataset=Melano}
}
\references{
  Regression with linear predictors (2010)
  
  Andersen, P.K. and Skovgaard, L.T.
  
  Springer Verlag
}
\examples{
data(Melano)
head(Melano)
}
\keyword{datasets}
