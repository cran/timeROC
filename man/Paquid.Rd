\name{Paquid}
\alias{Paquid}
\docType{data}

\title{
Paquid cohort data 
}

\description{
PAQUID is a prospective cohort study initiated in 1988 in South Western France to explore functional and cerebral ageing. This sample includes n=2561 subjects. Data contains a time-to-event (minimum between time from enrolment to dementia onset and time from enrolment to death without dementia), a status indicator, and two cognitive scores measured at baseline.
}

\usage{data(Paquid)}

\format{
  A data frame with 2561 observations on the following 4 variables.
  \describe{
    \item{\code{time}}{ : the time-to-event (in years). It is defined as the minimum between time from enrolment to dementia onset and time from enrolment to death without dementia.}
    \item{\code{status}}{ : the status indicator : \code{0} = censored, \code{1} = dementia onset and \code{2} = death without dementia.}
    \item{\code{DSST}}{ : score at the Digit Symbol Substitution Score Test measured at baseline. This test explores attention and psychomotor speed.}
    \item{\code{MMSE}}{ : score at the Mini Mental State Examination measured at baseline. This test is often used as an index of global cognitive performance.}
  }
}


\references{
	
	Dartigues, J., Gagnon, M., Barberger-Gateau, P., Letenneur, L., Commenges, D.,
	Sauvel, C., Michel, P., and Salamon, R. (1992).
	The paquid epidemiological program on brain ageing.
	Neuroepidemiology, 11(1):14--18.

	Blanche P., Dartigues J-F and Jacqmin-Gadda H. (2012). Estimating and Comparing time-dependent areas under ROC curves
	for censored event times with competing risks. Submitted. 
}

\examples{
data(Paquid)
head(Paquid)
}
\keyword{datasets}
