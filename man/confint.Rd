\name{confint}
\alias{confint.ipcwsurvivalROC}
\alias{confint.ipcwcompetingrisksROC}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Confidence intervals for areas under time-dependent ROC curves
}
\description{
This function compute the confidence interval for areas under time-dependent ROC curves (time-dependent AUC). Confidence interval are computed from the asymptotic normality of time-dependent AUC estimators. Standard errors are estimated from the iid-representation of the estimator. The method is at present only implemented for inverse probability of censoring weights computed from a Kaplan-Meier estimator. 
}
\usage{
\method{confint}{ipcwsurvivalROC}(object, parm=NULL, level = 0.95, ...)
\method{confint}{ipcwcompetingrisksROC}(object, parm=NULL, level = 0.95, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{ An object of class "ipcwsurvivalROC" or "ipcwcompetingrisksROC". The object \code{object} must have been estimated by the \code{\link{timeROC}} function with arguments \code{weighting="marginal"} and \code{iid = TRUE}.
}
  \item{parm}{ Dummy argument. (Not used).
}
  \item{level}{ the confidence level required. Default is \code{0.95}. 
}
  \item{...}{ Dummy argument. (Not used).
}
}
\details{
Time-dependent AUC estimators are asymptotically normally distributed. Then, confidence intervals are computed using an estimate of the variance and the quantiles of the standard normal distribution.
To compute the variance estimates, the function computes  the empirical variance estimates of the estimated iid-representations of the time-dependent AUC estimators.  
}
\value{
\bold{Without competing risks :} a matrix. Columns correspond to the lower and the upper bounds of the confidence intervals. Rows correspond to the  time points for which time-dependent AUC estimator was computed.

\bold{With competing risks :} a list of two matrices, one for each time-dependent AUC definition (\bold{(i)} and \bold{(ii)}, see details about \code{\link{timeROC}} function). Rows correspond to the  time points for which time-dependent AUC estimator was computed.

}
\references{
	Hung, H. and Chiang, C. (2010). Estimation methods for time-dependent AUC with survival data.
	Canadian Journal of Statistics, 38(1):8-26

	Blanche P., Dartigues J-F and Jacqmin-Gadda H. (2012). Estimating and Comparing time-dependent areas under ROC curves
	for censored event times with competing risks. Submitted. 
}
\author{
Paul Blanche \email{Paul.Blanche@isped.u-bordeaux2.fr}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{compare}},\code{\link{timeROC}}
}
\examples{
##-------------Without competing risks-------------------

data(pbc)
head(pbc)
pbc<-pbc[!is.na(pbc$trt),] # select only randomised subjects
pbc$status<-as.numeric(pbc$status==2) # create event indicator: 1 for death, 0 for censored

# we evaluate bilirubin as a prognostic biomarker for death.
ROC.bili<-timeROC(T=pbc$time,
                  delta=pbc$status,marker=pbc$bili,
                  cause=1,weighting="marginal",
                  times=quantile(pbc$time,probs=seq(0.2,0.8,0.1)),
                  iid=TRUE)
ROC.bili
confint(ROC.bili)


##-------------With competing risks-------------------

data(Melano)
head(Melano)

# Evaluate tumor thickness as a prognostic biomarker for
# death from malignant melanoma.
ROC.thick<-timeROC(T=Melano$time,delta=Melano$status,
                   marker=Melano$thick,cause=1,
                   times=quantile(Melano$time,probs=seq(0.2,0.8,0.2)),
                   iid=TRUE)
ROC.thick
confint(ROC.thick)

\dontrun{
	data(Paquid)
	head(Paquid)
	
	# Computation requires approximately 30 seconds 
	# (because iid=TRUE and n=2561 subjects)	
	# evaluate DDST cognitive score as a prognostic tool for
	# dementia onset, accounting for death without dementia competing risk.	
	ROC.DSST<-timeROC(T=Paquid$time,
                      delta=Paquid$status,marker=-Paquid$DSST,
                      cause=1,weighting="marginal",times=c(3,5),
                      ROC=TRUE,iid=TRUE)
	ROC.DSST
	confint(ROC.DSST)
} 
}

