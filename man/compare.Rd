\name{compare}
\alias{compare}

\title{
Compute tests for comparing two time-dependent AUC
}
\description{
This function computes the p-value for testing the null hypothesis that asserts that two time-dependent AUCs of two markers are equal. 
The two markers must have been measured on the same subjects.
}

\usage{
compare(x, y, adjusted = FALSE, abseps = 1e-06)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class "ipcwsurvivalROC" or "ipcwcompetingrisksROC". The object \code{x} must have been estimated by the \code{\link{timeROC}} function with arguments \cr
\code{weighting="marginal"} and \code{iid = TRUE}.
}
  \item{y}{
An object of class "ipcwsurvivalROC" or "ipcwcompetingrisksROC". The object \code{y} must have been estimated by the \code{\link{timeROC}} function with arguments \cr
 \code{weighting="marginal"} and \code{iid = TRUE}.
}
  \item{adjusted}{
A logical value that indicates if we also want to compute adjusted p-values, for accounting for the mutiplicity of tests (as many tests as there are time points for which we have estimated the AUCs of the two markers). 
}

  \item{abseps}{
relative error tolerance as double. This optional technical argument is the argument of function \code{\link[mvtnorm]{GenzBretz}} that is used when \code{adjusted = TRUE}. Default is \code{1e-06}.
}
}
\details{
For each time points, the function computes the difference between estimated AUCs of both markers, the variance of the difference using the iid-representation of the AUC estimators, and return the p-value of the comparison test. For each time point "t", the null hypothesis that is tested is : "AUC of both markers are equal at time "t" ".
When option \code{adjusted = TRUE} is chosen, then the function also computes the adjusted p-values, to account for the fact that we compute as many p-values as there as time points. The computation is based on the fact that (i) the vector of the differences of AUCs are asymptotically normally distributed, and  that (ii)  the variance-covariance matrix can be consistently estimated using the iid-representations of the AUC estimators at all time points.


}
\value{
The function \code{compare} returns a list. The list contains p-values of the comparison tests computed at all time points. If \code{adjusted = TRUE}, then it also contains the correlation matrix of the test statistics, that was used to compute adjusted p-values, to account for multiple testing.

}
\references{
	Chiang CT, Hung H.(2010). Non-parametric estimation for time-dependent AUC. Journal of Statistical Planning and Inference, 140:1162-1174.

	Bretz, F., Hothorn, T., and Westfall, P. (2010). Multiple comparisons using R. Chapman \& Hall/CRC.
	
	Blanche, P., Dartigues, J. F., & Jacqmin-Gadda,	H. (2013). Estimating and comparing time-dependent areas under
	receiver operating characteristic curves for censored event times with competing risks. 
	Statistics in medicine, 32(30), 5381-5397.
}
\author{
Paul Blanche \email{pabl@sund.ku.dk}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{timeROC}}, \code{\link[=confint.ipcwsurvivalROC]{confint}}
  }
\examples{
##-------------Without competing risks-------------------
library(survival)
data(pbc)
head(pbc)
pbc<-pbc[!is.na(pbc$trt),] # select only randomised subjects
pbc$status<-as.numeric(pbc$status==2) # create event indicator: 1 for death, 0 for censored

# we evaluate bilirubin as a prognostic biomarker. 
ROC.bili<-timeROC(T=pbc$time,
                  delta=pbc$status,marker=pbc$bili,
                  cause=1,weighting="marginal",
                  times=quantile(pbc$time,probs=seq(0.2,0.8,0.1)),
                  iid=TRUE)

# we evaluate albumin as a prognostic biomarker.
ROC.albumin<-timeROC(T=pbc$time,
                  delta=pbc$status,marker=-pbc$albumin,
                  cause=1,weighting="marginal",
                  times=quantile(pbc$time,probs=seq(0.2,0.8,0.1)),
                  iid=TRUE)

# we compare albumin and bilirubin as prognostic biomarkers.
ROC.albumin #print results for albumin
ROC.bili    #print results for bilirubin
compare(ROC.albumin,ROC.bili) #compute p-values of comparison tests
compare(ROC.albumin,ROC.bili,adjusted=TRUE)

##-------------With competing risks-------------------

data(Melano)
head(Melano)

# Evaluate tumor thickness as a prognostic biomarker for
# death from malignant melanoma.
ROC.thick<-timeROC(T=Melano$time,delta=Melano$status,
                   marker=Melano$thick,cause=1,
                   times=quantile(Melano$time,probs=seq(0.2,0.8,0.2)),
                   iid=TRUE)

# Evaluate age at operation as a prognostic tool for
# death from malignant melanoma.
ROC.age<-timeROC(T=Melano$time,delta=Melano$status,
                 marker=Melano$age,cause=1,
                 times=quantile(Melano$time,probs=seq(0.2,0.8,0.2)),
                 ROC=TRUE,iid=TRUE)

# compare the predictive abilities of tumor thickness and 
# age at operation for death from malignant melanoma.
ROC.thick
ROC.age
compare(ROC.age,ROC.thick,adjusted=TRUE)



\dontrun{
	data(Paquid)
	head(Paquid)

	# Computation requires approximately   1 minute 
	# (because n=2561 subjects, iid=TRUE, and times=c(3,5,10))
	# evaluate DDST cognitive score as a prognostic tool for
	# dementia onset, accounting for death without dementia competing risk.
	ROC.DSST<-timeROC(T=Paquid$time,delta=Paquid$status,
    		              marker=-Paquid$DSST,cause=1,
        		          times=c(3,5,10),ROC=TRUE,iid=TRUE)
	ROC.DSST

	# Computation requires approximately   1 minute 
	# (because n=2561 subjects, iid=TRUE, and times=c(3,5,10))
	# evaluate MMSE cognitive score as a prognostic tool for
	# dementia onset, accounting for death without dementia competing risk.
	ROC.MMSE<-timeROC(T=Paquid$time,delta=Paquid$status,
    		              marker=-Paquid$MMSE,cause=1,
        		          times=c(3,5,10),ROC=TRUE,iid=TRUE)
	
	# we compare MMSE and DSST cognitive tests as prognostic tools
	# for dementia, accounting for death without dementia competing risk.
	ROC.DSST
	ROC.MMSE
	compare(ROC.DSST,ROC.MMSE,adjusted=TRUE)
}



}

