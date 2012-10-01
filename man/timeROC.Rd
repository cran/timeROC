\name{timeROC}
\alias{timeROC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Time-dependent ROC curve estimation
}
\description{
Inverse Probability of Censoring Weighting (IPCW) estimation of Cumulative/Dynamic time-dependent ROC curve. The function works in the usual survival setting as well as in the competing risks setting. Computation of the iid-representation of areas under time-dependent ROC curves is implemented. This enables computation of inference procedures: Confidence intervals and tests for comparing two AUCs of two different markers measured on the same subjects.
}
\usage{
timeROC(T, delta, marker, other_markers = NULL, cause,
	    weighting = "marginal", times, ROC = TRUE, iid = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T}{
The vector of (censored) event-times.
}
  \item{delta}{
The vector of event indicators at the corresponding value of the vector \code{T}. Censored observations must be denoted by the value \code{0}.
}
  \item{marker}{
The vector of the marker values for which we want to compute the time-dependent ROC curves. 
Without loss of generality, the function assumes that larger values of the marker are associated with higher risks of events. If lower values of the marker are associated with higher risks of events, then reverse the association adding a minus to the marker values.
}
  \item{other_markers}{
A matrix that contains values of other markers that we want to take into account for computing the inverse probability of censoring weights. The different columns represent the different markers. This argument is optional, and ignored if \code{method="marginal"}. Default value is \code{other_markers=NULL}.
}
  \item{cause}{
The value of the event indicator that represents the event of interest for which we aim to compute the time-dependent ROC curve. Without competing risks, it must be the value that indicates a non-censored obsevation (usually \code{1}). With competing risks, subjects can undergo different type of events; then, it must be the value corresponding to the event of interest, for which we aim to compute the ROC curve (usually \code{1} or \code{2}).
}
  \item{weighting}{
The method used to compute the weights. \code{weighting="marginal"} uses the Kaplan-Meier estimator of the censoring distribution. \code{weighting="cox"} and \code{weighting="aalen"} model the censoring by the Cox model and the additive Aalen model respectively. Default value is \code{weighting="marginal"}.
}
  \item{times}{
The vector of times points "t" at which we want to compute the time-dependent ROC curve. If vector \code{times} contains only a single value, then value zero is added.
}
  \item{ROC}{
A logical value that indicates if we want to save the estimates of sensitivities and specificties. Default value is \code{ROC = TRUE}.}
  \item{iid}{
A logical value that indicates if we want to compute the iid-representation of the area under time-dependent ROC curve estimator. \code{iid = TRUE} is required for computation of all inference procedures (Confidence intervals or test for comparing AUCs). For large sample size (greater than 2000, say) and/or large length of vector \code{times}, the computation of the iid representations might be time-consuming.  Default value is \code{iid = FALSE}.}
}


\details{
This function computes Inverse Probability of Censoring Weighting (IPCW) estimates of Cumulative/Dynamic time-dependent ROC curve. 
By definition, time-dependent ROC curve intrinsically depends on the definitions of time-dependent cases and controls.\cr
Let \eqn{T_i}{T_i} denote the event time of the subject \eqn{i}{i}.

\bold{Without competing risks :}  A case is defined as a subject \eqn{i}{i} with \eqn{T_i \leq t}{T_i <=t}. A control is defined as a subject \eqn{i}{i} with \eqn{T_i > t}{T_i > t}.


\bold{With competing risks :} In this setting, subjects may undergo different type of events, denoted by \eqn{\delta_i}{\delta_i} in the following. Let suppose that we are interested in the event \eqn{\delta_i=1}{\delta_i=1}. Then, a case is defined as a subject \eqn{i}{i} with \eqn{T_i \leq t}{T_i <=t} and \eqn{\delta_i=1}{\delta_i = 1}.
With competing risks, two definitions of controls were suggested: \bold{(i)} a control is defined as a subject \eqn{i}{i} that is free of any event, i.e with   \eqn{T_i > t}{T_i > t}, and \bold{(ii)} a control is defined as a subject \eqn{i}{i} that is not a case, i.e with \eqn{T_i > t}{T_i > t} or with \eqn{T_i \leq t}{T_i <=t} and \eqn{\delta_i \neq 1}{\delta_i != 1 }.
For all outputs of this package, objects named with \code{_1} refer to definition \bold{(i)}. For instance \code{AUC_1} or \code{se_1} refer to time-dependent area under the ROC curve and its estimated standard error according to the definition \bold{(i)}. Objects named with \code{_2} refer to definition \bold{(ii)} .

}


\value{
  Object of class "ipcwsurvivalROC" or "ipcwcompetingrisksROC", depending on if there is competing risk or not, that is a list. For these classes, there are print, plot and confint methods. Most objects that they contain are similar, but some are specific to each class.
  
  \bold{Specific objects of class "ipcwsurvivalROC" :}
  
  \itemize{
  \item \code{AUC} : vector of time-dependent AUC estimates at each time points.
  \item \code{TP} : matrix of time-dependent True Positive fraction (sensitivity) estimates. 
  \item \code{FP} : matrix of time-dependent False Positive fraction (1-specificity) estimates. 
  }
  
  \bold{Specific objects of class "ipcwcompetingrisksROC" :}
    \itemize{
    		\item \code{AUC_1} : vector of time-dependent AUC estimates at each time points with definition \bold{(i)} of controls (see Details). 
    		\item \code{AUC_2} : vector of time-dependent AUC estimates at each time points with definition \bold{(ii)}  of controls (see Details).  
  		\item \code{TP} : matrix of time-dependent True Positive fraction (sensitivity) estimates. 
  		\item \code{FP_1} : matrix of time-dependent False Positive fraction (1-specificity) estimates with definition \bold{(i)} of controls (see Details). 
  		\item \code{FP_2} : matrix of time-dependent False Positive fraction (1-specificity) estimates with definition \bold{(ii)}  of controls (see Details). 
  	}
  
  \bold{Objects common to  both classes :}
    \itemize{  
  		\item \code{times} : the time points for which the time-dependent ROC curves were computed. 
  		\item \code{weights} : a object of class "IPCW", containing all informations about the weights. See \code{\link[pec]{ipcw}} function of \code{pec} package.
  		\item \code{computation_time} : the total computation time.  
  		\item \code{CumulativeIncidence} : the vector of estimated probabilities of being a case at each time points.
  		\item \code{survProb} : the vector of estimated probabilities of being event-free at each time points.
  		\item \code{Stats} : a matrix containing descriptive statistics at each time points (like numbers of observed cases or censored observations before each time points).
  		\item \code{iid} : the logical value of parameter \code{iid} used in argument.
  		\item \code{n} : the sample size, after having omitted missing vaues. 
  		\item \code{inference} : a list that contains, among other things, iid-representations and estimated standard errors of the estimators, and that is used for computation of comparison tests and confidence intervals.
  		\item \code{computation_time} : the computation time, in seconds.
 	}
  
  
    
}
\references{
	Hung, H. and Chiang, C. (2010). Estimation methods for time-dependent AUC with survival data.
	Canadian Journal of Statistics, 38(1):8-26

	Uno, H., Cai, T., Tian, L. and Wei, L. (2007). Evaluating prediction rules for t-years survivors with censored regression models.
	Journal of the American Statistical Association, 102(478):527-537.

	Blanche P., Dartigues J-F and Jacqmin-Gadda H. (2012). Estimating and Comparing time-dependent areas under ROC curves
	for censored event times with competing risks. Submitted. 	
}
\author{
Paul Blanche \email{Paul.Blanche@isped.u-bordeaux2.fr}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

  \itemize{
  \item \code{\link{print.ipcwsurvivalROC}} or \code{\link{print.ipcwcompetingrisksROC}} for what the print method does.
  \item \code{\link{compare}} for testing a difference of time-dependent AUCs.
  \item \code{\link{confint.ipcwsurvivalROC}} and \code{\link{confint.ipcwcompetingrisksROC}} for confidence intervals of  time-dependent AUC.
  \item \code{\link{plot.ipcwsurvivalROC}} and \code{\link{plot.ipcwcompetingrisksROC}} for plotting  time-dependent ROC curves.
  }
}



\examples{

##-------------Without competing risks-------------------

data(pbc)
head(pbc)
pbc<-pbc[!is.na(pbc$trt),] # select only randomised subjects
pbc$status<-as.numeric(pbc$status==2) # create event indicator: 1 for death, 0 for censored

# we evaluate bilirubin as a prognostic biomarker for death.

# 1) with the Kaplan-Meier estimator for computing the weights (default).
ROC.bili.marginal<-timeROC(T=pbc$time,
                  delta=pbc$status,marker=pbc$bili,
                  cause=1,weighting="marginal",
                  times=quantile(pbc$time,probs=seq(0.2,0.8,0.1)),
                  iid=TRUE)
ROC.bili.marginal

# 2) with a Cox model (with covariates bili, chol and albumin) for computing the weights.
ROC.bili.cox<-timeROC(T=pbc$time,
                      delta=pbc$status,marker=pbc$bili,
                      other_markers=as.matrix(pbc[,c("chol","albumin")]),
                      cause=1,weighting="cox",
                      times=quantile(pbc$time,probs=seq(0.2,0.8,0.1)))
ROC.bili.cox

##-------------With competing risks-------------------


#---------Example with Melano data-------
data(Melano)

# Evaluate tumor thickness as a prognostic biomarker for
# death from malignant melanoma.
ROC.thick<-timeROC(T=Melano$time,delta=Melano$status,
                   weighting="aalen",
                   marker=Melano$thick,cause=1,
                   times=c(1800,2000,2200))
ROC.thick

#---------Example with Paquid data--------
data(Paquid)

# evaluate DDST cognitive score as a prognostic tool for
# dementia onset, accounting for death without dementia competing risk.
ROC.DSST<-timeROC(T=Paquid$time,delta=Paquid$status,
                  marker=-Paquid$DSST,cause=1,
                  weighting="cox",
                  other_markers=as.matrix(Paquid$MMSE),
                  times=c(3,5,10),ROC=TRUE)
ROC.DSST 
plot(ROC.DSST,time=5)        
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{survival}% 
\keyword{ROC}% 
