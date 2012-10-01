\name{SeSpPPVNPV}
\alias{SeSpPPVNPV}
\title{
Time-dependent Sensitivity (Se), Specificity (Sp), Positive Predictive
Value (PPV) and Negative Predictive Value (NPV) estimation
}
\description{
  This function aim at estimating time-dependent Sensitivity (Se),
  Specificity (Sp), Positive Predictive Value (PPV) and Negative
  Predictive Value (NPV) at a given cutpoint.
  Standard error computation via iid-representation of the estimator is also implemented.
}
\usage{
SeSpPPVNPV(cutpoint, T, delta, marker, other_markers = NULL, cause,
           weighting = "marginal", times, iid = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cutpoint}{
The cutpoint for maker value at  which we aim at estimating Se, Sp, PPV and NPV.
}
  \item{T}{
The vector of (censored) event-times.
}
  \item{delta}{
The vector of event indicators at the corresponding value of the vector
\code{T}. Censored observations must be denoted by the value \code{0}.
}
  \item{marker}{
The vector of the marker values for which we want to compute the time-dependent ROC curves. 
Without loss of generality, the function assumes that larger values of
the marker are associated with higher risks of events. If lower values of the marker are
associated with higher risks of events, then reverse the association
adding a minus to the marker values.
}
  \item{other_markers}{
    A matrix that contains values of other markers that we want to take into
    account for computing the inverse probability of censoring
    weights. The different columns 
    represent the different markers. This argument is optional, and
    ignored if \code{method="marginal"}. Default value is \code{other_markers=NULL}.
}
  \item{cause}{
The value of the event indicator that represents the event of interest
for which we aim to compute the time-dependent ROC curve. Without
competing risks, it must be the value that indicates a non-censored
obsevation (usually \code{1}). With competing risks, subjects can
undergo different type of events; then, it must be the value
corresponding to the event of interest, for which we aim to compute the
ROC curve (usually \code{1} or \code{2}).
}
  \item{weighting}{
The method used to compute the weights. \code{weighting="marginal"} uses
the Kaplan-Meier estimator of the censoring distribution. \code{weighting="cox"} and
\code{weighting="aalen"} model the censoring by the Cox model and the
additive Aalen model respectively. Default value is \code{weighting="marginal"}.
}
  \item{times}{
The vector of times points "t" at which we want to compute the
time-dependent ROC curve. If vector \code{times} contains only a single
value, then value zero is added.
}
  \item{iid}{
A logical value that indicates if we want to compute the
iid-representation of the area under time-dependent ROC curve
estimator. \code{iid = TRUE} is required for computation of all
inference procedures (Confidence intervals or test for comparing AUCs).
For large sample size (greater than 2000, say) and/or large length of
vector \code{times}, the computation of the iid representations might be
time-consuming. 
}
}
\details{
This function computes Inverse Probability of Censoring Weighting (IPCW)
estimates of Sensitivity (Se), Specificity (Sp), Positive Predictive
Value (PPV) and Negative Predictive Value (NPV) for Cumulative/Dynamic
definition of cases and controls.

Let \eqn{T_i}{T_i} denote the event time of the subject \eqn{i}{i}.

\bold{Without competing risks :}  A case is defined as a subject \eqn{i}{i} with \eqn{T_i \leq t}{T_i <=t}. A control is defined as a subject \eqn{i}{i} with \eqn{T_i > t}{T_i > t}.


\bold{With competing risks :} In this setting, subjects may undergo different type of events, denoted by \eqn{\delta_i}{\delta_i} in the following. Let suppose that we are interested in the event \eqn{\delta_i=1}{\delta_i=1}. Then, a case is defined as a subject \eqn{i}{i} with \eqn{T_i \leq t}{T_i <=t} and \eqn{\delta_i=1}{\delta_i = 1}.
With competing risks, two definitions of controls were suggested: \bold{(i)} a control is defined as a subject \eqn{i}{i} that is free of any event, i.e with   \eqn{T_i > t}{T_i > t}, and \bold{(ii)} a control is defined as a subject \eqn{i}{i} that is not a case, i.e with \eqn{T_i > t}{T_i > t} or with \eqn{T_i \leq t}{T_i <=t} and \eqn{\delta_i \neq 1}{\delta_i != 1 }.
For all outputs of this package, objects named with \code{_1} refer to definition \bold{(i)}. For instance \code{AUC_1} or \code{se_1} refer to time-dependent area under the ROC curve and its estimated standard error according to the definition \bold{(i)}. Objects named with \code{_2} refer to definition \bold{(ii)} .
}
\value{
  Object of class "ipcwsurvivalSeSpPPVNPV" or "ipcwcompetingrisksSeSpPPVNPV", depending on if there is competing risk or not, that is a list. For these classes, there are print, plot and confint methods. Most objects that they contain are similar, but some are specific to each class.
  
  \bold{Specific objects of class "ipcwsurvivalSeSpPPVNPV" :}
  
  \itemize{
  \item \code{TP} : vector of time-dependent True Positive fraction
  (sensitivity) estimates at each time points.
  \item \code{FP} : vector of time-dependent False Positive fraction
  (1-specificity) estimates at each time points. 
  \item \code{PPV} : vector of time-dependent Positive Predictive Value
  estimates at each time points. 
  \item \code{NPV} : vector of time-dependent Negative Predictive Value
  estimates at each time points. 
  }
  
  \bold{Specific objects of class "ipcwcompetingrisksSeSpPPVNPV" :}
  
  \itemize{
  \item \code{TP} : vector of time-dependent True Positive fraction
  (sensitivity) estimates at each time points.
  \item \code{FP_1} : vector of time-dependent False Positive fraction
  (1-specificity) estimates at each time points with definition
  \bold{(i)} of controls (see Details).
  \item \code{FP_2} : vector of time-dependent False Positive fraction
  (1-specificity) estimates at each time points with definition
  \bold{(ii)} of controls (see Details).  
  \item \code{PPV_1} : vector of time-dependent Positive Predictive Value
  estimates at each time points with definition
  \bold{(i)} of controls (see Details).
  \item \code{NPV_2} : vector of time-dependent Negative Predictive Value
  estimates at each time points with definition
  \bold{(ii)} of controls (see Details).
  }

  \bold{Objects common to  both classes :}
    \itemize{  
  		\item \code{times} : the time points for which Se, Sp,
                PPV, etc.. were computed. 
 		\item \code{cutpoint} : the cutpoint for which Se, Sp,
                PPV, etc.. were computed. 
  		\item \code{weights} : a object of class "IPCW", containing all informations about the weights. See \code{\link[pec]{ipcw}} function of \code{pec} package.
  		\item \code{computation_time} : the total computation time.  
  		\item \code{Stats} : a matrix containing descriptive statistics at each time points (like numbers of observed cases or censored observations before each time points).
  		\item \code{iid} : the logical value of parameter \code{iid} used in argument.
  		\item \code{n} : the sample size, after having omitted missing vaues. 
  		\item \code{inference} : a list that contains, among other things, iid-representations and estimated standard errors of the estimators.
  		\item \code{computation_time} : the computation time, in seconds.
 	}    
}
\references{
  	Blanche, P., Dartigues, J. F., & Jacqmin-Gadda,	H. (2013). Estimating and comparing time-dependent areas under
	receiver operating characteristic curves for censored event times with competing risks. 
	Statistics in medicine, 32(30), 5381-5397.
}
\author{
Paul Blanche \email{pabl@sund.ku.dk}
}

\seealso{
\code{\link{timeROC}}
}
\examples{
##-------------Without competing risks-------------------
library(survival)
data(pbc)
head(pbc)
pbc<-pbc[!is.na(pbc$trt),] # select only randomised subjects
pbc$status<-as.numeric(pbc$status==2) # create event indicator: 1 for death, 0 for censored     
# Se, Sp, PPV and NPV computation for serum bilirunbin at threshold c=0.9(mg/dl) 
res.SeSpPPVNPV.bili <- SeSpPPVNPV(cutpoint=0.9,
                                  T=pbc$time,
                                  delta=pbc$status,marker=pbc$bili,
                                  cause=1,weighting="marginal",
                                  times=quantile(pbc$time,probs=seq(0.2,0.8,0.1)),
                                  iid=TRUE)
res.SeSpPPVNPV.bili

##-------------With competing risks-------------------

#---------Example with Paquid data--------
data(Paquid)
# Se, Sp, PPV and NPV computation for DSST at threshold c=22
res.SeSpPPVNPV.DSST <- SeSpPPVNPV(cutpoint=22,
                                  T=Paquid$time,
                                  delta=Paquid$status,marker=Paquid$DSST,
                                  cause=1,weighting="cox",
                                  times=c(3,5,8,10))
res.SeSpPPVNPV.DSST

#---------Example with Melano data-------
data(Melano)
# Se, Sp, PPV and NPV computation for tumor thickness at threshold c=3 (1/100 mm)
res.SeSpPPVNPV.thick <- SeSpPPVNPV(cutpoint=3,
                                  T=Melano$time,delta=Melano$status,
                                  weighting="marginal",
                                  marker=Melano$thick,cause=1,
                                  times=c(1800,2000,2200),
                                  iid=TRUE)
res.SeSpPPVNPV.thick
}
