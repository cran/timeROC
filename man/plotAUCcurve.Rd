\name{plotAUCcurve}
\alias{plotAUCcurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot time-dependent AUC curve
}
\description{
  This function plots the curve of time-dependent-AUC: AUC(t) versus t.
  Pointwise and simultaneous confidence bands for this curve can also be plotted
  when inverse probability of censoring weights are computed from a Kaplan-Meier estimator. 
}
\usage{
plotAUCcurve(object, FP = 2, add = FALSE, conf.int = FALSE,
             conf.band = FALSE, col = "black")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An object of class "ipcwsurvivalROC" or "ipcwcompetingrisksROC"
previously estimated from the \code{\link{timeROC}} function.
}
\item{FP}{
  In the competing risks setting, a numeric value that indicates which
  definition of AUC is plotted. \code{1} for definition \bold{(i)} and
  \code{2}  for definition \bold{(ii)}. 
  (See details of \code{\link{timeROC}} function for definitions \bold{(i)} and \bold{(ii)}). Default is \code{FP = 2}.
}
  \item{add}{
A logical value that indicates if  you want to  add the AUC curve to a pre-existing plot. Default is \code{add = FALSE}.
}
  \item{conf.int}{
A logical value that indicates whether or not you want to plot the bands
of pointwise confidence intervals. Default is  \code{conf.int = FALSE}.
For choosing \code{conf.int = TRUE}, the object \code{object} must have been estimated by the \code{\link{timeROC}} function with arguments \code{weighting="marginal"} and \code{iid = TRUE}.
}
  \item{conf.band}{
A logical value that indicates whether or not you want to plot the simultaneous confidence bands. Default is  \code{conf.band = FALSE}.
For choosing \code{conf.band = TRUE}, the object \code{object} must have been estimated by the \code{\link{timeROC}} function with arguments \code{weighting="marginal"} and \code{iid = TRUE}.
}
  \item{col}{
The color to plot the AUC curve. Default is \code{col = "black"}.
}
}

\references{
	Blanche P., Dartigues J-F and Jacqmin-Gadda H. (2013). Estimating and Comparing time-dependent areas under ROC curves
	for censored event times with competing risks. Submitted.
	
	Hung, H. and Chiang, C. (2010). Estimation methods for time-dependent AUC with survival data.
	Canadian Journal of Statistics, 38(1):8-26
}
\author{
Paul Blanche \email{Paul.Blanche@isped.u-bordeaux2.fr}
}

\seealso{
  \itemize{
  \item \code{\link[=confint.ipcwsurvivalROC]{confint}} for confidence intervals and confidence bands computation of time-dependentAUC.
  \item \code{\link{plotAUCcurveDiff}} for plotting  the curve of the
  difference of two time-dependent AUCs over time 
  with eventually confidence intervals and
  simultaneous confidence bands.
 }
}
\examples{

\dontrun{
## computation times is roughly 10 seconds

##-------------Without competing risks-------------------
data(pbc)
head(pbc)
pbc<-pbc[!is.na(pbc$trt),] # select only randomised subjects
pbc$status<-as.numeric(pbc$status==2) # create event indicator: 1 for death, 0 for censored     
# we evaluate bilirubin as a prognostic biomarker for death.
ROC.bili<-timeROC(T=pbc$time,
                  delta=pbc$status,marker=pbc$bili,
                  cause=1,weighting="marginal",
                  times=quantile(pbc$time,probs=seq(0.2,0.8,0.02)),
                  iid=TRUE)
ROC.bili    
# we evaluate bilirubin as a prognostic biomarker for death.
ROC.albumin<-timeROC(T=pbc$time,
                     delta=pbc$status,marker=-pbc$albumin,
                     cause=1,weighting="marginal",
                     times=quantile(pbc$time,probs=seq(0.2,0.8,0.02)),
                     iid=TRUE)
ROC.albumin
# plot AUC curve for albumin only with pointwise confidence intervals
# and simultaneous confidence bands
plotAUCcurve(ROC.albumin,conf.int=TRUE,conf.band=TRUE)
# plot AUC curve for albumin and bilirunbin  with pointwise confidence intervals
plotAUCcurve(ROC.albumin,conf.int=TRUE,col="red")
plotAUCcurve(ROC.bili,conf.int=TRUE,col="blue",add=TRUE)
legend("bottomright",c("albumin","bilirunbin"),col=c("red","blue"),lty=1,lwd=2)


##-------------With competing risks-------------------     
data(Melano)
head(Melano)     
# Evaluate tumor thickness as a prognostic biomarker for
# death from malignant melanoma.
ROC.thick<-timeROC(T=Melano$time,delta=Melano$status,
                   marker=Melano$thick,cause=1,
                   times=quantile(Melano$time,probs=seq(0.2,0.8,0.01)),
                   iid=TRUE)
ROC.thick
# plot AUC curve for tumor thickness with pointwise confidence intervals
# and simultaneous confidence bands
plotAUCcurve(ROC.thick,FP=2,conf.int=TRUE,conf.band=TRUE)
}
}
