\name{plotAUCcurveDiff}
\alias{plotAUCcurveDiff}

\title{
  Plot the curve of the difference of two time-dependent AUCs over time
}
\description{
 This function plots the curve of the difference of two time-dependent
  AUCs over time.  Pointwise and simultaneous confidence bands for this curve can also be plotted
  when inverse probability of censoring weights are computed from a Kaplan-Meier estimator. 
}
\usage{
plotAUCcurveDiff(object1, object2, FP = 2, add = FALSE, conf.int = FALSE,
                 conf.band = FALSE, col = "black", ylim = c(-0.5, 0.5))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object1}{
An object of class "ipcwsurvivalROC" or "ipcwcompetingrisksROC"
previously estimated from the \code{\link{timeROC}} function.
}
  \item{object2}{
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
For choosing \code{conf.int = TRUE}, the objects \code{object1} and
\code{object2} must have been estimated by 
the \code{\link{timeROC}} function with arguments \code{weighting="marginal"} and \code{iid = TRUE}.
}
  \item{conf.band}{
A logical value that indicates whether or not you want to plot the simultaneous confidence bands. Default is  \code{conf.band = FALSE}.
For choosing \code{conf.band = TRUE}, the objects \code{object1} and
\code{object2} must have been estimated by the \code{\link{timeROC}} function with arguments \code{weighting="marginal"} and \code{iid = TRUE}.
}
  \item{col}{
The color to plot the AUC curve. Default is \code{col = "black"}.
}
  \item{ylim}{
The range of the y-axis.  Default is \code{ylim = c(-0.5,0.5)}.
}
}
\details{
Simultaneous confidence bands can be of particular interest for testing
null hypotheses such as "for all time t within an interval, AUC(t) for
 both markers are equal", by observing whether or not the zero line is contained within the band.
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
\note{
The two markers evluated in objects \code{object1} and
\code{object2} must have been measured on the same subjects.
}


\seealso{
  \itemize{
  \item \code{\link[=confint.ipcwsurvivalROC]{confint}} for confidence intervals  and confidence bands computation  of  time-dependentAUC.
  \item \code{\link{plotAUCcurve}} for plotting  the curve of time-dependent-AUC: AUC(t) versus t.  Confidence intervals and simultaneous confidence bands can also be plotted.
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
# plot AUC curve for albumin and bilirunbin  with pointwise confidence interval
plotAUCcurve(ROC.albumin,conf.int=TRUE,col="red")
plotAUCcurve(ROC.bili,conf.int=TRUE,col="blue",add=TRUE)
legend("bottomright",c("albumin","bilirunbin"),col=c("red","blue"),lty=1,lwd=2)
#plot the curve of the difference of the two time-dependent AUCs over time
plotAUCcurveDiff(ROC.bili,ROC.albumin,conf.int=TRUE,conf.band=TRUE,ylim=c(-0.2,0.5))

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
ROC.age<-timeROC(T=Melano$time,delta=Melano$status,
                   marker=Melano$age,cause=1,
                   times=quantile(Melano$time,probs=seq(0.2,0.8,0.01)),
                   iid=TRUE)
ROC.age
# plot the two AUC curves on the same plot
plotAUCcurve(ROC.thick,FP=2,conf.int=TRUE,col="blue")
plotAUCcurve(ROC.age,FP=2,conf.int=TRUE,col="red",add=TRUE)
legend("bottomright",c("thickness","age"),col=c("blue","red"),lty=1,lwd=2)
# plot the curve of the difference of the two time-dependent AUCs over time
plotAUCcurveDiff(ROC.thick,ROC.age,FP=2,conf.int=TRUE,conf.band=TRUE,col="red")
}
}
