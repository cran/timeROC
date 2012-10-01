\name{plot.ipcwsurvivalROC}
\alias{plot.ipcwsurvivalROC}

\title{
Plot function for time-dependent ROC curve
}
\description{
Plots time-dependent ROC curve estimate in the usual survival setting (without competing risks).
}
\usage{
\method{plot}{ipcwsurvivalROC}(x, time, col = "red", add = FALSE, title = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class "ipcwsurvivalROC". The object \code{x} must have been estimated by the \code{\link{timeROC}} function with argument \code{ROC = TRUE} (default argument).
}
  \item{time}{
A numeric value that indicates the time point at which the ROC curve is plotted.
}
  \item{col}{
The color to plot the ROC curve. Default is \code{col = "red"}.
}
  \item{add}{
A logical value that indicates if  you only want to  add the ROC curve estimate to a pre-existing plot. Default is \code{add = FALSE}.
}
  \item{title}{
A logical value that indicates if  you  want to  add a generic title, that indicates the chosen time point and the AUC estimate. Default is \code{title = TRUE}.
}
  \item{\dots}{
Arguments to be passed to plot method. (See \code{\link[graphics]{plot}}).
}
}



\author{
Paul Blanche \email{Paul.Blanche@isped.u-bordeaux2.fr}
}


\seealso{
\code{\link{timeROC}}
}


\examples{
data(pbc)
pbc<-pbc[!is.na(pbc$trt),] # select only randomised subjects
pbc$status<-as.numeric(pbc$status==2) # create event indicator: 1 for death, 0 for censored

# we evaluate bilirubin as a prognostic biomarker
#(weights computed using an additive Aalen model 
# with covariates bili, chol and albumin). 
ROC.bili.aalen<-timeROC(T=pbc$time,
                      delta=pbc$status,marker=pbc$bili,
                      other_markers=as.matrix(pbc[,c("chol","albumin")]),
                      cause=1,weighting="aalen",
                      times=c(1800,2000,2200))
#print estimates
ROC.bili.aalen
#plot the ROC curve
plot(ROC.bili.aalen,time=2000)

# we evaluate albumin and  cholesterol as a prognostic biomarker.
#(weights computed using an additive Aalen model
# with covariates bili, chol and albumin).
ROC.albu.aalen<-timeROC(T=pbc$time,
                        delta=pbc$status,marker=-pbc$albumin,
                        other_markers=as.matrix(pbc[,c("chol","bili")]),
                        cause=1,weighting="aalen",
                        times=c(1800,2000,2200))

ROC.chol.aalen<-timeROC(T=pbc$time,
                        delta=pbc$status,marker=pbc$chol,
                        other_markers=as.matrix(pbc[,c("bili","albumin")]),
                        cause=1,weighting="aalen",
                        times=c(1800,2000,2200))
# print estimates
ROC.albu.aalen
ROC.chol.aalen

# plot all ROC curves
plot(ROC.bili.aalen,time=2000,lwd=2,title=FALSE)
plot(ROC.albu.aalen,time=2000,col="blue",add=TRUE,lwd=2,lty=2)
plot(ROC.chol.aalen,time=2000,col="black",add=TRUE,lwd=2,lty=3)
# add legend
legend("bottomright",c("bilirubin","albumin","cholesterol"),
       col=c("red","blue","black"),lty=1:3)
}



