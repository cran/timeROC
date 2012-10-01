\name{plot}
\alias{plot.ipcwsurvivalROC}
\alias{plot.ipcwcompetingrisksROC}

\title{
Plot function for time-dependent ROC curve
}
\description{
This function plots time-dependent ROC curve estimate.
}
\usage{
\method{plot}{ipcwsurvivalROC}(x, time, col = "red", add = FALSE, title = TRUE, ...)
\method{plot}{ipcwcompetingrisksROC}(x, FP = 2, time, col = "red", add = FALSE, title = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class "ipcwcompetingrisksROC". The object \code{x} must have been estimated by the \code{\link{timeROC}} function with argument \code{ROC = TRUE} (default argument).
}
  \item{FP}{
A numeric value that indicates which definition of controls the ROC
curve is plotted in the competing risks setting. \code{1} for definition
\bold{(i)} and \code{2}  for definition \bold{(ii)}. See details of
\code{\link{timeROC}} function for definitions \bold{(i)} and
\bold{(ii)}.
Default is \code{FP = 2} and this argument is ignored when there is no
competing risks.
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
##-------------Without competing risks----------------

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
#plot the ROC curve  at time t=2000
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

# plot all ROC curves at time t=2000
plot(ROC.bili.aalen,time=2000,lwd=2,title=FALSE)
plot(ROC.albu.aalen,time=2000,col="blue",add=TRUE,lwd=2,lty=2)
plot(ROC.chol.aalen,time=2000,col="black",add=TRUE,lwd=2,lty=3)
# add legend
legend("bottomright",c("bilirubin","albumin","cholesterol"),
       col=c("red","blue","black"),lty=1:3)



##-------------With competing risks-------------------

#---------Example with Melano data------------- 
data(Melano)

# Evaluate tumor thickness as a prognostic biomarker for
# death from malignant melanoma.
ROC.thick<-timeROC(T=Melano$time,delta=Melano$status,
                   weighting="aalen",
                   marker=Melano$thick,cause=1,
                   times=c(1800,2000,2200))
plot(ROC.thick,time=1800)

#---------Example with Paquid data------------- 
data(Paquid)

# evaluate DDST cognitive score as a prognostic tool for
# dementia onset, accounting for death without dementia competing risk.
ROC.DSST<-timeROC(T=Paquid$time,delta=Paquid$status,
                  marker=-Paquid$DSST,cause=1,
                  weighting="cox",
                  other_markers=as.matrix(Paquid$MMSE),
                  times=c(3,5,10),ROC=TRUE)

# we compare MMSE and DSST cognitive tests as prognostic tools
# for dementia, accounting for death without dementia competing risk.
ROC.MMSE<-timeROC(T=Paquid$time,delta=Paquid$status,
                  marker=-Paquid$MMSE,cause=1,
                  weighting="cox",
                  other_markers=as.matrix(Paquid$DSST),
                  times=c(3,5,10),ROC=TRUE)

plot(ROC.DSST,time=5,title=FALSE,lwd=2)
plot(ROC.MMSE,time=5,col="blue",add=TRUE,title=FALSE,lwd=2)
legend("right",c("DSST","MMSE"),col=c("red","blue"),lwd=2)
}



