\name{plot.ipcwcompetingrisksROC}
\alias{plot.ipcwcompetingrisksROC}

\title{
Plot function for time-dependent ROC curve
}
\description{
Plots time-dependent ROC curve estimate, in the competing risks setting.
}
\usage{
\method{plot}{ipcwcompetingrisksROC}(x, FP = 2, time, col = "red", add = FALSE, title = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class "ipcwcompetingrisksROC". The object \code{x} must have been estimated by the \code{\link{timeROC}} function with argument \code{ROC = TRUE} (default argument).
}
  \item{FP}{
A numeric value that indicates which definition of controls the ROC curve is plotted. \code{1} for definition \bold{(i)} and \code{2}  for definition \bold{(ii)}. (See details of \code{\link{timeROC}} function for definitions \bold{(i)} and \bold{(ii)}). Default is \code{FP = 2}.
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



