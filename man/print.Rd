\name{print}
\alias{print.ipcwsurvivalROC}
\alias{print.ipcwcompetingrisksROC}
\alias{print.ipcwcompetingrisksSeSpPPVNPV}
\alias{print.ipcwsurvivalSeSpPPVNPV}

\title{Print objects in the timeROC library}
\description{
  Printing of objects created with the timeROC function
}
\usage{
\method{print}{ipcwsurvivalROC}(x, No.lines=5,digits=2, ...)
\method{print}{ipcwcompetingrisksROC}(x, No.lines=5,digits=2, ...)
\method{print}{ipcwsurvivalSeSpPPVNPV}(x, No.lines=5,digits=2, ...)
\method{print}{ipcwcompetingrisksSeSpPPVNPV}(x, No.lines=5,digits=2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Object of class "ipcwsurvivalROC", "ipcwcompetingrisksROC",
    "ipcwsurvivalSeSpPPVNPV" or "ipcwcompetingrisksSeSpPPVNPV".
  }
  \item{No.lines}{
   The (maximum) number of lines printed. Each line corresponds to a time point included in the vector \code{times} of the object \code{x}. 
   For example, if \code{No.lines=5}, then the function chooses the five time points corresponding to the quantiles of the vector  \code{times} of the object \code{x}. If \code{No.lines=10}, it chooses the deciles, etc...
  }
  \item{digits}{
   The number of significant digits. Default value is \code{digits = 2}.
  }
\item{\dots}{
  Not used.
} 
  
}

\details{
The print function recalls the sample size (after having removed missing data), the AUC estimates, and  the estimated standard errors (only if they have been estimated) for at maximum \code{No.lines} time points. In addition, it displays the frequencies of :
\itemize{
\item observed cases: subjects for which we know they undergo the (main) event prior the time "t" of interest.
\item survivors : event-free subjects at time "t" of interest.
\item censored subjects : censored subjects prior the time "t" of interest, for which we cannot know if they undergo an event or not prior time "t" (and so for which we cannot know if they are cases or controls at time "t").
\item other events:  (in the competing risks setting only) subjects for which we know that they undergo an event different from the main event prior the time "t" of interest.
}
Furthermore, the function recalls the method used to compute the inverse probability of censoring weights.
}


\author{
Paul Blanche \email{Paul.Blanche@isped.u-bordeaux2.fr}
}

\seealso{
\code{\link{timeROC}}, \code{\link{SeSpPPVNPV}}
}


\examples{
##-------------Without competing risks-------------------
data(pbc)
head(pbc)
pbc<-pbc[!is.na(pbc$trt),] # select only randomised subjects
pbc$status<-as.numeric(pbc$status==2) # create event indicator: 1 for death, 0 for censored

ROC.bili.cox<-timeROC(T=pbc$time,
                      delta=pbc$status,marker=pbc$bili,
                      other_markers=as.matrix(pbc[,c("chol","albumin")]),
                      cause=1,weighting="cox",
                      times=quantile(pbc$time,probs=seq(0.2,0.8,0.01)))

# prints descriptive statistics and AUC estimates (5,10 and 20 lines)
print(ROC.bili.cox)
print(ROC.bili.cox,No.lines=10)
print(ROC.bili.cox,No.lines=20,digits=1)

# Se, Sp, PPV and NPV computation for serum bilirunbin at threshold c=0.9(mg/dl) 
res.SeSpPPVNPV.bili <- SeSpPPVNPV(cutpoint=0.9,
                                  T=pbc$time,
                                  delta=pbc$status,marker=pbc$bili,
                                  other_markers=as.matrix(pbc[,c("chol","albumin")]),
                                  cause=1,weighting="cox",
                                  times=quantile(pbc$time,probs=seq(0.2,0.8,0.1)))

# prints descriptive statistics and Se, Sp, PPV and NPV
#  estimates for serum bilirunbin at threshold c=0.9(mg/dl) 
print(res.SeSpPPVNPV.bili,No.lines=20,digits=1)

ROC.bili.marginal<-timeROC(T=pbc$time,
                           delta=pbc$status,marker=pbc$bili,
                           cause=1,weighting="marginal",
                           times=quantile(pbc$time,probs=seq(0.1,0.9,0.2)),
                           iid=TRUE)

# prints descriptive statistics, AUC estimates and also standard errors
# of AUCs because weighting="marginal" and iid=TRUE were used.
print(ROC.bili.marginal)

##-------------With competing risks-------------------
data(Melano)
ROC.thick<-timeROC(T=Melano$time,delta=Melano$status,
                   marker=Melano$thick,cause=1,
                   weighting="marginal",
                   times=c(1500,2000,2500),iid=TRUE)

# prints descriptive statistics, AUC estimates and also standard errors
# of AUCs because weighting="marginal" and iid=TRUE were used.
print(ROC.thick)


# Se, Sp, PPV and NPV computation for tumor thickness at
#threshold c=3 (1/100 mm)
res.SeSpPPVNPV.thick <- SeSpPPVNPV(cutpoint=3,
                                  T=Melano$time,delta=Melano$status,
                                  weighting="marginal",
                                  marker=Melano$thick,cause=1,
                                  times=c(1800,2000,2200),
                                  iid=TRUE)
print(res.SeSpPPVNPV.thick,digits=1)
}
