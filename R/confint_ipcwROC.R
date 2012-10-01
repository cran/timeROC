confint.ipcwsurvivalROC<-function(object,parm=NULL,level=0.95,...){

  if(object$iid==FALSE){  
    stop(paste("Confidence intervals cannot be computed because you have chosen iid=FALSE (default) in the input of timeROC(). \n",sep=""))}
  
  lower<-object$AUC*100-object$inference$vect_sd_1*100*qnorm(1-(1-level)/2)
  upper<-object$AUC*100+object$inference$vect_sd_1*100*qnorm(1-(1-level)/2)
  
  tab_AUC_1<-round(cbind(lower,upper),2)
  colnames(tab_AUC_1)<-c(paste(((1-level)/2)*100,"%",sep=""),paste((1-(1-level)/2)*100,"%",sep=""))
  
  return(CI_AUC_1=tab_AUC_1)
}



confint.ipcwcompetingrisksROC<-function(object,parm=NULL,level=0.95,...){
  
  if(object$iid==FALSE){  
  stop(paste("Confidence intervals cannot be computed because you have chosen iid=FALSE (default) in the input of timeROC(). \n",sep=""))}
  
  lower_1<-object$AUC_1*100-object$inference$vect_sd_1*100*qnorm(1-(1-level)/2)
  upper_1<-object$AUC_1*100+object$inference$vect_sd_1*100*qnorm(1-(1-level)/2)
  
  tab_AUC_1<-round(cbind(lower_1,upper_1),2)
  colnames(tab_AUC_1)<-c(paste(((1-level)/2)*100,"%",sep=""),paste((1-(1-level)/2)*100,"%",sep=""))
  
  lower_2<-object$AUC_2*100-object$inference$vect_sd_2*100*qnorm(1-(1-level)/2)
  upper_2<-object$AUC_2*100+object$inference$vect_sd_2*100*qnorm(1-(1-level)/2)
  
  
  tab_AUC_2<-round(cbind(lower_2,upper_2),2)
  colnames(tab_AUC_2)<-c(paste(((1-level)/2)*100,"%",sep=""),paste((1-(1-level)/2)*100,"%",sep=""))
  
  return(list(CI_AUC_1=tab_AUC_1, CI_AUC_2=tab_AUC_2))
  
}