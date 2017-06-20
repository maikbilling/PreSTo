PreSTo<-function(path,data,nyears,ccvec=0,mode=0){
  dev.off() # delete all plots
  rm(list=ls())# delete everything in the list
  
  setwd(path)
  source("raingen.R")
  source("raingen.summary.R")
  source("estimate.models.R")
  source("estimate.bias.R")
  
  ### estimate best models
  best.models<-estimate.models(data)
  
  ### estimate bias
  bias<-estimate.bias(data,models=best.models)
  #bias<--24.08281 #for Eneabba data, use "estimate.bias" for other sites! See documentation
  
  raingen.list<-raingen(data,nyears,cc.vec=ccvec,mode=mode,bias=bias,sound=F,models=best.models)
  summary<-raingen.summary(raingen.list)
  
  return(list("raingen.list"=raingen.list,"summary"=summary))
}