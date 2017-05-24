#'@title Estimates bias correction of interannual variability
#'@description \code{esimate.bias} returns the bias correction of the variance of the generated annual rainfall.
#'@details Stochastic weather generators tend to under- or overestimate interannual variability of the generated data. To face this problem, a bias correction
#'of the variance of the generated annual rainfall is needed. The bias is estimates iteratively:
#'Data is generated with zero bias correction in the first step. In each step, the bias correction is adjusted by the difference of the variance of generated data and historical data.
#'@param data a data frame (historical data set) with 4 columns that contain numeric values only. Column names: \code{year}, \code{month}, \code{day}, \code{pre} (precipiation in mm).
#'@param models model formulas as a character strings in a list (list members: $wet,$amount). See: \code{estimate.models}
#'@param iterations number of iterations (default: 15). The minimum recommended number of iterations is 10.
#'@param nyears number of years to simulate in each interation step (default: 1000). The minimum recommended number of years is 500.
#'@return returns model formulas as a character string in a list (list members: $wet,$amount). The list can be used directly as input
#'for the functions \code{estimate.bias} and \code{raingen}.
#'@examples estimate.models(eneabbadata)
#'estimate.bias(eneabbadata)
#'@seealso link to my documentation
#'
#'
#'
#'
#'@export

estimate.bias<-function(data,models,iterations=15,nyears=1000){
  
  options(warn=1)
  checkval<-T
  if(is.data.frame(data)){
    if(length(names(data))>3){
      if(!any(c("year","day","month","pre")  %in% names(data) ==F)){
        if(length(unique(data$year))>10){
          
        }else{
          warning("data record should be a least 10 years")
          checkval<-F
        }
      }else{
        warning("columns not named correctly")
        checkval<-F
      }
    }else{
      warning("number of columns of data frame is less than 4")
      checkval<-F
    }
    
  }else{
    warning("data is no data frame")
    checkval<-F
  }
  
  ###error messages and decition
  
  if(checkval==F){
    warning("Please provide data in the following way:\n
            - Data frame with 4 columns, numeric values only.\n
            - Column names: `year', `month', `day', `pre'. (pre=daily
            precipitation amount)")
  }
  
  if(checkval==F){
    if(menu(c("Yes", "No"), title="Do you still want to continue?")==1){
      print("ok! continue...")
    }else
    {
      stop('Manually stopped by user.')
    }
  }
  
  options(warn=0)
  
  source("raingen.R")
  
  vBias<-rep(NA,iterations+1)
  
  bias<-0
  vBias[1]<-0
  
  for(i in 1:iterations){
    
    raingen.obj<-raingen(data,nyears,bias=bias,models=models)
    y0<-sd(raingen.obj$raw.annual)
    y1<-sd(raingen.obj$annual.output)
    
    bias<-y0-y1+bias
    vBias[i+1]<-bias
  }
  
  return(mean(vBias[ceiling(iterations/3):iterations+1]))
}
