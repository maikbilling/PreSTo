#'@title Estimates best fitting models
#'@description \code{esimate.models} returns well fitting predictors for the rainfall occurance and amount model by using a stepwise model selection based on the Bayesian information criterion (BIC).
#'@details NameofGenerator generates daily precipiation data according to the approach of Stern and Coe [1]. This approach uses
#'two GLMs: (1) a model for the occurrence of dry and wet days and (2) a model to predict the rainfall amount on wet days.
#'Predictors for both models may vary from site to site and thus for each dataset. Therefore, it is recommended to determine the model
#'predictors before using other functions like code{estimate.bias} or code{raingen}.
#'Possible predictors include:
#'monthly precipitation, square root of monthly precipitation, their interaction, the number of wet days in month (only for precipitation amount) and their interaction with the monthly precipitation.
#'@param data a data frame (historical data set) with 4 columns that contain numeric values only.
#'Column names: \code{year}, \code{month}, \code{day}, \code{pre} (precipiation in mm).
#'@return returns model formulas as a character string in a list (list members: $wet,$amount). The list can be used directly as input
#'for the functions \code{estimate.bias} and \code{raingen}.
#'@examples estimate.models(eneabbadata)
#'@references [1] R. Coe R. D. Stern. A model fitting analysis of daily rainfall data. Journal of the Royal Statistical Society. Series A (General), 147(1):1-34, 1984.
#'@seealso link to my documentation
#'
#'
#'
#'
#'@export

estimate.models<-function(data){
  
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

  
  require(VGAM)
  require(zoo)
  require(fitdistrplus)
  require(MASS)
  
  options(warn=-1)
  
  ###### create predictors for models ######
  
  #create annual rainfall
  annual.pre<- aggregate(data$pre, list(data$year), sum, na.rm=T) 
  names(annual.pre)<-c("year","annual.pre")
  #create new column with annual rain
  data<-merge(data, annual.pre,by.x = "year", by.y = "year")
  #create new column with rain amount of yesterday)
  data$pre.yday<-c(NA,data$pre[1:(dim(data)[1]-1)])
  #create new column with day of year
  data$doy <- strptime(paste(data$year,data$month,data$day,sep="-"),format="%Y-%m-%d")$yday
  #create new column with wet (1) or dry (0)
  data$wet<-data$pre>0
  data$wet[data$wet]<-1
  #create new column with wet (1) or dry (0) for yesterday
  data$wet.yday<-c(NA,data$wet[1:(dim(data)[1]-1)])
  data$wet.yday2<-c(NA,NA,data$wet[1:(dim(data)[1]-2)])
  
  #create new column with only rain amount >0
  data$pre.glm<-data$pre
  data$pre.glm[data$pre.glm==0]<-NA
  #create monthly rainfall
  data.sum.month<-aggregate(data$pre, list(month = data$month,year=data$year), sum, na.rm=TRUE, na.action=NULL)
  data.sum.month$wet.days.month<-aggregate(data$wet, list(month = data$month,year=data$year),FUN=sum,na.rm=TRUE, na.action=NULL)$x
  names(data.sum.month)<-c("month","year","monthly.pre","wet.days.month")
  data<-merge(data, data.sum.month,by.x = c("year","month"), by.y = c("year","month"))
  data.sum.month<-merge(data.sum.month, annual.pre,by.x = "year", by.y = "year")
  data.sum.month$month <- as.character(data.sum.month$month)
  
  data.sum.month$mean.pre<-aggregate(data$pre.glm, list(month = data$month,year=data$year),FUN=mean,na.rm=TRUE, na.action=NULL)$x
  data.sum.month$mean.pre[is.na(data.sum.month$mean.pre)] <- 0
  
  #create annual cathegories
  cat.th<-quantile(annual.pre$annual.pre, c(.25, .75))
  data$annual.cat[data$annual.pre<cat.th[[1]]]<-"dry"
  data$annual.cat[data$annual.pre>=cat.th[[1]] & data$annual.pre<cat.th[[2]]]<-"intermediate"
  data$annual.cat[data$annual.pre>=cat.th[[2]]]<-"wet"
  
  data$month.char <- as.character(data$month)
  
  data$monthly.pre.pw2<-data$monthly.pre^2
  data$monthly.pre.pw3<-data$monthly.pre^3
  data$monthly.pre.pw4<-data$monthly.pre^4
  
  data$monthly.pre.sqrt<-data$monthly.pre^(0.5)
  
  
  ### get best distribution
  
  ###estimate distribution of precipitation amount on wet days
  ###requires zoo and fitdistrplus package
  pre.amounts<-data$pre.glm[which(data$wet==T)]
  distr.txt<-c("gamma","lnorm","weibull") #distribution names
  nbins<-round(2*(length(pre.amounts))^(2/5)) #see paper ? for best number of bins for chisq test
  h <- hist(pre.amounts,breaks=nbins,plot=F) #create histogram of pre. amount data
  dist.table<-data.frame(distribution=distr.txt,chisq=NA,estimate1=NA,estimate2=NA) #create empty dataframe
  
  for(i in 1:length(distr.txt)){ #for all distributions
    fit <- fitdist(pre.amounts, distr.txt[i]) #fit distribution to data
    breaks<-switch(distr.txt[i], #calculate breaks in histrogram for a specific distribution
                   "gamma" = pgamma(h$breaks, shape=fit$estimate[[1]], scale=1/fit$estimate[[2]]),
                   "lnorm" = plnorm(h$breaks,meanlog=fit$estimate[[1]],sdlog=fit$estimate[[2]]),
                   "weibull" = pweibull(h$breaks,shape=fit$estimate[[1]],scale=fit$estimate[[2]]))
    null.probs <- rollapply(breaks, 2, function(x) x[2]-x[1]) #compute difference of each break to get probability for each "bucket" in histogram
    chi <- chisq.test(h$counts, p=null.probs, rescale.p=TRUE, simulate.p.value=TRUE) #perform chi-square test
    dist.table$chisq[i]<-chi$statistic[[1]] #write result of chi-square test in dist.table
    dist.table$estimate1[i]<-fit$estimate[[1]]
    dist.table$estimate2[i]<-fit$estimate[[2]]
  }
  
  dist.table
  best.distr<-as.character(dist.table$distribution[which(dist.table$chisq==min(dist.table$chisq))]) #best distrib. = lowest chi square value
  
  
  #plot(data.sum.month$wet.days.month,data.sum.month$mean.pre)
  
  ###### model section ######
  
  
  ### wetness ###
  w<-2*pi/12
  
  mod.wet.data<-data
  mod.wet.data<-mod.wet.data[complete.cases(mod.wet.data[,c("wet","wet.yday","monthly.pre","wet.yday2")]),]
  
  frm.wet.null="wet ~ monthly.pre"
  
  frm.wet.full="wet ~ monthly.pre+
                      monthly.pre.sqrt+
                      monthly.pre:monthly.pre.sqrt"
  
  
  mod.wet.null <- glm(as.formula(frm.wet.null),family=binomial,data=mod.wet.data)
  mod.wet.full <- glm(as.formula(frm.wet.full),family=binomial,data=mod.wet.data)
  
  n<-length(which(!is.na(mod.wet.data$wet)))
  
  steped.wet<-step(mod.wet.null, scope = list(lower=mod.wet.null,upper=mod.wet.full),direction="both",k=log(n))
  best.wet<-Reduce(paste, deparse(steped.wet$formula))

  
  ### precipitation amount ###
  
  x<-c("monthly.pre","monthly.pre.sqrt","wet.days.month",
       "wet.days.month:monthly.pre")
  myvec<-t(combn(x, 1, paste0, collapse = "+"))[1,]
  for(i in 2:length(x)){
    myvec<-c(myvec,t(combn(x, i, paste0, collapse = "+"))[1,])
    }
  #myvec<-c("monthly.pre+monthly.pre.sqrt+wet.days.month+wet.days.month:monthly.pre+monthly.pre:monthly.pre.sqrt")
  df.best.fit.amount<-data.frame(formular=myvec,BIC=NA)
  
  
  if(best.distr=="gamma"){
    for(i in 1:length(myvec)){
      model<-glm(as.formula(paste("pre.glm~", df.best.fit.amount$formular[i], sep = "")),family=Gamma,data=data)
      df.best.fit.amount$BIC[i]<-BIC(model)
    }
  }
  if(best.distr=="lnorm"){
    for(i in 1:length(myvec)){
      model<-glm(as.formula(paste("pre.glm~", df.best.fit.amount$formular[i], sep = "")),family=gaussian(link="log"),data=data)
      df.best.fit.amount$BIC[i]<-BIC(model)
    }
  }
  if(best.distr=="weibull"){
    for(i in 1:length(myvec)){
      model<-vglm(as.formula(paste("pre.glm~", df.best.fit.amount$formular[i], sep = "")), family = weibullR, data = data)
      df.best.fit.amount$BIC[i]<-BIC(model)
      #print(BIC(model))
    }
  }
  
  
  best.amount<-paste("pre.glm~",df.best.fit.amount$formular[which(df.best.fit.amount$BIC==min(df.best.fit.amount$BIC))],sep="")
  
  return(list(
    "wet" = best.wet, 
    "amount"=  best.amount))
  options(warn=0)
}
