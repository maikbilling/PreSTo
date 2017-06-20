#' raingen
#'
#' The core function of the generator produces daily rainfall data.
#' It requires at least the historical record of daily precipitation data and the number of years to simulate.
#' To run climate change scenarios a numeric vector with 12 entries as input is necessary.
#' It is recommended to run 'estimate.models()' and
#' and estimate.bias() for each data set once and and hand their outputs over to
#' the function.

#' @param data historical climate data. The data needs be on a daily
#' resolution. It must be provided in a data frame with 4 columns that contain numeric
#' values only. The columns must be named as follows: `year', `month', `day', `pre'(pre=daily
#' precipitation amount).
#' @param nyears The number of years to simulate.
#' @param mode It is possible to generate data in three different
#' operation modi: mode = 0 (default) generates rainfall data that fits well to the historical record
#' i.e. without climate change effect, mode = "linear" increases climate change effects linearly from the
#' first generated year up to the total number of generared years, mode = "fullimp" applies climate change effects
#' by their full strength immediately and to the same extent for all generated years
#' @param cc.vec Defines how climate change is simulated. Each entry of the vector represents the expected mean changes in precipitation of each month
#' in percent, for example as output from climate projections. The preset value of the vector is zero, i.e. no climate change.
#' @param sound defines whether a notification sound is played when the data generation has finished. The default setting is no sound.
#' @param models model formulas as character strings in a list. List members are $wet, $amount. \code{estimate.bias}
#' @return Returns a list that contains the generated data in a data frame with the following elements:
#' MAP.recdata mean annual precipitation of the data record
#' MAP.gendata mean annual precipitation of the generated data
#' SD.recdata annual standard deviation of the data record
#' SD.gendata annual standard deviation of the generated data
#' monthly.recdata monthly data of the data record (number of wet days, monthly precipiations,
#'                                                 etc.)
#' monthly.gendata monthly data of the generated data
#' switched.cat comparison of years (annual precipitations as input), that switched their
#' annual category after data generation (output) in a table
#' prop.switched.cat proportion table of `switched.cat'
#' @details
#' @export

raingen<-function(data,nyears,mode=0,cc.vec=0,bias=0,sound=F,
                  models=list("wet" = "wet ~ monthly.pre",
                              "amount"=  "pre.glm~monthly.pre"
                              )){
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
  
  
  frm.wet<-models$wet
  frm.amount<-models$amount
  
  require(beepr)
  require(VGAM)
  require(zoo)
  require(fitdistrplus)
  
  options(warn=-1)
  
  ####choose operation mode and generate climate change matrix
  
  if(mode=="linear" | mode=="fullimp"){
    cc.matrix<-switch(mode, #calculate breaks in histrogram for a specific distribution
                      "linear" = outer(1:nyears, cc.vec , FUN=function(year,change) (change/nyears)*year ),
                      "fullimp" = matrix(cc.vec,nyears,12,byrow=TRUE))
  }else{
    cc.matrix<-0
    cc.vec<-0
  }
  
  print("data preparation...")
  
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
  #create monthly rainfall and wet days per month
  data.sum.month<-aggregate(data$pre, list(month = data$month,year=data$year), sum, na.rm=TRUE, na.action=NULL)
  data.sum.month$wet.days.month<-aggregate(data$wet, list(month = data$month,year=data$year),FUN=sum,na.rm=TRUE, na.action=NULL)$x
  names(data.sum.month)<-c("month","year","monthly.pre","wet.days.month")
  data<-merge(data, data.sum.month,by.x = c("year","month"), by.y = c("year","month"))
  data.sum.month<-merge(data.sum.month, annual.pre,by.x = "year", by.y = "year")
  data.sum.month$month <- as.character(data.sum.month$month)
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
  
  
  ###### model section ######
  
  ####fractionize annual rainfalls
  
  frac.mat<-matrix(data=NA,ncol=14,nrow=length(unique(data.sum.month$year)))
  frac.mat[,1]<-unique(data.sum.month$year)
  for(i in 1:length(unique(data.sum.month$year))){
    df<-subset(data.sum.month,data.sum.month$year==frac.mat[i,1])
    frac.mat[i,2:13]<-df$monthly.pre/df$annual.pre
    frac.mat[i,14]<-df$annual.pre[1]
  }
  frac.mat<-frac.mat[order(frac.mat[, 14]), ]
  
  ####define function to get monthly rainfalls from annual rainfalls and fractions
  getnewfrac<-function(annual.pre,frac.mat){
    x_0<-which(frac.mat[,14]>annual.pre)[1]
    if(is.na(x_0)){x_0<-length(frac.mat[, 14])}
    x_range<-(x_0-3):(x_0+3)
    x_range<-intersect(x_range,1:length(frac.mat[, 14]))
    #newfracs<-apply(frac.mat[x_range,2:13],2,FUN=mean)
    inewfrac<-sample(x_range, 1)
    newfracs<-frac.mat[inewfrac,2:13]
    return(newfracs*annual.pre)
  }
  
  ####estimate distribution of precipitation amount on wet days (requires zoo and fitdistrplus package)
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
  
  #### model for wetness of day
  
  w<-2*pi/12
  
  mod.wet <- glm(as.formula(frm.wet),family=binomial,data=data)
  summary(mod.wet)
  BIC(mod.wet)
  
  #### model for precipitation amount
  
  mod.amount<-switch(best.distr,
                     "gamma" = glm(as.formula(frm.amount),family=Gamma,data=data),
                     "lnorm" = glm(as.formula(frm.amount),family=gaussian(link="log"),data=data),
                     "weibull" = vglm(as.formula(frm.amount), family = weibullR, data = data))
  summary(mod.amount)
  BIC(mod.amount)
  
  max.daily.pre<-max(data$pre.glm,na.rm=T)
  
  
  ################## Generator ###################
  
  print("generate data...")

  ###predict annual precipitation
  #draw annual precip. amounts from normal distribution
  annual.in<-rnorm(nyears, mean=mean(annual.pre$annual.pre), sd= sd(annual.pre$annual.pre)+bias) 
  
  #generate empty data frame for target data
  gen.data<-data.frame(year=rep(1:nyears,each=365),
                       doy=rep(1:365,nyears),
                       annual.pre.in=rep(annual.in,each=365),
                       pre=rep(NA,nyears*365),
                       wet=rep(NA,nyears*365))
  #create a new column with month
  gen.data$month<-as.numeric(format(strptime(paste("2007", gen.data$doy), format="%Y %j") , "%m"))
  #create a new column with day
  gen.data$day<-rep(c(1:31,1:28,1:31,1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31),times=nyears)
  # cathegorize annual.in (input data) in dry, intermediate and wet years
  input<-data.frame(annual.in=annual.in)
  input$cat[input$annual.in<cat.th[[1]]]<-"dry"
  input$cat[input$annual.in>=cat.th[[1]] & input$annual.in<cat.th[[2]]]<-"intermediate"
  input$cat[input$annual.in>=cat.th[[2]]]<-"wet"
  
  ### predict monthly precipitation
  # apply predition of monthly rainfall by fractionizing annual precipitations
  for(i in 1:nyears){
    newfrac<-getnewfrac(annual.in[i],frac.mat)
    if (mode=="linear" | mode=="fullimp"){
      newfrac<-newfrac*(1+cc.matrix[i,]/100)
      }
    gen.data$monthly.pre.in[gen.data$year==i]<-rep(newfrac,times=c(31,28,31,30,31,30,31,31,30,31,30,31))
  }
  
  # generate square of monthly precipitation
  gen.data$monthly.pre.pw2<-gen.data$monthly.pre.in^2
  gen.data$monthly.pre.sqrt<-gen.data$monthly.pre.in^(1/2)
  
  # create progress bar
  pb <- txtProgressBar(min = 0, max = nyears, style = 3)
  
  ### initialize main loop:
  wet.yday2<-wet.yday<-pre.yday<-0
  
  ### Main loop to predict daily rainfall:
  
  for(j in 1:(nyears)){
    
    #cut out sub data frame from main data frame
    gen.data.sub<-subset(gen.data,gen.data$year==j)

    #loop over all days in year
    for(i in 1:365){
      
      #generated data frame for model prediction
      newdata<-data.frame(monthly.pre=gen.data.sub$monthly.pre.in[i],
                          month=gen.data.sub$month[i],
                          month.char=as.character(gen.data.sub$month[i]),
                          wet.yday=wet.yday,
                          wet.yday2=wet.yday2,
                          monthly.pre.pw2=gen.data.sub$monthly.pre.pw2[i],
                          monthly.pre.sqrt=gen.data.sub$monthly.pre.sqrt[i])
      
      ### logistic regession ###
      if(newdata$monthly.pre[1]==0){  #if monthly precipitation is 0, all days in month are dry
        gen.data.sub$wet[i]<-0
      }else{
        p.wet<-predict(mod.wet,newdata=newdata,type="response") #predict prob. of wet day for one day
        gen.data.sub$wet[i]<-sample(c(1,0), size=1, replace=TRUE, prob=c(p.wet[[1]],1-p.wet[[1]])) #draw wet day (1(T) or 0(F))
      }
      # pass variables to one day before (for the next iteration step)
      wet.yday2<-wet.yday
      wet.yday<-gen.data.sub$wet #and wet.yday for the next day is 0
    }
    
    gen.data.sub.wet.days.month<-aggregate(gen.data.sub$wet, list(month = gen.data.sub$month,year=gen.data.sub$year),FUN=sum,na.rm=TRUE, na.action=NULL)
    names(gen.data.sub.wet.days.month)<-c("month","year","wet.days.month")
    gen.data.sub<-merge(gen.data.sub, gen.data.sub.wet.days.month,by.x = c("year","month"), by.y = c("year","month"))
    
    
    wet.yday2<-wet.yday<-pre.yday<-0
    for(i in 1:365){
      
      newdata<-data.frame(monthly.pre=gen.data.sub$monthly.pre.in[i],
                          month=gen.data.sub$month[i],
                          month.char=as.character(gen.data.sub$month[i]),
                          wet.yday=wet.yday,
                          wet.yday2=wet.yday2,
                          monthly.pre.pw2=gen.data.sub$monthly.pre.pw2[i],
                          monthly.pre.sqrt=gen.data.sub$monthly.pre.sqrt[i],
                          wet.days.month=gen.data.sub$wet.days.month[i])
      
      ### glm precipitation amount ###
      if(gen.data.sub$wet[i]==0){ # if false (no pres.)
        gen.data.sub$pre[i]<-0 #precipitation amount = 0
        
        # pass variables to one day before (for the next iteration step)
        wet.yday2<-wet.yday
        wet.yday<-0 #and wet.yday for the next day is 0
        pre.yday<-gen.data.sub$pre[i]
        
      } else{ #if true (wet day)
        mu.pre<-predict(mod.amount,newdata=newdata,type="response")
        drawn.pre<-switch(best.distr,
                                "gamma" = rgamma(1,shape= dist.table$estimate1[1], scale=mu.pre/ dist.table$estimate1[1]),
                                "lnorm" = rlnorm(1,meanlog=log(mu.pre)-((dist.table$estimate2[2])^2)/2, sdlog=dist.table$estimate2[2]),
                                "weibull" = rweibull(1,shape=dist.table$estimate1[3], scale=mu.pre/gamma(1+1/dist.table$estimate1[3])))
        
        if(drawn.pre<max.daily.pre){  #if drawn amount exceeds the maximum observed amount, set it to the observed maximum
          gen.data.sub$pre[i]<-drawn.pre
        }else{
          gen.data.sub$pre[i]<-max.daily.pre}
        
        # pass variables to one day before (for the next iteration step)
        wet.yday2<-wet.yday
        wet.yday<-1 #wet.yday for the next day (i+1) is 1
        pre.yday<-gen.data.sub$pre[i]
      }
      
    }
    
    # insert sub data into main data frame
    gen.data.sub<- subset(gen.data.sub, select = -c(wet.days.month) ) #drop wet.days.month
    gen.data<-gen.data[names(gen.data.sub)] #reorder gen.data same like gen.data.sub
    gen.data[gen.data$year==j,]<-gen.data.sub
    # next step in progress bar
    setTxtProgressBar(pb, j)
  }
  close(pb) #close progress bar
  
  
  ######### data post processing ##########
  
  
  #create new column with annual rain
  gen.annual.pre.out<- aggregate(gen.data$pre, list(gen.data$year), sum, na.rm=T) 
  names(gen.annual.pre.out)<-c("year","annual.pre.out")
  gen.data<-merge(gen.data, gen.annual.pre.out,by.x = "year", by.y = "year")
 
  #cathegorize generated annual precipitation
  gen.annual.pre.out$cat[gen.annual.pre.out$annual.pre.out<cat.th[[1]]]<-"dry"
  gen.annual.pre.out$cat[gen.annual.pre.out$annual.pre.out>=cat.th[[1]] & gen.annual.pre.out$annual.pre.out<cat.th[[2]]]<-"intermediate"
  gen.annual.pre.out$cat[gen.annual.pre.out$annual.pre.out>=cat.th[[2]]]<-"wet"
  #create new column with annual cathegory
  gen.data$annual.cat[gen.data$annual.pre.out<cat.th[[1]]]<-"dry"
  gen.data$annual.cat[gen.data$annual.pre.out>=cat.th[[1]] & gen.data$annual.pre.out<cat.th[[2]]]<-"intermediate"
  gen.data$annual.cat[gen.data$annual.pre.out>=cat.th[[2]]]<-"wet"
  
  #create new column with monthly rain
  gen.sum.month<-aggregate(gen.data$pre, list(month = gen.data$month,year=gen.data$year), sum)
  names(gen.sum.month)<-c("month","year","monthly.pre.out")
  gen.data<-merge(gen.data, gen.sum.month,by.x = c("year","month"), by.y = c("year","month"))
 
  #create dataframe with monthly input
  monthly.in<-aggregate(gen.data$monthly.pre.in, list(month = gen.data$month,year=gen.data$year), unique)
  names(monthly.in)<-c("month","year","monthly.pre")
  
  #reorder and rename data frame
  gen.data <- gen.data[c("year","month","day","doy","pre","wet","monthly.pre.out","annual.pre.out","annual.cat")]
  names(gen.data)[names(gen.data) == "annual.pre.out"] <- "annual.pre" 
  names(gen.data)[names(gen.data) == "monthly.pre.out"] <- "monthly.pre"
  
  #list with important results as output
  output <- list(
    "gen.data" = gen.data, 
    "annual.output"=  gen.annual.pre.out$annual.pre.out,
    "annual.input" = annual.in,
    "monthly.output" = gen.sum.month,
    "monthly.input" =monthly.in,
    "cat.input"=input$cat,
    "cat.output"=gen.annual.pre.out$cat,
    "cat.th"=cat.th,
    "distr.fit" = dist.table, 
    "best.distr" = best.distr,
    "summary.amount" = summary(mod.amount),
    "summary.wet" = summary(mod.wet),
    "raw.data"=data,
    "raw.annual"=annual.pre$annual.pre,
    "raw.monthly"=data.sum.month$monthly.pre)
  
  print("completed")
  
  if(sound){beep(5)} #play sound when finished
  return(output)
  options(warn=0)
}