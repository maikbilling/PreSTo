################## Main file of rain generator, by Maik Billing, 2017 ##################

### input data has to be structured as follows:
### - a data frame with at least 4 columns named:
###    - "year" contains the year
###    - "month" contains the month 
###    - "day" contains the day of the month
###    - "pre" contains the daily precipitation amount

##################################################################################################

dev.off() # delete all plots
rm(list=ls())# delete everything in the list

##################################################################################################

#### load needed files/functions/parts

path<-"C:/Users/S_HK/Documents/Maik/projects/raingenerator/code/final"

setwd(path)
source("load.data.R")
source("raingen.R")
source("raingen.summary.R")
source("estimate.models.R")
source("estimate.bias.R")

### number of years to simulate
nyears<-100 

#### load historical data 

# load your data here!!
# data<-yourloadeddata

### estimate best models
best.models<-estimate.models(data)

### estimate bias
bias<-estimate.bias(data,models=best.models)
#bias<--24.08281 #for Eneabba data, use "estimate.bias" for other sites! See documentation

#### build projected climat change vector

cc.vec45<-c(-2.4,3.8,5.2,1.9,-7.6,-12.9,-11.3,-14.5,-20.3,-17.8,-12.9,-6.4)*100/90
cc.vec85<-c(5.7,10.3,11.6,4.2,-17.8,-27,-29.4,-33.5,-40.7,-41.1,-27.1,-11.5)*100/90
#cc.vectest<-rep(-20,12)

#### generate data
raingen.list<-raingen(data,nyears,cc.vec=cc.vec85,mode="linear",bias=bias,sound=T,models=best.models)

#### summarize data
summary<-raingen.summary(raingen.list)
