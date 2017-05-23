source("raingen.R")
source("raingen.summary.R")
source("estimate.models.R")
source("estimate.bias.R")

### number of years to simulate
nyears<-100 

### load historical data (load your data here)
load("eneabba.data.RData")

### estimate best models
best.models<-estimate.models(data)

### estimate bias (takes quite long) (alternatively: b<--24)
b<-estimate.bias(data,models=best.models)

### projected climat change vector
cc<-c(-2.4,3.8,5.2,1.9,-7.6,-12.9,-11.3,-14.5,-20.3,-17.8,-12.9,-6.4)

#### generate data
raingen.list<-raingen(data,nyears,bias=b,models=best.models,sound=T)
raingen.list.linear<-raingen(data,nyears,cc.vec=cc,mode="linear",bias=b,models=best.models,sound=T)
raingen.list.fullimp<-raingen(data,nyears,cc.vec=cc,mode="fullimp",bias=b,models=best.models,sound=T)

#### summarize data
summary<-raingen.summary(raingen.list)
summary.linear<-raingen.summary(raingen.list.linear)
summary.fullimp<-raingen.summary(raingen.list.fullimp)