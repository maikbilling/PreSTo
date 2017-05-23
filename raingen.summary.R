#' raingen.summary
#'
#' This function allows to plot the statistics of the generated data compared to the historical
#' record. Therefore, `raingen.summary()' requires the list which is returned by the `raingen()'
#' function, as input. The function delivers box plots, QQ-plots, scatter plots, tables and
#' signifcance tests of annual and monthly variables, as well as the input and output precipitations
#' for the annual, the monthly and the daily level. The box plots of monthly precipitations, wet days per month and precipitation
#' amounts are additionally separated into dry, intermediate and wet years. Dry (wet) years are
#' designed as years that fall below (exceed) the first (third) quartile of the annual precipitations
#' of the record. All other years are denoted as intermediate years.
#'
#' @param raingen the list that is returned by the raingen function. For the detailed contents of the list see --> table?
#' @return a list containing several statistics of the historical and generated data. For the data record these are -> table?
#' @export

raingen.summary<-function(raingen){
  
  library(ggplot2)
  
  col.historical<-"white"
  col.generated<-"darkseagreen3"
  
  col.lab.bg<-"white"
  col.lab.border<-"black"
  
  raw.data<-raingen$raw.data
  gen.data<-raingen$gen.data
  
  gen.annual<-raingen$annual.output
  
  raw.annual<-raingen$raw.annual
  raw.monthly<-raingen$raw.monthly
  
  
  ######## Boxplots per month
  
  mytheme<-theme(panel.grid.major = element_blank(),
                 panel.border = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks.x=element_blank(),
                 axis.text=element_text(size=18),
                 axis.title=element_text(size=18),
                 axis.text.y = element_text(size=18,colour="black"),
                 axis.line.y=element_line(colour="black",size=0.5),
                 legend.title=element_text(),
                 legend.key = element_blank(),
                 strip.text=element_text(size=14), 
                 strip.background = element_rect(colour=col.lab.border, fill=col.lab.bg))
  
  myfacet_grid<-facet_grid( ~ month, scales="free",switch="x")
  
  mygeom_boxplot<-geom_boxplot(aes(fill=typ),size=0.2)
  
  myscale_fill_manual<-scale_fill_manual(values = c(col.historical, col.generated),name="",labels=c("Historical","Generated"))
  
  gen.data$typ<-"generated"
  raw.data$typ<-"data"
  
  df.total <- merge(subset(gen.data,wet==T), subset(raw.data,wet==T),all=T) 
  
  p <-ggplot(data = df.total, aes(x=month,y=pre))
  p<-p+mytheme+myfacet_grid+mygeom_boxplot+myscale_fill_manual
  p<-p+ylab("Precipitation amount on wet days [mm]")+xlab("Month")+ggtitle("Precipitation amount on wet days")
  print(p)
  
  cat.th<-raingen$cat.th
  cat<-c("dry","intermediate","wet")
  
  for(i in 1:3){
    df.total <- merge(subset(gen.data,wet==T & annual.cat==cat[i]), subset(raw.data,wet==T & annual.cat==cat[i]),all=T) 
    p <-ggplot(data = df.total, aes(x=month,y=pre))
    p<-p+mytheme+myfacet_grid+mygeom_boxplot+myscale_fill_manual
    p<-p+xlab("Month")+ylab("Precipitation amount on wet days")+ggtitle(paste("Precipitation amounts on wet days,",cat[i]," years"))
    print(p)
    }
  
  ###############################
  
  gen.data$pre.glm<-gen.data$pre
  gen.data$pre.glm[gen.data$pre.glm==0]<-NA
  
  gen.sum.month<-aggregate(gen.data$pre, list(month = gen.data$month,year=gen.data$year), sum)
  names(gen.sum.month)<-c("month","year","monthly.pre")
  
  gen.sum.month$wet.days<-aggregate(gen.data$wet, list(month = gen.data$month,year=gen.data$year), sum)$x
  gen.sum.month$mean.pre<-aggregate(gen.data$pre.glm, list(month = gen.data$month,year=gen.data$year), mean,na.rm=TRUE)$x
  gen.sum.month$mean.pre[is.na(gen.sum.month$mean.pre)]<-0
  
  gen.sum.month$typ<-"generated"
  
  gen.sum.month<-merge(gen.sum.month,unique(gen.data[,c("year","annual.cat")]))
  
  data.sum.month<-aggregate(raw.data$pre, list(month = raw.data$month,year=raw.data$year),FUN=sum,na.rm=TRUE, na.action=NULL)
  names(data.sum.month)<-c("month","year","monthly.pre")
  
  data.sum.month$wet.days<-aggregate(raw.data$wet, list(month = raw.data$month,year=raw.data$year),FUN=sum,na.rm=TRUE, na.action=NULL)$x
  data.sum.month$typ<-"data"
  data.sum.month$mean.pre<-aggregate(raw.data$pre.glm, list(month = raw.data$month,year=raw.data$year), mean,na.rm=TRUE)$x
  data.sum.month$mean.pre[is.na(data.sum.month$mean.pre)]<-0
  
  
  data.sum.month<-merge(data.sum.month,unique(raw.data[,c("year","annual.cat")]))
  
  df.total <- merge(gen.sum.month, data.sum.month,all=T) 
  p <-ggplot(data = df.total, aes(x=month,y=wet.days))
  p<-p+mytheme+myfacet_grid+mygeom_boxplot+myscale_fill_manual
  p<-p+ylab("number of wet days")+ggtitle("Number of wet days per month")
  print(p)
  
  for(i in 1:3){
    df.total <- merge(subset(gen.sum.month,annual.cat==cat[i]), subset(data.sum.month,annual.cat==cat[i]) ,all=T) 
    p <-ggplot(data = df.total, aes(x=month,y=wet.days))
    p<-p+mytheme+myfacet_grid+mygeom_boxplot+myscale_fill_manual
    p<-p+ylab("number of wet days")+ggtitle(paste("Number of wet days per month,",cat[i]," years"))
    print(p)
    }
  
  df.total <- merge(gen.sum.month, data.sum.month,all=T) 
  p <-ggplot(data = df.total, aes(x=month,y=monthly.pre))
  p<-p+mytheme+myfacet_grid+mygeom_boxplot+myscale_fill_manual
  p<-p+ylab("Cumulative monthly precipitation")+ggtitle("Cumulative monthly percipitation")
  print(p)
  
  for(i in 1:3){
    df.total <- merge(subset(gen.sum.month,annual.cat==cat[i]), subset(data.sum.month,annual.cat==cat[i]) ,all=T) 
    p <-ggplot(data = df.total, aes(x=month,y=monthly.pre))
    p<-p+mytheme+myfacet_grid+mygeom_boxplot+myscale_fill_manual
    p<-p+ylab("Cumulative monthly precipitation")+ggtitle(paste("Cumulative monthly percipitation,",cat[i]," years"))
    print(p)
    }
  
  
  ###compare input of random annual (before loop) amount to output of annual amount (generated data)
  #plot both
  
  hist(gen.annual,main="Distribution of generated annual precipitation",xlab="Annual precipiation") #draw histogram
  qqnorm(gen.annual,main="Normal QQ-Plot for generated annual precipitation") #qq-plot
  qqline(gen.annual) #draw line in qq-plot
  shap.gen.an.pre<-shapiro.test(gen.annual) #perform shapiro-wilk-test, watch out: H0: data is normally distributed -> high p-value: H0 is not rejected
  legend(x="topleft",legend=paste("Shapiro-Wilk-Test: p-value =",toString(round(shap.gen.an.pre$p.value,digits=5))))
  
  hist(raw.annual,main="Distribution of historical annual precipitation",xlab="Annual precipitation") #draw histogram
  qqnorm(raw.annual,main="Normal QQ-Plot for historical annual precipitation") #qq-plot
  qqline(raw.annual) #draw line in qq-plot
  shap.gen.an.pre<-shapiro.test(raw.annual) #perform shapiro-wilk-test, watch out: H0: data is normally distributed -> high p-value: H0 is not rejected
  legend(x="topleft",legend=paste("Shapiro-Wilk-Test: p-value =",toString(round(shap.gen.an.pre$p.value,digits=5))))
  
  ###perform Welch-Test to check if means of real and generated annual pre. could are consistent
  Welch.test<-t.test(raw.annual,gen.annual,var.equal=FALSE) 
  
  ###perform F-Test to check if variances of real and generated annual pre. could are consistent
  F.test<-var.test(raw.annual,gen.annual)
  
  par(mar=c(5.1, 4.1, 4.1, 10.1), xpd=TRUE)
  boxplot(raw.annual,gen.annual,main="Boxplot of annual precipitation of real and generated data",ylab="Annual precipitation",names=c("Historical","Generated")) 
  legend(x="topright",inset=c(-0.45,0),title="P-values",legend=c(paste("Welch-Test:",toString(round(Welch.test$p.value,digits=3))),paste("F-Test:",toString(round(F.test$p.value,digits=3)))))
  par(xpd=FALSE)
  
  par(cex.lab=1.5)
  par(cex.axis=1.5)
  boxplot(raw.annual,gen.annual, 
                      col=c(col.historical,col.generated),
                      names=c("Historical","Generated"),
                      ylab="Annual precipitation")
  
  plot(raingen$annual.input,gen.annual,main="Comparison of annual input and output",xlab="Input annual precipitation",ylab="Output annual precipitation")
  abline(lm(gen.annual ~ raingen$annual.input),col="red")
  abline(a=0,b=1)
  legend(x="topright",legend=c("Ideal","Fitted"),lty=c(1,1),col=c("black","red"))
  
  boxplot(raingen$annual.input,gen.annual,main="Boxplot of annual input and output",ylab="Annual precipitation",names=c("Input","Output")) 
  
  #### Monthly scale

  plot(raingen$monthly.input$monthly.pre,raingen$monthly.output$monthly.pre.out,main="Comparison of monthly input and output",xlab="Input monthly precipitation",ylab="Output monthly precipitation")
  abline(lm(raingen$monthly.output$monthly.pre.out ~ raingen$monthly.input$monthly.pre),col="red")
  abline(a=0,b=1)
  legend(x="topright",legend=c("Ideal","Fitted"),lty=c(1,1),col=c("black","red"))
  
  boxplot(raw.monthly,raingen$monthly.input$monthly.pre,main="Boxplot of monthly precipitatoins",ylab="monthly precipitation",names=c("Historical","Input for daily level")) 
  
  boxplot(raw.monthly,raingen$monthly.output$monthly.pre.out,main="Boxplot of monthly precipitations",ylab="monthly precipitation",names=c("Historical","Generated")) 
  
  
  # table with switched categories of annual precipitation (input to output)
  cat.switched<-table(raingen$cat.input,raingen$cat.output,dnn=c("input","output"))
  
  print(cat.switched)
  
  # compare annual stat. parameters of annual gen. data to historical
  x<-data.frame(historical=c(mean(raw.annual),sd(raw.annual)),
                generated=c(mean(gen.annual),sd(gen.annual)),
                devation=c((mean(gen.annual)-mean(raw.annual))/mean(raw.annual),(sd(gen.annual)-sd(raw.annual))/sd(raw.annual) ))
  
  row.names(x)<-c("mean","sd")
  
  print(x)
  
  #mean monthly precipitation generated
  gen.MMP<-rep(NA,12)
  for(i in 1:12){gen.MMP[i]<-mean(raingen$monthly.output$monthly.pre.out[raingen$monthly.output$month==i])}
  
  #mean monthly precipitation generated
  raw.MMP<-rep(NA,12)
  for(i in 1:12){raw.MMP[i]<-mean(matrix(raingen$raw.monthly, nrow=12)[i,])}
  
  
  output<- list("MAP.recdata"=mean(raw.annual),
                "MAP.gendata"=mean(gen.annual),
                "SD.recdata"=sd(raw.annual),
                "SD.gendata"=sd(gen.annual),
                "monthly.gendata"=gen.sum.month,
                "monthly.recdata"=data.sum.month,
                "switched.cat"=cat.switched,
                "prop.switched.cat"=prop.table(cat.switched, 1))
  
  return(output)
}
