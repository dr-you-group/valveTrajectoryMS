library(pROC)
library(ggplot2)
library(survival)
library(dplyr)
library(epiR)
library(fmsb)
library(rms)
library(splines)
library(ggplot2)

ms <- read.csv("./ms_trajectory_clinical.csv")

## preparing for analysis ## 

ms$class <- ifelse(ms$group == 1, 0, 1)

#### ROC #### 

roc_main<-pROC::roc(class~rvsp,data=ms,ci=T)
roc_auc <- formatC( round(roc_main$auc[1], 3 ), format='f', digits=3 )

par(pty = "s")
plot.roc(roc_main,   
         print.auc=F,
         print.thres = T,
         print.thres.pch = 16,
         print.thres.cex=0.9,
         asp = NA
         
)


legend("bottomright", 
       legend=paste0("auc= ", roc_auc),
       #col="#FF3333", 
       lwd=2, cex=0.8)   


#### Time dependent cox ####

ms$MVR <- as.factor(ms$MVR)
ms$PMV <- as.factor(ms$PMV)
ms$mva10 <- ms$mvaPlanimetry*10

msTime <- tmerge(data1=ms, data2=ms, id=ptno, composite = event(Compiste_d,Composite))
msTime <- tmerge(msTime, ms, id=ptno, mvr = tdc(MVR_d))
msTime <- tmerge(msTime, ms, id=ptno, pmv = tdc(PMV_d))

## Univariate analysis ##

catVar <- c("class","age","HTN","DM","CKD60","AF","mvMpg","mva10")

dataframe <- NULL

for (varname in catVar) {
  
  msCox <- coxph(as.formula(paste('Surv(msTime$tstart,msTime$tstop,msTime$composite)~ ',paste(c(varname,'mvr','pmv'),collapse= "+"))),data=msTime)
  
  HR <- exp(msCox$coefficients[1])
  lower <- exp(confint(msCox)[varname,1])
  upper <- exp(confint(msCox)[varname,2])
  pValue <- summary(msCox)$coefficients[varname,5]
  
  tmp <-data.frame(HR,lower,upper,pValue)
  names(tmp) <- c("HR","lower","upper","p-value")
  dataframe <- rbind(dataframe,tmp)
  
}


## Multivariate analysis ##
msCox <- coxph(Surv(msTime$tstart,msTime$tstop,msTime$composite)~class+age+HTN+DM+CKD60+mvr+pmv,data=msTime)
summary(msCox)


#### Logistic regression ####

ms <- ms %>%
  mutate(rvsp_40 = ifelse(rvsp <= 40, 0,1))

## Univariate logistic ## 

catVar <- c("rvsp_40","age","HTN","DM","CKD60","AF","mvMpg","mva10")

dataframe <- NULL

for (varname in catVar) {
  
  out <- glm(as.formula(paste('class~',varname)),family = binomial, data= ms)
  
  OR <- exp(out$coefficients)[2]
  lower <- exp(confint(out)[varname,1])
  upper <- exp(confint(out)[varname,2])
  pValue <- coef(summary(out))[varname,4]
  
  tmp <-data.frame(OR,lower,upper,pValue)
  names(tmp) <- c("OR","lower","upper","p-value")
  dataframe <- rbind(dataframe,tmp)
  
}


## multivariate logistic ##
out <- glm(class ~ rvsp_40+age+HTN+DM+AF+mvMpg,family=binomial,data=ms)
summary(out)
exp(out$coefficients)
exp(confint(out))


#### Spline curve #### 

ms_sub<- ms %>%
  dplyr::select(ptno,rvsp,Composite,Compiste_d)

ts<-with(ms_sub,survival::Surv(Compiste_d,Composite))
ddist <-rms::datadist(ms_sub)
options(datadist="ddist")
ddist$limits["Adjust to","rvsp"] <- 40   # cut-off 
splineBp <- rms::cph(ts~rms::rcs(rvsp,4),ms_sub,x=T,y=T)

splineBpCurve <- rms::Predict(splineBp,rvsp,ref.zero=T,fun=exp)
ggplot(splineBpCurve) + theme_classic() + geom_line(colour="royalblue3")
