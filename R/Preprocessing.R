library(dplyr)
library(tidyverse)
library(ggplot2)

'%!in%' <- Negate('%in%')

csvFilePath = "./ms_trajectory.csv"
echo <- read.csv(csvFilePath, stringsAsFactors = F)

## calculate age ## 

age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

echo$studyDate<-strftime(echo$studyDate, format="%Y-%m-%d")
echo$studyDate<-as.Date(echo$studyDate, format="%Y-%m-%d")
echo$birthDate<-as.Date(echo$birthDate, format="%Y-%m-%d")

echo$age<-age(echo$birthDate, echo$studyDate)
echo$age65 <- (echo$age-65)/10 

## sex ## 
echo$sexDummy<-NA
echo$sexDummy[echo$sex=="M"]<-0
echo$sexDummy[echo$sex=="F"]<-1


## cleaning mvStenosis ## 
echo <- echo %>% mutate(mvSte = case_when(grepl("[Nn]o",mvStenosis) ~ "no",
                                          grepl("^[Mm]ild$",mvStenosis) ~ "mild",
                                          grepl("[Mm]ild.+[Mm]od*.+",mvStenosis) ~ "mild to moderate",
                                          grepl("[Mm]od.+[Ss]ev.*",mvStenosis) ~ "moderate to severe",
                                          mvStenosis %in% c("Severe", "R/O Severe","R/O severe","Severe to severe", "R/O severe AS",  "r/o Severe","Very severe") ~ "severe",
                                          mvStenosis %in% c("No","-","--") ~ "no",
                                          mvStenosis %in% c("Mild","mild") ~ "mild",
                                          mvStenosis %in% c("Mod to mild", "mod to mild") ~ "mild to moderate",
                                          mvStenosis %in% c("Moderate","moderate","Moderate?") ~ "moderate"
                                          #TRUE ~ NA
)
)


## Transforming class of mvStenosis ## 
echo$mvSte <- factor(echo$mvSte, levels = c("no", "mild", "mild to moderate", "moderate", "moderate to severe", "severe"))
echo$mvSteNum <- as.numeric(echo$mvSte)


## MVA into numeric
echo$mvaByPlanimetry[echo$mvaByPlanimetry=="."]<-NA
echo$mvaByPlanimetry[echo$mvaByPlanimetry==""]<-NA
echo$mvaByPlanimetry[echo$mvaByPlanimetry=="-"]<-NA
echo$mvaPlanimetry <- as.numeric(echo$mvaByPlanimetry)

## MDPG into numeric
echo$mvMeanPressureGradient[echo$mvMeanPressureGradient==""]<-NA
echo$mvMeanPressureGradient[echo$mvMeanPressureGradient=="-"]<-NA
echo$mvMpg <- as.numeric(echo$mvMeanPressureGradient)

## RVSP into numeric
echo$rightVentricularPeakSystolicPressurervsp[echo$rightVentricularPeakSystolicPressurervsp=="."]<-NA
echo$rightVentricularPeakSystolicPressurervsp[echo$rightVentricularPeakSystolicPressurervsp==""]<-NA
echo$rightVentricularPeakSystolicPressurervsp[echo$rightVentricularPeakSystolicPressurervsp=="-"]<-NA
echo$rvsp <- as.numeric(echo$rightVentricularPeakSystolicPressurervsp)

### Sampling ###  

sample <- echo %>%
  group_by(ptno) %>%
  arrange(ptno,studySeq) %>%
  filter(cummax(between(mvaPlanimetry, 1.0,1.5)) > 0) 

## remove COPD patients using COPD List ## 
copdList <- read.csv("./copdList.csv",stringsAsFactors = F)

sample <- sample %>%
  filter(.data$ptno %!in% copdList$ptno)

## remove follow up duration < 90 days & TTE < 3 ## 

sample <- sample %>%
  group_by(ptno) %>%
  arrange(ptno,studySeq) %>%
  filter(last(studyDate)- first(studyDate) >= 90) %>%
  filter(length(mvaPlanimetry) >= 3)
