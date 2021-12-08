library(lcmm)

#### Preliminary Model for selecting favoured k(number of classes) #### 

set.seed(100)
model_1 <- lcmm::hlme(fixed = rvsp ~ diffInDay,
                      random = ~ diffInDay,
                      ng = 1,
                      subject = "ptno",
                      data = data.frame(sample) )


for (i in 2:7){
  set.seed(100)
  m<-paste("model_",i,sep = "")
  
  assign(m, lcmm::hlme(fixed = rvsp ~ diffInDay,
                       mixture= ~ diffInDay,
                       random = ~ diffInDay,
                       ng = i,
                       nwg = TRUE,
                       subject = "ptno",
                       idiag = TRUE,
                       data = data.frame(sample) ),
         envir = parent.frame())
  
}

lin <- data.frame("k"=c(1,2,3,4,5,6,7), "BIC" = c(model_1$BIC,model_2$BIC,model_3$BIC,model_4$BIC,model_5$BIC,model_6$BIC,model_7$BIC))

modelout <- knitr::kable(lin,row.names = F, align = "c")
modelout # choosing k that has the lowest BIC 

#### refine the model using k derived in step 1 #### 

set.seed(100)
model_A <- lcmm::hlme(fixed = rvsp ~ diffInDay ,
                      mixture= ~ diffInDay,
                      random = ~ diffInDay,
                      ng = 2,
                      nwg = TRUE,
                      subject = "ptno",
                      idiag = TRUE,
                      data = data.frame(sample) )

set.seed(100)
model_B <- lcmm::hlme(fixed = rvsp ~ diffInDay ,
                      mixture= ~ diffInDay,
                      random = ~ diffInDay,
                      ng = 2,
                      nwg = TRUE,
                      subject = "ptno",
                      idiag = F,
                      data = data.frame(sample) )

set.seed(100)
model_C <- lcmm::hlme(fixed = rvsp ~ diffInDay ,
                      mixture= ~ diffInDay,
                      random = ~ diffInDay,
                      ng = 2,
                      nwg = F,
                      subject = "ptno",
                      idiag = TRUE,
                      data = data.frame(sample) )

set.seed(100)
model_D <- lcmm::hlme(fixed = rvsp ~ diffInDay ,
                      mixture= ~ diffInDay,
                      random = ~ diffInDay,
                      ng = 2,
                      nwg = F,
                      subject = "ptno",
                      idiag = F,
                      data = data.frame(sample) )



set.seed(100)
model_E <- lcmm::hlme(fixed = rvsp ~ diffInDay + sexDummy + age65,
                      mixture= ~ diffInDay + age65,
                      random = ~ diffInDay + age65,
                      ng = 2,
                      nwg = TRUE,
                      subject = "ptno",
                      idiag = TRUE,
                      data = data.frame(sample) )

set.seed(100)
model_F <- lcmm::hlme(fixed = rvsp ~ diffInDay + sexDummy + age65,
                      mixture= ~ diffInDay + age65,
                      random = ~ diffInDay + age65,
                      ng = 2,
                      nwg = TRUE,
                      subject = "ptno",
                      idiag = FALSE,
                      data = data.frame(sample) )

set.seed(100)
model_G <- lcmm::hlme(fixed = rvsp ~ diffInDay + sexDummy + age65,
                      mixture= ~ diffInDay + age65,
                      random = ~ diffInDay + age65,
                      ng = 2,
                      nwg = FALSE,
                      subject = "ptno",
                      idiag = TRUE,
                      data = data.frame(sample) )

set.seed(100)
model_H <- lcmm::hlme(fixed = rvsp ~ diffInDay + sexDummy + age65,
                      mixture= ~ diffInDay + age65,
                      random = ~ diffInDay + age65,
                      ng = 2,
                      nwg = FALSE,
                      subject = "ptno",
                      idiag = FALSE,
                      data = data.frame(sample) )

set.seed(100)
model_I <- lcmm::hlme(fixed = rvsp ~ 1+diffInDay + I(diffInDay^2), 
                      mixture= ~ diffInDay ,
                      random = ~ diffInDay,
                      ng = 2,
                      nwg = FALSE,
                      subject = "ptno",
                      idiag = FALSE,
                      data = data.frame(sample) )

set.seed(100)
model_J <- lcmm::hlme(fixed = rvsp ~ 1+diffInDay + I(diffInDay^2), 
                      mixture= ~ diffInDay ,
                      random = ~ diffInDay,
                      ng = 2,
                      nwg = T,
                      subject = "ptno",
                      idiag = FALSE,
                      data = data.frame(sample) )

## choose model that has the lowest BIC 


#### make model that chosen in step 2 ####

set.seed(100)
Option1_1 <- lcmm::hlme(fixed = rvsp ~ diffInDay + sexDummy + age65,
                        random = ~ diffInDay + age65,
                        ng = 1,
                        subject = "ptno",
                        data = data.frame(sample) )

for (i in 2:4){
  set.seed(100)
  m<-paste("Option1_",i,sep = "")
  
  assign(m, lcmm::hlme(fixed = rvsp ~ diffInDay + sexDummy + age65,
                       mixture= ~ diffInDay + age65,
                       random = ~ diffInDay + age65,
                       ng = i,
                       nwg = T,
                       subject = "ptno",
                       idiag = T,
                       data = data.frame(sample) ),
         envir = parent.frame())
  
}

#### evaluate the model adequacy and confirm the optimal number of K ####

# devtools::install_github("hlennon/LCTMtools", force = T)
library(LCTMtools)

a<-c("Model 1", "Model 2", "Model 3","Model 4")


B1<-sprintf("%.2f", Option1_1$BIC)
B2<-sprintf("%.2f", Option1_2$BIC)
B3<-sprintf("%.2f", Option1_3$BIC)
B4<-sprintf("%.2f", Option1_4$BIC)


b<-c(B1, B2, B3, B4)

C1<-sprintf("%.2f", LCTMtoolkit(Option1_2)[5])
C2<-sprintf("%.2f", LCTMtoolkit(Option1_3)[5])
C3<-sprintf("%.2f", LCTMtoolkit(Option1_4)[5])


c<-c("*",C1,C2,C3)

T1<-sprintf("%.2f", postprob(Option1_2)[[1]][2,1])
T2<-sprintf("%.2f", postprob(Option1_2)[[1]][2,2])
T3<-sprintf("%.2f", postprob(Option1_3)[[1]][2,1])
T4<-sprintf("%.2f", postprob(Option1_3)[[1]][2,2])
T5<-sprintf("%.2f", postprob(Option1_3)[[1]][2,3])
T6<-sprintf("%.2f", postprob(Option1_4)[[1]][2,1])
T7<-sprintf("%.2f", postprob(Option1_4)[[1]][2,2])
T8<-sprintf("%.2f", postprob(Option1_4)[[1]][2,3])
T9<-sprintf("%.2f", postprob(Option1_4)[[1]][2,4])


d<-c("100",
     paste0(T1, " : ", T2),
     paste0(T3, " : ", T4," : ",T5),
     paste0(T6, " : ", T7," : ",T8," : ", T9)
     
)

D1<-sprintf("%.2f", postprob(Option1_2)[[2]][1,1])
D2<-sprintf("%.2f", postprob(Option1_2)[[2]][2,2])
D3<-sprintf("%.2f", postprob(Option1_3)[[2]][1,1])
D4<-sprintf("%.2f", postprob(Option1_3)[[2]][2,2])
D5<-sprintf("%.2f", postprob(Option1_3)[[2]][3,3])
D6<-sprintf("%.2f", postprob(Option1_4)[[2]][1,1])
D7<-sprintf("%.2f", postprob(Option1_4)[[2]][2,2])
D8<-sprintf("%.2f", postprob(Option1_4)[[2]][3,3])
D9<-sprintf("%.2f", postprob(Option1_4)[[2]][4,4])

e<-c("100",
     paste0(D1, " : ", D2),
     paste0(D3, " : ", D4," : ",D5),
     paste0(D6, " : ", D7," : ",D8," : ", D9)
)

table<-data.frame(a,b,c,d,e)

names(table)<-c("Model","BIC","Relative entropy", "Proportions per class %", "mean of posterior probabilities" )


#### plot the graph ####

library(gridExtra)
sample$diffYear <- sample$diffInDay/365

## k = 2 ## 
people2 <- as.data.frame(Option1_2$pprob[,1:2])

sample$group2 <- factor(people2$class[sapply(as.numeric(sample$ptno), function(x) which(people2$ptno == x))])


p1 <- ggplot(sample, aes(x=diffYear, y=rvsp, group = ptno, colour = group2)) +
  geom_line() +
  #geom_smooth(aes(group=group2), method = "loess", size = 2, se = F)  +
  scale_x_continuous(limits = c(0,10),breaks=c(0,2,4,6,8,10)) +
  theme_classic() +
  theme(text = element_text(size=15)) +
  labs(x = "", y = "", colour = "Latent Class", title = "Raw") 

p2 <- ggplot(sample, aes(x=diffYear, y=rvsp, group = ptno, colour = group2)) +
  geom_smooth(aes(group = ptno, colour = group2),size = 0.5, se = F) +
  geom_smooth(aes(group = group2), method = "loess", size = 2, se = T)  +
  scale_x_continuous(limits = c(0,10),breaks=c(0,2,4,6,8,10)) +
  labs(x = "",y = "",colour = "Latent Class", title = "Smoothed") +
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=15)) 


grid.arrange(p1,p2,ncol=2)

## k = 3 ## 
people3 <- as.data.frame(Option1_3$pprob[,1:2])

sample$group3 <- factor(people3$class[sapply(as.numeric(sample$ptno), function(x) which(people3$ptno == x))])


p3 <- ggplot(sample, aes(x=diffYear, y=rvsp, group = ptno, colour = group3)) +
  geom_line() +
  #geom_smooth(aes(group=group3), method = "loess", size = 2, se = F)  +
  scale_x_continuous(limits = c(0,10),breaks=c(0,2,4,6,8,10)) +
  theme_classic() +
  theme(text = element_text(size=15)) +
  labs(x = "", y = "", colour = "Latent Class", title = "Raw") 

p4 <- ggplot(sample, aes(x=diffYear, y=rvsp, group = ptno, colour = group3)) +
  geom_smooth(aes(group = ptno, colour = group3),size = 0.5, se = F) +
  geom_smooth(aes(group = group3), method = "loess", size = 2, se = T)  +
  scale_x_continuous(limits = c(0,10),breaks=c(0,2,4,6,8,10)) +
  labs(x = "",y = "",colour = "Latent Class", title = "Smoothed") +
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=15)) 

grid.arrange(p3,p4,ncol=2)


## k = 4 ##  
people4 <- as.data.frame(Option1_4$pprob[,1:2])

sample$group4 <- factor(people4$class[sapply(as.numeric(sample$ptno), function(x) which(people4$ptno == x))])

p5 <- ggplot(sample, aes(x=diffYear, y=rvsp, group = ptno, colour = group4)) +
  geom_line() +
  #geom_smooth(aes(group=group4), method = "loess", size = 2, se = F)  +
  scale_x_continuous(limits = c(0,10),breaks=c(0,2,4,6,8,10)) +
  theme_classic() +
  theme(text = element_text(size=15)) +
  labs(x = "", y = "", colour = "Latent Class", title = "Raw") 

p6 <- ggplot(sample, aes(x=diffYear, y=rvsp, group = ptno, colour = group4)) +
  geom_smooth(aes(group = ptno, colour = group4),size = 0.5, se = F) +
  geom_smooth(aes(group = group4), method = "loess", size = 2, se = T)  +
  scale_x_continuous(limits = c(0,10),breaks=c(0,2,4,6,8,10)) +
  labs(x = "",y = "",colour = "Latent Class", title = "Smoothed") +
  theme_classic() +
  theme(legend.position = "none", text = element_text(size=15)) 

grid.arrange(p5,p6,ncol=2)