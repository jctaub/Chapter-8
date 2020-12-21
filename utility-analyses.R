#one unified script

#this script is written with jen as a sub in name where any of the synthetic 
#datasets or orignal can be run through these analyses

jen <- #dataset

#first for the orignal and two of the chen datasets the marital status variable
# had a single divoricee, which is being recoded as not known


#jen$mar_stat<- ifelse(jen$mar_stat=='Divorced', '4', jen$mar_stat)
#jen$mar_stat<- ifelse(jen$mar_stat=='2', 'Married', jen$mar_stat)
#jen$mar_stat<- ifelse(jen$mar_stat=='3', 'Married spouse absent', jen$mar_stat)
#jen$mar_stat<- ifelse(jen$mar_stat=='4', 'Not known', jen$mar_stat)
#jen$mar_stat<- ifelse(jen$mar_stat=='5', 'Single', jen$mar_stat)
#jen$mar_stat<- ifelse(jen$mar_stat=='6', 'Widowed', jen$mar_stat)


#univariate Frequency tables

table(jen$mar_stat)
table.mar <- table(jen$mar_stat)
prop.table(table.mar)*100

table(jen$sex)
tab.sex <- table(jen$sex)
prop.table(tab.sex)*100


table(jen$employ)
tab.employ<-table(jen$employ)
prop.table(tab.employ)*100

table(jen$ctry_bth)

jen$country<- jen$ctry_bth
jen$country <- ifelse(jen$ctry_bth != "SCT", "Born Other Country", jen$country)
table(jen$country)
tab.country<-table(jen$country)
prop.table(tab.country)*100

jen$disabled <- jen$disability
jen$disabled<-ifelse(jen$disability != "None", "Disabled", jen$disabled)
table(jen$disabled)

jen$edinburgh<-jen$parish
jen$edinburgh <- ifelse(jen$parish != 'EDINBURGH', 'Other Parish', jen$edinburgh)
table(jen$edinburgh)
tab.edinburgh<-table(jen$edinburgh)
prop.table(tab.edinburgh)*100

jen$country<- jen$ctry_bth
jen$country <- ifelse(jen$ctry_bth != "SCT", "Other", jen$country)
prop.tble(table(jen$country))

jen$agegrp <- jen$age
jen$agegrp[data$age <= 20] <- "0-20"
jen$agegrp[data$age> 20 & jen$age<=40] <- "21-40"
jen$agegrp[data$age>40 & jen$age<=60] <-"41-60"
jen$agegrp[data$age>60 & jen$age<=80] <-"61-80"
jen$agegrp[data$age>80] <- "81+"
prop.table(table(jen$agegrp))*100
0
jen$servants <- jen$nservants
jen$servants <- ifelse(jen$nservants >0, 1, jen$servants)
jen$servants <- ifelse(jen$nservants == 0, 0, jen$servants)
prop.table(table(jen$servants))


jen$boarders<-jen$nboarders
jen$boarders[jen$boarders>0]<-1
prop.table(table(jen$boarders))*100

jen$lodgers<-jen$nlodgers
jen$lodgers[jen$lodgers>0]<-1
prop.table(table(jen$lodgers))

jen$visitors<-jen$nvisitors
jen$visitors[jen$visitors>0]<-1
prop.table(table(jen$visitors))

jen$unknwn<-jen$nkn
jen$unkwn[jen$unkwn>0]<-1
prop.table(table(jen$unkwn))

jen$famgteq15<-jen$nfamgteq15
jen$famgteq15[jen$famgteq15>0]<-1
prop.table(table(jen$famgteq15))

jen$famlt15<-jen$nfamlt15
jen$famlt15[jen$famlt15>0]<-1
prop.table(table(jen$famlt15))


tab.sex.mar<-table(jen$sex, jen$mar_stat)
prop.table(tab.sex.mar,1)*100

table.sex.agegrp <-table(jen$sex, jen$agegrp)
table.sex.agegrp
prop.table(table.sex.agegrp, 1)*100

table.agegrp.mar <-table(jen$agegrp, jen$mar_stat)
table.agegrp.mar
prop.table(table.agegrp.mar,1)*100

table.agegrp.employ<-table(jen$agegrp, jen$employ)
table.agegrp.employ
prop.table(table.agegrp.employ,1)




#this function calculates the confidence interval

CI <- function(variable){
  mean<- mean(variable)
  sd<-sd(variable)
  error <- qt(0.975,df=length(variable)-1)*sd(variable)/sqrt(length(variable))
  upperbound <- mean(variable)+error
  lowerbound <- mean(variable)-error
  print("mean")
  print(mean)
  print("sd" )
  print(sd)
  print("upperbound")
  print(upperbound)
  print("lowerbound")
  print(lowerbound)
}

CI(jen$age)

CImv <- function(variable){
  mean<- mean(variable, na.rm = TRUE)
  sd<-sd(variable, na.rm = TRUE)
  error <- qt(0.975,df=length(variable)-1)*sd/sqrt(length(variable))
  upperbound <- mean+error
  lowerbound <- mean-error
  print("mean")
  print(mean)
  print("sd" )
  print(sd)
  print("upperbound")
  print(upperbound)
  print("lowerbound")
  print(lowerbound)
}

CImv(jen$pperroom)

CI(jen$hsize)

CImv(jen$totrooms)



#dummies for model, sex, marital status, employ, age, parish

#employ dummy

#employ use not working as null


jen$worker <- jen$employ
jen$worker <- ifelse(jen$employ == 'W', 1, jen$worker)
jen$worker <- ifelse(jen$employ != "W", 0, jen$worker)


jen$employer <- jen$employ
jen$employer <-ifelse(jen$employ == 'E', 1, jen$employer)
jen$employer <- ifelse(jen$employ != 'E', 0, jen$employer)

#for charest
jen$worker <- jen$employ
jen$worker <- ifelse(jen$employ == '3', 1, jen$worker)
jen$worker <- ifelse(jen$employ != "3", 0, jen$worker)


jen$employer <- jen$employ
jen$employer <-ifelse(jen$employ == '2', 1, jen$employer)
jen$employer <- ifelse(jen$employ != '2', 0, jen$employer)



#make sex dummy

jen$female <-jen$sex
jen$female <- ifelse(jen$sex == 'F', 1, jen$female)
jen$female <- ifelse(jen$sex != 'F', 0, jen$female)

#marital status dummy, married null, igonore divorce and unknown
jen$single <-jen$mar_stat
jen$single <- ifelse(jen$mar_stat== 'Single', 1, jen$single)
jen$single <- ifelse(jen$mar_stat!='Single', 0, jen$single)

jen$mar_abs <-jen$mar_stat
jen$mar_abs <-ifelse(jen$mar_stat == 'Married spouse absent', 1, jen$mar_abs)
jen$mar_abs <- ifelse(jen$mar_stat != 'Married spouse absent', 0, jen$mar_abs)

jen$widow <- jen$mar_stat
jen$widow <- ifelse(jen$mar_stat == 'Widowed', 1, jen$widow)
jen$widow <- ifelse (jen$mar_stat != 'Widowed', 0, jen$widow)

#regressions


#regression people per room

reg.pperroom <-lm(pperroom~worker+ employer, data=jen)
summary(reg.pperroom)

#build up single level regression
#variables: age+ female+ single + mar_abs +widow+ employer+ worker

null.single<- lm(pperroom ~ 1, data = jen)
summary(null.single)
logLik(null.single)


reg1 <- lm(pperroom~age, data=jen)
summary(reg1)
logLik(reg1)

reg2<- lm(pperroom~ age + female, data= jen)
summary(reg2)
logLik(reg2)

reg3<- lm(pperroom~age + female+ single + mar_abs +widow, data = jen)
tidy_reg3<- tidy(reg3)
summary(reg3)
logLik(reg3)

reg4<- lm(pperroom~age + female+ single + mar_abs +widow + employer + worker, data = jen)
#library(broom)
tidy_reg4<- tidy(reg4)
summary(reg4)
logLik(reg4)

#log regression of servants or not
#recode numebr of servants to present or not

jen$servent <- jen$nservants

jen$servent[jen$nservants>=1]<- 1
jen$servent[jen$nservants==0]<- 0

#logreg<- lm(servent~age + female+ single + mar_abs +widow + employer + worker, data = jen)
#summary(logreg)

#multiple regression, parish level 

install.packages('lme4')
library(lme4)

MLM<-function(x, y, mydata){
  null.mlm<- lmer(y~ (1 | x), data=mydata, REML=FALSE)
  null.single<- lm(y ~ 1, data = data)
  print(summary(null.mlm))
  print(logLik(null.mlm))
  print(summary(null.single))
  print(logLik(null.single))
  
}

#MLM(jen$parish, jen$pperroom, jen)



null.mlm<- lmer(pperroom~ (1 | parish), data=jen, REML=FALSE)
summary(null.mlm)
tidy_mlm<- tidy(null.mlm)
logLik(null.mlm)

mlm.3<-lmer(pperroom~ age+ (1|parish), data = jen, REML = FALSE)
summary(mlm.3)
logLik(mlm.3)

mlm.4<-lmer(pperroom~ age+ female+ (1|parish), data = jen, REML = FALSE)
summary(mlm.4)
logLik(mlm.4)

mlm.5<-lmer(pperroom~ age+ female+ single + mar_abs +widow+ (1|parish), data = jen, REML = FALSE)
summary(mlm.5)
logLik(mlm.5)

mlm.6<-lmer(pperroom~ age+ female+ single + mar_abs +widow+ employer+ worker+(1|parish), data = jen, REML = FALSE)
summary(mlm.6)
logLik(mlm.6)

#MLM career level 

mlm.occ.null.single <- lm(pperroom ~ 1, data = jen)
summary(mlm.occ.null.single)
logLik(mlm.occ.null.single)

mlm.occ.null.mlm<- lmer(pperroom~ (1 | occlab1), data=jen, REML=FALSE)
summary(mlm.occ.null.mlm)
logLik(mlm.occ.null.mlm)


mlm.occ.null.mlm3<- lmer(pperroom~ (1 | occlab1/occlab2), data=jen, REML=FALSE)
summary(mlm.occ.null.mlm3)
logLik(mlm.occ.null.mlm3)


mlm.occ.mlm3 <- lmer(pperroom~age+ female+ single + mar_abs +widow+ employer+ worker+(1 | occlab1), data=jen, REML=FALSE)
summary(mlm.occ.mlm3)


#multiple correspondence analysis


#subset to categorical

jen$edinburgh<- ifelse(jen$edinburgh=='12', 'Edinburgh', jen$edinburgh)
#jen$edinburgh<- ifelse(jen$edinburgh=='2', 'Other Parish', jen$edinburgh)
jen$employ<- ifelse(jen$employ== 'E', 'Employer', jen$employ)
jen$employ<- ifelse(jen$employ== '1', 'Not working', jen$employ)
jen$employ<- ifelse(jen$employ=='3', 'Worker', jen$employ)
#jen$country<- ifelse(jen$country=='43', 'Born Scottland', jen$country)
#jen$country<- ifelse(jen$country=='2', 'Born Other Country', jen$country)
jen$disabled<- ifelse(jen$disabled=='6', 'Able', jen$disabled)
#jen$disabled<- ifelse(jen$disabled=='2', 'Disabled', jen$disabled)



jen$edinburgh<- factor(jen$edinburgh)
jen$disabled<- factor(jen$disabled)
jen$mar_stat<- factor(jen$mar_stat)
jen$employ<- factor(jen$employ)
jen$country<- factor(jen$country)

cvars<- c('mar_stat', 'sex', 'employ', 'edinburgh', 'disabled', 'country')

subjen <- jen[, cvars]

cats = apply(subjen, 2, function(x) nlevels(as.factor(x)))
cats


library(FactoMineR)

mca1 = MCA(subjen, graph = FALSE)
mca1

mca1$eig

mca1$var

#write.csv(mca1$var, '~/Dowloads/mca.csv')

# data frame with variable coordinates
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))

# data frame with observation coordinates
mca1_obs_df = data.frame(mca1$ind$coord)

mca1_obs_df

library(ggplot2)

# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) 


mca_coord<- as.data.frame(mca1$var$coord)

#Let's try single level CA

marvars<- c('mar_stat', 'sex')

marjen<- jen[, marvars]

marcats<- apply(marjen, 2, function(x) nlevels(as.factor(x)))

mcamar = MCA(marjen, graph = FALSE)
mcamar

mcamar$eig
mcamar$var
mcamar$ind$cos2
mcamar$call
mcamar$var$v.test
mcamar$var$contrib
mcamar$var$cos2
mcamar$var$coord

mcamar_vars_df = data.frame(mcamar$var$coord, Variable = rep(names(marcats), marcats))
mcamar_vars_df

mcamar_obs_df = data.frame(mcamar$ind$coord)


ggplot(data=mcamar_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mcamar_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) 


#MCA occlab1 

jen$occlab1<-factor(jen$occlab1)

ocvars<- c('occlab1', 'sex')

occjen<- jen[, ocvars]

cats2 = apply(occjen, 2, function(x) nlevels(as.factor(x)))
cats2

mca2 = MCA(occjen, graph = FALSE)

# data frame with variable coordinates
mca2_vars_df = data.frame(mca2$var$coord, Variable = rep(names(cats2), cats2))

# data frame with observation coordinates
mca2_obs_df = data.frame(mca2$ind$coord)


# plot of variable categories
ggplot(data=mca2_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca2_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) 


jen$occlab1<- ifelse(jen$occlab1=='NA', 'not working', jen$occlab1)

jen$occlab1[jen$occlab1=='NA']<- 'not working'





































