#propensity score
#written for the datasets to be subbed in
syn<-#dataset


orig$dataset<-'orig'
syn$dataset<-'syn'
combo<-rbind(orig, syn)

combo$dbin<-0
combo$dbin[combo$dataset=='syn']<-1

log_prop<-glm(dbin~factor(parish)+ factor(sex)+ age + factor(mar_stat)+ factor(disability)+ 
                factor(occlab1)+ factor(employ)+ factor(inactive)+ factor(ctry_bth)+pperroom,
              data = combo, family = binomial(link='logit'))

#log_prop<-glm(dbin~factor(parish)+ factor(sex)+ age + factor(ctry_bth)+ pperroom,
#             data = combo, family = binomial(link='logit'))

library(broom)


log_prop_td<-tidy(log_prop)

vector<-log_prop$fitted.values

prop_score<-sum((vector-0.5)^2)/nrow(combo)

epmse<- ((nrow(log_prop_td)-1)*0.5^3)/nrow(combo)
stdev_pmse<- (sqrt(2*(nrow(log_prop_td)-1))*0.5^3)/nrow(combo)

standpmse<-(prop_score-epmse)/stdev_pmse

pmseratio<- prop_score/epmse


prop_score
standpmse
pmseratio











































































