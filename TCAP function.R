#Here is the code for the TCAP function (this was previously called the RCAP function)

###############################################################
#TCAP function for key size 6
RCAP6<- function(dataset, syndata1, key1, key2, key3, key4, key5, key6, target){
  #here I'm just changing the names so that the function can run with any variable name
  dataset$orirow<- (1:nrow(dataset))
  dataset$Var1<- dataset[[key1]]
  dataset$Var2<- dataset[[key2]]
  dataset$Var3<- dataset[[key3]]
  dataset$Var4<- dataset[[key4]]
  dataset$Var5<- dataset[[key5]]
  dataset$Var6<- dataset[[key6]]
  dataset$Var7<- dataset[[target]]
  vars<- c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6','Var7','orirow') 
  table3<- dataset[, vars]
  
  #here I am calculating the synthetic cap score
  syn1tab1<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]], 
                                 syndata1[[key4]],syndata1[[key5]], syndata1[[key6]],
                                 syndata1[[target]]))
  syn1tab2<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]],
                                 syndata1[[key4]], syndata1[[key5]], syndata1[[key6]]))
  
  syn1tab3<-merge(syn1tab1, syn1tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'))
  syn1tab3<-subset(syn1tab3, syn1tab3$Freq.y!=0)
  
  
  syndata1$Var1<- syndata1[[key1]]
  syndata1$Var2<- syndata1[[key2]]
  syndata1$Var3<- syndata1[[key3]] 
  syndata1$Var4<- syndata1[[key4]]
  syndata1$Var5<- syndata1[[key5]]
  syndata1$Var6<- syndata1[[key6]]
  syndata1$Var7<- syndata1[[target]]
  
  syn1tab3<- merge(syndata1, syn1tab3, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6', 'Var7'))
  
  syn1tab3$syn_cap<- syn1tab3$Freq.x/syn1tab3$Freq.y
  
  
  #here I am reducing the synthetic dataset to just CAP scores of 1
  rcap_sub<- subset(syn1tab3, syn1tab3$syn_cap>=1)
  vars_syn<- c('Var1', 'Var2', 'Var3', 'Var4', 'Var5','Var6','Var7', 'Freq.x', 'Freq.y', 'syn_cap')
  rcap_sub<- rcap_sub[, vars_syn]
  uniq_vars<-c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6')
  
  rcap_sub<-unique(rcap_sub, by=uniq_vars)
  
  
  #here I am matching the reduced synthetic dataset back to the original dataset
  combined<-merge(table3,rcap_sub, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'), all.x=FALSE)
  
  #return(combined)
  #break()
  
  #here I am removing the non-matches
  combined$Var7.y[is.na(combined$Var7.y)]<-'missing'
  taub<-subset(combined, combined$Var7.y!='missing')
  
  
  #here I am calculating the rcap score
  taub$rcap<- 1
  taub$rcap<- ifelse(taub$Var7.x!=taub$Var7.y, 0, taub$rcap)
  
  
  #this is just printing out the average
  type= c('rcap')
  rcap= mean(taub$rcap)
  df= data.frame(type, rcap)
  return(df)
}

##############################################################
#TCAP function for key size 5
RCAP5<- function(dataset, syndata1, key1, key2, key3, key4, key5, target){
  #here I'm just changing the anmes so that the function can run with any variable name
  dataset$orirow<- (1:nrow(dataset))
  dataset$Var1<- dataset[[key1]]
  dataset$Var2<- dataset[[key2]]
  dataset$Var3<- dataset[[key3]]
  dataset$Var4<- dataset[[key4]]
  dataset$Var5<- dataset[[key5]]
  dataset$Var6<- dataset[[target]]
  vars<- c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6','orirow') 
  table3<- dataset[, vars]
  
  #here I am calculating the synthetic cap score
  syn1tab1<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]], 
                                 syndata1[[key4]],syndata1[[key5]], syndata1[[target]]))
  syn1tab2<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]],
                                 syndata1[[key4]], syndata1[[key5]]))
  
  syn1tab3<-merge(syn1tab1, syn1tab2, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'))
  syn1tab3<-subset(syn1tab3, syn1tab3$Freq.y!=0)
  
  
  syndata1$Var1<- syndata1[[key1]]
  syndata1$Var2<- syndata1[[key2]]
  syndata1$Var3<- syndata1[[key3]] 
  syndata1$Var4<- syndata1[[key4]]
  syndata1$Var5<- syndata1[[key5]]
  syndata1$Var6<- syndata1[[target]]
  #syndata1$synrow<- (1:nrow(syndata1))
  syn1tab3<- merge(syndata1, syn1tab3, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Var6'))
  
  syn1tab3$syn_cap<- syn1tab3$Freq.x/syn1tab3$Freq.y
  
  #here I am reducing the synthetic dataset to just CAP scores of 1
  rcap_sub<- subset(syn1tab3, syn1tab3$syn_cap>=1)
  vars_syn<- c('Var1', 'Var2', 'Var3', 'Var4', 'Var5','Var6', 'Freq.x', 'Freq.y', 'syn_cap')
  rcap_sub<- rcap_sub[, vars_syn]
  uniq_vars<-c('Var1', 'Var2', 'Var3', 'Var4', 'Var5')
  
  rcap_sub<-unique(rcap_sub, by=uniq_vars)
  
  #here I am matching the reduced synthetic dataset back to the original dataset
  combined<-merge(table3,rcap_sub, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=FALSE)
  
  #here I am removing the non-matches
  combined$Var6.y[is.na(combined$Var6.y)]<-'missing'
  taub<-subset(combined, combined$Var6.y!='missing')
  
  #here I am calculating the rcap score
  taub$rcap<- 1
  taub$rcap<- ifelse(taub$Var6.x!=taub$Var6.y, 0, taub$rcap)
  
  #this is just printing out the average
  type= c('rcap')
  rcap= mean(taub$rcap)
  df= data.frame(type, rcap)
  return(df)
}

#############################################################
#TCAP function for key size 4
RCAP4<- function(dataset, syndata1, key1, key2, key3, key4, target){
  #here I'm just changing the anmes so that the function can run with any variable name
  dataset$orirow<- (1:nrow(dataset))
  dataset$Var1<- dataset[[key1]]
  dataset$Var2<- dataset[[key2]]
  dataset$Var3<- dataset[[key3]]
  dataset$Var4<- dataset[[key4]]
  dataset$Var5<- dataset[[target]]
  vars<- c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'orirow') 
  table3<- dataset[, vars]
  
  #here I am calculating the synthetic cap score
  syn1tab1<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]], 
                                 syndata1[[key4]], syndata1[[target]]))
  syn1tab2<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]],
                                 syndata1[[key4]]))
  
  syn1tab3<-merge(syn1tab1, syn1tab2, by= c('Var1', 'Var2', 'Var3', 'Var4'))
  syn1tab3<-subset(syn1tab3, syn1tab3$Freq.y!=0)
  
  
  syndata1$Var1<- syndata1[[key1]]
  syndata1$Var2<- syndata1[[key2]]
  syndata1$Var3<- syndata1[[key3]] 
  syndata1$Var4<- syndata1[[key4]]
  syndata1$Var5<- syndata1[[target]]
  #syndata1$synrow<- (1:nrow(syndata1))
  syn1tab3<- merge(syndata1, syn1tab3, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'))
  
  syn1tab3$syn_cap<- syn1tab3$Freq.x/syn1tab3$Freq.y
  
  #here I am reducing the synthetic dataset to just CAP scores of 1
  rcap_sub<- subset(syn1tab3, syn1tab3$syn_cap>=1)
  vars_syn<- c('Var1', 'Var2', 'Var3', 'Var4', 'Var5', 'Freq.x', 'Freq.y', 'syn_cap')
  rcap_sub<- rcap_sub[, vars_syn]
  uniq_vars<-c('Var1', 'Var2', 'Var3', 'Var4')
  
  rcap_sub<-unique(rcap_sub, by=uniq_vars)
  
  #here I am matching the reduced synthetic dataset back to the original dataset
  combined<-merge(table3,rcap_sub, by= c('Var1', 'Var2', 'Var3', 'Var4'), all.x=FALSE)
  
  #here I am removing the non-matches
  combined$Var5.y[is.na(combined$Var5.y)]<-'missing'
  taub<-subset(combined, combined$Var5.y!='missing')
  
  #here I am calculating the rcap score
  taub$rcap<- 1
  taub$rcap<- ifelse(taub$Var5.x!=taub$Var5.y, 0, taub$rcap)
  
  #this is just printing out the average
  type= c('rcap')
  rcap= mean(taub$rcap)
  df= data.frame(type, rcap)
  return(df)
}

##############################################################
#TCAP function for key size 3
RCAP3<- function(dataset, syndata1, key1, key2, key3, target){
  #here I'm just changing the names so that the function can run with any variable name
  dataset$orirow<- (1:nrow(dataset))
  dataset$Var1<- dataset[[key1]]
  dataset$Var2<- dataset[[key2]]
  dataset$Var3<- dataset[[key3]]
  dataset$Var4<- dataset[[target]]
  vars<- c('Var1', 'Var2', 'Var3', 'Var4', 'orirow') 
  table3<- dataset[, vars]
  
  #here I am calculating the synthetic cap score
  syn1tab1<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]], 
                                 syndata1[[target]]))
  syn1tab2<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]]))
  
  syn1tab3<-merge(syn1tab1, syn1tab2, by= c('Var1', 'Var2', 'Var3'))
  syn1tab3<-subset(syn1tab3, syn1tab3$Freq.y!=0)
  
  
  syndata1$Var1<- syndata1[[key1]]
  syndata1$Var2<- syndata1[[key2]]
  syndata1$Var3<- syndata1[[key3]] 
  syndata1$Var4<- syndata1[[target]]
  #syndata1$synrow<- (1:nrow(syndata1))
  syn1tab3<- merge(syndata1, syn1tab3, by= c('Var1', 'Var2', 'Var3', 'Var4'))
  
  syn1tab3$syn_cap<- syn1tab3$Freq.x/syn1tab3$Freq.y
  
  #here I am reducing the synthetic dataset to just CAP scores of 1
  rcap_sub<- subset(syn1tab3, syn1tab3$syn_cap>=1)
  vars_syn<- c('Var1', 'Var2', 'Var3', 'Var4', 'Freq.x', 'Freq.y', 'syn_cap')
  rcap_sub<- rcap_sub[, vars_syn]
  uniq_vars<-c('Var1', 'Var2', 'Var3')
  
  rcap_sub<-unique(rcap_sub, by=uniq_vars)
  
  
  #here I am matching the reduced synthetic dataset back to the original dataset
  combined<-merge(table3,rcap_sub, by= c('Var1', 'Var2', 'Var3'), all.x=FALSE)
  
  
  #here I am removing the non-matches
  combined$Var4.y[is.na(combined$Var4.y)]<-'missing'
  taub<-subset(combined, combined$Var4.y!='missing')
  
  
  #here I am calculating the rcap score
  taub$rcap<- 1
  taub$rcap<- ifelse(taub$Var4.x!=taub$Var4.y, 0, taub$rcap)
  
  #this is just printing out the average
  type= c('rcap')
  rcap= mean(taub$rcap)
  df= data.frame(type, rcap)
  return(df)
}

