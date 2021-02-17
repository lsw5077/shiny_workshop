rm(list=ls())

library(shiny)
library(tidyverse)
library(rfishbase)

List1 <- data.frame(Species = c("Sander vitreus","Salvelinus namaycush","Perca flavescens", "Esox lucius","Esox masquinongy","Lota lota","Acipenser fulvescens"),
                   Common_Name = c("Walleye","Lake trout","Yellow perch","Northern pike","Muskellunge", "Burbot","Lake sturgeon")) 
fish <- popgrowth()
numbers1 <-  List1 %>%left_join(fish, by = "Species") %>%group_by(Species) %>%
  mutate(lAsym = mean(TLinfinity, na.rm = TRUE),k = mean(K, na.rm = TRUE),t0 = mean(to, na.rm = TRUE),tmax = round(mean(tmax, na.rm = TRUE))) %>%select(Species, Common_Name, lAsym, k, t0, tmax) %>%distinct()

fish <- popgrowth() %>% select(Species,TLinfinity,K,to,tmax)
fish2 <- length_weight() %>% select(Species,a,b)
numbers2 <-  List1 %>%left_join(fish) %>% left_join(fish2) %>%group_by(Species) %>%
  mutate(lAsym = mean(TLinfinity,na.rm=TRUE),k=mean(K,na.rm=TRUE),t0=mean(to,na.rm=TRUE),tmax = round(mean(tmax,na.rm=TRUE)),a=mean(a,na.rm=TRUE),b=mean(b,na.rm=TRUE))%>%
  select(Species, Common_Name, lAsym, k, t0, tmax, a, b) %>%distinct()

fish1=numeric()
for (i in 1:nrow(List1)){
  cond=numbers1$Common_Name == List1$Common_Name[i]
  tmp <- data.frame(Age = 0:numbers1$tmax[cond], 
                    Common_Name = List1$Common_Name[i])
  tmp$Length <- numbers1$lAsym[cond]*(1-exp(-numbers1$k[cond]*(tmp$Age - numbers1$t0[cond])))
  
  cond=numbers2$Species==List1$Species[i]
  tmp$mass <- numbers2$a[cond]*tmp$Length^numbers2$b[cond]
  fish1=rbind(fish1,tmp)
}

setwd('U:\\uf\\sesync course\\CrappieFishApp')
write.csv(fish1,'fishy data.csv',row.names=F)