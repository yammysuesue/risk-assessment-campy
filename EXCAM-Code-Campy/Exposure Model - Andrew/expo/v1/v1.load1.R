#library(foreign)
library(dplyr)
library(ggplot2)
library(reshape)
#library(rjags)

ver <- "v1";
setwd("~/stat/EXCAM/Bruce/childbx/freq/v1/")

agecat <- 1;
# two times of observations at different age; 
# agecat = 1 or 2;

#load data;
#directly load Bruce preprocessed data for first observation;
hh_obs <- readRDS("~/stat/EXCAM/Bruce/Behavior Observation Data/Processed Data/observational_data_tp1_processed.RDS")
hh_obs1 <- hh_obs
#hh_obs <- readRDS("~/stat/EXCAM/Bruce/Behavior Observation Data/Processed Data/observational_data_tp2_processed.RDS")
mean(hh_obs$age_days[which(!duplicated(hh_obs$hh_id))])
range(hh_obs$age_days[which(!duplicated(hh_obs$hh_id))])

extract.obs_id <- function(name){
  paste(strsplit(name,c("[-.]+"))[[1]][2:3],collapse = "_")
}

hh_obs$obs_id <- sapply(hh_obs$name,extract.obs_id)
hh_obs$obs_id[which(hh_obs$obs_id=="meri_126_named corrected from 125")] <- "meri_126"

hh_obs$start <- as.POSIXlt(strptime(hh_obs$Start.datetime,"%m/%d/%Y %H:%M:%S"));
hh_obs$end <- as.POSIXlt(strptime(hh_obs$End.datetime,"%m/%d/%Y %H:%M:%S"));

#simplify the items;
hh_obs$item_simple <- hh_obs$item
hh_obs$item_simple[which(hh_obs$item %in% c("carried-mom arms","carried-mom back"))] <- "carried-mom"
hh_obs$item_simple[which(hh_obs$item %in% c("carried-other arms","carried-other back"))] <- "carried-other"
hh_obs$item_simple[which(hh_obs$item == "On homestead")] <- "on homestead"
hh_obs$item_simple[which(hh_obs$item %in% c("drinking-breastmilk","drinking-other","drinking-water"))] <- "drinking"

#drop some columns;
hh_obs <- hh_obs[,c("hh_id","obs_id","start","end","category","item_simple","age_days")]
names(hh_obs)[which(names(hh_obs)=="item_simple")] <- "item"

#fill in the gaps between two drinking behavior and two bathing behavior with awake.
hh_obs <- hh_obs[order(hh_obs$obs_id,hh_obs$start),]
temp.add <- hh_obs[1,]
for (i.id in unique(hh_obs$obs_id)){
  temp <- hh_obs[which(hh_obs$obs_id==i.id & hh_obs$category=="Activity"),]
  if (length(temp$start)>1){
    temp.new <- temp[1,]
    temp.new$category <- "Activity"
    temp.new$item <- "awake"
    for (j in 2:length(temp$start)){
      if (temp$start[j]>temp$end[j-1]+1 & temp$item[j-1]=="drinking" & temp$item[j]=="drinking"){
        temp.add <- rbind(temp.add,temp.new)
        temp.add[length(temp.add[,1]),"start"] <- temp$end[j-1]+1
        temp.add[length(temp.add[,1]),"end"] <- temp$start[j]-1
      } else if (temp$start[j]>temp$end[j-1]+1 & temp$item[j-1]=="bathing" & temp$item[j]=="bathing"){
        temp.add <- rbind(temp.add,temp.new)
        temp.add[length(temp.add[,1]),"start"] <- temp$end[j-1]+1
        temp.add[length(temp.add[,1]),"end"] <- temp$start[j]-1
      } else if (temp$start[j]<temp$end[j-1]+1){
        temp$start[j] <- temp$end[j-1]+1
      }
    }
  }
}
hh_obs <- rbind(hh_obs,temp.add[-1,])

hh_obs <- hh_obs[order(hh_obs$obs_id,hh_obs$start),]
for (i.id in unique(hh_obs$obs_id)){
  temp.loc <- NA
  temp.comp <- NA
  temp.act <- NA
  temp.act.cnt <- NA
  temp.ind <- which(hh_obs$obs_id==i.id)
  counter <- 1
  for (j in temp.ind){
    if (hh_obs$category[j]=="Location"){
      temp.loc <- hh_obs$item[j]
    }
    if (hh_obs$category[j]=="Compartment"){
      temp.comp <- hh_obs$item[j]
    }
    if (hh_obs$category[j]=="Activity"){
      temp.act <- hh_obs$item[j]
      temp.act.cnt <- paste(hh_obs$item[j],counter)
      counter <- counter + 1
    }
    hh_obs$loc[j] <- temp.loc
    hh_obs$comp[j] <- temp.comp
    hh_obs$act[j] <- temp.act
    hh_obs$act.cnt[j] <- temp.act.cnt
  }
}

hh_obs_freq <- hh_obs %>% filter(category %in% c("Eating","Mouthing","Pica","Touching"))

hh_obs_freq$comp_act <- paste(hh_obs_freq$comp,hh_obs_freq$act,sep="_")

count_table <- as.data.frame.matrix(table(hh_obs_freq$comp_act,hh_obs_freq$item))

hh_obs_dur <- hh_obs %>% filter(category %in% c("Activity","Compartment","Location"))
hh_obs_dur <- hh_obs_dur[which(hh_obs_dur$item!=""),]

# #find and remove deuplicated the continuous compartment, activity based on the simplified definition;
# hh_obs_dur <- hh_obs_dur[-(which(hh_obs_dur$item[2:length(hh_obs_dur$obs_id)]==hh_obs_dur$item[1:(length(hh_obs_dur$obs_id)-1)] &
#                                    hh_obs_dur$obs_id[2:length(hh_obs_dur$obs_id)]==hh_obs_dur$obs_id[1:(length(hh_obs_dur$obs_id)-1)]) + 1),]

for (i.id in unique(hh_obs_dur$obs_id)){
  temp.loc <- NA
  temp.comp <- NA
  temp.act <- NA
  temp.ind <- which(hh_obs_dur$obs_id==i.id)
  for (j in temp.ind){
    if (hh_obs_dur$category[j]=="Location"){
      temp.loc <- hh_obs_dur$item[j]
    }
    if (hh_obs_dur$category[j]=="Compartment"){
      temp.comp <- hh_obs_dur$item[j]
    }
    if (hh_obs_dur$category[j]=="Activity"){
      temp.act <- hh_obs_dur$item[j]
    }
    hh_obs_dur$loc[j] <- temp.loc
    hh_obs_dur$comp[j] <- temp.comp
    hh_obs_dur$act[j] <- temp.act
    # if (j==max(temp.ind)){
    #   hh_obs_dur$dur[j] <-   difftime(hh_obs_dur$end[j],hh_obs_dur$start[j],units = "secs")
    # } else {
    #   hh_obs_dur$dur[j] <- difftime(hh_obs_dur$start[j+1],hh_obs_dur$start[j],units = "secs")
    # }
  }
}

#remove few obs at the beginning;
hh_obs_dur <- hh_obs_dur[which(!is.na(hh_obs_dur$loc) & !is.na(hh_obs_dur$comp) & !is.na(hh_obs_dur$act)),]
hh_obs_dur$loc_comp <- paste(hh_obs_dur$loc,hh_obs_dur$comp,sep="_")
hh_obs_dur$state <- paste(hh_obs_dur$loc,hh_obs_dur$comp,hh_obs_dur$act,sep="_")

#find and remove deuplicated the continuous compartment, activity based on the simplified definition;
#calculate the duration of each state and then remove those records with duration equal to 0.

while(length(which(hh_obs_dur$state[2:length(hh_obs_dur$obs_id)]==hh_obs_dur$state[1:(length(hh_obs_dur$obs_id)-1)]))>0){
  hh_obs_dur <- hh_obs_dur[-which(hh_obs_dur$state[2:length(hh_obs_dur$obs_id)]==hh_obs_dur$state[1:(length(hh_obs_dur$obs_id)-1)]),]
  
  for (i.id in unique(hh_obs_dur$obs_id)){
    temp.ind <- which(hh_obs_dur$obs_id==i.id)
    for (j in temp.ind){
      if (j==max(temp.ind)){
        hh_obs_dur$dur[j] <-   difftime(hh_obs_dur$end[j],hh_obs_dur$start[j],units = "secs")
      } else {
        hh_obs_dur$dur[j] <- difftime(hh_obs_dur$start[j+1],hh_obs_dur$start[j],units = "secs")
      }
    }
  }
  if (length(which(hh_obs_dur$dur==0)>0)){
    hh_obs_dur <- hh_obs_dur[-which(hh_obs_dur$dur==0),]
  }
}


#change the unit of time to minute;
hh_obs_dur$dur <- hh_obs_dur$dur/60
hh_obs_dur$comp_act <- paste(hh_obs_dur$comp,hh_obs_dur$act,sep="_")

#rename
hh_obs_dur1 <- hh_obs_dur
hh_obs_freq1 <- hh_obs_freq

library(foreign)
library(dplyr)
library(ggplot2)
library(rjags)

ver <- "v1";
setwd("~/stat/EXCAM/Bruce/childbx/rate/v1/")

#subset the data;
dat_obs <- hh_obs_dur[,c("start","end","obs_id","loc","comp","act","dur","loc_comp","state")]

dat_obs$obs_id <- factor(dat_obs$obs_id)
dat_obs$loc_comp <- factor(dat_obs$loc_comp)
dat_obs$act <- factor(dat_obs$act)

#format data
#number of neighborhood;
n.neighb <- 1
n.comp <- length(unique(dat_obs$loc_comp))
n.behav <- length(unique(dat_obs$act))
n.subj <- length(unique(dat_obs$obs_id))
n.obs <- array(NA,dim=c(n.neighb,n.subj))
n.obs[1,] <- as.vector(table(dat_obs$obs_id))

nmax.subj <- max(n.subj)
nmax.obs <- max(n.obs)

obs.comp <- array(NA,dim=c(n.neighb,nmax.subj,nmax.obs));
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    #for(k.obs in 1:n.obs[k.neighb,k.subj]) {
    for(k.obs in 1:n.obs[k.subj]) {
      obs.comp[k.neighb,k.subj,k.obs] <- dat_obs$loc_comp[which(dat_obs$obs_id==levels(dat_obs$obs_id)[k.subj])[k.obs]]
    }
  }
}

obs.behav <- array(NA,dim=c(n.neighb,nmax.subj,nmax.obs));
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    #for(k.obs in 1:n.obs[k.neighb,k.subj]) {
    for(k.obs in 1:n.obs[k.subj]) {
      obs.behav[k.neighb,k.subj,k.obs] <- dat_obs$act[which(dat_obs$obs_id==levels(dat_obs$obs_id)[k.subj])[k.obs]]
    }
  }
}

obs.time <- array(NA,dim=c(n.neighb,nmax.subj,nmax.obs));
for(k.neighb in 1:n.neighb) {
  for(k.subj in 1:n.subj[k.neighb]) {
    #for(k.obs in 1:n.obs[k.neighb,k.subj]) {
    for(k.obs in 1:n.obs[k.subj]) {
      obs.time[k.neighb,k.subj,k.obs] <- dat_obs$dur[which(dat_obs$obs_id==levels(dat_obs$obs_id)[k.subj])[k.obs]]
    }
  }
}

logr           <-  0.5; # 2.303;
tau.loglambda  <-  4.0;
mu0.loglambda  <-  6.0;
tau0.loglambda <-  0.1;
zeros <- array(0,dim=c(n.neighb,nmax.subj,nmax.obs));

#"awake"    "bathing"  "drinking" "sleeping"
#"off homestead_carried-mom"       "off homestead_carried-other"    "off homestead_down-with barrier" "on homestead_carried-mom"       
#"on homestead_carried-other"      "on homestead_down-bare ground"  "on homestead_down-with barrier"
comp1.mask <- c(1,0,1,1);
comp2.mask <- c(1,0,0,0);
comp3.mask <- c(0,0,1,0);
comp4.mask <- c(1,1,1,1);
comp5.mask <- c(1,1,1,1);
comp6.mask <- c(1,0,0,0);
comp7.mask <- c(1,1,1,1);
comp.mask <- rbind(comp1.mask,comp2.mask,comp3.mask,
                   comp4.mask,comp5.mask,comp6.mask,comp7.mask);

sel.data <- list("n.neighb"=n.neighb,"n.comp"=n.comp,
                 "n.subj"=n.subj,"n.obs"=n.obs,
                 "n.behav"=n.behav, "obs.comp"=obs.comp,
                 "comp.mask"=comp.mask,
                 "obs.behav"=obs.behav,
                 "obs.time"=obs.time,"zeros"=zeros,
                 "logr"=logr,"tau.loglambda"=tau.loglambda,
                 "mu0.loglambda"=mu0.loglambda,
                 "tau0.loglambda"=tau0.loglambda)