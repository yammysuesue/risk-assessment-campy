library(foreign)
library(dplyr)
library(ggplot2)
library(rjags)

ver <- "v1";
setwd("~/stat/EXCAM/Bruce/childbx/rate/v1/")

agecat <- 2;
# two times of observations at different age; 
# agecat = 1 or 2;

#load data;
#directly load Bruce preprocessed data for first observation;
hh_obs <- readRDS("~/stat/EXCAM/Bruce/Behavior Observation Data/Processed Data/observational_data_tp2_processed.RDS")

hh_obs_dur <- hh_obs %>% filter(Duration>0)

#remove 5 records with missing activity 
hh_obs_dur <- hh_obs_dur[-which(hh_obs_dur$item==""),]

#simplify the items;
hh_obs_dur$item_simple <- hh_obs_dur$item
hh_obs_dur$item_simple[which(hh_obs_dur$item %in% c("carried-mom arms","carried-mom back"))] <- "carried-mom"
hh_obs_dur$item_simple[which(hh_obs_dur$item %in% c("carried-other arms","carried-other back"))] <- "carried-other"
hh_obs_dur$item_simple[which(hh_obs_dur$item == "On homestead")] <- "on homestead"
hh_obs_dur$item_simple[which(hh_obs_dur$item %in% c("drinking-breastmilk","drinking-other","drinking-water"))] <- "drinking"

extract.obs_id <- function(name){
  paste(strsplit(name,c("[-.]+"))[[1]][2:3],collapse = "_")
}

hh_obs_dur$obs_id <- sapply(hh_obs_dur$name,extract.obs_id)

# hh_obs_dur$start <- as.POSIXlt(strptime(hh_obs_dur$Start.datetime,"%m/%d/%Y %H:%M:%S"));
# hh_obs_dur$end <- as.POSIXlt(strptime(hh_obs_dur$End.datetime,"%m/%d/%Y %H:%M:%S"));
hh_obs_dur$start <- hh_obs_dur$Start.datetime
hh_obs_dur$end <- hh_obs_dur$End.datetime

hh_obs_dur <- hh_obs_dur[order(hh_obs_dur$obs_id,hh_obs_dur$start),]

# #find and remove deuplicated the continuous compartment, activity based on the simplified definition;
# hh_obs_dur <- hh_obs_dur[-(which(hh_obs_dur$item_simple[2:length(hh_obs_dur$obs_id)]==hh_obs_dur$item_simple[1:(length(hh_obs_dur$obs_id)-1)] &
#                                    hh_obs_dur$obs_id[2:length(hh_obs_dur$obs_id)]==hh_obs_dur$obs_id[1:(length(hh_obs_dur$obs_id)-1)]) + 1),]

for (i.id in unique(hh_obs_dur$obs_id)){
  temp.loc <- NA
  temp.comp <- NA
  temp.act <- NA
  temp.ind <- which(hh_obs_dur$obs_id==i.id)
  for (j in temp.ind){
    if (hh_obs_dur$category[j]=="Location"){
      temp.loc <- hh_obs_dur$item_simple[j]
    }
    if (hh_obs_dur$category[j]=="Compartment"){
      temp.comp <- hh_obs_dur$item_simple[j]
    }
    if (hh_obs_dur$category[j]=="Activity"){
      temp.act <- hh_obs_dur$item_simple[j]
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

#subset the data;
dat_obs <- hh_obs_dur[,c("start","end","obs_id","loc","comp","act","dur","loc_comp","state")]

#dat_obs$obs_id[which(dat_obs$obs_id=="meri_126_named corrected from 125")] <- "meri_126"

dat_obs$obs_id <- factor(dat_obs$obs_id)
dat_obs$loc_comp <- factor(dat_obs$loc_comp)
dat_obs$act <- factor(dat_obs$act)
#change the unit of time to minute;
dat_obs$dur <- dat_obs$dur/60

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

comp1.mask <- c(1,0,1,0);
comp2.mask <- c(1,0,0,0);
comp3.mask <- c(1,0,0,0);
comp4.mask <- c(1,0,1,0);
comp5.mask <- c(1,1,1,1);
comp6.mask <- c(1,1,1,1);
comp7.mask <- c(1,1,1,0);
comp8.mask <- c(1,1,1,1);
comp.mask <- rbind(comp1.mask,comp2.mask,comp3.mask,comp4.mask,
                   comp5.mask,comp6.mask,comp7.mask,comp8.mask);

sel.data <- list("n.neighb"=n.neighb,"n.comp"=n.comp,
                 "n.subj"=n.subj,"n.obs"=n.obs,
                 "n.behav"=n.behav, "obs.comp"=obs.comp,
                 "comp.mask"=comp.mask,
                 "obs.behav"=obs.behav,
                 "obs.time"=obs.time,"zeros"=zeros,
                 "logr"=logr,"tau.loglambda"=tau.loglambda,
                 "mu0.loglambda"=mu0.loglambda,
                 "tau0.loglambda"=tau0.loglambda)
