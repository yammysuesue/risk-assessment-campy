#library(foreign)
library(dplyr)
library(ggplot2)
#library(rjags)

ver <- "v1";
setwd("~/stat/EXCAM/Bruce/childbx/freq/v1/")

agecat <- 1;
# two times of observations at different age; 
# agecat = 1 or 2;

#load data;
#directly load Bruce preprocessed data for first observation;
hh_obs <- readRDS("~/stat/EXCAM/Bruce/Behavior Observation Data/Processed Data/observational_data_tp1_processed.RDS")
#hh_obs <- readRDS("~/stat/EXCAM/Bruce/Behavior Observation Data/Processed Data/observational_data_tp2_processed.RDS")

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

hh_obs <- hh_obs[order(hh_obs$obs_id,hh_obs$Start.time),]
for (i.id in unique(hh_obs$obs_id)){
  temp.loc <- NA
  temp.comp <- NA
  temp.act <- NA
  temp.act.cnt <- NA
  temp.ind <- which(hh_obs$obs_id==i.id)
  counter <- 1
  for (j in temp.ind){
    if (hh_obs$category[j]=="Location"){
      temp.loc <- hh_obs$item_simple[j]
    }
    if (hh_obs$category[j]=="Compartment"){
      temp.comp <- hh_obs$item_simple[j]
    }
    if (hh_obs$category[j]=="Activity"){
      temp.act <- hh_obs$item_simple[j]
      temp.act.cnt <- paste(hh_obs$item_simple[j],counter)
      counter <- counter + 1
    }
    hh_obs$loc[j] <- temp.loc
    hh_obs$comp[j] <- temp.comp
    hh_obs$act[j] <- temp.act
    hh_obs$act.cnt[j] <- temp.act.cnt
  }
}


hh_obs_freq <- hh_obs %>% filter(Duration==0)
#remove records with category "Mistake" or "mistake"; 
hh_obs_freq <- hh_obs_freq[-which(hh_obs_freq$category %in% c("Mistake","mistake")),]

hh_obs_freq$comp_act <- paste(hh_obs_freq$comp,hh_obs_freq$act,sep="_")

count_table <- as.data.frame.matrix(table(hh_obs_freq$comp_act,hh_obs_freq$item_simple))

hh_obs_dur <- hh_obs %>% filter(Duration>0)
hh_obs_dur <- hh_obs_dur[which(hh_obs_dur$item_simple!=""),]

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


#change the unit of time to minute;
hh_obs_dur$dur <- hh_obs_dur$dur/60

hh_obs_dur$comp_act <- paste(hh_obs_dur$comp,hh_obs_dur$act,sep="_")

dur_vec <- aggregate(hh_obs_dur$dur, list(hh_obs_dur$comp_act), FUN=sum)
names(dur_vec) <- c("comp_act","duration")

if (all.equal(rownames(count_table),dur_vec$comp_act)){
  rate_table <- count_table/dur_vec$duration
} else {cat("There is a mismatch for the compartment and activity state.")}

colSums(count_table)/sum(dur_vec$duration)

save(rate_table,file="./output/rate_table1.rda")

#################################################################

agecat <- 2;
# two times of observations at different age; 
# agecat = 1 or 2;

#load data;
#directly load Bruce preprocessed data for first observation;
#hh_obs <- readRDS("~/stat/EXCAM/Bruce/Behavior Observation Data/Processed Data/observational_data_tp1_processed.RDS")
hh_obs <- readRDS("~/stat/EXCAM/Bruce/Behavior Observation Data/Processed Data/observational_data_tp2_processed.RDS")

extract.obs_id <- function(name){
  paste(strsplit(name,c("[-.]+"))[[1]][2:3],collapse = "_")
}

hh_obs$obs_id <- sapply(hh_obs$name,extract.obs_id)

hh_obs$start <- as.POSIXlt(strptime(hh_obs$Start.datetime,"%Y-%m-%d %H:%M:%S"));
hh_obs$end <- as.POSIXlt(strptime(hh_obs$End.datetime,"%Y-%m-%d %H:%M:%S"));

#simplify the items;
hh_obs$item_simple <- hh_obs$item
hh_obs$item_simple[which(hh_obs$item %in% c("carried-mom arms","carried-mom back"))] <- "carried-mom"
hh_obs$item_simple[which(hh_obs$item %in% c("carried-other arms","carried-other back"))] <- "carried-other"
hh_obs$item_simple[which(hh_obs$item == "On homestead")] <- "on homestead"
hh_obs$item_simple[which(hh_obs$item %in% c("drinking-breastmilk","drinking-other","drinking-water"))] <- "drinking"

hh_obs <- hh_obs[order(hh_obs$obs_id,hh_obs$Start.time),]
for (i.id in unique(hh_obs$obs_id)){
  temp.loc <- NA
  temp.comp <- NA
  temp.act <- NA
  temp.act.cnt <- NA
  temp.ind <- which(hh_obs$obs_id==i.id)
  counter <- 1
  for (j in temp.ind){
    if (hh_obs$category[j]=="Location"){
      temp.loc <- hh_obs$item_simple[j]
    }
    if (hh_obs$category[j]=="Compartment"){
      temp.comp <- hh_obs$item_simple[j]
    }
    if (hh_obs$category[j]=="Activity"){
      temp.act <- hh_obs$item_simple[j]
      temp.act.cnt <- paste(hh_obs$item_simple[j],counter)
      counter <- counter + 1
    }
    hh_obs$loc[j] <- temp.loc
    hh_obs$comp[j] <- temp.comp
    hh_obs$act[j] <- temp.act
    hh_obs$act.cnt[j] <- temp.act.cnt
  }
}


hh_obs_freq <- hh_obs %>% filter(Duration==0)
#remove records with category "Mistake" or "mistake"; 
hh_obs_freq <- hh_obs_freq[-which(hh_obs_freq$category %in% c("Mistake","mistake")),]
hh_obs_freq <- hh_obs_freq[-which(hh_obs_freq$comp=="" | hh_obs_freq$act==""),]

hh_obs_freq$comp_act <- paste(hh_obs_freq$comp,hh_obs_freq$act,sep="_")

count_table <- as.data.frame.matrix(table(hh_obs_freq$comp_act,hh_obs_freq$item_simple))
count_table <- rbind(count_table[1:7,],rep(0,16),count_table[8:14,])
rownames(count_table)[8] <- "carried-other_sleeping"









hh_obs_dur <- hh_obs %>% filter(Duration>0)
hh_obs_dur <- hh_obs_dur[which(hh_obs_dur$item_simple!=""),]

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


#change the unit of time to minute;
hh_obs_dur$dur <- hh_obs_dur$dur/60

hh_obs_dur$comp_act <- paste(hh_obs_dur$comp,hh_obs_dur$act,sep="_")

dur_vec <- aggregate(hh_obs_dur$dur, list(hh_obs_dur$comp_act), FUN=sum)
names(dur_vec) <- c("comp_act","duration")

count_table <- rbind(count_table[2:9,],rep(0,16),rep(0,16),count_table[12:15,])
if (all.equal(rownames(count_table),dur_vec$comp_act)){
  rate_table <- count_table/dur_vec$duration
} else {cat("There is a mismatch for the compartment and activity state.")}

save(rate_table,file="./output/rate_table2.rda")
