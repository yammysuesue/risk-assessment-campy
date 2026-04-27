#library(foreign)
#install.packages("dplyr")
#install.packages("ggplot2")
library(dplyr)
library(ggplot2)
#library(rjags)

ver <- "v1";
setwd("~/research/Song_Liang/EXCAM/andrew")

agecat <- 1;
# two times of observations at different age; 
# agecat = 1 or 2;

#load data;
#directly load Bruce preprocessed data for first observation;
hh_obs <- readRDS("observational_data_tp1_processed.RDS")
#hh_obs <- readRDS("~/stat/EXCAM/Bruce/Behavior Observation Data/Processed Data/observational_data_tp2_processed.RDS")
#table(hh_obs$name)
hh_obs$name <- toupper(hh_obs$name)
extract.obs_id <- function(name){
  paste(strsplit(name,c("[-._]+"))[[1]][2:3],collapse = "_")
}
hh_obs$obs_id <- sapply(hh_obs$name,extract.obs_id)

hh_obs$Start.datetime <- as.POSIXlt(strptime(hh_obs$Start.datetime,"%m/%d/%Y %H:%M:%S"));
hh_obs$End.datetime <- as.POSIXlt(strptime(hh_obs$End.datetime,"%m/%d/%Y %H:%M:%S"));
colnames(hh_obs)[colnames(hh_obs)=='Ending.time'] <- 'End.time'
#simplify the items;
hh_obs$item <- tolower(hh_obs$item)
hh_obs$item_simple <- hh_obs$item
hh_obs$item_simple[which(hh_obs$item %in% c("carried-mom arms","carried-mom back"))] <- "carried-mom"
hh_obs$item_simple[which(hh_obs$item %in% c("carried-other arms","carried-other back"))] <- "carried-other"
hh_obs$item_simple[which(hh_obs$item %in% c("drinking-breastmilk","drinking-other","drinking-water"))] <- "drinking"
hh_obs$item_simple[which(hh_obs$item %in% c("eating-injera", "eating-other",  "eating-raw plant"))] <- "eaing"
hh_obs$item_simple[which(hh_obs$item %in% c("mouthing-child hand", "mouthing-mom hand",  "mouthing-other adult hand"))] <- "mouthing-others hand"
table(hh_obs$item_simple)

table(hh_obs$category)
hh_obs$category <- as.character(hh_obs$category)
hh_obs$category[hh_obs$category=='mistake'] <- 'Mistake'
hh_obs$category[! (hh_obs$category %in% c('Location', 'Compartment', 'Activity', 'Mistake'))] <- 'Event'
table(hh_obs$category)

hh_obs <- hh_obs[order(hh_obs$hh_id,hh_obs$Start.datetime),]

dat.loc <- subset(hh_obs, category=='Location', select=c(hh_id, obs_id, Start.datetime, End.datetime, Start.time, End.time, item_simple, item))
dat.com <- subset(hh_obs, category=='Compartment', select=c(hh_id, obs_id, Start.datetime, End.datetime, Start.time, End.time, item_simple, item))
dat.act <- subset(hh_obs, category=='Activity', select=c(hh_id, obs_id, Start.datetime, End.datetime, Start.time, End.time, item_simple, item))
dat.evt <- subset(hh_obs, category=='Event', select=c(hh_id, obs_id, Start.datetime, End.datetime, Start.time, End.time, item_simple, item))
################################
# check time gaps for each layer
################################


# check large time gaps in location. We compare such gaps with time intervals of compartments. 
dat.loc$gap <- NA
for(i in 2:nrow(dat.loc))
  if(dat.loc$obs_id[i] == dat.loc$obs_id[i-1]) dat.loc$gap[i] <- dat.loc$Start.time[i] - dat.loc$End.time[i-1]
hist(dat.loc$gap[!is.na(dat.loc$gap) & dat.loc$gap>10], breaks=50)
# if lag is more than 5 seconds, take a further look
check.id <- unique(dat.loc$obs_id[!is.na(dat.loc$gap) & dat.loc$gap>10]) 
check <- subset(dat.loc, obs_id %in% check.id)
# examine entries with large gaps
# BALLO_014, BALLO_082, MERI_091, MERI_111, FAYO_132 had large gaps in compartment.
# All gaps seem to be accidental error, as they are all covered by consecutive compartment time intervals.
obs.id = 'BALLO_082'
check.loc = subset(dat.loc, obs_id==obs.id)
check.com = subset(dat.com, obs_id==obs.id)

# check large time gaps in compartment. We compare such gaps with time intervals of activities. 
dat.com$gap <- NA
for(i in 2:nrow(dat.com))
  if(dat.com$obs_id[i] == dat.com$obs_id[i-1]) dat.com$gap[i] <- dat.com$Start.time[i] - dat.com$End.time[i-1]
hist(dat.com$gap[!is.na(dat.com$gap) & dat.com$gap>50], breaks=50)
check.id <- unique(dat.com$obs_id[!is.na(dat.com$gap) & dat.com$gap>50]) 
check <- subset(dat.com, obs_id %in% check.id)
# FAYO_025, FAYO_059, FAYO_075, BALLO_114 had large gaps in compartment.
# All gaps seem to be accidental error, as they are all covered by consecutive activity time intervals.
obs.id = 'BALLO_114'
check.com = subset(dat.com, obs_id==obs.id)
check.act = subset(dat.act, obs_id==obs.id)

# check large time gaps in compartment. We compare such gaps with time intervals of activities. 
dat.act$gap <- NA
for(i in 2:nrow(dat.act))
  if(dat.act$obs_id[i] == dat.act$obs_id[i-1]) dat.act$gap[i] <- dat.act$Start.time[i] - dat.act$End.time[i-1]
hist(dat.act$gap[!is.na(dat.act$gap) & dat.act$gap>50], breaks=50)
check.id <- unique(dat.act$obs_id[!is.na(dat.act$gap) & dat.act$gap>50]) 
check <- subset(dat.act, obs_id %in% check.id)
# FAYO_001, FAYO_009, FAYO_010, FAYO_013, FAYO_016, BALLO_106 had large gaps in compartment.
# For FAYO, nearly all gaps occurred during drinking breast milk. We may simply bridge most gaps as they are most likely drinking breast milk.
# For BALLO_106, it seems to an accidental error, but it's not clear whether the gap was associated with sleeping or awake.
obs.id = 'BALLO_106'
check.com = subset(dat.com, obs_id==obs.id)
check.act = subset(dat.act, obs_id==obs.id)
check.evt = subset(dat.evt, obs_id==obs.id)



newdat.loc <- dat.loc[1,]
j<-1
for(i in 2:nrow(dat.loc)){
  # if same obs_id and item_simple appear multiple times, combine them into a single entry
  if(dat.loc$obs_id[i] == dat.loc$obs_id[i-1] & dat.loc$item_simple[i] == dat.loc$item_simple[i-1]){
    newdat.loc$Ending.time[j] <- dat.loc$Ending.time[i]
  } else{
    newdat.loc <- rbind(newdat.loc, dat.loc[i,])
    j <- j+1
    # if two consecutive entries have the same obs_id and the end time of the first is exactly the same as
    # the start time of the next, add 1 second to the start time of the next. This is merely to
    # create non-overlapping time segments, which may not be necessary.
    if(dat.loc$obs_id[i] == dat.loc$obs_id[i-1] & dat.loc$Start.time[i] == dat.loc$Ending.time[i-1])  
      newdat.loc$Start.time[j] <- newdat.loc$Start.time[j] + 1
  }
}
newdat.loc$item <- NULL

newdat.com <- dat.com[1,]
j<-1
for(i in 2:nrow(dat.com)){
  if(dat.com$obs_id[i] == dat.com$obs_id[i-1] & dat.com$item_simple[i] == dat.com$item_simple[i-1]){
    newdat.com$Ending.time[j] <- dat.com$Ending.time[i]
  } else{
    newdat.com <- rbind(newdat.com, dat.com[i,])
    j <- j+1
    if(dat.com$obs_id[i] == dat.com$obs_id[i-1] & dat.com$Start.time[i] == dat.com$Ending.time[i-1])  
      newdat.com$Start.time[j] <- newdat.com$Start.time[j] + 1
  }
}
newdat.com$item <- NULL

newdat.act <- dat.act[1,]
j<-1
for(i in 2:nrow(dat.act)){
  if(dat.act$obs_id[i] == dat.act$obs_id[i-1] & dat.act$item_simple[i] == dat.act$item_simple[i-1]){
    newdat.act$Ending.time[j] <- dat.act$Ending.time[i]
  } else{
    newdat.act <- rbind(newdat.act, dat.act[i,])
    j <- j+1
    if(dat.act$obs_id[i] == dat.act$obs_id[i-1] & dat.act$Start.time[i] == dat.act$Ending.time[i-1])  
      newdat.act$Start.time[j] <- newdat.act$Start.time[j] + 1
  }
}
newdat.act$item <- NULL

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
check <- subset(hh_obs, select=c(obs_id, Start.time, Ending.time, category, item_simple, item, loc, comp, act, act.cnt))

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

save(rate_table,file="./output/rate_table.rda")
