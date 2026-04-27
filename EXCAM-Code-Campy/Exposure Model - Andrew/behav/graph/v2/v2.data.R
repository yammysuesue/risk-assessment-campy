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

dur_vec <- aggregate(hh_obs_dur$dur, list(hh_obs_dur$comp_act), FUN=sum)
names(dur_vec) <- c("comp_act","duration")

if (all.equal(rownames(count_table),dur_vec$comp_act)){
  rate_table <- count_table/dur_vec$duration
} else {cat("There is a mismatch for the compartment and activity state.")}

colSums(count_table)/sum(dur_vec$duration)

#save(rate_table,file="./output/rate_table1.rda")

#rename
hh_obs_dur1 <- hh_obs_dur
hh_obs_freq1 <- hh_obs_freq

sum(hh_obs_dur$dur,na.rm=T)/60
dat1 <- aggregate(hh_obs_dur$dur,by=list(hh_obs_dur$hh_id,hh_obs_dur$act),FUN=sum,na.rm=T)
names(dat1) <- c("ID","act","dur")
prop1 <- cast(dat1, ID ~ act)
prop1[which(is.na(prop1[,3])),3] <- 0
prop1[,-1] <- prop1[,-1]/aggregate(hh_obs_dur$dur,by=list(hh_obs_dur$hh_id),FUN=sum,na.rm=T)[,-1]
colMeans(prop1[,-1],na.rm=T)

table(hh_obs_dur$category)
dat1com <- aggregate(hh_obs_dur$dur,by=list(hh_obs_dur$hh_id,hh_obs_dur$comp),FUN=sum,na.rm=T)
names(dat1com) <- c("ID","comp","dur")
prop1com <- cast(dat1com, ID ~ comp)
prop1com[which(is.na(prop1com[,3])),3] <- 0
prop1com[which(is.na(prop1com[,4])),4] <- 0
prop1com[,-1] <- prop1com[,-1]/aggregate(hh_obs_dur$dur,by=list(hh_obs_dur$hh_id),FUN=sum,na.rm=T)[,-1]
colMeans(prop1com[,-1],na.rm=T)

dat1loc <- aggregate(hh_obs_dur$dur,by=list(hh_obs_dur$hh_id,hh_obs_dur$loc),FUN=sum,na.rm=T)
names(dat1loc) <- c("ID","loc","dur")
prop1loc <- cast(dat1loc, ID ~ loc)
prop1loc[which(is.na(prop1loc[,2])),2] <- 0
prop1loc[,-1] <- prop1loc[,-1]/aggregate(hh_obs_dur$dur,by=list(hh_obs_dur$hh_id),FUN=sum,na.rm=T)[,-1]
colMeans(prop1loc[,-1],na.rm=T)


#################################################################
agecat <- 2;
# two times of observations at different age; 
# agecat = 1 or 2;

#load data;
#directly load Bruce preprocessed data for first observation;
#hh_obs <- readRDS("~/stat/EXCAM/Bruce/Behavior Observation Data/Processed Data/observational_data_tp1_processed.RDS")
hh_obs <- readRDS("~/stat/EXCAM/Bruce/Behavior Observation Data/Processed Data/observational_data_tp2_processed.RDS")
hh_obs2 <- hh_obs

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
count_table <- rbind(count_table[2:8,],rep(0,16),count_table[9:15,])
rownames(count_table)[8] <- "carried-other_sleeping"

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

dur_vec <- aggregate(hh_obs_dur$dur, list(hh_obs_dur$comp_act), FUN=sum)
names(dur_vec) <- c("comp_act","duration")

# count_table <- rbind(count_table[c(2:7,9,8),],rep(0,16),rep(0,16),count_table[12:15,],rep(0,16))
# rownames(count_table)[9:10] <- c("down-bare ground_awake","down-bare ground_bathing")
# rownames(count_table)[15] <- "down-with barrier_sleeping"
if (all.equal(rownames(count_table),dur_vec$comp_act)){
  rate_table <- count_table/dur_vec$duration
} else {cat("There is a mismatch for the compartment and activity state.")}

#save(rate_table,file="./output/rate_table2.rda")

#rename
hh_obs_dur2 <- hh_obs_dur
hh_obs_freq2 <- hh_obs_freq

dat2 <- aggregate(hh_obs_dur$dur,by=list(hh_obs_dur$hh_id,hh_obs_dur$act),FUN=sum,na.rm=T)
names(dat2) <- c("ID","act","dur")
prop2 <- cast(dat2, ID ~ act)
prop2[which(is.na(prop2[,3])),3] <- 0
prop2[which(is.na(prop2[,5])),5] <- 0
prop2[,-1] <- prop2[,-1]/aggregate(hh_obs_dur$dur,by=list(hh_obs_dur$hh_id),FUN=sum,na.rm=T)[,-1]
colMeans(prop2[,-1],na.rm=T)

colMeans(prop1[,-1],na.rm=T)
colMeans(prop2[,-1],na.rm=T)

table(hh_obs_dur$category)
dat2com <- aggregate(hh_obs_dur$dur,by=list(hh_obs_dur$hh_id,hh_obs_dur$comp),FUN=sum,na.rm=T)
names(dat2com) <- c("ID","comp","dur")
prop2com <- cast(dat2com, ID ~ comp)
prop2com[which(is.na(prop2com[,3])),3] <- 0
prop2com[which(is.na(prop2com[,4])),4] <- 0
prop2com[,-1] <- prop2com[,-1]/aggregate(hh_obs_dur$dur,by=list(hh_obs_dur$hh_id),FUN=sum,na.rm=T)[,-1]
colMeans(prop2com[,-1],na.rm=T)

dat2loc <- aggregate(hh_obs_dur$dur,by=list(hh_obs_dur$hh_id,hh_obs_dur$loc),FUN=sum,na.rm=T)
names(dat2loc) <- c("ID","loc","dur")
prop2loc <- cast(dat2loc, ID ~ loc)
prop2loc[which(is.na(prop2loc[2])),2] <- 0
prop2loc[,-1] <- prop2loc[,-1]/aggregate(hh_obs_dur$dur,by=list(hh_obs_dur$hh_id),FUN=sum,na.rm=T)[,-1]
colMeans(prop2loc[,-1],na.rm=T)

prop1$timepoint <- "01"
prop1 <- as.data.frame(prop1[,-1])
plot.prop1 <- melt(prop1,id.vars="timepoint",variable.names="category")
plot.prop1$type <- "activity"
prop2$timepoint <- "02"
prop2 <- as.data.frame(prop2[,-1])
plot.prop2 <- melt(prop2,id.vars="timepoint",variable.names="category")
plot.prop2$type <- "activity"
prop1com$timepoint <- "01"
prop1com <- as.data.frame(prop1com[,-1])
plot.prop1com <- melt(prop1com,id.vars="timepoint",variable.names="category")
plot.prop1com$type <- "compartment"
prop2com$timepoint <- "02"
prop2com <- as.data.frame(prop2com[,-1])
plot.prop2com <- melt(prop2com,id.vars="timepoint",variable.names="category")
plot.prop2com$type <- "compartment"
prop1loc$timepoint <- "01"
prop1loc <- as.data.frame(prop1loc[,-1])
plot.prop1loc <- melt(prop1loc,id.vars="timepoint",variable.names="category")
plot.prop1loc$type <- "location"
prop2loc$timepoint <- "02"
prop2loc <- as.data.frame(prop2loc[,-1])
plot.prop2loc <- melt(prop2loc,id.vars="timepoint",variable.names="category")
plot.prop2loc$type <- "location"
plot.prop <- rbind(plot.prop1,plot.prop2,plot.prop1com,plot.prop2com,plot.prop1loc,plot.prop2loc)

p_prop <- ggplot(plot.prop,aes(x=variable,y=value,fill=timepoint)) +
  geom_boxplot() + ylim(0,1) + labs(x="",y="proportion") + 
  facet_grid( ~ type, scales = "free", space = "free") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5, size=15),
        axis.text.y = element_text(size=15),axis.title.y = element_text(size=25),
        strip.text.x = element_text(size = 20),legend.title = element_text(size=20),
        legend.text = element_text(size=20))
ggsave("./output/prop_plot.pdf",p_prop,width=12,height=8)

