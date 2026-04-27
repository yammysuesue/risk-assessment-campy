hh_obs$start.time <- as.POSIXct(strftime(hh_obs$start, format = "%H:%M:%S"), tryFormats = "%H:%M:%S")
hh_obs$end.time <- as.POSIXct(strftime(hh_obs$end, format = "%H:%M:%S"), tryFormats = "%H:%M:%S")

##create time data;
list.id <- unique(hh_obs$hh_id)
time.dat <- data.frame()
for (i in 1:length(list.id)){
  k.id <- list.id[i]
  temp.dat <- hh_obs[which(hh_obs$hh_id == k.id), ]
  temp.dat <- temp.dat[order(temp.dat$start.time),]
  temp.time.dat <- data.frame(time = as.POSIXct(
    seq(min(as.numeric(temp.dat$start.time)), max(as.numeric(temp.dat$end.time)),by = 1), ## time interval is 1 sec
    origin = "1970-01-01"))
  temp.time.dat$id <- k.id
  temp.time.dat$age_days <- unique(hh_obs$age_days[which(hh_obs$hh_id == k.id)])
  temp.time.dat$sleep <- 0
  temp.time.dat$awake <- 0
  temp.time.dat$drinking <- 0
  temp.time.dat$bathing <- 0
  temp.time.dat$carried.mom <- 0
  temp.time.dat$carried.other <- 0
  temp.time.dat$down.barrier <- 0
  temp.time.dat$down.bareGround <- 0
  temp.time.dat$freq.var <- NA ## list the frequency variables
  
  for (j in 1:length(temp.dat$hh_id)){
    if (temp.dat$item[j] == "sleeping"){
      temp.time.dat$sleep[which(temp.time.dat$time >= temp.dat$start.time[j] & temp.time.dat$time <= temp.dat$end.time[j])] <- 1
    } 
    else if (temp.dat$item[j] == "awake"){
      temp.time.dat$awake[which(temp.time.dat$time >= temp.dat$start.time[j] & temp.time.dat$time <= temp.dat$end.time[j])] <- 1
    } 
    else if (temp.dat$item[j] == "drinking"){
      temp.time.dat$drinking[which(temp.time.dat$time >= temp.dat$start.time[j] & temp.time.dat$time <= temp.dat$end.time[j])] <- 1
    } 
    else if (temp.dat$item[j] == "bathing"){
      temp.time.dat$bathing[which(temp.time.dat$time >= temp.dat$start.time[j] & temp.time.dat$time <= temp.dat$end.time[j])] <- 1
    } 
    else if (temp.dat$item[j] == "carried-mom"){
      temp.time.dat$carried.mom[which(temp.time.dat$time >= temp.dat$start.time[j] & temp.time.dat$time <= temp.dat$end.time[j])] <- 1
    } 
    else if (temp.dat$item[j] == "carried-other"){
      temp.time.dat$carried.other[which(temp.time.dat$time >= temp.dat$start.time[j] & temp.time.dat$time <= temp.dat$end.time[j])] <- 1
    } 
    else if (temp.dat$item[j] == "down-with barrier"){
      temp.time.dat$down.barrier[which(temp.time.dat$time >= temp.dat$start.time[j] & temp.time.dat$time <= temp.dat$end.time[j])] <- 1
    } 
    else if (temp.dat$item[j] == "down-bare ground"){
      temp.time.dat$down.bareGround[which(temp.time.dat$time >= temp.dat$start.time[j] & temp.time.dat$time <= temp.dat$end.time[j])] <- 1
    } 
    else {
      temp.time.dat$freq.var[which(temp.time.dat$time == temp.dat$start.time[j]) & temp.time.dat$time == temp.dat$end.time[j] ] <- temp.dat$item[j]
    }
  }
  time.dat <- rbind(time.dat,temp.time.dat)
}

#time.dat <- time.dat[-which(sum(time.dat[,4:11])!=2),]

time.dat$freq.var[which(time.dat$freq.var == "On homestead" | 
                          time.dat$freq.var == "on homestead" |
                          time.dat$freq.var == "off homestead")] <- NA
time.dat$freq.var <- as.factor(time.dat$freq.var)

## subset Activity variables
time.dat.act <- time.dat %>% select(1:7) %>% mutate(act.var = case_when(
  sleep == 1 ~ 1,
  awake == 1 ~ 2,
  drinking == 1 ~ 3,
  bathing == 1 ~ 4,
  TRUE ~ 0
)) %>% arrange(age_days, id)

time.dat.act$id <- factor(time.dat.act$id, levels = unique(time.dat.act$id)) # arrange hh_id by age_days with an ascending order


## plot the activity variables
## use age_days as label for hh_id
my_label <- hh_obs %>% select(hh_id, age_days) %>% group_by(hh_id) %>% 
  summarise(label = unique(age_days)) %>% arrange(label)
my_label$label <- as.character(my_label$label)

par(mfrow=c(1,1))
options(repr.plot.width = 10, repr.plot.height = 3)
g1 <- ggplot(time.dat.act, aes(time, id)) + 
  geom_tile(aes(fill = factor(act.var, levels = c("1", "2", "3", "4", "0")))) +  
  scale_fill_manual(name = "Activity keys",
                    values = c("gold","skyblue","green4",
                                     "violet","grey"
                    ),
                    labels = c("Sleeping", "Awake", "Drinking", "Bathing", "NA")
  ) + ylab("Child age when observed (days)") + xlab("Time") + 
  theme(axis.text.y = element_text(size = 4)) + scale_y_discrete(labels = my_label$label)
g1

ggsave("~/stat/EXCAM/Bruce/Data monitor/activity_keys_age1.tiff", plot = g1,
       units="in", width=5, height=4, dpi=300, compression = 'lzw')
ggsave("~/stat/EXCAM/Bruce/Data monitor/activity_keys_age2.tiff", plot = g1,
       units="in", width=5, height=4, dpi=300, compression = 'lzw')

## subset Compartment variables
time.dat.com <- time.dat %>% select(1:3, 8:11) %>% mutate(com.var = case_when(
  carried.mom == 1 ~ 1,
  carried.other == 1 ~ 2,
  down.barrier == 1 ~ 3,
  down.bareGround == 1 ~ 4,
  TRUE ~ 0
)) %>% arrange(age_days, id)

time.dat.com$id <- factor(time.dat.com$id, levels = unique(time.dat.com$id)) # arrange hh_id by age_days with an ascending order

## plot the compartment variables
par(mfrow=c(1,1))
options(repr.plot.width = 10, repr.plot.height = 3)
g2 <- ggplot(time.dat.com, aes(time, id)) + 
  geom_tile(aes(fill = factor(com.var, levels = c("1", "2", "3", "4", "0")))) +  
  scale_fill_manual(name = "Compartment keys",
                    values = c("gold","green3","violet", "blue3", "grey"
                    ),
                    labels = c("Carried by mom", "Carried by others", 
                               "Down with barrier ", "Down-bare ground" ,"NA")
  ) + ylab("Child age when observed (days)") + xlab("Time") + 
  theme(axis.text.y = element_text(size = 4)) + scale_y_discrete(labels = my_label$label)
g2
ggsave("~/stat/EXCAM/Bruce/Data monitor/compartment_keys_age1.tiff", plot = g2,
       units="in", width=5, height=4, dpi=300, compression = 'lzw')
ggsave("~/stat/EXCAM/Bruce/Data monitor/compartment_keys_age2.tiff", plot = g2,
       units="in", width=5, height=4, dpi=300, compression = 'lzw')

##############################################################################
load("~/stat/EXCAM/Bruce/childbx/freq/v1/output/rate_table1.rda")

library(reshape)
rate_table <- rbind(rate_table,colSums(count_table)/sum(dur_vec$duration))
rownames(rate_table)[14] <- "total"
temp <- cbind(rep(rownames(rate_table),15),melt(rate_table))
names(temp) <- c("state","behavior","rate")
ggplot(temp, aes(behavior, state, fill= rate)) + 
  geom_tile() + geom_text(aes(label = round(rate,2)), color = "black", size = 4) +
  scale_fill_gradient(low="ivory", high="red",limits=c(0,1.2),breaks=c(0,0.3,0.6,0.9,1.2),labels=c(0,0.3,0.6,0.9,1.2)) +
  theme(axis.text.x=element_text(angle=90,hjust=1))
ggsave("./output/heat1.pdf",width=8,height=6)

load("~/stat/EXCAM/Bruce/childbx/freq/v1/output/rate_table2.rda")
rate_table <- rbind(rate_table,colSums(count_table)/sum(dur_vec$duration))
rownames(rate_table)[16] <- "total"
temp <- cbind(rep(rownames(rate_table),16),melt(rate_table))
names(temp) <- c("state","behavior","rate")
ggplot(temp, aes(behavior, state, fill= rate)) + 
  geom_tile() + geom_text(aes(label = round(rate,2)), color = "black", size = 4) +
  scale_fill_gradient(low="ivory", high="red",limits=c(0,1.2),breaks=c(0,0.3,0.6,0.9,1.2),labels=c(0,0.3,0.6,0.9,1.2)) +
  theme(axis.text.x=element_text(angle=90,hjust=1))
ggsave("./output/heat2.pdf",width=8,height=6)
