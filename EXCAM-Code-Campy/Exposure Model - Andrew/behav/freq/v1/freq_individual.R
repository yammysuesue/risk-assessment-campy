library(reshape)

dat.total.dur1 <- aggregate(dur ~ hh_id,data=hh_obs_dur,sum)
dat.act1 <- aggregate(dur ~ hh_id + act,data=hh_obs_dur,sum)
dat.act1 <- cast(dat.act1,hh_id ~ act)
dat.comp1 <- aggregate(dur ~ hh_id + comp,data=hh_obs_dur,sum)
dat.comp1 <- cast(dat.comp1,hh_id ~ comp)

# dat.awake1 <- aggregate(dur ~ hh_id,data=hh_obs_dur[which(hh_obs_dur$act=="awake"),],sum)
# dat.bathing1 <- aggregate(dur ~ hh_id,data=hh_obs_dur[which(hh_obs_dur$act=="bathing"),],sum)
# dat.drinking1 <- aggregate(dur ~ hh_id,data=hh_obs_dur[which(hh_obs_dur$act=="drinking"),],sum)
# dat.sleeping1 <- aggregate(dur ~ hh_id,data=hh_obs_dur[which(hh_obs_dur$act=="sleeping"),],sum)
# dat.mom1 <- aggregate(dur ~ hh_id,data=hh_obs_dur[which(hh_obs_dur$comp=="carried-mom"),],sum)
# dat.other1 <- aggregate(dur ~ hh_id,data=hh_obs_dur[which(hh_obs_dur$comp=="carried-other"),],sum)
# dat.ground1 <- aggregate(dur ~ hh_id,data=hh_obs_dur[which(hh_obs_dur$comp=="down-bare ground"),],sum)
# dat.barrier1 <- aggregate(dur ~ hh_id,data=hh_obs_dur[which(hh_obs_dur$comp=="down-with barrier"),],sum)

dat.total.freq1 <- aggregate(act ~ hh_id + item,data=hh_obs_freq,length)
dat.freq1 <- cast(dat.total.freq1,hh_id ~ item)

dat1 <- merge(merge(merge(dat.total.dur1,dat.act1,by="hh_id"),dat.comp1,by="hh_id"),dat.freq1,by="hh_id")
dat1[is.na(dat1)] <- 0
dat1[,-c(1,2)] <- dat1[,-c(1,2)]/dat1[,2]
dat1 <- merge(dat1,hh_obs[-which(duplicated(hh_obs$hh_id)),c("hh_id","age_days")],by="hh_id")
dat1$timepoint <- 1

dat.total.dur2 <- aggregate(dur ~ hh_id,data=hh_obs_dur,sum)
dat.act2 <- aggregate(dur ~ hh_id + act,data=hh_obs_dur,sum)
dat.act2 <- cast(dat.act2,hh_id ~ act)
dat.comp2 <- aggregate(dur ~ hh_id + comp,data=hh_obs_dur,sum)
dat.comp2 <- cast(dat.comp2,hh_id ~ comp)
dat.total.freq2 <- aggregate(act ~ hh_id + item,data=hh_obs_freq,length)
dat.freq2 <- cast(dat.total.freq2,hh_id ~ item)

dat2 <- merge(merge(merge(dat.total.dur2,dat.act2,by="hh_id"),dat.comp2,by="hh_id"),dat.freq2,by="hh_id")
dat2[is.na(dat2)] <- 0
dat2[,-c(1,2)] <- dat2[,-c(1,2)]/dat2[,2]
dat2 <- merge(dat2,hh_obs[-which(duplicated(hh_obs$hh_id)),c("hh_id","age_days")],by="hh_id")
dat2$timepoint <- 2

dat1$"Pica-feces" <- 0
dat1 <- dat1[,c(1:18,28,19:27)]

dat_long <- rbind(dat1,dat2)

names(dat1)[2:27] <- paste0(names(dat1)[2:27],1)
names(dat2)[2:27] <- paste0(names(dat2)[2:27],2)
dat_wide <- merge(dat1[-28],dat2[-28],by="hh_id")

# save(dat_long,file="./output/dat_long.rda")
# save(dat_wide,file="./output/dat_wide.rda")

pdf("./output/scatterplot_vs_age.pdf",height=8,width=6)
par(mfrow=c(3,2))
for (i in 3:26){
  plot(dat_long$age_days,dat_long[,i],xlab="age in days",ylab=names(dat_long)[i])
  abline(lm(dat_long[,i] ~ dat_long$age_days))
}
dev.off()

dat_ratio <- dat_wide[,29:53]/dat_wide[3:27]
names(dat_ratio) <- names(dat_long)[3:27]
pdf("./output/paried_ratio_two_timepoints.pdf",height=8,width=6)
par(mfrow=c(1,1))
par(mar=c(12.1,4.1,1,1))
boxplot(log10(dat_ratio),xlab="",las=2,ylab="log10 ratio timepoint 2 vs. timepoint 1")
dev.off()

plot_freq <- melt(dat_long[,c(11:26,28)],id.vars=c("timepoint"),variable.names="category")
plot_freq$timepoint <- ifelse(plot_freq$timepoint==1,"01","02")

p_freq <- ggplot(plot_freq,aes(x=variable,y=value,fill=timepoint)) +
  geom_boxplot() + labs(x="",y="rate") + scale_y_continuous(trans='log10') + #ylim(0.001,1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5, size=15),
        axis.text.y = element_text(size=15),axis.title.y = element_text(size=25),
        strip.text.x = element_text(size = 20),legend.title = element_text(size=20),
        legend.text = element_text(size=20))
ggsave(paste0("./output/rate_plot",agecat,".pdf",p_freq,width=8,height=8)
