# EC-MUG andChromocult data cleaning
## Define working directory
setwd("~/stat/EXCAM/env_conc/data")

## Read packages
library(readxl) # package to read in Excel spreadsheets
library(tidyverse) # package to facilitate data processing
library(MPN)

## Read and clean data
ecmug <- read_excel("Final E.coli data 20221202.xlsx", sheet = "EC-MUG")
names(ecmug) <- c("sample_id","sample_type2","vol_collected","vol_tested","vol_tested_undiluted","dilution","num_reaction","conc_factor","num_well_flur")

meta.dat <- read_excel("Final E.coli data 20221202.xlsx", sheet = "Metadata")

chromo <- read_excel("Final E.coli data 20221202.xlsx", sheet = "Chromocult")

#merge
ecmug <- merge(ecmug,chromo,all=T)
ecmug <- merge(ecmug,meta.dat,all.x=T)

# Extract information from sample_id;
id.nchar <- nchar(ecmug$sample_id)
ecmug$study_id <- NA
ecmug$source <- NA
ecmug$member <- NA
ecmug$timepoint <- NA
for (i in 1:length(id.nchar)){
  ecmug$study_id[i] <- substr(ecmug$sample_id[i],1,3)
  ecmug$source[i] <- substr(ecmug$sample_id[i],4,4)
  ecmug$member[i] <- substr(ecmug$sample_id[i],5,id.nchar[i]-2)
  ecmug$timepoint[i] <- substr(ecmug$sample_id[i],id.nchar[i]-1,id.nchar[i])
}

# Create sample_type;
ecmug$sample_type <- NA
ecmug$sample_type[which(ecmug$source=="H" & ecmug$member=="A")] <- "areola swab"
ecmug$sample_type[which(ecmug$source=="E" & ecmug$member=="B")] <- "bathing water"
ecmug$sample_type[which(ecmug$source=="H" & ecmug$member=="B")] <- "breastmilk"
ecmug$sample_type[which(ecmug$source=="E" & ecmug$member=="D")] <- "drinking water"
ecmug$sample_type[which(ecmug$source=="E" & ecmug$member=="F")] <- "food"
ecmug$sample_type[which(ecmug$source=="H" & ecmug$member=="H")] <- "sibling handrinse"
ecmug$sample_type[which(ecmug$source=="H" & ecmug$member=="P")] <- "mother handrinse"
ecmug$sample_type[which(ecmug$source=="H" & ecmug$member=="R")] <- "child handrinse"
ecmug$sample_type[which(ecmug$source=="E" & ecmug$member=="S")] <- "soil"
# Need to check with Amanda about what types of fomites those are.
ecmug$sample_type[which(ecmug$member %in% c("V(1)","V(2)","V(3)"))] <- "fomite"

# calculate concentration of samples tested and then back calculate the concentration of raw samples with different units;
# areola swab (per swab) and fomite (per swab) need to find out how large the swabbed area (in cm2)
# water samples (per mL)
# food (per g) # need to find out the lab protocol for food.
# handrinse (per pair of hands).
# first change the conc_factor from 0 to 1 

#concentration factor NA means no dilution???
ecmug$conc_factor[which(is.na(ecmug$conc_factor) & !is.na(ecmug$Concentration_factor))] <- ecmug$Concentration_factor[which(is.na(ecmug$conc_factor) & !is.na(ecmug$Concentration_factor))]
ecmug$conc_factor[which(ecmug$conc_factor==0)] <- 1

ecmug$conc <- NA

# bathing water, drinking water (per mL)
ecmug$conc[which(ecmug$sample_type %in% c("bathing water","drinking water"))] <- (ecmug$num_well_flur/(ecmug$num_reaction*180)*ecmug$conc_factor*1000)[which(ecmug$sample_type %in% c("bathing water","drinking water"))]
# handrinse (per pair of hands)
ecmug$conc[which(ecmug$sample_type %in% c("mother handrinse","child handrinse","sibling handrinse"))] <- (ecmug$num_well_flur/(ecmug$num_reaction*180)*ecmug$conc_factor*1000*200)[which(ecmug$sample_type %in% c("mother handrinse","child handrinse","sibling handrinse"))]
# swab (per swab); do we know the size of the area swabbed?
ecmug$conc[which(ecmug$sample_type %in% c("areola swab"))] <- (ecmug$num_well_flur/(ecmug$num_reaction*180)*ecmug$conc_factor*1000*3/4*5)[which(ecmug$sample_type %in% c("areola swab"))]
# fomite sponge (per sponge); do we know the size of the area of fomite?
ecmug$conc[which(ecmug$sample_type %in% c("fomite"))] <- (ecmug$num_well_flur/(ecmug$num_reaction*180)*ecmug$conc_factor*1000*3/9*10)[which(ecmug$sample_type %in% c("fomite"))]
# food (per gram)
ecmug$conc[which(ecmug$sample_type %in% c("food"))] <- (ecmug$num_well_flur/(ecmug$num_reaction*180)*ecmug$conc_factor*1000*3/8*9)[which(ecmug$sample_type %in% c("food"))]
# breastmilk (per mL)
ecmug$conc[which(ecmug$sample_type %in% c("breastmilk"))] <- (ecmug$Ecoli_colony_counted/0.2*ecmug$conc_factor)[which(ecmug$sample_type %in% c("breastmilk"))]
# soil surface (per surface): what's the size of area of soil surface?
ecmug$conc[which(ecmug$sample_type %in% c("soil"))] <- (ecmug$Ecoli_colony_counted/0.2*ecmug$conc_factor*15)[which(ecmug$sample_type %in% c("soil"))]
# for food tested using chromo; food (per gram);
ecmug$conc[which(ecmug$sample_type %in% c("food") & is.na(ecmug$conc))] <- (ecmug$Ecoli_colony_counted/0.2*ecmug$conc_factor*3/8*9)[which(ecmug$sample_type %in% c("food") & is.na(ecmug$conc))]


# Replace samples with more than 1 well tested using MPN calculation;

ecmug_sub <- ecmug[which(ecmug$num_reaction>1),]

mpn_res <- rep(NA, nrow(ecmug_sub))

excam_dat <- data.frame(
  positive = ecmug_sub$num_well_flur,
  tubes = ecmug_sub$num_reaction,
  amount = rep(180,length(ecmug_sub$vol_tested_undiluted))
)

for(i in 1:nrow(excam_dat)) {
  result = mpn(positive = excam_dat[i, 1],
               tubes = excam_dat[i, 2],
               amount = excam_dat[i, 3])
  mpn_res[i] = result$MPN
}

mpn_res[which(ecmug_sub$sample_type %in% c("bathing water","drinking water"))] = mpn_res[which(ecmug_sub$sample_type %in% c("bathing water","drinking water"))]*1000
mpn_res[which(ecmug_sub$sample_type %in% c("mother handrinse","child handrinse","sibling handrinse"))] = mpn_res[which(ecmug_sub$sample_type %in% c("mother handrinse","child handrinse","sibling handrinse"))]*1000*200
mpn_res[which(ecmug_sub$sample_type %in% c("areola swab"))] = mpn_res[which(ecmug_sub$sample_type %in% c("areola swab"))]*1000*3/4*5
mpn_res[which(ecmug_sub$sample_type %in% c("fomite"))] = mpn_res[which(ecmug_sub$sample_type %in% c("fomite"))]*1000*3/9*10
mpn_res[which(ecmug_sub$sample_type %in% c("food"))] = mpn_res[which(ecmug_sub$sample_type %in% c("food"))]*1000*3/8*9

mpn_res[which(mpn_res==Inf)] <- ecmug_sub$conc[which(mpn_res==Inf)]

ecmug$conc[which(ecmug$num_reaction>1)] <- mpn_res

#Limit of detection? May consider to use 0.5 to replace 0 which will give us a small but non-zero value for negative samples.
# "areola swab"       "bathing water"     "breastmilk"        "child handrinse"  
# "drinking water"    "fomite"            "food"              "mother handrinse" 
# "sibling handrinse" "soil"
# using 1 wells or 1 count as LOD for each sample type.
list.LOD <- c(25/6,25/396,5,1250/99,
              25/396,100/27,3.75,1250/99,
              1250/99,75)


library(fitdistrplus)
library(MASS)

list_sample_type <- sort(unique(ecmug$sample_type))
n.iter <- 10000

env.paras <- list()

pdf(file="~/stat/EXCAM/env_conc/plots/est_conc_082323.pdf",width=8,height=8)
for (i in 1:length(list_sample_type)){
  temp <- data.frame(cbind(ecmug$conc[which(ecmug$sample_type==list_sample_type[i])],
                ecmug$conc[which(ecmug$sample_type==list_sample_type[i])]))
  names(temp) <- c("left","right")
  temp$left <- replace(temp$left,temp$left==0,NA)
  temp$right <- replace(temp$right,temp$right==0,list.LOD[i])
  fit_cens <- fitdistcens(log10(temp),"norm")
  env.paras[[i]] <- list(list_sample_type[i],summary(fit_cens)$estimate,summary(fit_cens)$vcov)
  para <- mvrnorm(n.iter,summary(fit_cens)$estimate,summary(fit_cens)$vcov)
  conc <- 10^(rnorm(n.iter,mean = para[,1], sd = para[,2]))
  conc_cens <- conc
  conc_cens[which(conc<list.LOD[i])] <- list.LOD[i]
  par(mfrow=c(3,1))
  hist(log10(conc),freq = F,main=paste0(list_sample_type[i],", Simulated"),xlim=c(-10,10),breaks=seq(-10,10,by=0.5))
  hist(log10(conc_cens),freq = F,main="Simulated Censored",xlim=c(-10,10),breaks=seq(-10,10,by=0.5))
  hist(log10(temp$right),freq = F,main="Observed",xlim=c(-10,10),breaks=seq(-10,10,by=0.5))
  par(mfrow=c(1,1))
  qqplot(log10(conc_cens),log10(temp$right),main=list_sample_type[i])
  abline(coef = c(0, 1))
}
dev.off()

save(env.paras,file="~/stat/EXCAM/env_conc/res/env.paras_080823.rda")

#######################################################
#sample size
table(ecmug$sample_type,ecmug$timepoint)
table(ecmug$timepoint)
#percent of positive
table(ecmug$sample_type[which(ecmug$conc>0)],ecmug$timepoint[which(ecmug$conc>0)])/table(ecmug$sample_type,ecmug$timepoint)
table(ecmug$timepoint[which(ecmug$conc>0)])/table(ecmug$timepoint)
#mean
aggregate(data=ecmug[which(ecmug$conc>0),],conc ~ timepoint + sample_type,FUN=mean,na.rm=T)

#######################################################
library(reshape)

ecmug_data <- ecmug[,c("study_id","timepoint","sample_type","conc")]
ecmug_data1 <- ecmug_data %>% filter(timepoint=="01")
ecmug_data2 <- ecmug_data %>% filter(timepoint=="02")

ec.data1 <- cast(ecmug_data1[,-which(names(ecmug_data1)=="timepoint")], study_id ~ sample_type, mean)

ec.data <- cast(ecmug_data, timepoint + study_id ~ sample_type, mean, na.rm=T)

#two prop test;
for (i in 3:11){
  print(names(ec.data[i]))
  temp <- matrix(c(length(which(ec.data[,i]>0 & ec.data$timepoint=="01")),length(which(ec.data[,i]==0 & ec.data$timepoint=="01")),
                 length(which(ec.data[,i]>0 & ec.data$timepoint=="02")),length(which(ec.data[,i]==0 & ec.data$timepoint=="02"))),
                 nrow=2,byrow = T, dimnames = list(c("01", "02"), c("Pos", "Neg")))
  print(prop.test(temp))
}

#t.test;
for (i in 3:11){
  print(names(ec.data[i]))
  print(t.test(log10(ec.data[which(ec.data[,i]>0 & ec.data$timepoint=="01"),i]),log10(ec.data[which(ec.data[,i]>0 & ec.data$timepoint=="02"),i])))
}
t.test(log10(ec.data[which(ec.data[,3]>0 & ec.data$timepoint=="01"),3]),log10(ec.data[which(ec.data[,3]>0 & ec.data$timepoint=="02"),3]))

#Limit of detection? May consider to use 0.5 to replace 0 which will give us a small but non-zero value for negative samples.
# "areola swab"       "bathing water"     "breastmilk"        "child handrinse"  
# "drinking water"    "fomite"            "food"              "mother handrinse" 
# "sibling handrinse" "soil"
# using 1 wells or 1 count as LOD for each sample type.
list.LOD <- c(25/6,25/396,5,1250/99,
              25/396,100/27,3.75,1250/99,
              1250/99,75)

for (i in 1:length(list.LOD)){
  ec.data[which(ec.data[,i+2]==0),i+2] <- list.LOD[i]
  ec.data[,i+2] <- log10(ec.data[,i+2])
}


library(psych)
pdf(file="~/stat/EXCAM/env_conc/plots/corr_plot.pdf",width=8,height=8)
pairs.panels(ec.data[,-c(1,2)],
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman", # Correlation method (also "spearman" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals
dev.off()

#####################################################
ecmug1 <- ecmug[which(ecmug$timepoint=="01"),]
ecmug2 <- ecmug[which(ecmug$timepoint=="02"),]

env.paras <- list()
for (i in 1:(length(list_sample_type)-1)){
  temp <- data.frame(cbind(ecmug1$conc[which(ecmug1$sample_type==list_sample_type[i])],
                           ecmug1$conc[which(ecmug1$sample_type==list_sample_type[i])]))
  names(temp) <- c("left","right")
  temp$left <- replace(temp$left,temp$left==0,NA)
  temp$right <- replace(temp$right,temp$right==0,list.LOD[i])
  fit_cens <- fitdistcens(log10(temp),"norm")
  env.paras[[i]] <- list(list_sample_type[i],summary(fit_cens)$estimate,summary(fit_cens)$vcov)
  para <- mvrnorm(n.iter,summary(fit_cens)$estimate,summary(fit_cens)$vcov)
  conc <- 10^(rnorm(n.iter,mean = para[,1], sd = para[,2]))
  conc_cens <- conc
  conc_cens[which(conc<list.LOD[i])] <- list.LOD[i]
  # par(mfrow=c(3,1))
  # hist(log10(conc),freq = F,main=paste0(list_sample_type[i],", Simulated"),xlim=c(-10,10),breaks=seq(-10,10,by=0.5))
  # hist(log10(conc_cens),freq = F,main="Simulated Censored",xlim=c(-10,10),breaks=seq(-10,10,by=0.5))
  # hist(log10(temp$right),freq = F,main="Observed",xlim=c(-10,10),breaks=seq(-10,10,by=0.5))
  # par(mfrow=c(1,1))
  # qqplot(log10(conc_cens),log10(temp$right),main=list_sample_type[i])
  # abline(coef = c(0, 1))
}
env.paras[[10]] <- env.paras2[[10]]
env.paras
save(env.paras,file="~/stat/EXCAM/env_conc/res/env.paras_T1_101023.rda")

env.paras <- list()
for (i in 1:length(list_sample_type)){
  temp <- data.frame(cbind(ecmug2$conc[which(ecmug2$sample_type==list_sample_type[i])],
                           ecmug2$conc[which(ecmug2$sample_type==list_sample_type[i])]))
  names(temp) <- c("left","right")
  temp$left <- replace(temp$left,temp$left==0,NA)
  temp$right <- replace(temp$right,temp$right==0,list.LOD[i])
  fit_cens <- fitdistcens(log10(temp),"norm")
  env.paras[[i]] <- list(list_sample_type[i],summary(fit_cens)$estimate,summary(fit_cens)$vcov)
  para <- mvrnorm(n.iter,summary(fit_cens)$estimate,summary(fit_cens)$vcov)
  conc <- 10^(rnorm(n.iter,mean = para[,1], sd = para[,2]))
  conc_cens <- conc
  conc_cens[which(conc<list.LOD[i])] <- list.LOD[i]
  # par(mfrow=c(3,1))
  # hist(log10(conc),freq = F,main=paste0(list_sample_type[i],", Simulated"),xlim=c(-10,10),breaks=seq(-10,10,by=0.5))
  # hist(log10(conc_cens),freq = F,main="Simulated Censored",xlim=c(-10,10),breaks=seq(-10,10,by=0.5))
  # hist(log10(temp$right),freq = F,main="Observed",xlim=c(-10,10),breaks=seq(-10,10,by=0.5))
  # par(mfrow=c(1,1))
  # qqplot(log10(conc_cens),log10(temp$right),main=list_sample_type[i])
  # abline(coef = c(0, 1))
}
env.paras2 <- env.paras
save(env.paras,file="~/stat/EXCAM/env_conc/res/env.paras_T2_101023.rda")
