### simulate behavior sequences
#load rate table for poisson process;
load("~/stat/EXCAM/Bruce/childbx/freq/v1/output/rate_table1.rda")
freq.beh.label <- colnames(rate_table)

#find unique edges;
source("~/stat/EXCAM/expo/v1/v1.load1.R")

unique.edges <- c()
for (i in 1:dim(obs.comp)[2]){
  unique.edges <- unique(c(unique.edges,
                           paste(as.vector(obs.comp[1,i,1:(dim(obs.comp)[3]-1)]),
                                 as.vector(obs.comp[1,i,-1]),
                                 as.vector(obs.behav[1,i,1:(dim(obs.behav)[3]-1)]),
                                 as.vector(obs.behav[1,i,-1]))))
}
unique.edges <- sort(unique.edges[-grep("NA",unique.edges)])
#load rate parameters;
source("~/stat/EXCAM/Bruce/childbx/rate/v1/v1.extract.r");
#load behavior sequence simulation functions;
source("~/stat/EXCAM/Bruce/childbx/simul/v1/v1.simul.r")

neighbourhoods <- "Ethiopia"
compartments <-c("off homestead_carried-mom","off homestead_carried-other","off homestead_down-with barrier",
                 "on homestead_carried-mom","on homestead_carried-other","on homestead_down-bare ground","on homestead_down-with barrier");
behaviours <- c("awake", "bathing", "drinking", "sleeping");

n.neighb <- length(neighbourhoods);
n.comp <- length(compartments);
n.behav <-length(behaviours);
n.iter <- 3000;

for (c1 in 1:7){
  for (c2 in 1:7){
    for (a1 in 1:4){
      for (a2 in 1:4){
        if (!paste(c1,c2,a1,a2) %in% unique.edges){
          rate.parameters$lambda.mc[1,c1,c2,a1,a2,] <- rep(0,n.iter)
        }
      }
    }
  }
}

rate.parameters$lambda.mc[1,2,1,1,1,] <- rate.parameters$lambda.mc[1,5,4,1,1,]
rate.parameters$lambda.mc[1,1,2,1,1,] <- rate.parameters$lambda.mc[1,4,5,1,1,]
rate.parameters$lambda.mc[1,1,1,3,1,] <- rate.parameters$lambda.mc[1,4,4,3,1,]
rate.parameters$lambda.mc[1,1,1,1,3,] <- rate.parameters$lambda.mc[1,4,4,1,3,]
rate.parameters$lambda.mc[1,1,1,1,4,] <- rate.parameters$lambda.mc[1,4,4,1,4,]

r <- rate.parameters$r;
lambda.mc <- rate.parameters$lambda.mc;

# load environmental parameters;
load("~/stat/EXCAM/env_conc/res/env.paras_T1_101023.rda")
