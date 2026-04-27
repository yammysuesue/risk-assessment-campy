### simulate behavior sequences
#load rate table for poisson process;
load("./output/rate_table2.rda")
freq.beh.label <- colnames(rate_table)

#find unique edges;
source("D:/1. Yixuan Work/Spring 2026/RA/Risk Assessment/EXCAM-main/expo/v1/v1.load2.R")

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
load("./output/v1-mcmc-2.rda");
#load behavior sequence simulation functions;
source("./simul/v1/v1.simul.r")

neighbourhoods <- "Ethiopia"
compartments <-c("off homestead_carried-mom","off homestead_carried-other","off homestead_down-bare ground","off homestead_down-with barrier",
                 "on homestead_carried-mom","on homestead_carried-other","on homestead_down-bare ground","on homestead_down-with barrier");
behaviours <- c("awake", "bathing", "drinking", "sleeping");

n.neighb <- length(neighbourhoods);
n.comp <- length(compartments);
n.behav <-length(behaviours);
n.iter <- 3000;

for (c1 in 1:8){
  for (c2 in 1:8){
    for (a1 in 1:4){
      for (a2 in 1:4){
        if (!paste(c1,c2,a1,a2) %in% unique.edges){
          rate.parameters$lambda.mc[1,c1,c2,a1,a2,] <- rep(0,n.iter)
        }
      }
    }
  }
}

r <- rate.parameters$r;
lambda.mc <- rate.parameters$lambda.mc;

# load environmental parameters;
# load environmental parameters;
load("D:/1. Yixuan Work/Spring 2026/RA/Risk Assessment/EXCAM-main/env_conc/data/output/env.paras_via_T2_tot.rda")
env.paras <- env.paras_via_T2
rm(env.paras_via_T2)
load("D:/1. Yixuan Work/Spring 2026/RA/Risk Assessment/EXCAM-main/env_conc/data/output/env.paras_via_T2.rda")
env.paras$ES <- env.paras_via_T2$ES
env.paras$ED <- env.paras_via_T2$ED
