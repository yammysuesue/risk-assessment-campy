ver <- "v1";
agecat <- 2;
load(paste("~/stat/EXCAM/Bruce/childbx/rate/v1/output/",ver,"-post-",agecat,".rda",sep=""))

n.neighb <- 1;
n.comp <- 8;
n.behav <- 4;
n.chains <- 3;
n.iter <- 1000;

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
logr           <-  0.5; # 2.303;

lambda.mc <- array(NA,dim=c(n.neighb,
                            n.comp,n.comp,
                            n.behav,n.behav,
                            n.chains*n.iter));
tmp <- array(NA,dim=c(n.chains,n.iter));
for(k.neighb in 1:n.neighb){
  for(k.oldcomp in 1:n.comp){
    for(k.newcomp in 1:n.comp){
      for(k.oldbehav in 1:n.behav){
        for(k.newbehav in 1:n.behav){
          label <- as.character(paste("loglambda[",k.neighb,",",
                                      k.oldcomp,",",k.newcomp,",",
                                      k.oldbehav,",",k.newbehav,"]",sep=""));
          for(k.chains in 1:n.chains) {
            tmp[k.chains,] <- as.vector(mcmc.post[[k.chains]][,label]);
          }
          lambda.mc[k.neighb,k.oldcomp,k.newcomp,k.oldbehav,k.newbehav,] <-
            exp(c(tmp)) *
            comp.mask[k.oldcomp,k.oldbehav]*
            comp.mask[k.newcomp,k.newbehav]*
            (1-as.numeric((k.oldcomp==k.newcomp)&(k.oldbehav==k.newbehav)));
        }
      }
    }
  }
}

r <- exp(logr);

rate.parameters <- list("r"=r,"lambda.mc"=lambda.mc);


# r <- rate.parameters$r;
# lambda.mc <- rate.parameters$lambda.mc;
# 
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

# save(rate.parameters,file=paste("./output/",ver,"-mcmc-",
#                          subset[1],subset[2],".rda",sep=""),
#      ascii=TRUE);
