ver <- "v1";
agecat <- 1;
load(paste("~/stat/EXCAM/Bruce/childbx/rate/v1/output/",ver,"-post-",agecat,".rda",sep=""))

n.neighb <- 1;
n.comp <- 7;
n.behav <- 4;
n.chains <- 3;
n.iter <- 1000;

comp1.mask <- c(1,0,1,1);
comp2.mask <- c(1,0,0,0);
comp3.mask <- c(0,0,1,0);
comp4.mask <- c(1,1,1,1);
comp5.mask <- c(1,1,1,1);
comp6.mask <- c(1,0,0,0);
comp7.mask <- c(1,1,1,1);
comp.mask <- rbind(comp1.mask,comp2.mask,comp3.mask,
                   comp4.mask,comp5.mask,comp6.mask,comp7.mask);
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

# save(rate.parameters,file=paste("./output/",ver,"-mcmc-",
#                          subset[1],subset[2],".rda",sep=""),
#      ascii=TRUE);
