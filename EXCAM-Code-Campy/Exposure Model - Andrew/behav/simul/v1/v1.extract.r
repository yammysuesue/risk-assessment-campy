ver <- "v1";
setwd("~/stat/EXCAM/Bruce/childbx/simul/v1/")
# load hazard rate estimates
# source("~/stat/EXCAM/Bruce/childbx/rate/v1/v1.extract.r");

r <- rate.parameters$r;
lambda.mc <- rate.parameters$lambda.mc;

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
