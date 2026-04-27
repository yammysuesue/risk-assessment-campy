ver <- "v1";
source("~/stat/EXCAM/Bruce/childbx/rate/v1/v1.data.r");
#source("~/stat/EXCAM/Bruce/childbx/rate/v1/v1.data2.r");


# obs.agecat <- function(k.neighb,k.subj){
#   age <- obs.global.attr[k.neighb,k.subj,1];
#   if(is.na(age)) age <- 0;
#   return(age);
# }

obs.agecat <- function(k.neighb,k.subj){
  age <- 1
  return(age);
}

obs.subj <- function(k.neighb,k.subj){
  n.obs <- length(na.omit(obs.behav[k.neighb,k.subj,]));
  bc <- rep(NA,n.obs-1);
  for(k.obs in 2:n.obs){
    old.comp <- obs.comp[k.neighb,k.subj,k.obs-1];
    old.behav <- obs.behav[k.neighb,k.subj,k.obs-1];
    new.comp <- obs.comp[k.neighb,k.subj,k.obs];
    new.behav <- obs.behav[k.neighb,k.subj,k.obs];
    bc[k.obs-1] <- paste(old.comp,old.behav,new.comp,new.behav);
  }
  return(bc);
}

obs.sequence <- function(k.neighb,k.agecat){
  n.subj <- length(na.omit(obs.behav[k.neighb,,1]));
  k.subj <- 1;
  # while(obs.agecat(k.neighb,k.subj) != k.agecat){
  #   k.subj <- k.subj + 1;
  # }
  bc <- obs.subj(k.neighb,k.subj);
  while(k.subj < n.subj){
    bc <- c(bc,obs.subj(k.neighb,k.subj));
    k.subj <- k.subj + 1;
  }
  return(bc);
}

make.adj <- function(bc){
  adj <- array(NA,dim=c(n.comp*n.behav,n.comp*n.behav));
  for(k.comp.old in 1:n.comp){
    for(k.behav.old in 1:n.behav){
      for(k.comp.new in 1:n.comp){
        for(k.behav.new in 1:n.behav){
          adj[(k.comp.old-1)*n.behav+k.behav.old,
              (k.comp.new-1)*n.behav+k.behav.new] <-
            length(which(bc==paste(k.comp.old,k.behav.old,
                             k.comp.new,k.behav.new)));
        }
      }
    }
  }
  return(adj);
}

make.edges <- function(bc){
  edg <- array(NA,dim=c(n.comp*n.behav*n.comp*n.behav,3));
  k <- 1;
  for(k.comp.old in 1:n.comp){
    for(k.behav.old in 1:n.behav){
      for(k.comp.new in 1:n.comp){
        for(k.behav.new in 1:n.behav){
          edg[k,] <- c((k.comp.old-1)*n.behav+k.behav.old,
                       (k.comp.new-1)*n.behav+k.behav.new,
            length(which(bc==paste(k.comp.old,k.behav.old,
                             k.comp.new,k.behav.new))));
          k <- k + 1;
        }
      }
    }
  }
  # return(edg[edg[,3]!=0,]);
  return(edg);
}

# for(k.neighb in 1:n.neighb){
#   beh.seq <- obs.sequence(k.neighb);
#   adj.mat <- make.adj(beh.seq);
#   edg.lst <- make.edges(beh.seq);
#   write.csv(adj.mat,file=paste("./adj",k.neighb,".csv",sep=""));
#   write.csv(edg.lst,file=paste("./edg",k.neighb,".csv",sep=""));
# }
