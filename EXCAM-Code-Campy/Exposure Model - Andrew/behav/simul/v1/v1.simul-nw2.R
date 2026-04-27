ver <- "v1";
# library(network);
# library(sna);
library(igraph);
setwd("~/stat/EXCAM/Bruce/childbx/simul/v1/")
# col <- c("dark gray","light gray","white","pink","red");
# lab <- c("play","slp","hw","bath","def","eat")

rate.parameters$lambda.mc[1,4,4,1,3,] <- rate.parameters$lambda.mc[1,8,8,1,3,]

lambda.mc <- rate.parameters$lambda.mc

make.sequence <- function(k.neighb,n.sim){
  old.comp <- 5;
  old.behav <- 4;
  bc <- array(NA,dim=c(n.sim,2));
  for(k.sim in 1:n.sim){
    newstate <- gen.behav(k.neighb,old.comp,old.behav);
    new.comp <- newstate[1];
    new.behav <- newstate[2];
    bc[k.sim,] <- c(paste(old.comp,old.behav),paste(new.comp,new.behav));
    old.comp <- new.comp;
    old.behav <- new.behav;
  }
  return(bc);
}

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

col <- c("light gray","dark gray","red","pink","light gray","dark gray","red","pink")
lab <- c("awake", "bathing", "drinking", "sleeping")

xcoord <- c(1,3,9,11,
            2,4,8,10,
            2,4,8,10,
            3,5,7,9,
            1,3,9,11,
            2,4,8,10,
            2,4,8,10,
            3,5,7,9);
ycoord <- c(4,3.8,3.8,4,
            3,2.6,2.6,3,
            1,0.2,0.2,1,
            2,1.4,1.4,2,
            5,5.2,5.2,5,
            6,6.4,6.4,6,
            8,8.8,8.8,8,
            7,7.6,7.6,7)

vert.labels <- rep(NA,n.comp*n.behav);
vert.colors <- rep(NA,n.comp*n.behav);
for(k.comp in 1:n.comp){
  for(k.behav in 1:n.behav){
    vert.labels[(k.comp-1)*n.behav+k.behav] <- lab[k.behav];
    vert.colors[(k.comp-1)*n.behav+k.behav] <- col[k.comp];
  }
}


make.edgeweights <- function(bs){
  edg.mat <- make.edges(bs);
  edg.wgt  <- edg.mat[edg.mat[,3]!=0,3];
  return(edg.wgt);
}

make.nw <- function(k.neighb,k.agecat){
  #beh.seq <- obs.sequence(k.neighb,k.agecat);
  beh.seq <- sim.sequence
  adj.mat <- make.adj(beh.seq);
  adj.mat[adj.mat!=0]=1;
  nw <- graph.adjacency(adj.mat);
  E(nw)$weight <- make.edgeweights(beh.seq);
  V(nw)$name <- vert.labels;
  V(nw)$color <- vert.colors;
  V(nw)$x <- xcoord;
  V(nw)$y <- ycoord;
  return(nw);
}

pathstrength <- function(gr,from.list,to.list,direction){
  wt.sel <- 0;
  wt.nonsel <- 0;
  for(k.from in 1:length(from.list)){
    nbr <- neighbors(gr,from.list[k.from],direction);
    sel <- intersect(nbr,to.list);
    nonsel <- setdiff(nbr,to.list);
    # cat("yes: ",sel,"no: ",nonsel,"\n");
    if(direction=="out" & length(sel)!=0){
      for(k.to in 1:length(sel)){
        wt.sel <- wt.sel +
          E(gr,P=c(from.list[k.from],sel[k.to]))$weight;
      }
    }
    if(direction=="out" & length(nonsel)!=0){
      for(k.to in 1:length(nonsel)){
        wt.nonsel <- wt.nonsel +
          E(gr,P=c(from.list[k.from],nonsel[k.to]))$weight;
      }
    }
    if(direction=="in" & length(sel)!=0){
      for(k.to in 1:length(sel)){
        wt.sel <- wt.sel +
          E(gr,P=c(sel[k.to],from.list[k.from]))$weight;
      }
    }
    if(direction=="in" & length(nonsel)!=0){
      for(k.to in 1:length(nonsel)){
        wt.nonsel <- wt.nonsel +
          E(gr,P=c(nonsel[k.to],from.list[k.from]))$weight;
      }
    }
  }
  return(c(wt.sel,wt.nonsel));
}

awake  <- seq(1,25,4);
bathe <- seq(2,26,4);
drink <- seq(3,27,4);
sleep <- seq(4,28,4);

num.sim <- 10000;
sim.seq <- make.sequence(1,num.sim);
sim.sequence <- apply(sim.seq,1,paste,collapse=" ")

age2 <- make.nw(1,2);

col.pl <- colorRampPalette(c("gray80", "black"))

edge.wt <- round(E(age2)$weight^(0.25),1)
edge.col.list <- col.pl(max(edge.wt)*10-min(edge.wt)*10+1)

age2 <- delete_vertices(age2, which(degree(age2)==0))

plot(age2,vertex.label.family="Helvetica",vertex.label.cex=0.75,edge.width=edge.wt,edge.color=edge.col.list[edge.wt*10-9])
abline(h = -0.1)
mtext("off homestead",2,at=-0.7)
mtext("on homestead",2,at=0.5)

fileps <- function(name) paste("./output/",ver,".TimePoint",agecat,"_iter_",num.sim,"_",Sys.Date(),".eps",sep="");
plot.nw <- function(graph,k.neighb,k.agecat){
  setEPS();
  # par(mar=c(0.1,0.1,0.1,0.1));
  postscript(fileps(paste(k.neighb,k.agecat,sep="")),
             width=6,height=6);
  plot(graph,vertex.label.family="Helvetica",vertex.label.cex=0.75,edge.width=edge.wt,edge.color=edge.col.list[edge.wt*10-9]);
  abline(h = -0.1)
  mtext("off homestead",2,at=-0.7)
  mtext("on homestead",2,at=0.5)
  dev.off();
}

plot.nw(age2,1,1)
