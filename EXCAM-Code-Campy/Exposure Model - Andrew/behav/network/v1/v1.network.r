ver <- "v1";
# library(network);
# library(sna);
library(igraph);
setwd("~/stat/EXCAM/Bruce/childbx/network/v1/")
# col <- c("dark gray","light gray","white","pink","red");
# lab <- c("play","slp","hw","bath","def","eat")

col <- c("light gray","dark gray","pink","light gray","dark gray","red","pink")
lab <- c("awake", "bathing", "drinking", "sleeping")

xcoord <- c(1,3,9,11,
            2,4,8,10,
            3,5,7,9,
            1,3,9,11,
            2,4,8,10,
            2,4,8,10,
            3,5,7,9);
ycoord <- c(3,2.8,2.8,3,
            2,1.6,1.6,2,
            1,0.4,0.4,1,
            4,4.2,4.2,4,
            5,5.4,5.4,5,
            7,7.8,7.8,7,
            6,6.6,6.6,6)

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
  beh.seq <- obs.sequence(k.neighb,k.agecat);
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

age1 <- make.nw(1,1);

unique.edges <- c()
for (i in 1:157){
  unique.edges <- unique(c(unique.edges,
                    paste(as.vector(obs.comp[1,i,1:(dim(obs.comp)[3]-1)]),
                          as.vector(obs.comp[1,i,-1]),
                          as.vector(obs.behav[1,i,1:(dim(obs.behav)[3]-1)]),
                          as.vector(obs.behav[1,i,-1]))))
}
unique.edges <- sort(unique.edges[-grep("NA",unique.edges)])
                      
col.pl <- colorRampPalette(c("gray80", "black"))

edge.wt <- round(E(age1)$weight^(0.25),1)
edge.col.list <- col.pl(max(edge.wt)*10-min(edge.wt)*10+1)

age1 <- delete_vertices(age1, which(degree(age1)==0))

plot(age1,vertex.label.family="Helvetica",vertex.label.cex=0.75,edge.width=edge.wt,edge.color=edge.col.list[edge.wt*10-9])
abline(h = -0.05)
mtext("off homestead",2,at=-0.7)
mtext("on homestead",2,at=0.5)

fileps <- function(name) paste("./output/",ver,".",name,"_",Sys.Date(),".eps",sep="");
plot.nw <- function(graph,k.neighb,k.agecat){
  setEPS();
  # par(mar=c(0.1,0.1,0.1,0.1));
  postscript(fileps(paste(k.neighb,k.agecat,sep="")),
             width=8,height=8);
  plot(graph,vertex.label.family="Helvetica",vertex.label.cex=0.75,vertex.size=20,
       edge.width=edge.wt,edge.color=edge.col.list[edge.wt*10-9]);
  abline(h = -0.05)
  mtext("off homestead",2,at=-0.7)
  mtext("on homestead",2,at=0.5)
  dev.off();
}

plot.nw(age1,1,1)


# # eating when first washed hands
# eat.hw.0 <- cbind(pathstrength(alajo.0,eat,handw,"in"),
#                   pathstrength(bukom.0,eat,handw,"in"),
#                   pathstrength(oldfadama.0,eat,handw,"in"),
#                   pathstrength(shiabu.0,eat,handw,"in"));
# eat.hw.1 <- cbind(pathstrength(alajo.1,eat,handw,"in"),
#                   pathstrength(bukom.1,eat,handw,"in"),
#                   pathstrength(oldfadama.1,eat,handw,"in"),
#                   pathstrength(shiabu.1,eat,handw,"in"));
# eat.hw.2 <- cbind(pathstrength(alajo.2,eat,handw,"in"),
#                   pathstrength(bukom.2,eat,handw,"in"),
#                   pathstrength(oldfadama.2,eat,handw,"in"),
#                   pathstrength(shiabu.2,eat,handw,"in"));
# # defecation followed by handwashing
# def.hw.0 <- cbind(pathstrength(alajo.0,defec,handw,"out"),
#                   pathstrength(bukom.0,defec,handw,"out"),
#                   pathstrength(oldfadama.0,defec,handw,"out"),
#                   pathstrength(shiabu.0,defec,handw,"out"));
# def.hw.1 <- cbind(pathstrength(alajo.1,defec,handw,"out"),
#                   pathstrength(bukom.1,defec,handw,"out"),
#                   pathstrength(oldfadama.1,defec,handw,"out"),
#                   pathstrength(shiabu.1,defec,handw,"out"));
# def.hw.2 <- cbind(pathstrength(alajo.2,defec,handw,"out"),
#                   pathstrength(bukom.2,defec,handw,"out"),
#                   pathstrength(oldfadama.2,defec,handw,"out"),
#                   pathstrength(shiabu.2,defec,handw,"out"));
# # eating when first bathed
# eat.bt.0 <- cbind(pathstrength(alajo.0,eat,bathe,"in"),
#                   pathstrength(bukom.0,eat,bathe,"in"),
#                   pathstrength(oldfadama.0,eat,bathe,"in"),
#                   pathstrength(shiabu.0,eat,bathe,"in"));
# eat.bt.1 <- cbind(pathstrength(alajo.1,eat,bathe,"in"),
#                   pathstrength(bukom.1,eat,bathe,"in"),
#                   pathstrength(oldfadama.1,eat,bathe,"in"),
#                   pathstrength(shiabu.1,eat,bathe,"in"));
# eat.bt.2 <- cbind(pathstrength(alajo.2,eat,bathe,"in"),
#                   pathstrength(bukom.2,eat,bathe,"in"),
#                   pathstrength(oldfadama.2,eat,bathe,"in"),
#                   pathstrength(shiabu.2,eat,bathe,"in"));
# # defecation followed by bathing
# def.bt.0 <- cbind(pathstrength(alajo.0,defec,bathe,"out"),
#                   pathstrength(bukom.0,defec,bathe,"out"),
#                   pathstrength(oldfadama.0,defec,bathe,"out"),
#                   pathstrength(shiabu.0,defec,bathe,"out"));
# def.bt.1 <- cbind(pathstrength(alajo.1,defec,bathe,"out"),
#                   pathstrength(bukom.1,defec,bathe,"out"),
#                   pathstrength(oldfadama.1,defec,bathe,"out"),
#                   pathstrength(shiabu.1,defec,bathe,"out"));
# def.bt.2 <- cbind(pathstrength(alajo.2,defec,bathe,"out"),
#                   pathstrength(bukom.2,defec,bathe,"out"),
#                   pathstrength(oldfadama.2,defec,bathe,"out"),
#                   pathstrength(shiabu.2,defec,bathe,"out"));
# 
# # connected or not: subcomponent(graph,vertex,mode=c("in","out","all")
# # or: neighbors(graph,vertex1, vertex2,mode-c("in","out","all")
# # closeness(alajo)[handw]
# # betweenness(alajo)[handw]
# # degree(alajo,handw,"in")
# # authority.score(alajo)$vector
# # hub.score(alajo)$vector
