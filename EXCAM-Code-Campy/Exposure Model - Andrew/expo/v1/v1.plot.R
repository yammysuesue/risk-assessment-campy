n.sim <- 1000
res.ingest <- array(NA,dim=c(n.sim,12))
res.my.mat <- array(NA,dim=c(13,13,n.sim))

#Timepoint 1
source("~/stat/EXCAM/expo/v1/v1.main.R")
for (k.sim in 1:n.sim){
  print(paste("iteration",k.sim))
  expo <- exp.by.cat(1,1,14*60)
  res.ingest[k.sim,] <- c(k.sim,colSums(expo[,11:21],na.rm=T))
  net.array <- array(0, dim=c(13,13,length(expo[,1])))
  mat.labels <- c("Mouth","Hand","Food","Mother","Adult",
                  "Other Child","Livestock","Fomite","Bathing Water","Soil",
                  "Drinking Water","Breast Milk","Areola Surface")

  for (k.iter in 2:length(expo[,1])){
    #bathing
    if (expo$behav[k.iter]=="bathing"){
      net.array[2,9,k.iter] <- sum(expo[k.iter-1,4:10])-sum(expo[k.iter,4:10])
      net.array[9,1,k.iter] <- expo$`ingest_bathing water`[k.iter]
    }
    #drinking
    if (expo$behav[k.iter]=="drinking"){
      net.array[11,1,k.iter] <- expo$`ingest_drinking water`[k.iter]
      net.array[12,1,k.iter] <- expo$`ingest_breast milk`[k.iter]
      net.array[13,1,k.iter] <- expo$`ingest_areola surface`[k.iter]
    }
    #Touching mom hands
    if (expo$behav[k.iter]=="Touching-mom hand"){
      net.array[4,2,k.iter] <- max(expo$hand_mother[k.iter]-expo$hand_mother[k.iter-1],0)
      net.array[2,4,k.iter] <- sum(expo[k.iter-1,5:10])-sum(expo[k.iter,5:10])+max(expo$hand_mother[k.iter-1]-expo$hand_mother[k.iter],0)
    }
    #Touching other adult hands
    if (expo$behav[k.iter]=="Touching-other adult hand"){
      net.array[5,2,k.iter] <- max(expo$`hand_other adult`[k.iter]-expo$`hand_other adult`[k.iter-1],0)
      net.array[2,5,k.iter] <- sum(expo[k.iter-1,c(4,6:10)])-sum(expo[k.iter,c(4,6:10)])+max(expo$`hand_other adult`[k.iter-1]-expo$`hand_other adult`[k.iter],0)
    }
    #Touching other child hands
    if (expo$behav[k.iter]=="Touching-child hand"){
      net.array[6,2,k.iter] <- max(expo$`hand_other child`[k.iter]-expo$`hand_other child`[k.iter-1],0)
      net.array[2,6,k.iter] <- sum(expo[k.iter-1,c(4:5,7:10)])-sum(expo[k.iter,c(4:5,7:10)])+max(expo$`hand_other child`[k.iter-1]-expo$`hand_other child`[k.iter],0)
    }
    #Touching livestock
    if (expo$behav[k.iter]=="Touching-livestock"){
      net.array[7,2,k.iter] <- max(expo$hand_livestock[k.iter]-expo$hand_livestock[k.iter-1],0)
      net.array[2,7,k.iter] <- sum(expo[k.iter-1,c(4:6,8:10)])-sum(expo[k.iter,c(4:6,8:10)])+max(expo$hand_livestock[k.iter-1]-expo$hand_livestock[k.iter],0)
    }
    #Touching fomite
    if (expo$behav[k.iter]=="Touching-fomite"){
      net.array[8,2,k.iter] <- max(expo$hand_fomite[k.iter]-expo$hand_fomite[k.iter-1],0)
      net.array[2,8,k.iter] <- sum(expo[k.iter-1,c(4:7,9:10)])-sum(expo[k.iter,c(4:7,9:10)])+max(expo$hand_fomite[k.iter-1]-expo$hand_fomite[k.iter],0)
    }
    #Mouthing baby hands
    if (expo$behav[k.iter]=="Mouthing-baby hand"){
      net.array[2,1,k.iter] <- sum(expo[k.iter-1,4:10])-sum(expo[k.iter,4:10])
    }
    #Mouthing mom hands
    if (expo$behav[k.iter]=="Mouthing-mom hand"){
      net.array[4,1,k.iter] <- expo$ingest_mother[k.iter]
    }
    #Mouthing other adult hands
    if (expo$behav[k.iter]=="Mouthing-other adult hand"){
      net.array[5,1,k.iter] <- expo$`ingest_other adult`[k.iter]
    }
    #Mouthing other child hands
    if (expo$behav[k.iter]=="Mouthing-child hand"){
      net.array[6,1,k.iter] <- expo$`ingest_other child`[k.iter]
    }
    #Mouthing fomite
    if (expo$behav[k.iter]=="Mouthing-fomite"){
      net.array[8,1,k.iter] <- expo$ingest_fomite[k.iter]
    }
    #Eating
    if (expo$behav[k.iter] %in% c("Eating-injera","Eating-other","Eating-raw plant")){
      net.array[3,2,k.iter] <- max(expo$hand_food[k.iter]-expo$hand_food[k.iter-1],0)
      net.array[2,3,k.iter] <- sum(expo[k.iter-1,c(4:8,10)])-sum(expo[k.iter,c(4:8,10)])+max(expo$hand_food[k.iter-1]-expo$hand_food[k.iter],0)
      net.array[3,1,k.iter] <- sum(expo[k.iter,11:21])
    }
    #pica soil
    if (expo$behav[k.iter]=="Pica-soil"){
      net.array[10,2,k.iter] <- max(expo$hand_soil[k.iter]-expo$hand_soil[k.iter-1],0)
      net.array[2,10,k.iter] <- sum(expo[k.iter-1,4:9])-sum(expo[k.iter,4:9])+max(expo$hand_soil[k.iter-1]-expo$hand_soil[k.iter],0)
      net.array[10,1,k.iter] <- sum(expo[k.iter,11:21])
    }
    #pica other?
  }

  #total amount matrix
  res.my.mat[,,k.sim] <- apply(net.array,1:2,sum);
}

res.ingest1 <- res.ingest
res.my.mat1 <- res.my.mat
save(res.ingest1,file="~/stat/EXCAM/expo/v1/output/res.ingest1.rda")
save(res.my.mat1,file="~/stat/EXCAM/expo/v1/output/res.my.mat1.rda")

#for plot without livestock
##################################################################
#total amount matrix
my.mat <- apply(res.my.mat,1:2,mean)[-7,-7]
mat.labels <- c("Mouth","Hand","Food","Mother","Adult",
                "Other Child","Fomite","Bathing Water","Soil",
                "Drinking Water","Breast Milk","Areola Surface")
edge <- array(0,dim=c(144,3))
for (i in 1:12){
  for (j in 1:12){
    edge[(i-1)*12+j,1] <- mat.labels[i];
    edge[(i-1)*12+j,2] <- mat.labels[j];
    edge[(i-1)*12+j,3] <- my.mat[i,j];
  }
}
edge1 <- edge[which(edge[,3]!=0),];
edge2 <- data.frame(start=edge1[,1],end=edge1[,2], size=edge1[,3])

edge.network<-graph.data.frame(edge2, directed=T);
n.node <- length(V(edge.network))

eqarrowPlot1 <- function(graph, layout, edge.lty=rep(1, ecount(graph)),
                         edge.arrow.size=rep(1, ecount(graph)),
                         vertex.shape="circle",
                         edge.curved=autocurve.edges(graph), ...) {
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape="none")
  for (e in seq_len(ecount(graph))) {
    graph2 <- delete.edges(graph, E(graph)[(1:ecount(graph))[-e]])
    plot(graph2, edge.lty=edge.lty[e], edge.arrow.size=edge.arrow.size[e],
         edge.curved=edge.curved[e], edge.width=edge.arrow.size[e],
         layout=layout, vertex.shape="none",vertex.label=NA, add=TRUE, ...)
  }
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape=vertex.shape, add=TRUE, ...)
  invisible(NULL)
}

V(edge.network)$size<-1
V(edge.network)$color <- ifelse(V(edge.network)$name=="Mouth","lightslateblue",
                                ifelse(is.element(V(edge.network)$name,c("Hand","Food")),"gold1",
                                       ifelse(is.element(V(edge.network)$name,c("Bathing Water")),"green1","red1")));
E(edge.network)$color <- "red";
E(edge.network)[ V(edge.network) %->% V(edge.network)[name=="Bathing Water"] ]$color <- "green";
E(edge.network)$curved=FALSE;

weight<-log10(as.numeric(E(edge.network)$size));

l=matrix(c(0,2,-2,-3,-3,-2,0,0,4,4,2,4,
           0,4,4,2,-2,-4,4,-4,4,-4,-4,0),12,2);

pdf(file="~/stat/EXCAM/expo/v1/output/timepoint1.pdf",width=10,height=10)
eqarrowPlot1(edge.network, l, edge.arrow.size=weight, vertex.size=35,
             vertex.label.color="black");
dev.off()

# pdf(file="~/stat/EXCAM/expo/v1/output/timepoint1_truncated.pdf",width=10,height=10)
# eqarrowPlot1(edge.network, l, edge.arrow.size=weight, vertex.size=35,
#              vertex.label.color="black");
# dev.off()


#Timepoint 2

n.sim <- 1000
res.ingest <- array(NA,dim=c(n.sim,12))
res.my.mat <- array(NA,dim=c(13,13,n.sim))

source("~/stat/EXCAM/expo/v1/v1.main2.R")
for (k.sim in 1:n.sim){
  print(paste("iteration",k.sim))
  expo <- exp.by.cat(1,2,14*60)
  res.ingest[k.sim,] <- c(k.sim,colSums(expo[,11:21],na.rm=T))
  net.array <- array(0, dim=c(13,13,length(expo[,1])))
  # mat.labels <- c("Mouth","Hand","Food","Mother","Adult",
  #                 "Other Child","Livestock","Fomite","Bathing Water","Soil",
  #                 "Drinking Water","Breast Milk","Areola Surface")

  for (k.iter in 2:length(expo[,1])){
    #bathing
    if (expo$behav[k.iter]=="bathing"){
      net.array[2,9,k.iter] <- sum(expo[k.iter-1,4:10])-sum(expo[k.iter,4:10])
      net.array[9,1,k.iter] <- expo$`ingest_bathing water`[k.iter]
    }
    #drinking
    if (expo$behav[k.iter]=="drinking"){
      net.array[11,1,k.iter] <- expo$`ingest_drinking water`[k.iter]
      net.array[12,1,k.iter] <- expo$`ingest_breast milk`[k.iter]
      net.array[13,1,k.iter] <- expo$`ingest_areola surface`[k.iter]
    }
    #Touching mom hands
    if (expo$behav[k.iter]=="Touching-mom hand"){
      net.array[4,2,k.iter] <- max(expo$hand_mother[k.iter]-expo$hand_mother[k.iter-1],0)
      net.array[2,4,k.iter] <- sum(expo[k.iter-1,5:10])-sum(expo[k.iter,5:10])+max(expo$hand_mother[k.iter-1]-expo$hand_mother[k.iter],0)
    }
    #Touching other adult hands
    if (expo$behav[k.iter]=="Touching-other adult hand"){
      net.array[5,2,k.iter] <- max(expo$`hand_other adult`[k.iter]-expo$`hand_other adult`[k.iter-1],0)
      net.array[2,5,k.iter] <- sum(expo[k.iter-1,c(4,6:10)])-sum(expo[k.iter,c(4,6:10)])+max(expo$`hand_other adult`[k.iter-1]-expo$`hand_other adult`[k.iter],0)
    }
    #Touching other child hands
    if (expo$behav[k.iter]=="Touching-child hand"){
      net.array[6,2,k.iter] <- max(expo$`hand_other child`[k.iter]-expo$`hand_other child`[k.iter-1],0)
      net.array[2,6,k.iter] <- sum(expo[k.iter-1,c(4:5,7:10)])-sum(expo[k.iter,c(4:5,7:10)])+max(expo$`hand_other child`[k.iter-1]-expo$`hand_other child`[k.iter],0)
    }
    #Touching livestock
    if (expo$behav[k.iter]=="Touching-livestock"){
      net.array[7,2,k.iter] <- max(expo$hand_livestock[k.iter]-expo$hand_livestock[k.iter-1],0)
      net.array[2,7,k.iter] <- sum(expo[k.iter-1,c(4:6,8:10)])-sum(expo[k.iter,c(4:6,8:10)])+max(expo$hand_livestock[k.iter-1]-expo$hand_livestock[k.iter],0)
    }
    #Touching fomite
    if (expo$behav[k.iter]=="Touching-fomite"){
      net.array[8,2,k.iter] <- max(expo$hand_fomite[k.iter]-expo$hand_fomite[k.iter-1],0)
      net.array[2,8,k.iter] <- sum(expo[k.iter-1,c(4:7,9:10)])-sum(expo[k.iter,c(4:7,9:10)])+max(expo$hand_fomite[k.iter-1]-expo$hand_fomite[k.iter],0)
    }
    #Mouthing baby hands
    if (expo$behav[k.iter]=="Mouthing-baby hand"){
      net.array[2,1,k.iter] <- sum(expo[k.iter-1,4:10])-sum(expo[k.iter,4:10])
    }
    #Mouthing mom hands
    if (expo$behav[k.iter]=="Mouthing-mom hand"){
      net.array[4,1,k.iter] <- expo$ingest_mother[k.iter]
    }
    #Mouthing other adult hands
    if (expo$behav[k.iter]=="Mouthing-other adult hand"){
      net.array[5,1,k.iter] <- expo$`ingest_other adult`[k.iter]
    }
    #Mouthing other child hands
    if (expo$behav[k.iter]=="Mouthing-child hand"){
      net.array[6,1,k.iter] <- expo$`ingest_other child`[k.iter]
    }
    #Mouthing fomite
    if (expo$behav[k.iter]=="Mouthing-fomite"){
      net.array[8,1,k.iter] <- expo$ingest_fomite[k.iter]
    }
    #Eating
    if (expo$behav[k.iter] %in% c("Eating-injera","Eating-other","Eating-raw plant")){
      net.array[3,2,k.iter] <- max(expo$hand_food[k.iter]-expo$hand_food[k.iter-1],0)
      net.array[2,3,k.iter] <- sum(expo[k.iter-1,c(4:8,10)])-sum(expo[k.iter,c(4:8,10)])+max(expo$hand_food[k.iter-1]-expo$hand_food[k.iter],0)
      net.array[3,1,k.iter] <- sum(expo[k.iter,11:21])
    }
    #pica soil
    if (expo$behav[k.iter]=="Pica-soil"){
      net.array[10,2,k.iter] <- max(expo$hand_soil[k.iter]-expo$hand_soil[k.iter-1],0)
      net.array[2,10,k.iter] <- sum(expo[k.iter-1,4:9])-sum(expo[k.iter,4:9])+max(expo$hand_soil[k.iter-1]-expo$hand_soil[k.iter],0)
      net.array[10,1,k.iter] <- sum(expo[k.iter,11:21])
    }
    #pica other?
  }

  #total amount matrix
  res.my.mat[,,k.sim] <- apply(net.array,1:2,sum);
}

res.ingest2 <- res.ingest
res.my.mat2 <- res.my.mat
save(res.ingest2,file="~/stat/EXCAM/expo/v1/output/res.ingest2.rda")
save(res.my.mat2,file="~/stat/EXCAM/expo/v1/output/res.my.mat2.rda")
#for plot without livestock
##################################################################
#total amount matrix
my.mat <- apply(res.my.mat,1:2,mean,na.rm=T)[-7,-7]
mat.labels <- c("Mouth","Hand","Food","Mother","Adult",
                "Other Child","Fomite","Bathing Water","Soil",
                "Drinking Water","Breast Milk","Areola Surface")
edge <- array(0,dim=c(144,3))
for (i in 1:12){
  for (j in 1:12){
    edge[(i-1)*12+j,1] <- mat.labels[i];
    edge[(i-1)*12+j,2] <- mat.labels[j];
    edge[(i-1)*12+j,3] <- my.mat[i,j];
  }
}
edge1 <- edge[which(edge[,3]!=0),];
edge2 <- data.frame(start=edge1[,1],end=edge1[,2], size=edge1[,3])

edge.network<-graph.data.frame(edge2, directed=T);
n.node <- length(V(edge.network))

eqarrowPlot1 <- function(graph, layout, edge.lty=rep(1, ecount(graph)),
                         edge.arrow.size=rep(1, ecount(graph)),
                         vertex.shape="circle",
                         edge.curved=autocurve.edges(graph), ...) {
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape="none")
  for (e in seq_len(ecount(graph))) {
    graph2 <- delete.edges(graph, E(graph)[(1:ecount(graph))[-e]])
    plot(graph2, edge.lty=edge.lty[e], edge.arrow.size=edge.arrow.size[e],
         edge.curved=edge.curved[e], edge.width=edge.arrow.size[e],
         layout=layout, vertex.shape="none",vertex.label=NA, add=TRUE, ...)
  }
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape=vertex.shape, add=TRUE, ...)
  invisible(NULL)
}

V(edge.network)$size<-1
V(edge.network)$color <- ifelse(V(edge.network)$name=="Mouth","lightslateblue",
                                ifelse(is.element(V(edge.network)$name,c("Hand","Food")),"gold1",
                                       ifelse(is.element(V(edge.network)$name,c("Bathing Water")),"green1","red1")));
E(edge.network)$color <- "red";
E(edge.network)[ V(edge.network) %->% V(edge.network)[name=="Bathing Water"] ]$color <- "green";
E(edge.network)$curved=FALSE;

weight<-log10(as.numeric(E(edge.network)$size));

l=matrix(c(0,2,-2,-3,-3,-2,0,0,4,4,2,4,
           0,4,4,2,-2,-4,4,-4,4,-4,-4,0),12,2);

pdf(file="~/stat/EXCAM/expo/v1/output/timepoint2.pdf",width=10,height=10)
eqarrowPlot1(edge.network, l, edge.arrow.size=weight, vertex.size=35,
             vertex.label.color="black");
dev.off()

pdf(file="~/stat/EXCAM/expo/v1/output/timepoint2_truncated.pdf",width=10,height=10)
eqarrowPlot1(edge.network, l, edge.arrow.size=weight, vertex.size=35,
             vertex.label.color="black");
dev.off()

#######################################################################
frac0 <- function(mc) return(1-length(mc[mc>0])/length(mc));
non0 <- function(mc){
  tmp <- mc; tmp[!(tmp>0)] <- NA; return(tmp);
}

par(xpd=FALSE)
layout(matrix(c(1,2), 2, 1, byrow = TRUE), heights=c(1,3))
par(mgp = c(3, 1.5, 0))
par(mar=c(0.5, 6, 2, 0.5))

bar.dat<-c()
box.dat<-list()
for (k.type in 1:11){
  bar.dat <- c(bar.dat, 1-frac0(res.ingest1[,k.type+1]),1-frac0(res.ingest2[,k.type+1]))
  box.dat[[k.type*2-1]] <- log10(non0(res.ingest1[,k.type+1]))
  box.dat[[k.type*2]] <- log10(non0(res.ingest2[,k.type+1]))
}

#remove livestock
bar.dat <- bar.dat[-c(7,8)]
box.dat <- box.dat[-c(7,8)]

barplot(bar.dat,main="",ylab="",las=2,ylim=c(0,1),cex.main=1, cex.names = 1, cex.axis = 1,col=rep(c("tomato","skyblue"),11));
mtext("Fraction Exposed", side=2, line=3, cex=1)
grid(nx=NA, ny=NULL,col="darkgray",lwd=1);

par(mar=c(8, 6, 0.5, 0.5))
boxplot(box.dat,outline=FALSE,ylim=c(0,7),las = 2,xaxt="n",col=rep(c("tomato","skyblue"),11),
        ylab=expression(paste("log10 (dose)")),
        cex.axis = 1,cex.lab=1);
grid(nx=NA, ny=NULL,col="darkgray",lwd=1);
#axis(1, at=(1:11*2)-0.5, labels=c("Mother","Other Adult","Other Child","Livestock","Fomite","Food","Soil","Bathing Water","Drinking Water","Breast Milk","Areola Surface"), las=2, cex.axis=1)
axis(1, at=(1:10*2)-0.5, labels=c("Mother","Other Adult","Other Child","Fomite","Food","Soil","Bathing Water","Drinking Water","Breast Milk","Areola Surface"), las=2, cex.axis=1)
legend("topright",legend=c("Timepoint 1","Timepoint 2"),fill=c("tomato","skyblue"),bty="n")

#######################################################################
#treemap
library(treemap)
library(treemapify)
library(gtable)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(dplyr)

t1.ingest <- data.frame(cbind(c("Mother","Other Adult","Other Child","Fomite","Food","Soil","Bathing Water","Drinking Water","Breast Milk","Areola Surface"),
                              colMeans(res.ingest1)[-c(1,5)]))
t2.ingest <- data.frame(cbind(c("Mother","Other Adult","Other Child","Fomite","Food","Soil","Bathing Water","Drinking Water","Breast Milk","Areola Surface"),
                              colMeans(res.ingest2)[-c(1,5)]))
names(t1.ingest) <- c("Pathway","Exposure")
t1.ingest$Exposure <- as.numeric(t1.ingest$Exposure)
t1.ingest$Timepoint <- "Timepoint 1"
t1.ingest$perc <- round(t1.ingest$Exposure/sum(t1.ingest$Exposure)*100,1)

names(t2.ingest) <- c("Pathway","Exposure")
t2.ingest$Exposure <- as.numeric(t2.ingest$Exposure)
t2.ingest$Timepoint <- "Timepoint 2"
t2.ingest$perc <- round(t2.ingest$Exposure/sum(t2.ingest$Exposure,na.rm=T)*100,1)

dat <- rbind(t1.ingest,t2.ingest)

dat <-  dat %>%
  mutate(Pathway = factor(.$Pathway, levels = factor(unique(Pathway))))

dat %>%
  group_by(Timepoint) %>% {
    ggplot(., aes(area = Exposure,
                  fill = log10(Exposure),
                  label = paste(Pathway, "\n", perc, "%"), "\n")) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre", grow = F, reflow = T) +
      scale_fill_gradient(
        low = "yellow", high = "darkred",
        breaks = log10(range(.$Exposure)),
        labels = round(range(log10(.$Exposure)),)
      ) + theme_bw() +
      facet_wrap(~Timepoint, nrow = 1) +
      theme(legend.position = "none") +
      labs(fill = "Exposure",
           title = "Total Exposure in Rural Ethiopia")
  } -> p1
p1
ggsave("~/stat/EXCAM/expo/v1/output/treemap_ethiopia.pdf",p1,width=6,height=4)
#ggsave("~/stat/EXCAM/expo/v1/output/treemap_ethiopia_truncated.pdf",p1,width=6,height=4)
