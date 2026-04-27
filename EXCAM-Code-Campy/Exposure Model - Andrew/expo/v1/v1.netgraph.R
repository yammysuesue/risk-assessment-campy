library(igraph);

expo <- exp.by.cat(1,1,14*60)

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
my.mat <- apply(net.array,1:2,sum);
mat.labels <- c("Mouth","Hand","Food","Mother","Adult",
                "Other Child","Livestock","Fomite","Bathing Water","Soil",
                "Drinking Water","Breast Milk","Areola Surface")
edge <- array(0,dim=c(169,3))
for (i in 1:13){
  for (j in 1:13){
    edge[(i-1)*13+j,1] <- mat.labels[i];
    edge[(i-1)*13+j,2] <- mat.labels[j];
    edge[(i-1)*13+j,3] <- my.mat[i,j];
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

l=matrix(c(0,2,-2,-3,-3,-4,-2,0,0,4,4,2,4,
           0,4,4,2,-2,0,-4,4,-4,4,-4,-4,0),13,2);

pdf(file="~/stat/EXCAM/expo/v1/output/output071123.pdf",width=10,height=10)
eqarrowPlot1(edge.network, l, edge.arrow.size=weight, vertex.size=35, 
             vertex.label.color="black");
dev.off()

