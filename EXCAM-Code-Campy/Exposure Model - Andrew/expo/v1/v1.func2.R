exp.by.cat<-function(k.neighb,k.age,tot.dur) {
  states <- gen.beh.seq(k.neighb,8,4,tot.dur);
  n.states <- nrow(states);
  expos <- data.frame(array(NA,dim=c(n.states,21)));
  time <- states[,1];
  behav <- states[,2];
  comp <- states[,3];
  n.hand <- c(0,0,0,0,0,0,0);
  for(k.state in 1:n.states){
    expos[k.state,1:3] <- states[k.state,];
    res <- expos.by.state(k.neighb,k.age,states[k.state,3],states[k.state,2],n.hand);
    if(length(res)==18){
      n.hand <- res[1:7];
      expos[k.state,4:10] <- res[1:7];
      expos[k.state,11:21] <- res[8:18];
    }
  }
  expos[1,c(4:21)]<-0
  names(expos) <- c("time", "behav", "comp", 
                    "hand_mother", "hand_other adult", "hand_other child", "hand_livestock", "hand_fomite", "hand_food", "hand_soil", 
                    "ingest_mother", "ingest_other adult", "ingest_other child", "ingest_livestock", "ingest_fomite", "ingest_food", 
                    "ingest_soil", "ingest_bathing water", "ingest_drinking water", "ingest_breast milk", "ingest_areola surface")
  return(expos);
}

### Generate frequency based behavior;
gen.freq.seq <- function(seq.bc){
  seq.beh <- array(NA, dim=c(dim(rate_table)[2]+3,length(seq.bc[1,])))
  seq.beh[1:3,] <- seq.bc
  
  for (i in 1:ncol(rate_table)){
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(1,5) & seq.beh[2,]==1)] <- as.numeric(rate_table[1,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(1,5) & seq.beh[2,]==1)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(1,5) & seq.beh[2,]==2)] <- as.numeric(rate_table[2,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(1,5) & seq.beh[2,]==2)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(1,5) & seq.beh[2,]==3)] <- as.numeric(rate_table[3,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(1,5) & seq.beh[2,]==3)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(1,5) & seq.beh[2,]==4)] <- as.numeric(rate_table[4,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(1,5) & seq.beh[2,]==4)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(2,6) & seq.beh[2,]==1)] <- as.numeric(rate_table[5,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(2,6) & seq.beh[2,]==1)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(2,6) & seq.beh[2,]==2)] <- as.numeric(rate_table[6,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(2,6) & seq.beh[2,]==2)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(2,6) & seq.beh[2,]==3)] <- as.numeric(rate_table[7,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(2,6) & seq.beh[2,]==3)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(2,6) & seq.beh[2,]==4)] <- as.numeric(rate_table[8,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(2,6) & seq.beh[2,]==4)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(3,7) & seq.beh[2,]==1)] <- as.numeric(rate_table[9,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(3,7) & seq.beh[2,]==1)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(3,7) & seq.beh[2,]==2)] <- as.numeric(rate_table[10,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(3,7) & seq.beh[2,]==2)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(3,7) & seq.beh[2,]==3)] <- as.numeric(rate_table[11,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(3,7) & seq.beh[2,]==3)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(4,8) & seq.beh[2,]==1)] <- as.numeric(rate_table[12,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(4,8) & seq.beh[2,]==1)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(4,8) & seq.beh[2,]==2)] <- as.numeric(rate_table[13,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(4,8) & seq.beh[2,]==2)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(4,8) & seq.beh[2,]==3)] <- as.numeric(rate_table[14,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(4,8) & seq.beh[2,]==3)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(4,8) & seq.beh[2,]==4)] <- as.numeric(rate_table[15,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(4,8) & seq.beh[2,]==4)])
  }
  
  freq.dat <- matrix(ncol = 2, nrow = 0)
  colnames(freq.dat) <- c('time', 'freq.act')
  
  for (i in 1:ncol(rate_table)){
    t.cumsum <- cumsum(seq.beh[3,])
    t.adj.cumsum <- cumsum(seq.beh[3+i,])
    t.adj.sum <- sum(seq.beh[3+i,])
    temp.point <- cumsum(rexp(1000,1))
    if (max(temp.point)>t.adj.sum){
      temp.point <- temp.point[which(temp.point<t.adj.sum)]
    } else {
      temp.point <- cumsum(rexp(100000,1))
      temp.point <- temp.point[which(temp.point<t.adj.sum)]
    }
    t.point <- c()
    for (j in 1:length(temp.point)){
      k.interval <- which(temp.point[j]>c(0,t.adj.cumsum[-length(t.adj.cumsum)]) & temp.point[j]<=t.adj.cumsum)
      t.point[j] <- t.cumsum[k.interval-1] + seq.beh[3,k.interval]*(temp.point[j]-t.adj.cumsum[k.interval-1])/seq.beh[3+i,k.interval]
    }
    freq.dat <- rbind(freq.dat,cbind(t.point,rep(freq.beh.label[i],length(t.point))))
  }
  freq.dat <- data.frame(freq.dat)
  freq.dat <- freq.dat[which(!is.na(freq.dat$time)),]
  return(freq.dat)
}
