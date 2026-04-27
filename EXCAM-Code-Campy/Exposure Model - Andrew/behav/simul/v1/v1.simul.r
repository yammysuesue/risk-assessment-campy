library(Hmisc);
version <- "v1";

# generate new behaviour/new compartment from previous state
gen.behav <- function(k.neighb,prev.comp,prev.behav) {
  time <- array(NA,dim=c(n.comp,n.behav));
  k.iter <- sample((1:n.iter),1);
  new.comp <- prev.comp;
  new.behav <- prev.behav;
  # dur <- 100;
  # while(dur > 60){
    for(k.comp in 1:n.comp){
      for(k.behav in 1:n.behav) {
        lam <- lambda.mc[k.neighb,prev.comp,k.comp,prev.behav,k.behav,k.iter];
        if(lam!=0){
          time[k.comp,k.behav] <- rweibull(n=1,shape=r,scale=lam);
        }else{
          time[k.comp,k.behav] <- Inf;
        }
      }
    }
    dur <- min(time);
    posmin <- which(time==dur,arr.ind=TRUE);
    new.comp <- posmin[1];
    new.behav <- posmin[2];
  # }
  # return(time);
  return(c(new.comp,new.behav,dur));
}

gen.behav.spec <- function(k.neighb,prev.comp,prev.behav,mask) {
  time <- array(NA,dim=c(n.comp,n.behav));
  k.iter <- sample((1:n.iter),1);
  new.comp <- prev.comp;
  new.behav <- prev.behav;
  # dur <- 100;
  # while(dur > 60){
    for(k.comp in 1:n.comp){
      for(k.behav in 1:n.behav) {
        lam <- lambda.mc[k.neighb,prev.comp,k.comp,prev.behav,k.behav,k.iter]*
               mask[k.comp,k.behav];
        if(lam!=0){
          time[k.comp,k.behav] <- rweibull(n=1,shape=r,scale=lam);
        }else{
          time[k.comp,k.behav] <- Inf;
        }
      }
    }
    dur <- min(time);
    posmin <- which(time==dur,arr.ind=TRUE);
    new.comp <- posmin[1];
    new.behav <- posmin[2];
  # }
  # return(time);
  return(c(new.comp,new.behav,dur));
}

# generate sequence of behaviours/compartments within given period
gen.period.seq <- function(k.neighb,start.comp,start.behav,tot.dur) {
  act <- gen.behav(k.neighb,start.comp,start.behav);
  time <- act[3];
  prev.comp <- act[1];
  prev.behav <- act[2];
  while(time <= tot.dur) {
    new.state <- gen.behav(k.neighb,prev.comp,prev.behav);
    time <- time + new.state[3];
    prev.comp <- new.state[1];
    prev.behav <- new.state[2];
    act <- cbind(act,new.state);
  }
  act[3,length(act[1,])] <- tot.dur - sum(act[3,-length(act[1,])])
  return(act);
}

# generate sequence of given number of behaviours/compartments
gen.number.seq <- function(k.neighb,start.comp,start.behav,tot.num) {
  act <- array(NA,dim=c(3,tot.num));
  prev.behav <- start.behav;
  prev.comp <- start.comp;
  time <- 0;
  k <- 1;
  while(k <= tot.num) {
    act[,k] <- gen.behav(k.neighb,prev.comp,prev.behav);
    time <- time + act[3,k];
    prev.comp <- act[1,k];
    prev.behav <- act[2,k];
    k <- k + 1;
  }
  return(act);
}

# collect behaviour sequence as random walk through state space
# collect behaviour sequences (given number) into array
rw.number <- function(k.neighb,start.comp,start.behav,tot.num){
  sim.seq <- gen.number.seq(k.neighb,start.comp,start.behav,tot.num);
  time.seq <- sim.seq[3,2:tot.num];
  comp.seq <- sim.seq[1,1:tot.num-1];
  behav.seq <- sim.seq[2,1:tot.num-1];
  return(cbind(time.seq,comp.seq,behav.seq));
}

# collect behaviour sequence as random walk through state space
# collect behaviour sequences (given period) into array
rw.period <- function(k.neighb,start.comp,start.behav,tot.dur){
  sim.seq <- gen.period.seq(k.neighb,start.comp,start.behav,tot.dur);
  tot.num <- length(sim.seq[1,!is.na(sim.seq[1,])]);
  if(tot.num>1){
    time.seq <- sim.seq[3,2:tot.num];
    comp.seq <- sim.seq[1,1:tot.num-1];
    behav.seq <- sim.seq[2,1:tot.num-1];
  } else {
    time.seq <- c(NA);
    comp.seq <- c(NA);
    behav.seq <- c(NA);
  }
  return(cbind(time.seq,comp.seq,behav.seq));
}

# collect behaviour sequence as random walk through state space
# collect behaviour sequence for graphing random walks...
randomwalk <- function(k.neighb,start.comp,start.behav,tot.dur) {
  sim.seq <- gen.period.seq(k.neighb,start.comp,start.behav,tot.dur);
  len <- length(sim.seq[1,!is.na(sim.seq[1,])]);
  t0 <- rep(0,2*len+1);
  c0 <- rep(0,2*len+1);
  b0 <- rep(0,2*len+1);
  b0[1] <- start.behav;
  c0[1] <- start.comp;
  k <- 1;
  while(k <= len){
    t0[2*k] <- t0[2*k-1] + sim.seq[3,k];
    c0[2*k] <- c0[2*k-1];
    b0[2*k] <- b0[2*k-1];
    t0[2*k+1] <- t0[2*k];
    c0[2*k+1] <- sim.seq[1,k];
    b0[2*k+1] <- sim.seq[2,k];
    k <- k+1;
  }
  return(cbind(t0,c0,b0));
}

# make a graph of a random walk sequence
plot.rw <- function(k.neighb,start.comp,start.behav,tot.dur){
  sim.rw <- randomwalk(k.neighb,start.comp,start.behav,tot.dur);
  setEPS();
  postscript(paste("./output/","seq","-",neighbourhoods[k.neighb],
                   "-comp",".eps",sep=""),width=7,height=3);
  plot(sim.rw[,1]/60,sim.rw[,2],"l",yaxt="n",
       xlab="time (hr)",ylab="",ylim=c(1,5));
  axis(side=2,at=1:n.comp,labels=compartments,las=2);
  # title(main=neighbourhoods[k.neighb]);
  dev.off();
  postscript(paste("./output/","seq","-",neighbourhoods[k.neighb],
                   "-behav",".eps",sep=""),width=7,height=3);
  plot(sim.rw[,1]/60,sim.rw[,3],"l",yaxt="n",
       xlab="time (hr)",ylab="",ylim=c(1,6));
  # title(main=neighbourhoods[k.neighb]);
  axis(side=2,at=1:n.behav,labels=behaviours,las=2);
  dev.off();
}

# generate vector of durations for a given compartment/behaviour
# collecting only nonzero durations
# all within a specified total duration
# starting from behaviour 2(sleep) in compartment 3 (off ground)
gen1 <- function(k.neighb,k.comp,k.behav,tot.dur){
  len <- 0;
  num.act <- 0;
  num.per <- 0;
  while(len==0){
    tmp <- gen.period.seq(k.neighb,3,2,tot.dur);
    num.per <- num.per+1;
    num.act <- num.act+length(tmp[3,!is.na(tmp[3,])]);
    tmp <- tmp[3,(tmp[1,]==k.comp & tmp[2,]==k.behav)];
    len <- length(tmp[!is.na(tmp)]);
  }
  return(c(tmp[!is.na(tmp)],num.per,num.act));
}

# generate vector of durations of given compartment/behaviour
# of (arbitrary) length num
gen1sim <- function(k.neighb,k.comp,k.behav,tot.dur,num){
  res <- rep(NA,num);
  ind <- 1;
  num.act <- 0;
  num.per <- 0;
  while(ind<num+1){
    tmp <- gen1(k.neighb,k.comp,k.behav,tot.dur);
    num.act <- num.act + tmp[length(tmp)];
    num.per <- num.per + tmp[length(tmp)-1];
    tmp <- tmp[1:(length(tmp)-2)];
    len <- length(tmp);
    if((ind+len)>num) tmp <- tmp[1:(num-ind+1)];
    res[ind:(ind+length(tmp)-1)] <- tmp;
    ind <- ind+length(tmp);
  }
  return(c(res,num.per,num.act));
}
