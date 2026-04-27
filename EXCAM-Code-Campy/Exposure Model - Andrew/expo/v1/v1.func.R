### Generate frequency based behavior;
gen.freq.seq <- function(seq.bc){
  seq.beh <- array(NA, dim=c(dim(rate_table)[2]+3,length(seq.bc[1,])))
  seq.beh[1:3,] <- seq.bc
  
  for (i in 1:ncol(rate_table)){
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(1,4) & seq.beh[2,]==1)] <- as.numeric(rate_table[1,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(1,4) & seq.beh[2,]==1)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(1,4) & seq.beh[2,]==2)] <- as.numeric(rate_table[2,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(1,4) & seq.beh[2,]==2)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(1,4) & seq.beh[2,]==3)] <- as.numeric(rate_table[3,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(1,4) & seq.beh[2,]==3)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(1,4) & seq.beh[2,]==4)] <- as.numeric(rate_table[4,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(1,4) & seq.beh[2,]==4)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(2,5) & seq.beh[2,]==1)] <- as.numeric(rate_table[5,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(2,5) & seq.beh[2,]==1)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(2,5) & seq.beh[2,]==2)] <- as.numeric(rate_table[6,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(2,5) & seq.beh[2,]==2)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(2,5) & seq.beh[2,]==3)] <- as.numeric(rate_table[7,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(2,5) & seq.beh[2,]==3)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(2,5) & seq.beh[2,]==4)] <- as.numeric(rate_table[8,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(2,5) & seq.beh[2,]==4)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(6) & seq.beh[2,]==1)] <- as.numeric(rate_table[9,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(6) & seq.beh[2,]==1)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(3,7) & seq.beh[2,]==1)] <- as.numeric(rate_table[10,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(3,7) & seq.beh[2,]==1)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(3,7) & seq.beh[2,]==2)] <- as.numeric(rate_table[11,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(3,7) & seq.beh[2,]==2)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(3,7) & seq.beh[2,]==3)] <- as.numeric(rate_table[12,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(3,7) & seq.beh[2,]==3)])
    seq.beh[4:dim(seq.beh)[1],which(seq.beh[1,] %in% c(3,7) & seq.beh[2,]==4)] <- as.numeric(rate_table[13,])%*%t(seq.beh[3,which(seq.beh[1,] %in% c(3,7) & seq.beh[2,]==4)])
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

gen.beh.seq <- function(k.neighb,start.comp,start.behav,tot.dur){
  seq.bc <- gen.period.seq(k.neighb,start.comp,start.behav,tot.dur)
  seq.freq <- gen.freq.seq(seq.bc)
  seq.bc <- rbind(seq.bc,cumsum(seq.bc[3,]))
  seq.dw.bath <- data.frame(matrix(cbind(seq.bc[4,which(seq.bc[2,] %in% c(2,3))],behaviours[seq.bc[2,which(seq.bc[2,] %in% c(2,3))]]),ncol=2))
  names(seq.dw.bath) <- names(seq.freq)
  seq.freq <- rbind(seq.freq,seq.dw.bath,c(0,"sleeping"))
  names(seq.freq) <- c("time","act")
  seq.freq$time <- as.numeric(seq.freq$time)
  interval.vector <- as.numeric(c(0,seq.bc[4,]))
  time.cut <- as.numeric(cut(seq.freq$time, breaks = interval.vector, include.lowest = TRUE))
  seq.freq$comp <- compartments[seq.bc[1,time.cut]]
  seq.beh <- seq.freq[order(seq.freq$time),]
  return(seq.beh)
}

# exposure calculation
exp.by.cat<-function(k.neighb,k.age,tot.dur) {
  states <- gen.beh.seq(k.neighb,7,4,tot.dur);
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

#hand contamination could from mother, other adult, other child, livestock, fomite, food, and soil;
#ingestion includes additional bathing water, drinking water, breast milk, and areola surface;
#### exposure by state;
expos.by.state<-function(k.neighb,k.age,k.comp,k.behav,n.hand){
  res<-c();
  if (k.behav=="bathing"){#may think about caregiver bathing the child, caregiver's hands to bathing water;
    res <- hand.bathe(n.hand);
    num.bathing.water <- round(volume.bathing.water(k.age)*sample(conc$`bathing water`,1),0) #Need the volume of bathing water ingested (in mL) during bathing, this may associated with how long the child bathe.
    return(c(res,c(0,0,0,0,0,0,0,num.bathing.water,0,0,0)));        
  }
  if (k.behav=="drinking"){
    #time point 1: breastmilk 1172, drinking water 90, other 156; time point 1: breastmilk 867, drinking water 139, other 165;
    p.drink.bm <- ifelse(k.age==1,rbeta(1,1173,247),rbeta(1,868,305)) #need to get data for choice between breastmilk and drinking water for drinking behavior
    if (rbinom(1,1,p.drink.bm)==1){
      num.bm <- round(volume.bm(k.age)*sample(conc$breastmilk,1),0) #Need the volume of breastmilk ingested (in mL) for different age groups in our study;
      num.areola <- round(sample(conc$`areola swab`,1),0) #Need the size of areola that baby may suck on.
      return(c(n.hand,c(0,0,0,0,0,0,0,0,0,num.bm,num.areola)))
    } else {
      num.dw <- round(volume.dw(k.age)*sample(conc$`drinking water`,1),0) #Need the volume of drinking water ingested (in mL) for different age groups in our study;
      return(c(n.hand,c(0,0,0,0,0,0,0,0,num.dw,0,0)))
    }
  }
  if (k.behav=="Touching-mom hand"){ #139.46 cm2 per hand which is 278.92 cm2 per pair of hands
    res <- hand.surface(n.hand,sample(conc$`mother handrinse`,1)/278.92,k.behav,k.age) #Need to check how large the area of mother's hand that the child touches and how large child's hand in contact.
    return(c(res,c(0,0,0,0,0,0,0,0,0,0,0)))
  }
  if (k.behav=="Touching-other adult hand"){#139.46 cm2 per hand which is 278.92 cm2 per pair of hands
    res <- hand.surface(n.hand,sample(conc$`mother handrinse`,1)/278.92,k.behav,k.age) #Need to check how large the area of mother's hand that the child touches and how large child's hand in contact.
    return(c(res,c(0,0,0,0,0,0,0,0,0,0,0)))
  }
  if (k.behav=="Touching-child hand"){#85.65 cm2 per hand which is 171.3 cm2 per pair of hands for a child
    res <- hand.surface(n.hand,sample(conc$`sibling handrinse`/171.3,1),k.behav,k.age) #Need to check how large the area of mother's hand that the child touches and how large child's hand in contact.
    return(c(res,c(0,0,0,0,0,0,0,0,0,0,0)))
  }
  if (k.behav=="Touching-fomite"){#100 cm2 for area swabbed;
    res <- hand.surface(n.hand,sample(conc$fomite/100,1),k.behav,k.age) #Need to check how large the area of fomite swabbed;
    return(c(res,c(0,0,0,0,0,0,0,0,0,0,0)))
  }
  if (k.behav=="Touching-livestock"){
    #res <- hand.surface(n.hand,sample(conc$fomite/100,1)*100,k.behav,k.age) #for now, just using the 100 times of fomite contamination;
    #To enable this, we need to know the livestock environmental contamination level;
    #return(c(res,c(0,0,0,0,0,0,0,0,0,0,0)))
    return(c(n.hand,c(0,0,0,0,0,0,0,0,0,0,0)))
  }
  if (k.behav=="Mouthing-baby hand"){
    trans <- hand.mouth(n.hand);
    n.hand<- n.hand - trans;
    return(c(n.hand,c(trans,0,0,0,0)))
  }
  if (k.behav=="Mouthing-mom hand"){
    num.mom.hand <- hand.mouth(c(round(sample(conc$`mother handrinse`,1),0),0,0,0,0,0,0))
    return(c(n.hand,c(num.mom.hand,0,0,0,0)))
  }
  if (k.behav=="Mouthing-other adult hand"){
    num.adult.hand <- hand.mouth(c(0,round(sample(conc$`mother handrinse`,1),0),0,0,0,0,0))
    return(c(n.hand,c(num.adult.hand,0,0,0,0)))
  }
  if (k.behav=="Mouthing-child hand"){
    num.child.hand <- hand.mouth(c(0,0,round(sample(conc$`sibling handrinse`,1),0),0,0,0,0))
    return(c(n.hand,c(num.child.hand,0,0,0,0)))
  }
  if (k.behav=="Mouthing-fomite"){
    num.fomite <- fomite.mouth(c(0,0,0,0,round(sample(conc$fomite,1),0),0,0))
    return(c(n.hand,c(num.fomite,0,0,0,0)))
  }
  if (k.behav=="Eating-injera"){
    num.food <- round(sample(conc$food,1)*50,0) #using 50 gram as serving size
    n.food <- c(0,0,0,0,0,num.food,0);
    res<-hand.food.contact(n.hand,n.food);
    return(c(res,0,0,0,0))
  }
  if (k.behav=="Eating-other"){#distinguish the concentration of injera and other's later
    num.food <- round(sample(conc$food,1)*50,0) #using 50 gram as serving size
    n.food <- c(0,0,0,0,0,num.food,0);
    res<-hand.food.contact(n.hand,n.food);
    return(c(res,0,0,0,0))
  }
  if (k.behav=="Eating-raw plant"){#distinguish the concentration of injera and other's later
    num.food <- round(sample(conc$food,1)*50,0) #using 50 gram as serving size
    n.food <- c(0,0,0,0,0,num.food,0);
    res<-hand.food.contact(n.hand,n.food);
    return(c(res,0,0,0,0))
  }
  if (k.behav=="Pica-soil"){
    num.soil <- round(sample(conc$soil,1)/50*1.25,0) #assuming 50 grams of soil attached to 1 swab and ingestion value is 1.25 gram 
    n.soil <- c(0,0,0,0,0,0,num.soil);
    res<-hand.soil.contact(n.hand,n.soil);
    return(c(res,0,0,0,0))
  }
  if (k.behav=="Pica-other"){#need to clarification of pica other
    # num.soil <- round(sample(conc$soil,1)/20*5,0) #assuming 20 grams of soil attached to 1 swab and ingestion value is 5 gram
    # n.soil <- c(0,0,0,0,0,0,num.soil);
    # res<-hand.soil.contact(n.hand,n.soil);
    return(c(n.hand,0,0,0,0,0,0,0,0,0,0,0))
  }
  if (k.behav=="Pica-feces"){
    return(c(n.hand,0,0,0,0,0,0,0,0,0,0,0))
  }
}

hand.bathe <- function(n.before){
  res<-c();
  p.det <- ifelse(sum(n.before)>1000,det.bact.bathe(),
                  ifelse(sum(n.before)!=0,rtruncnorm(1,0,0.99,0.5,0.2),0));
  res<-next.state(c(0,0,0,0,0,0,0,n.before),c(0,p.det))
  return(res[8:14]);
}

det.bact.bathe<-function(void){
  soap <- bath.soap();
  dur <- 60+rgamma(1,shape=4,scale=60); #arbitrary number >60s and mean 5 min
  if (soap==TRUE) r<- rnorm(1,-2.1795,0.5132)+rnorm(1,1.1922,0.1436)*log(dur);
  if (soap==FALSE) r<- rnorm(1,-0.6836,0.6036)+rnorm(1,0.8001,0.1394)*log(dur);
  if (r<0) r<-0.1;
  return(1-0.1^r);
}

# mother, other adult, other child, livestock, fomite, food, and soil;
next.state <- function(num.vec,p.vec){
  n.had <- c(0,0,0,0,0,0,0)
  n.intk <- c(0,0,0,0,0,0,0)
  for (i in 1:7){
    att <- ifelse(num.vec[i]>1e8, round(num.vec[i]*p.vec[1]),
                  rbinom(n=1,size=num.vec[i],prob=p.vec[1]));
    det <- ifelse(num.vec[i+7]>1e8, round(num.vec[i+7]*p.vec[2]),
                  rbinom(n=1,size=num.vec[i+7],prob=p.vec[2]));
    n.had[i] <- num.vec[i]-att+det;
    n.intk[i] <- num.vec[i+7]+att-det;
  }
  return(c(n.had,n.intk));
}

# These are from SaniPath data, do we have those information in EXCAM?
# hand.soap<-function(void) rbinom(1,1,rbeta(1,5,17))  #data: 4 with soap, 16 w/o soap for handwash;
# bath.soap<-function(void) rbinom(1,1,rbeta(1,45,19)) #data: 44 with soap, 18 w/o soap for bathing;

bath.soap<-function(void) return(0) #assume no soap use

volume.bm<-function(k.age){
  if (k.age==1) {return(runif(1,32.7,54.5))}
  if (k.age==2) {return(runif(1,54.5,68.125))}
}

volume.dw<-function(k.age){
  if (k.age==1) {return(runif(1,32.7,54.5))}
  if (k.age==2) {return(runif(1,54.5,68.125))}
}

volume.bathing.water<-function(k.age){
  if (k.age==1) {return(runif(1,0,2.69))} #mean 1.345 mL
  if (k.age==2) {return(runif(1,0,1.37))} #mean 0.685 mL
}

hand.surface <- function(n.hnd,conc,k.behav,k.age){ #conc unit need to be E. coli number per cm2;
  h.adult.area <- area.adult.hand();
  h.area <- area.hand(k.age);
  n.srf <- num.surface(conc,h.adult.area);
  p.att <- att.floor.hand();
  p.det <- det.floor.hand()*h.area/area.palm(k.age);
  if (k.behav=="Touching-mom hand") n.surf <- c(n.srf,0,0,0,0,0,0);
  if (k.behav=="Touching-other adult hand") n.surf <- c(0,n.srf,0,0,0,0,0);
  if (k.behav=="Touching-child hand") n.surf <- c(0,0,n.srf,0,0,0,0);
  if (k.behav=="Touching-fomite") n.surf <- c(0,0,0,0,n.srf,0,0);
  if (k.behav=="Touching-livestock") n.surf <- c(0,0,0,n.srf,0,0,0);
  res <- next.state(c(n.surf,n.hnd),c(p.att,p.det));
  return(res[8:14]);
}

area.palm <- function(k.age){
  if (k.age==1) {return(15.75)} #15.75 cm2 for 6 months old from HU meeting
  if (k.age==2) {return(29.25)} #29.25 cm2 for 12 months old from HU meeting
}
#area.hand<-function(void) exp(rtruncnorm(n=1,mean=2.75,sd=0.75,a=0,b=4))
area.hand<-function(k.age) runif(n=1,min = 1,max = area.palm(k.age))
area.adult.hand<- function(void) exp(rtruncnorm(n=1,mean=5,sd=0.5,a=4,b=6))
att.floor.hand<-function(void) rtriangle(n=1,a=0.01,b=0.19,c=0.1)
det.floor.hand<-function(void) rtriangle(n=1,a=0.25,b=0.75,c=0.5)

num.surface<-function(conc,area){
  lam <- area*conc;
  if(lam > 1e8) return(round(lam));
  return(rpois(n=1,lambda=lam));
}

hand.mouth<-function(n.hnd){
  p.att <- det.bact.hand.mouth()*fraction.hand.in.mouth();
  p.det <- 0;
  res <- next.state(c(n.hnd,0,0,0,0,0,0,0),c(p.att,p.det));
  return(res[8:14]);
}

fomite.mouth<-function(n.hnd){
  p.att <- det.bact.hand.mouth()*fomite.area.in.mouth()/100; #100 cm2 was assumed to be swabbed.
  p.det <- 0;
  res <- next.state(c(n.hnd,0,0,0,0,0,0,0),c(p.att,p.det));
  return(res[8:14]);
}

fomite.area.in.mouth <- function(void){
  return(runif(1,10,50)) #mouthing 10-50 cm2 of fomite 
}

# Bacterial fraction removed by hand-mouth contact; pr(removal)
# mean 33.97% Rusin w/ range from other trans eff
# source ????
det.bact.hand.mouth <- function(void) rtriangle(n=1,a=0.01,b=0.40,c=0.33);


#eating all by hands?
hand.eat<-function(void) return(1)
# hand.eat<-function(void) rbinom(1,1,rbeta(1,254,46)) #??? need to input number of eat by hands and number of eat by fork.

hand.food.contact<-function(n.hand,n.food){
  if (hand.eat()==1){
    p.att <- att.floor.hand();
    p.det <- det.floor.hand(); # assume all area of one hand touch the food, part of food were eaten by hands.
    ing <- c(0,0,0,0,0,0,0);
    state1 <- c();
    state1 <- next.state(c(n.food,n.hand),c(p.att,p.det));
    n.hand <- state1[8:14];
    n.food <- state1[1:7];
  }
  ing <- n.food;
  return(c(n.hand,ing));
}

hand.soil.contact<-function(n.hand,n.soil){
  p.att <- att.floor.hand();
  p.det <- det.floor.hand()
  ing <- c(0,0,0,0,0,0,0);
  state1 <- c();
  state1 <- next.state(c(n.soil,n.hand),c(p.att,p.det));
  n.hand <- state1[8:14];
  n.food <- state1[1:7];
  ing <- n.food;
  return(c(n.hand,ing));
}

# Fraction of surface area of hand placed in mouth, per hand
# source ????
fraction.hand.in.mouth <- function(void) rbeta(n=1,shape1=3.7,shape2=25)/2;

# area.areola <- function(void) rtruncnorm(n=1,mean=2.85,sd=1,a=0,b=10)
