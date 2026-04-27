#load packages;
library(MASS)
library(truncnorm)
library(triangle)
library(igraph)
#load data;
source("~/stat/EXCAM/expo/v1/v1.data1.R")
#load functions
source("~/stat/EXCAM/expo/v1/v1.func.R")

# conc <- data.frame(matrix(NA,nrow=1e6,ncol=10))
# #simul dists of env.samples e. coli concentration
# for (i in 1:length(env.paras)){
#   para <- mvrnorm(n.iter,env.paras[[i]][[2]],env.paras[[i]][[3]])
#   conc[,i] <- 10^(rnorm(1e6,mean = para[,1], sd = para[,2]))
#   names(conc)[i] <- env.paras[[i]][[1]]
# }


n.iter <- 1e6
conc <- data.frame(matrix(NA_real_, nrow = n.iter, ncol = length(env.paras)))

for (i in seq_along(env.paras)) {
  obj <- env.paras[[i]]
  src <- obj[[1]]
  names(conc)[i] <- src
  
  if (length(obj) == 9 && !is.null(obj$posterior_draws)) {
    draws <- obj$posterior_draws
    idx <- sample(seq_len(nrow(draws)), size = n.iter, replace = TRUE)
    
    mu_draw <- draws$mu_via[idx]
    sd_draw <- draws$sigma_via[idx]
    
  } else if (length(obj) == 3) {
    para <- MASS::mvrnorm(n.iter, mu = obj[[2]], Sigma = obj[[3]])
    mu_draw <- para[, 1]
    sd_draw <- para[, 2]
    
  } else {
    stop(paste("Unexpected structure for source", src))
  }
  
  ok <- is.finite(mu_draw) & is.finite(sd_draw) & (sd_draw > 0)
  
  x <- rep(NA_real_, n.iter)
  x[ok] <- 10^(rnorm(sum(ok), mean = mu_draw[ok], sd = sd_draw[ok]))
  
  conc[, i] <- x
}

save(conc, file="conc_0404.rds")

#truncate conc
# load("~/stat/EXCAM/env_conc/data/max_value.rda")
# max.value1 <- max.value[which(max.value$Group.1=="01" | max.value$Group.2=="soil"),]
# for (i in 1:length(max.value1$x)){
#   conc[which(conc[,max.value1$Group.2[i]]>max.value1$x[i]),max.value1$Group.2[i]] <- max.value1$x[i]
# }
# sapply(conc, max, na.rm = TRUE)


# seq.beh <- gen.beh.seq(1,7,4,14*60)
# 
# comps <- compartments
# behavs <- c(names(rate_table),"bathing","drinking")
# 
# for (i in 1:length(comps)){
#   for (j in 1:length(behavs)){
#     print(paste(comps[i],behavs[j]))
#     print(cbind(expos.by.state(1,1,comps[i],behavs[j],rep(0,7)),
#                                c("mother", "other adult", "other child", "livestock", "fomite", "food", "soil", "mother", "other adult", "other child", "livestock", "fomite", "food", "soil", "bathing water", "drinking water", "breast milk", "areola surface")))
#   }
# }
# 
# expo <- exp.by.cat(1,1,14*60)
# colSums(expo[,11:21],na.rm=TRUE)
