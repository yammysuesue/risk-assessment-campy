#load packages;
library(MASS)
library(truncnorm)
library(triangle)
library(igraph)
#load data;
source("~/stat/EXCAM/expo/v1/v1.data2.R")
#load functions
source("D:/1. Yixuan Work/Spring 2026/RA/Risk Assessment/EXCAM-main/expo/v1/v1.func.R")
source("D:/1. Yixuan Work/Spring 2026/RA/Risk Assessment/EXCAM-main/expo/v1/v1.func2.R")


env.paras <- env.paras_via_T2


# delete HA, HB (have NAs)
env.paras <- env.paras[sapply(env.paras, function(x) {
  mu <- x[[2]]
  Sigma <- x[[3]]
  all(is.finite(mu)) && all(is.finite(Sigma))
})]

conc <- matrix(NA, nrow = n.iter, ncol = length(env.paras))

for (i in 1:length(env.paras)) {
  para <- mvrnorm(n.iter, mu = env.paras[[i]][[2]], Sigma = env.paras[[i]][[3]])
  sd_draw <- pmax(para[,2], 1e-6)
  conc[, i] <- 10^(rnorm(n.iter, mean = para[,1], sd = sd_draw))
}

source_map <- c(
  EB = "bathing water",
  ED = "drinking water",
  EF = "food",
  ES = "soil",
  EV = "fomite",
  HA = "areola swab",
  HB = "breastmilk",
  HH = "sibling handrinse",
  HP = "mother handrinse",
  HR = "infant handrinse"
)

colnames(conc) <- source_map[names(env.paras)]
# make up "breastmilk" by hand
conc <- as.data.frame(conc)
#conc$breastmilk <- 0

# load("~/stat/EXCAM/env_conc/data/max_value.rda")
# max.value2 <- max.value[which(max.value$Group.1=="02" | max.value$Group.2=="soil"),]
# for (i in 1:length(max.value2$x)){
#   conc[which(conc[,max.value2$Group.2[i]]>max.value2$x[i]),max.value2$Group.2[i]] <- max.value2$x[i]
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
