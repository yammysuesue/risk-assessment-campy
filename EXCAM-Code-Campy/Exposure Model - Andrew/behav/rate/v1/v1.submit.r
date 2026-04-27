library(rjags);

ver <- "v1";
# nburn <- 1000;
# niter <- 10000;
# thin <- 10;
nburn <- 1000;
niter <- 10000;
thin <- 10;
# tomonitor <- c("logr","loglambda");
tomonitor <- c("loglambda");

# choose subset: subset <- c(domain,agecat)
# where domain = "hh" or "ns"
# and agecat = 0, 1, or 2 (years)

# source(paste(ver,"data.r",sep="."));

cat("<<< MCMC Sampling >>>\n");
model.jags <- jags.model(file=paste(ver,"model.jags",sep="."),
                         data=sel.data,
                         n.chains=3,n.adapt=nburn,quiet=FALSE);
mcmc.post <- coda.samples(model.jags,tomonitor,n.iter=niter,thin=thin);

save(mcmc.post,file=paste("./output/",ver,"-post-",
                          agecat,".rda",sep=""),ascii=TRUE);

cat("<<< Storing the results >>>\n");
sink(paste("./output/",ver,".results.",agecat,".txt",sep=""));
print(summary(mcmc.post));
sink();

cat("<<< Graphing results >>>\n");
pdf(paste(ver,"results",agecat,"pdf",sep="."));
plot(mcmc.post,trace=TRUE,density=FALSE);
plot(mcmc.post,trace=FALSE,density=TRUE);
# autocorr(mcmc.post);
# autocorr.plot(mcmc.post);
# crosscorr(mcmc.post);
# crosscorr.plot(mcmc.post);
# geweke.diag(mcmc.post);
dev.off();

# Extract mcmc values of the important parameters
# and save them in a text file
# cat("<<< Extract parameters >>>\n");
# source(paste(ver,"extract","r",sep="."));
