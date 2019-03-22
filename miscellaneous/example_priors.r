library(data.table)
library(ggplot2)

# gamma
n=1000
ks = seq(.5,3,by=.5)
ts = seq(1,7,by=1)
i=1
for(k in ks) {
	for(t in ts) { 
		if (i==1) results = data.table(k=rep(k,n), t=rep(t,n), est=rgamma(n,k,t))
		if (i>1) results = rbind(results, data.table(k=rep(k,n), t=rep(t,n), est=rgamma(n,k,t)))
	}
		i=i+1
}
results[, params:=paste0(k,',',t)]
results = results[est<quantile(est,.95)]
ggplot(results, aes(x=est)) + 
	geom_histogram(aes(y = ..density..), color='black') + 
	geom_smooth(stat='density') + 
	facet_wrap(~params, scales='free') + 
	theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
		
# beta
n=1000
as = seq(1,6,by=1)
bs = seq(1,6,by=1)
i=1
for(a in as) {
	for(b in bs) { 
		if (i==1) results = data.table(a=rep(a,n), b=rep(b,n), est=rbeta(n,a,b))
		if (i>1) results = rbind(results, data.table(a=rep(a,n), b=rep(b,n), est=rbeta(n,a,b)))
		i=i+1
	}
}
results
ggplot(results[est<5], aes(x=est)) + 
	geom_histogram(aes(y = ..density..), color='black') + 
	geom_smooth(stat='density') + 
	facet_grid(a~b, scales='free_y')

	
# normal

n=1000
mus = seq(-5,5,by=1)
sigmas = seq(.5,3,by=1)
i=1
for(mu in mus) {
	for(sigma in sigmas) { 
		if (i==1) results = data.table(mu=rep(mu,n), sigma=rep(sigma,n), est=rnorm(n,mu,sigma))
		if (i>1) results = rbind(results, data.table(mu=rep(mu,n), sigma=rep(sigma,n), est=rlnorm(n,mu,sigma)))
	}
		i=i+1
}
results[, params:=paste0(mu,',',sigma)]
results = results[est<quantile(est,.8)]
ggplot(results[est<4], aes(x=est)) + 
	geom_histogram(aes(y = ..density..), color='black') + 
	geom_smooth(stat='density') + 
	facet_wrap(~params, scales='free') + 
	theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
	
# lognormal

n=1000
ks = seq(.5,1.5,by=.25)
ts = seq(.5,3,by=.5)
i=1
for(k in ks) {
	for(t in ts) { 
		if (i==1) results = data.table(k=rep(k,n), t=rep(t,n), est=rlnorm(n,k,t))
		if (i>1) results = rbind(results, data.table(k=rep(k,n), t=rep(t,n), est=rlnorm(n,k,t)))
	}
		i=i+1
}
results[, params:=paste0(k,',',t)]
results = results[est<quantile(est,.8)]
ggplot(results[est<4], aes(x=est)) + 
	geom_histogram(aes(y = ..density..), color='black') + 
	geom_smooth(stat='density') + 
	facet_wrap(~params, scales='free') + 
	theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
