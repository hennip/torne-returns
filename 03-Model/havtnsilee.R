########################################################################
#HAVAITSEMISTODENN?K?ISYYS
########################################################################
#pitk? ikkuna
library(rjags)

M3="
model {
for(i in 1:nDist){
p[i]<-0.37*(exp(ip[i])/(1+exp(ip[i])))+0.63
ip[i]=a-b*dist[i]
}

# priors for observation process
a ~ dnorm(4,50)
b ~ dnorm(0.058,1000000)
}"
 
data80 <- list(dist=1:80, nDist=length(1:80))
niter<-1000
jm80=jags.model(textConnection(M3),data=data80,n.chains=1)
chains80=coda.samples(jm80,c("p","a", "b", "ip"),n.iter=niter)
#plot(chains80)
par(mfrow=c(1,1))

px<-array(NA, dim=c(80, niter))
for(i in 1:80){
  px[i,]<-chains80[,paste0("p[",i,"]")][[1]]
}
plot(c(1:80), px[,1], type="l", ylim=c(0.5,1), ylab="Havaitsemistodenn?k?isyys",xlab="Et?isyys luotaimesta metrein?")
for(i in 2:80){
  lines(c(1:80), px[,i])
}



data80 <- list(dist=1:80, nDist=length(1:80))
niter<-1000
jm80=jags.model(textConnection(M3),data=data80,n.chains=1)
chains80=coda.samples(jm80,c("p","a", "b", "ip"),n.iter=niter)
#plot(chains80)

px<-array(NA, dim=c(80, niter))
for(i in 1:80){
  px[i,]<-chains80[,paste0("p[",i,"]")][[1]]
}
plot(c(1:80), px[,1], type="l", ylim=c(0.5,1), ylab="Havaitsemistodenn?k?isyys",xlab="Et?isyys luotaimesta metrein?")
for(i in 2:80){
  lines(c(1:80), px[,i])
}


summary(chains80[,"p[80]"], quantiles=c(0.05,0.5,0.95)) #todenn?k?isyysv?lin valitseminen
#traceplot(chains80[,"b"])

# pick chains for comparison
c80<-chains80[[1]]

# Posterior

n_samp80 <-niter
#n_samp80<-length(c80[,"p"])
a_samp80<-c80[,"a"]
b_samp80<-c80[,"b"]
#sdX_samp80<-c80[,"sdX"]
#ip_samp80<-c80[,"ip"]
dist80<-1:80  #dist80=seq(4.5, 79.5, by=5))
#sd_samp80<-0.005*dist80
ndist80<-length(dist80)


p_samp80<-array(NA, dim=c(n_samp80,ndist80))
ip_samp80<-array(NA, dim=c(n_samp80,ndist80))

for(j in 1:n_samp80){
  for(i in 1:ndist80){
    ip_samp80[j,i]<-a_samp80[j]-b_samp80[j]*dist80[i]
    p_samp80[j,i]<-0.37*(exp(ip_samp80[j,i])/(1+exp(ip_samp80[j,i])))+0.63
  }
}

df80<-boxplot.df(p_samp80, dist80)
#df.prior<-boxplot.df(p_samp, dist)

ggplot(df80, aes(x,group=x))+
  geom_boxplot(
    mapping=aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Et?isyys", y="Todenn?k?isyys", title="Havaitsemistodenn?k?isyys")+
  geom_line(aes(x,q50))+
  theme_bw()+
  coord_cartesian(ylim=c(0.6,1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

###################################################################################
#lyhyt ikkuna

M3="
model {
for(i in 1:ndist){
pl[i]<-0.37*(exp(ip[i])/(1+exp(ip[i])))+0.63
ip[i]=a2-b2*dist[i]
}

# priors for observation process
a2 ~ dnorm(4,50)
b2 ~ dnorm(0.058,1000000)
}"

data40 <- list(dist=1:40, ndist=length(1:40))
niter<-1000
jm40=jags.model(textConnection(M3),data=data40,n.chains=1)
chains40=coda.samples(jm40,c("pl","a2", "b2", "ip"),n.iter=niter)
#plot(chains40)


px<-array(NA, dim=c(40, niter))
for(i in 1:40){
  px[i,]<-chains40[,paste0("pl[",i,"]")][[1]]
}

plot(c(1:40), px[,1], type="l", ylim=c(0.5,1), ylab="Havaitsemistodenn?k?isyys",xlab="Et?isyys")
for(i in 2:39){
  lines(c(1:40), px[,i])
}

summary(chains40[,"pl[40]"], quantiles=c(0.05,0.5,0.95)) #todenn?k?isyysv?lin valitseminen


# pick chains for comparison
c40<-chains40[[1]]

# Posterior

n_samp40 <-niter
a_samp40<-c40[,"a2"]
b_samp40<-c40[,"b2"]
dist40<-1:40
ndist40<-length(dist40)


p_samp40<-array(NA, dim=c(n_samp40,ndist40))
ip_samp40<-array(NA, dim=c(n_samp40,ndist40))

for(j in 1:n_samp40){
  for(i in 1:ndist40){
    ip_samp40[j,i]<-a_samp40[j]-b_samp40[j]*dist40[i]
    p_samp40[j,i]<-0.37*(exp(ip_samp40[j,i])/(1+exp(ip_samp40[j,i])))+0.63
  }
}

df40<-boxplot.df(p_samp40, dist40)
#df.prior<-boxplot.df(p_samp, dist)

ggplot(df40, aes(x,group=x))+
  geom_boxplot(
    mapping=aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Et?isyys", y="Todenn?k?isyys", title="Havaitsemistodenn?k?isyys")+
  geom_line(aes(x,q50))+
  theme_bw()+
  coord_cartesian(ylim=c(0.6,1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))






