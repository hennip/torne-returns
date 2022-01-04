

Ploc=w1*x1+w2*x2+(1-w1-w2)*x3


Ploc=w*x1+(1-w)*x2
x1~dnorm(20,1/(2*2))
x2~dnorm(50,1/(2*2))
w<-0.2 # dbeta(10,5)

x1~dnorm(mu_x1,1/(sd_x1*sd_x1))
x2~dnorm(mu_x2,1/(sd_x2*sd_x2))
mu_x1~dnorm()
sd_x1~dunif()
mu_x2~
sd_x2~


dnorm(dist[i],m1,s1)+(1-w)*dnorm(dist[i],m2,s2)

library(rjags)
library(ggplot2)


expected_true_fish_model3="
model {
Ploc=w1*x1+w2*x2+(1-w1-w2)*x3
x1~dnorm(15,0.001)
x2~dnorm(40,0.1)
x3~dnorm(65,0.0001)
w1<-0.33 # dbeta(10,5)
w2<-0.33
}"

jm=jags.model(textConnection(expected_true_fish_model3),n.chains=1)
chains=coda.samples(jm,c("Ploc", "x1", "x2", "x3"),n.iter=1000)


plot(density(chains[,"Ploc"][[1]]), ylim=c(0,0.05), xlim=c(0,100))


M="
model {
p=(1-pnorm(dist,m,s))*max

max<-0.9#~dnorm()
m<-50#~dnorm()
s<-d*#~dunif()
cv<-0.002*d+0.05
}"

jm=jags.model(textConnection(expected_true_fish_model3),n.chains=1)
chains=coda.samples(jm,c("Ploc", "x1", "x2", "x3"),n.iter=1000)


plot(density(chains[,"Ploc"][[1]]), ylim=c(0,0.05), xlim=c(0,100))



#**********************
boxplot.df<-function(param, X){ # chain object, variable name, values to x-axis
  # note: length of x and dim variable need to match
  Q5<-c();Q25<-c();Q50<-c();Q75<-c();Q95<-c()
  n<-length(X)
  
  for(i in 1:n){
    y <- as.mcmc(param[,i])
    tmp<-summary(y,quantiles=c(0.05,0.25,0.5,0.75,0.95))
    Q5[i] = tmp$quantiles[1]
    Q25[i] = tmp$quantiles[2]
    Q50[i] = tmp$quantiles[3]
    Q75[i] = tmp$quantiles[4]
    Q95[i] = tmp$quantiles[5]
  }
  
  df<-data.frame(
    x<-X,
    q5=Q5,
    q25=Q25,
    q50=Q50,
    q75=Q75,
    q95=Q95
  )
  colnames(df)<-c("x","q5","q25","q50","q75","q95")
  return(df)
}


M2="
model {
p<-(1-pnorm(dist,m,tau))*max
tau<-1/(s*s)
max~dbeta(50, 1)
m<-85#~dlnorm(4.65, 1/(0.07*0.07))
s<-40 #ei saa olla neg
}
"


M3="
model {
for(i in 1:nDist){
    p[i]<-0.35*(exp(ip[i])/(1+exp(ip[i])))+0.65
    ip[i]~dnorm(a-b*dist[i],1/pow(sd[i],2))
sd[i]<-0.005*dist[i]
}
# priors for observation process
a<-4#~dnorm(2.9,60)
b<-0.08#~dlnorm(-2.6,984)
#sd<-0.001#~dunif(0.01,0.5)#lnorm(-0.23,210)
}"


data80 <- list(dist=1:100, nDist=length(1:100))
niter<-200
jm80=jags.model(textConnection(M3),data=data80,n.chains=1)
chains80=coda.samples(jm80,c("p","a", "b", "sd"),n.iter=niter)

px<-array(NA, dim=c(100, niter))
for(i in 1:100){
px[i,]<-chains80[,paste0("p[",i,"]")][[1]]
}
dim(px)

plot(c(1:100), px[,1], type="l", ylim=c(0.5,1))
for(i in 2:99){
  lines(c(1:100), px[,i])
}
abline(v=c(41.5,81.5), h=0.7)





ax<-c80[,"a"]
bx<-c80[,"b"]
sdx<-c80[,"sd"]

length(ax)





###########################################################################################

# pick chains for comparison
c80<-chains80[[1]]

# Posterior

n_samp80<-length(c80[,"a"])
a_samp80<-c80[,"a"]
b_samp80<-c80[,"b"]
sd_samp80<-c80[,"sd"]

dist80<-1:100
ndist80<-length(dist80)

p_samp80<-array(NA, dim=c(n_samp80,ndist80))
x<-array(NA, dim=c(n_samp80,ndist80))

for(j in 1:n_samp80){
  for(i in 1:ndist80){
    x[j,i]<-rnorm(1,a_samp80[j]-b_samp80[j]*dist[i],sd_samp80[j])
    p_samp80[j,i]<-1*(exp(x[i])/(1+exp(x[i])))+0.7
  }
}

df80<-boxplot.df(p_samp80, dist80)
#df.prior<-boxplot.df(p_samp, dist)

ggplot(df80, aes(x,group=x))+
  #  geom_boxplot(data=df.prior,
  #               mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
  #               stat = "identity",
  #              col="grey", fill="grey95")+
  geom_boxplot(
    mapping=aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Distance", y="Probability", title="Probability that a salmon is observed")+
  geom_line(aes(x,q50))+
  # geom_line(data=df.prior, aes(x,q50), color="grey")+
  theme_bw()+
  coord_cartesian(ylim=c(0.2,1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

