library(rjags)
library(runjags)
library(coda)
library(ggplot2)

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



expected_true_fish_model2="
model {

for(d in 1:80){
P_star[d]=w*(1/(sqrt(2*3.141593)*s_d1))*exp(-0.5*pow(((d-mu_d1)/s_d1),2))+
(1-w)*(1/(sqrt(2*3.141593)*s_d2))*exp(-0.5*pow(((d-mu_d2)/s_d2),2))

P[d]=P_star[d]/sum(P_star[])
}

mu_d1 ~ dnorm(20,0.01)
mu_d2 ~ dnorm(60,0.01)
s_d1 ~dlnorm(log(20)-0.5*log(cv*cv+1),1/log(cv*cv+1))
s_d2 ~dlnorm(log(20)-0.5*log(cv*cv+1),1/log(cv*cv+1))
tau_d1 <-1/(s_d1*s_d1)
tau_d2 <-1/(s_d2*s_d2)
w ~dbeta(0.5*4,0.5*4)
 cv<-0.3
}"

jm=jags.model(textConnection(expected_true_fish_model2),n.chains=1)
etf2_chains=coda.samples(jm,c("P", "mu_d1", "mu_d2", "s_d1", "s_d2", "w"),n.iter=1000)
#plot(density(etf2_chains[,"P"][[1]]))
#summary(etf2_chains)

# pick chains for comparison
c2<-etf2_chains[[1]]

# Posterior
n_samp2<-length(c2[,"mu_d1"])

dist<-1:80
ndist<-length(dist)

P_samp2<-array(NA, dim=c(n_samp2,ndist))

for(i in 1:80){
  P_samp2[,i]<-c2[,paste0("P[",i,"]")]
}

df<-boxplot.df(P_samp2, dist)

ggplot(df, aes(x,group=x))+
  geom_boxplot(
    mapping=aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Etäisyys", y="Suhteellinen osuus", title="Lohien priorijakauma")+
  #geom_line(aes(x,q50))+
  theme_bw()+
  #coord_cartesian(ylim=c(0,1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

