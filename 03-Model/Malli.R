

library(rjags)
library(runjags)
load.module("mix")
library(tidyverse)
library(ggmcmc)
library(readxl)
library(forcats)
library(lubridate)
library(stringr)
require(gridExtra)
require(rjags)

# Path for input data
pathIn<-"C:/Users/candi/OneDrive/Tiedostot/1 GRADU/"

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



# todelliset vektorit
#kalat40 <- c(0, 0, 2, 5, 5, 18, 45,  104,  254,  193,  227,  521,  458,  353,  751, 1029,  765,  943,  821,  745, 591,  366,
#  701, 656, 384, 416,  548,  680,  554,  715,  501,  778,  512,  477,  359,  220,  196,  118,  207,  161,  298,  303,
#  219, 144, 133, 219,  250,  246,  247,  171,  168,  124,  117,  130,  168,  102,  112,   30,   39,   29,   34,   10,
#  11, 5, 8, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

#kalat80 <- c(0, 0, 0, 0, 0, 1,  10,  20,  42,  20,  26,  91,  71,  72, 139, 164, 145, 143, 150, 135,  73,  78, 131,  77,  53,
#39,  83, 75,  86,  83,  78,  93,  78,  68,  54,  22,  27,  25,  27,  27,  46,  45,  18,  16,  27,  35,  45,  37,  34,
#32,  23,  21,  31,  27,  28,  39,  43,  32,  26,  28,  13,  16,  23,  10,  15,  12,  15,   8,   5,  4,   2, 0, 0, 0, 0, 0, 0, 0, 0, 0)



ModelName<-"
model{

######################################################################################
# N: true number of fish crossing the site during the whole summer

N = round(exp(U))
U ~ dunif(7,13)

Np = round(exp(Up))
Up ~ dunif(7,13)

#####################################################################################
# Obervation model for the fish observed in counters
# X: Total number of fish observed by both counters
# XC[1:2]: Total number of fish observed by counters 1 and 2, respectively
# Q: probability that a fish passing the site becomes observed by one of the counters
# QC[1:2]: probability that a fish that becomes observed, is observed by counter 1 and 2, respectively
# q[1:2]: probability that a fish becomes observed by counters 1 and 2, respectively

X~dbin(Q,N)                     
XC[1:2]~dmulti(QC[1:2],X)

Q=q[1]+q[2]
QC[1]=q[1]/Q
QC[2]=q[2]/Q

# Observation model for daily observed distance frequencies in counters 1 and 2

X_Long[1:ndist]~dmulti(nu_1[1:ndist],XC[1])
X_Short[1:ndist]~dmulti(nu_2[1:ndist],XC[2])

#####################################################################################

# Model for the true distance distribution
# P[d]: probability that a fish that crosses the site crosses at distance d

for(d in 1:ndist){

P_star[d]=w*(1/(sqrt(2*3.141593)*s_d1))*exp(-0.5*pow(((d-mu_d1)/s_d1),2))+
(1-w)*(1/(sqrt(2*3.141593)*s_d2))*exp(-0.5*pow(((d-mu_d2)/s_d2),2))

P[d]=P_star[d]/sum(P_star[])

P_starp[d]=wp*(1/(sqrt(2*3.141593)*s_d1p))*exp(-0.5*pow(((d-mu_d1p)/s_d1p),2))+
(1-wp)*(1/(sqrt(2*3.141593)*s_d2p))*exp(-0.5*pow(((d-mu_d2p)/s_d2p),2))

Pp[d]=P_starp[d]/sum(P_starp[])

}

# priors

mu_d1 ~ dnorm(20,0.01)
mu_d2 ~ dnorm(60,0.01)
s_d1 ~dlnorm(log(20)-0.5*log(0.3*0.3+1),1/log(0.3*0.3+1))
s_d2 ~dlnorm(log(20)-0.5*log(0.3*0.3+1),1/log(0.3*0.3+1))
w ~dbeta(0.5*4,0.5*4)

mu_d1p ~ dnorm(20,0.01)
mu_d2p ~ dnorm(60,0.01)
s_d1p ~dlnorm(log(20)-0.5*log(0.3*0.3+1),1/log(0.3*0.3+1))
s_d2p ~dlnorm(log(20)-0.5*log(0.3*0.3+1),1/log(0.3*0.3+1))
wp ~dbeta(0.5*4,0.5*4)

#####################################################################################
# Observation models for the distance distributions in both counters
# Counter = 1 ;long beam
# Counter = 2 ;short beam
# Observation probability as a function of distance for counter=1.
#####################################################################

# pi_1[d]=probability that a fish that passes the site at distance d becomes observed by counter 1
# theta_1 = probability that the counter 1 is on
# kappa_1 = eksperttiperusteinen todn?k.
# gamma_1 = cut off distance

for(d in 1:ndist){
pi_1[d]=theta_1*kappa_1[d]#*step(gamma_1-distances[d])

# T?H?N HAVAITSEMISTN:N FUNKTIO PITK?LL? IKKUNALLA

kappa_1[d] = 0.37*(exp(ip[d])/(1+exp(ip[d])))+0.63
ip[d] ~ dnorm(a-b*distances[d],1/pow(sdX*distances[d],2))
}

theta_1=1/6 # assuming 10 minutes every hour
#gamma_1=82 #~dnorm()

# priors based on expert view:

a ~ dnorm(4,50)
b ~ dnorm(0.058,1000000)
#sdX <- 0.005 
sdX~dlnorm(log(0.005)-0.5*log(0.3*0.3+1),1/log(0.3*0.3+1))

ap ~ dnorm(4,50)
bp ~ dnorm(0.058,1000000)
#sdXp<-0.005 
sdXp~dlnorm(log(0.005)-0.5*log(0.3*0.3+1),1/log(0.3*0.3+1))


# Observation probability as a function of distance for counter=2.
######################################################################

# pi_2[d]=probability that a fish that passes the site at distance d becomes observed by counter 2
# theta_2 = probability that the counter 2 is on
# kappa_2 = ratio between counters 1 and 2
# gamma_2 = cut off distance
# LYHYELL? IKKUNALLA

for(d in 1:ndist){
pi_2[d]=theta_2*kappa_2[d] #*step(gamma_2-distances[d])

kappa_2[d] = 0.37*(exp(ip2[d])/(1+exp(ip2[d])))+0.63
ip2[d] ~ dnorm(a2-b2*distances[d],1/pow(sdX2*distances[d],2))
}

theta_2=5/6 # assuming 50 minutes every hour
#gamma_2=42 #~dnorm()I(,gamma_1)  #   Short beam, cut off must be lower than for the long beam

# priors based on expert view:

a2 ~ dnorm(4,50)
b2 ~ dnorm(0.058,1000000)
#sdX2 <- 0.005 
sdX2 ~dlnorm(log(0.005)-0.5*log(0.3*0.3+1),1/log(0.3*0.3+1))

a2p ~ dnorm(4,50)
b2p ~ dnorm(0.058,1000000)
#sdX2p <- 0.005 
sdX2p ~dlnorm(log(0.005)-0.5*log(0.3*0.3+1),1/log(0.3*0.3+1))


# Probability of observing a fish at distance d, in counters 1 and 2

for(d in 1:ndist){
PI_1[d]=P[d]*pi_1[d]   # Probability to cross * probablity to observe
PI_2[d]=P[d]*pi_2[d]   # Probability to cross * probablity to observe
}

# Probability of observing a fish in counters 1 and 2 at any distance

q[1]=sum(PI_1[])  # these plug into QC!
q[2]=sum(PI_2[])

# Probability that a fish that was observed in counter 1 (and 2) was at distance d

for(d in 1:ndist){
nu_1[d]=PI_1[d]/q[1]  
nu_2[d]=PI_2[d]/q[2]
}
}"

#vektorit kalahavaintojen lukum??rist?
kalat40 <- c(1, 1, 2, 5, 5, 18, 45,  104,  254,  193,  227,  521,  458,  353,  751, 1029,  765,  943,  821,  745, 591,  366,
             701, 656, 384, 416,  548,  680,  554,  715,  501,  778,  512,  477,  359,  220,  196,  118,  207,  161,  298,  303,
             219, 144, 133, 219,  250,  246,  247,  171,  168,  124,  117,  130,  168,  102,  112,   30,   39,   29,   34,   10,
             11, 5, 8, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

kalat80 <- c(1, 1, 1, 1, 1, 1,  10,  20,  42,  20,  26,  91,  71,  72, 139, 164, 145, 143, 150, 135,  73,  78, 131,  77,  53,
             39,  83, 75,  86,  83,  78,  93,  78,  68,  54,  22,  27,  25,  27,  27,  46,  45,  18,  16,  27,  35,  45,  37,  34,
             32,  23,  21,  31,  27,  28,  39,  43,  32,  26,  28,  13,  16,  23,  10,  15,  12,  15,   8,   5,  4,   2, 1, 1, 1, 
             1, 1, 1, 1, 1, 1)





model_final_data <- list(X_Long=kalat80, X_Short=kalat40, X=sum(kalat40)+sum(kalat80), XC=c(sum(kalat80),sum(kalat40)),  
                         ndist= 80, distances=seq(2.5, 81.5, by=1)) #muuta binssien m??r? 
#distances=seq(4.5, 79.5, by=5)
inits<- list(list(U=11),list(U=12)) # Alkuarvot, voi koittaa ensin ilman jolloin JAGS generoi alkuarvot

var_names<- c("N", "a", "b", "a2", "b2", "mu_d1", "mu_d2", "w", "s_d1", "s_d2", "sdX", "sdX2",
              "Np", "ap", "bp", "a2p", "b2p", "mu_d1p", "mu_d2p", "wp", "s_d1p", "s_d2p", "sdXp", "sdX2p")

run.jags(ModelName,
                 monitor= var_names,data=model_final_data, inits = inits,
                 n.chains = 2, method = 'parallel', thin=1, burnin =100,
                 modules = "mix",keep.jags.files=T,sample =2000, adapt = 100,
                 progress.bar=TRUE)



t1<-Sys.time();t1
run1 <- run.jags(ModelName,
                 monitor= var_names,data=data, inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =1000,
                 modules = "mix",keep.jags.files=T,sample =2000, adapt = 100,
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1) # T?ll? n?kee ajon keston

#save(run1, file="ajo.RData")


chains<-as.mcmc.list(run1)
chains<-window(chains,start=100000)
summary(run1)
plot(run1)
par(mfrow=c(3,3))
traceplot(chains[,"a2",], main="a2")
traceplot(chains[,"a"])
traceplot(chains[,"b2"])
traceplot(chains[,"b"])
traceplot(chains[,"mu_d1"])
traceplot(chains[,"mu_d2"])
traceplot(chains[,"w"])
traceplot(chains[,"s_d1"])
traceplot(chains[,"s_d2"])

#summary(chains)
#chains[,"N"]

chainsall <- combine.mcmc(mcmc.objects = chains)
summary(chainsall)

par(mfrow=c(3,3))
plot(density(chainsall[,"a"]), xlim=c(2,5))
lines(density(chainsall[,"ap"]),col="red")

plot(density(chainsall[,"a2"]), xlim=c(3,6))
lines(density(chainsall[,"a2p"]),col="red")

plot(density(chainsall[,"b"]), xlim=c(0.04,0.08))
lines(density(chainsall[,"bp"]),col="red")

plot(density(chainsall[,"b2"]))
lines(density(chainsall[,"b2p"]),col="red")

plot(density(chainsall[,"mu_d1"]))
lines(density(chainsall[,"mu_d1p"]),col="red")

plot(density(chainsall[,"mu_d2"]))
lines(density(chainsall[,"mu_d2p"]),col="red")

plot(density(chainsall[,"w"]), xlim=c(0,1))
lines(density(chainsall[,"wp"]),col="red")

plot(density(chainsall[,"s_d1"]))
lines(density(chainsall[,"s_d1p"]),col="red")

plot(density(chainsall[,"s_d2"]))
lines(density(chainsall[,"s_d2p"]),col="red")

plot(density(chainsall[,"sdX"]), xlim=c(0,0.02))
lines(density(chainsall[,"sdXp"]),col="red")

plot(density(chainsall[,"sdX2"]), xlim=c(-0.001,0.02))
lines(density(chainsall[,"sdX2p"]),col="red")



#gelman.diag(run1, confidence = 0.95, transform=FALSE, autoburnin=TRUE, # multivariate=TRUE)

#luotaimen paikka muuttuu, joten cut off distancelle prioria?

#miten a ja b yhdess? heijastuu miten kalan havaitsemistn on ollut priorina ja miten on posteriorina et?isyyden muuttuessa



