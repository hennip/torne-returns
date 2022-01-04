
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
s_d1 ~dlnorm(3,1/(0.3^2))
s_d2 ~dlnorm(3,1/(0.3^2))
w ~dbeta(2,2)

#sqrt(1/(1/log(1.09)))
#1/(0.295^2)

#sqrt(1/(11.1))
#1/(0.3^2)

#1/(2.953^2)

#1/10^2
#sqrt(1/(11.6))
#1/11.6 = 0.3^2
#0.3^2
#1/log(1.09)
#sqrt(1/11.1111111111111111)
#1/0.09
#log(20)-0.5*log(1.09)

mu_d1p ~ dnorm(20,0.01)
mu_d2p ~ dnorm(60,0.01)
s_d1p ~dlnorm(3,1/(0.3^2))
s_d2p ~dlnorm(3,1/(0.3^2))
wp ~dbeta(2,2)

#####################################################################################
# Observation models for the distance distributions in both counters
# Counter = 1 ;long beam
# Counter = 2 ;short beam
# Observation probability as a function of distance for counter=1.
#####################################################################

# pi_1[d]=probability that a fish that passes the site at distance d becomes observed by counter 1
# theta_1 = probability that the counter 1 is on
# kappa_1 = eksperttiperusteinen todnäk.
# gamma_1 = cut off distance

for(d in 1:ndist){
pi_1[d]=theta_1*kappa_1[d]#*step(gamma_1-distances[d])

# TÄHÄN HAVAITSEMISTN:N FUNKTIO PITKÄLLÄ IKKUNALLA

kappa_1[d] = 0.37*(exp(ip[d])/(1+exp(ip[d])))+0.63
ip[d]=a-b*distances[d]
}

theta_1=1/6 # assuming 10 minutes every hour
#gamma_1=80 #~dnorm()

# priors based on expert view:

a ~ dnorm(4,1/0.14^2)
b ~ dnorm(0.058,1000000)

ap ~ dnorm(4,1/0.14^2)
bp ~ dnorm(0.058,1000000)




# Observation probability as a function of distance for counter=2.
######################################################################

# pi_2[d]=probability that a fish that passes the site at distance d becomes observed by counter 2
# theta_2 = probability that the counter 2 is on
# kappa_2 = ratio between counters 1 and 2
# gamma_2 = cut off distance
# LYHYELLÄ IKKUNALLA

for(d in 1:ndist){
pi_2[d]=theta_2*kappa_2[d]*step(gamma_2-distances[d])

kappa_2[d] = 0.37*(exp(ip2[d])/(1+exp(ip2[d])))+0.63
ip2[d] = a2-b2*distances[d]
}

theta_2=5/6 # assuming 50 minutes every hour
gamma_2=65 #~dnorm()I(,gamma_1)  #   Short beam, cut off must be lower than for the long beam

# priors based on expert view:

a2 ~ dnorm(4,1/0.14^2)
b2 ~ dnorm(0.058,1000000)

a2p ~ dnorm(4,1/0.14^2)
b2p ~ dnorm(0.058,1000000)


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

#for(d in 1:ndist){
#nu_1[d]=  
#nu_2[d]=
#}

}"

model_final_data <- list(X_Long=kalat80, X_Short=kalat40, X=sum(kalat40)+sum(kalat80), XC=c(sum(kalat80),sum(kalat40)),  
                         ndist= 80, distances=seq(1, 80, by=1))
data<- model_final_data
inits<- list(list(U=11),list(U=12),list(a=4),list(a2=4),list(b=0.058),list(b2=0.058),list(a=4.1),list(a2=4.1),
             list(b=0.057),list(b2=0.057), list(mu_d1=20), list(mu_d2=60), list(w=0.5), list(s_d1=3), list(s_d2=3), 
             list(mu_d1=20.1), list(mu_d2=60.1), list(w=0.51), list(s_d1=3.1), list(s_d2=3.1))

var_names<- c("N", "a", "b", "a2", "b2", "mu_d1", "mu_d2", "w", "s_d1", "s_d2", "P", "q",
              "Np", "ap", "bp", "a2p", "b2p", "mu_d1p", "mu_d2p", "wp", "s_d1p", "s_d2p", "Pp")

t1<-Sys.time();t1
run1 <- run.jags(ModelName,
                 monitor= var_names,data=data, inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin = 1500,
                 modules = "mix",keep.jags.files=F,sample =2000, adapt = 100,
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1)


#t1<-Sys.time();t1
#run2 <- extend.jags(run1, add.monitor=c("P", "Pp"), combine=F,
#                    sample=2000, thin=100,
#                    keep.jags.files=F)
#t2<-Sys.time()
#difftime(t2,t1)
#help("extend.jags")


chains<-as.mcmc.list(run1)
chainsall <- combine.mcmc(mcmc.objects = chains)
#chains<-window(chains,start=150000)
#save(chains, file="FINAL.RData")


par(mfrow=c(3,3))
traceplot(chains[,"a"], main="a_1")
traceplot(chains[,"a2"], main="a_2")
traceplot(chains[,"b"], main="b_1")
traceplot(chains[,"b2"], main="b_2")
traceplot(chains[,"mu_d1"], main="mu_1")
traceplot(chains[,"mu_d2"], main="mu_2")
traceplot(chains[,"s_d1"], main="s_1")
traceplot(chains[,"s_d2"], main="s_2")
traceplot(chains[,"w"], main="w")

summary(run1)
#plot(run1)

#summary(chains)

#summary(chainsall)

par(mfrow=c(2,2))

plot(density(chainsall[,"a"]), xlim=c(2,5), main="a_1", ylab="Tiheys", xlab="Arvo")
lines(density(chainsall[,"ap"]),col="red")

plot(density(chainsall[,"a2"]), xlim=c(3,6), main="a_2", ylab="Tiheys", xlab="Arvo")
lines(density(chainsall[,"a2p"]),col="red")

plot(density(chainsall[,"b"]), xlim=c(0.04,0.08), main="b_1", ylab="Tiheys", xlab="Arvo")
lines(density(chainsall[,"bp"]),col="red")

plot(density(chainsall[,"b2"]), main="b_2", ylab="Tiheys", xlab="Arvo")
lines(density(chainsall[,"b2p"]),col="red")

par(mfrow=c(2,3))
plot(density(chainsall[,"N"]), xlim=c(23000,25000), main="N", ylab="Tiheys", xlab="Arvo")
lines(density(chainsall[,"Np"]),col="red")

plot(density(chainsall[,"mu_d1"]), main="mu_1", ylab="Tiheys", xlab="Arvo")
lines(density(chainsall[,"mu_d1p"]),col="red")

plot(density(chainsall[,"mu_d2"]), main="mu_2", ylab="Tiheys", xlab="Arvo")
lines(density(chainsall[,"mu_d2p"]),col="red")

plot(density(chainsall[,"w"]), xlim=c(0,1), main="w", ylab="Tiheys", xlab="Arvo")
lines(density(chainsall[,"wp"]),col="red")

plot(density(chainsall[,"s_d1"]), main="s_1", ylab="Tiheys", xlab="Arvo")
lines(density(chainsall[,"s_d1p"]),col="red")

plot(density(chainsall[,"s_d2"]), main="s_2", ylab="Tiheys", xlab="Arvo")
lines(density(chainsall[,"s_d2p"]),col="red")


par(mfrow=c(1,1))
hist((chainsall[,"N"]), xlim=c(23400,24200), main="N posteriori", ylab="Tiheys", xlab="Arvo")
hist((chainsall[,"Np"]), xlim=c(0,450000), main="N priori", ylab="Tiheys", xlab="Arvo")



summary(chains[,"a"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"a2"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"b"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"b2"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"mu_d1"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"mu_d2"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"w"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"s_d1"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"s_d2"], quantiles=c(0.05,0.25,0.5,0.75,0.95))

summary(chains[,"ap"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"a2p"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"bp"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"b2p"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"mu_d1p"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"mu_d2p"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"wp"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"s_d1p"], quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(chains[,"s_d2p"], quantiles=c(0.05,0.25,0.5,0.75,0.95))














