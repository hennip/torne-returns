library(rjags)
library(runjags)

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
# X= N*(q[1]+q[2])
# XC[1]~dbin(q[1]/(q[1]+q[2]),X)
# XC[1]=q[1]/(q[1]+q[2]) * N*(q[1]+q[2]) =q[1]*N
# XC[2]=q[2]/(q[1]+q[2]) * N*(q[1]+q[2]) =q[2]*N
#X_Long[1:ndist]~dmulti(nu_1[1:ndist],q[1]*N)
#X_Short[1:ndist]~dmulti(nu_2[1:ndist],q[2]*N)

# XC[2]: havaittu, ei määritellä uudelleen
# yo. muodossa Q:n ja QC:t voisi jättää pois
XC[1:2]~dmulti(QC[1:2],X)

Q=q[1]+q[2]
QC[1]=q[1]/Q
QC[2]=q[2]/Q

# Observation model for daily observed distance frequencies in counters 1 and 2

X_Long[1:ndist]~dmulti(nu_1[1:ndist],XC[1])
X_Short[1:ndist]~dmulti(nu_2[1:ndist],XC[2])

####################### ##############################################################

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
nu_1[d]=P[d]/q[1]  
nu_2[d]=P[d]/q[2]
}
}"

kalat40 <- c(1,2,2,4,9,21,62,131,179,130,123,288,480,276,248,535,852,634,591,687,655,
             577,570,307,316,539,575,317,305,292,432,566,395,588,398,391,610,459,291,375,240,161,
             148,155,78,168,107,203,256,235,128,104,110,165,178,189,185,203,143,130,114,96,84,
             102, 105,114,  91,  92,  24,  32,  30,  20,  27,  10,  10,   2,   5,   7,   2,   1)
kalat80<- c(1,   3,   7,  11,  35,  29,  12,  27,  81,  65,  55,  75, 144, 116, 121, 109, 132, 104, 107,  53,  55,
            91,  99,  49,  37,  32,  59  ,71,  46,  92,  60,  56,  78,  81,  53,  50,  40,  17,  25,  17,  21,  22,
            19,  33,  33,  28,  14,  15,  22,  24,  42,  29,  30,  25,  22,  26,  16,  21,  22,  20,  27,  19,  35,
            42,  23,  22,  17,  15,  15,  15,  15,   9,  12,  10,  10,  13,   5,   4,   6,   2)


model_final_data <- list(X_Long=kalat80, X_Short=kalat40, 
                         X=sum(kalat40)+sum(kalat80), 
                         XC=c(sum(kalat80),sum(kalat40)),  
                         ndist= 80, distances=seq(2.5, 81.5, by=1)) #muuta binssien m??r? 

#save(model_final_data, file="datalist.RData")

#distances=seq(4.5, 79.5, by=5)
data<- model_final_data # datalista
inits<- list(list(U=11),list(U=12)) # Alkuarvot, voi koittaa ensin ilman jolloin JAGS generoi alkuarvot

var_names<- c("N", "a", "b", "a2", "b2", "mu_d1", "mu_d2", "w", "s_d1", "s_d2", "sdX", "sdX2", #"P",
              "Np", "ap", "bp", "a2p", "b2p", "mu_d1p", "mu_d2p", "wp", "s_d1p", "s_d2p", "sdXp", "sdX2p") #"Pp")

t1<-Sys.time();t1
run1 <- run.jags(ModelName,
                 monitor= var_names,data=data, inits = inits,
                 n.chains = 2, method = 'parallel', thin=200, burnin =20000,
                 modules = "mix",keep.jags.files=T,sample =1000, adapt = 100,
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1) # T?ll? n?kee ajon keston


chains<-as.mcmc.list(run1)

gelman.diag(chains)

#chains<-window(chains,start=10000)
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

summary(chains)
chains[,"N"]

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

plot(density(chainsall[,"sdX"]), xlim=c(0,0.2))
lines(density(chainsall[,"sdXp"]),col="red")

plot(density(chainsall[,"sdX2"]), xlim=c(-3,6))
lines(density(chainsall[,"sdX2p"]),col="red")




#sdx samoiksi 



#gelman.diag(run1, confidence = 0.95, transform=FALSE, autoburnin=TRUE, # multivariate=TRUE)

#luotaimen paikka muuttuu, joten cut off distancelle prioria?

#miten a ja b yhdess? heijastuu miten kalan havaitsemistn on ollut priorina ja miten on posteriorina et?isyyden muuttuessa



