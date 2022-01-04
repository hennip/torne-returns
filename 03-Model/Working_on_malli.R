
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
#pathIn<-"C:/Users/candi/OneDrive/Tiedostot/1 GRADU/"

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


paiva1 <- c(1,4,2,7,5,1,4,2,7,5,3,1,8,4,6,11,14,32,27,25,21,14,12,12,5,1,4,2,7,5,3,1,8,4,6,1,4,2,7,5) #luotaimen luokka 2
paiva2 <- c(4,6,3,5,4,1,4,2,7,5,3,1,8,14,16,21,24,22,17,15,31,34,22,24,25,21,14,12,7,5,3,1,8,4,6,1,4,2,7,5) #luotaimen luokka 4
paiva3 <- c(3,1,8,4,6,1,4,2,7,5,3,1,8,4,6,1,4,2,7,5,1,4,2,7,5,1,4,2,7,5,3,1,8,4,6,1,4,2,7,5) #luotaimen luokka 1

kalat40 <- rbind(paiva1, paiva2, paiva3)


paiva180 <- c(1,4,2,7,5,1,4,2,7,5,3,1,8,4,6,11,14,32,27,25,21,14,12,12,5,1,4,2,7,5,3,1,8,4,6,1,4,2,7,5,
              1,4,2,7,5,1,4,2,7,5,3,1,8,4,6,11,14,32,27,25,21,14,12,12,5,1,4,2,7,5,3,1,8,4,6,1,4,2,7,5) #luotaimen luokka 2
paiva280 <- c(4,6,3,5,4,1,4,2,7,5,3,1,8,14,16,21,24,22,17,15,31,34,22,24,25,21,14,12,7,5,3,1,8,4,6,1,4,2,7,5,
              1,4,2,7,5,1,4,2,7,5,3,1,8,4,6,11,14,32,27,25,21,14,12,12,5,1,4,2,7,5,3,1,8,4,6,1,4,2,7,5) #luotaimen luokka 4
paiva380 <- c(3,1,8,4,6,1,4,2,7,5,3,1,8,4,6,1,4,2,7,5,11,4,2,7,5,1,4,2,7,5,3,1,8,4,6,1,4,2,7,5,
              1,4,2,7,5,1,4,2,7,5,3,1,8,4,6,11,14,32,27,25,21,14,12,12,5,1,4,2,7,5,3,1,8,4,6,1,4,2,7,5) #luotaimen luokka 1


kalat80 <- rbind(paiva180, paiva280, paiva380)

# todelliset vektorit
#kalat40 <- c(0, 0, 2, 5, 5, 18, 45,  104,  254,  193,  227,  521,  458,  353,  751, 1029,  765,  943,  821,  745, 591,  366,
#  701, 656, 384, 416,  548,  680,  554,  715,  501,  778,  512,  477,  359,  220,  196,  118,  207,  161,  298,  303,
#  219, 144, 133, 219,  250,  246,  247,  171,  168,  124,  117,  130,  168,  102,  112,   30,   39,   29,   34,   10,
#  11, 5, 8, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

#kalat80 <- c(0, 0, 0, 0, 0, 1,  10,  20,  42,  20,  26,  91,  71,  72, 139, 164, 145, 143, 150, 135,  73,  78, 131,  77,  53,
#39,  83, 75,  86,  83,  78,  93,  78,  68,  54,  22,  27,  25,  27,  27,  46,  45,  18,  16,  27,  35,  45,  37,  34,
#32,  23,  21,  31,  27,  28,  39,  43,  32,  26,  28,  13,  16,  23,  10,  15,  12,  15,   8,   5,  4,   2, 0, 0, 0, 0, 0, 0, 0, 0, 0)

rowSums(kalat80)

ModelName<-"
model{

######################################################################################

for(t in 1:time_steps){
    N[t]~dunif(1,60000)
}

#####################################################################################
# Obervation model for the fish observed in counters
# X: Total number of fish observed by both counters
# XC[1:2]: Total number of fish observed by counters 1 and 2, respectively
# Q: probability that a fish passing the site becomes observed by one of the counters
# QC[1:2]: probability that a fish that becomes observed, is observed by counter 1 and 2, respectively
# q[1:2]: probability that a fish becomes observed by counters 1 and 2, respectively

for(t in 1:time_steps){
Xtot[t]~dbin(Q[t],round(N[t]))                     
X[t,1:2]~dmulti(QC[t,1:2],Xtot[t])

   Q[t]=q[t,1]+q[t,2]#-0.001 # q's won't sum up to 1!
   QC[t,1]=q[t,1]/Q[t]
   QC[t,2]=q[t,2]/Q[t]
}

# Observation model for daily observed distance frequencies in counters 1 and 2

 for(t in 1:time_steps){
    X_Long[t,1:80]~dmulti(nu_1[t,1:80],X[t,1])
    X_Short[t,1:40]~dmulti(nu_2[t,1:40],X[t,2])
 }


#####################################################################################

# Model for the true distance distribution
# P[d]: probability that a fish that crosses the site crosses at distance d

for(t in 1:time_steps){
  for(D in 1:ndist){

    P_star[t,D]=w[t]*(1/(sqrt(2*3.141593)*s_d1[t]))*exp(-0.5*pow(((D-mu_d1[t])/s_d1[t]),2))+
    (1-w[t])*(1/(sqrt(2*3.141593)*s_d2[t]))*exp(-0.5*pow(((D-mu_d2[t])/s_d2[t]),2))

    P[t,D]=P_star[t,D]/sum(P_star[t,])

}
}

# priors
for(t in 1:time_steps){
mu_d1[t] ~ dnorm(20,0.01)
mu_d2[t] ~ dnorm(60,0.01)
s_d1[t] ~dlnorm(log(20)-0.5*log(0.3*0.3+1),1/log(0.3*0.3+1))
s_d2[t] ~dlnorm(log(20)-0.5*log(0.3*0.3+1),1/log(0.3*0.3+1))
w[t] ~dbeta(0.5*4,0.5*4)
}

#####################################################################################
# Observation probability as a function of distance for counter=1.
#####################################################################

# pi_1[d]=probability that a fish that passes the site at distance d becomes observed by counter 1
# theta_1 = probability that the counter 1 is on
# kappa_1 = eksperttiperusteinen todnäk.
# gamma_1 = cut off distance

for(t in 1:time_steps){
      for(d1 in 1:ndist1){
    
        pi_1[t,d1]=theta_1*kappa_1[t,d1]#*step(gamma_1-distances[d1])     
      
        kappa_1[t,d1] = (0.37*(exp(ip1[t,d1])/(1+exp(ip1[t,d1])))/1.5)+0.63
        
        ip1[t,d1] = a1[t]-b1[t]*distto1[t,d1]
      
        distto1[t,d1] <- distances[d1]+luotaimen_luokka[t]
      }
}
    theta_1 = 1/6
   for(t in 1:time_steps){
    a1[t] ~ dnorm(4,1/0.4^2)
    b1[t] ~ dnorm(0.058,10000)
   }



# Observation probability as a function of distance for counter=2.
######################################################################

# pi_2[d]=probability that a fish that passes the site at distance d becomes observed by counter 2
# theta_2 = probability that the counter 2 is on
# kappa_2 = ratio between counters 1 and 2
# gamma_2 = cut off distance

for(t in 1:time_steps){
      for(d2 in 1:ndist2){
    
        pi_2[t,d2]=theta_2*kappa_2[t,d2]#*step(gamma_2-distances[d2])     
      
        kappa_2[t,d2] = 0.37*(exp(ip2[t,d2])/(1+exp(ip2[t,d2])))+0.63 #eksperttiperusteinen havaitsemistn
        
        ip2[t,d2] = a2[t]-b2[t]*(distances[d2]+luotaimen_luokka[t])
      
        distto[t,d2] <- distances[d2]+luotaimen_luokka[t]
      }
}
    
   for(t in 1:time_steps){
    a2[t] ~ dnorm(4,1/0.4^2)
    b2[t] ~ dnorm(0.058,1000000)
   }
   
    theta_2 = 5/6
    gamma_2=42 #~dnorm()I(,gamma_1)
    
  

# Probability of observing a fish at distance d, in counters 1 and 2

  for(t in 1:time_steps){
   for(d1 in 1:ndist1){
       PI_1[t,d1]=P[t,luotaimen_luokka[t]+d1]*pi_1[t,d1]   # Probability to cross * probablity to observe
   }
    for(d2 in 1:ndist2){
       PI_2[t,d2]=P[t,luotaimen_luokka[t]+d2]*pi_2[t,d2]   # Probability to cross * probablity to observe
     }
   }

# Probability of observing a fish in counters 1 and 2 at any distance

  for(t in 1:time_steps){
    q[t,1]=sum(PI_1[t,])
    q[t,2]=sum(PI_2[t,])
  }

# Probability that a fish that was observed in counter 1 (and 2) was at distance d

 for(t in 1:time_steps){
   for(d1 in 1:ndist1){
     nu_1[t,d1]=0.5/q[t,1]#((PI_1[t,d1])/q[t,1])
     
     moska1[t,d1]=0.5/q[t,1]#(PI_1[t,d1])/q[t,1]+0.001
   }
     for(d2 in 1:ndist2){
     nu_2[t,d2]=0.5/q[t,2]#((PI_2[t,d2])/q[t,2] )
     
     moska2[t,d2]=0.5/q[t,2]#(PI_2[t,d2])/q[t,2]+0.001
   }
 }
 
}"



data <- list(X_Long=kalat80, X_Short=kalat40, Xtot=rowSums(kalat40)+rowSums(kalat80), X=cbind(rowSums(kalat40), rowSums(kalat80)),  
                         ndist= 120, ndist2 = 40, ndist1=80, distances=seq(1, 80, by=1), time_steps=3, luotaimen_luokka = c(2,15,39)) 

inits<- list(list(U=11),list(U=12))

var_names<- c("N", "a1", "b1", "a2", "b2", "mu_d1", "mu_d2", "w", "s_d1", "s_d2")#,
              #"Np", "a1p", "b1p", "a2p", "b2p", "mu_d1p", "mu_d2p", "wp", "s_d1p", "s_d2p")

var_names<-c("moska1","moska2")#"PI_1", 

t1<-Sys.time();t1
run1 <- run.jags(ModelName,
                 monitor= var_names,data=data, inits = inits,
                 n.chains = 2, method = 'parallel', thin=1, burnin =0,
                 modules = "mix",keep.jags.files=T,sample =200, adapt = 100,
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1)

#save(run1, file="ajo.RData")

#summary(run1)











