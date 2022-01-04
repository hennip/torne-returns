

ModelName<-"
model{

for(t in 1:time_steps){
    N[t]<-round(exp(U[t]))
    U[t]~dunif(0.001,11) #dnorm()
}


#####################################################################################
# Model for the true distance distribution each day
# P[d,t]: probability that a fish that crosses the site at day t crosses at distance d
# Note! In here the distance distribution means the (total) distance from the shore 0-point

# for(t in 1:time_steps){
#    for(d in 1:ndist){
      
#      P_star[t,d]=w*(1/(sqrt(2*3.141593)*s_d1))*exp(-0.5*pow(((d-mu_d1)/s_d1),2))+
#      (1-w)*(1/(sqrt(2*3.141593)*s_d2))*exp(-0.5*pow(((d-mu_d2)/s_d2),2))

#      P[t,d]=P_star[t,d]/sum(P_star[t,])
#    }
# }

#mu_d1 ~ dnorm(20,0.01)
#mu_d2 ~ dnorm(60,0.01)
#s_d1 ~dlnorm(3,1/(0.3^2))
#s_d2 ~dlnorm(3,1/(0.3^2))
#w ~dbeta(2,2)


#####################################################################################
# Obervation model for the fish observed in counters
# X[t]: Total number of fish observed by both counters
# XC[1:2,t]: Total number of fish observed by counters 1 and 2, respectively
# Q[t]: probability that a fish passing the site becomes observed by one of the counters
# QC[1:2,t]: probability that a fish that becomes observed, is observed by counter 1 and 2, respectively
# q[1:2,t]: probability that a fish becomes observed by counters 1 and 2, respectively

for(t in 1:time_steps){
Xtot[t]~dbin(Q[t],N[t])                     
X[t,1:2]~dmulti(QC[t,1:2],Xtot[t])

   Q[t]=q[t,1]+q[t,2] #-0.001 # q's won't sum up to 1!
   QC[t,1]=q[t,1]/Q[t]
   QC[t,2]=q[t,2]/Q[t]
}

# Observation model for daily observed distance frequencies in counters 1 and 2

 for(t in 1:time_steps){
    X_Long[t,1:ndist]~dmulti(nu_1[t,1:ndist],X[t,1])
    X_Short[t,1:ndist]~dmulti(nu_2[t,1:ndist],X[t,2])
 }


#####################################################################################
# Observation models for the distance distributions in both counters, 1=long beam, 2=short beam
#####################################################################
# Note! Observation probability depends on fish's distance from the device
# This is different from true distance distribution that depends on the distance from the shore (0-point)
# Could also be a function of time (water level).

# pi_1[d,t]=probability that a fish that passes the site at distance d becomes observed by counter 1
# theta_1 = probability that the counter 1 is on
# kappa_1 = maximum probability (eksperttiperusteinen todnäk.)
# gamma_1 = cut off distance

#for(t in 1:time_steps){
#  for(d in 1:ndist){
#    kappa_1[d] = 0.37*(exp(ip[d])/(1+exp(ip[d])))+0.63
#    ip[d] = a-b*distances[d]
#    pi_1[d,t]=theta_1*kappa_1[d]*step(gamma_1-distances[d])
#  }
#}
   
# theta_1=1/6
# gamma_1=82 #~dnorm()
 
#a ~ dnorm(4,1/0.4^2) #(4,1/0.001^2)
#b ~ dnorm(0.058,1000000)


######################################################################
# Could also be a function of time (water level).
# pi_2[d,t]=probability that a fish that passes the site at distance d on day t becomes observed by counter 2
# theta_2 = probability that the counter 2 is on
# kappa_2 = ratio between counters 1 and 2
# gamma_2 = cut off distance

#for(t in 1:time_steps){
#  for(d in 1:ndist){
#    pi_2[d,t]=theta_2*kappa_2[d]*step(gamma_2-distances[d])
#    kappa_2[d] = 0.37*(exp(ip2[d])/(1+exp(ip2[d])))+0.63  # kappa_2~dgamma()
#    ip2[d] = a2-b2*distances[d]
#  }
#} 
#  theta_2 = 5/6
#  gamma_2 = 42 #~dnorm()I(,gamma_1)

#a2 ~ dnorm(4,1/0.4^2) #(4,1/0.001^2)
#b2 ~ dnorm(0.058,1000000)


# Probability of observing a fish at distance d on a day t, in counters 1 and 2
  for(t in 1:time_steps){
   for(d in 1:ndist){
       PI_1[t,d]=P[t,d]*pi_1[t,d]   # Probability to cross * probablity to observe
       PI_2[t,d]=P[t,d]*pi_2[t,d]   # Probability to cross * probablity to observe
     }
   }

# Probability of observing a fish in counters 1 and 2 at any distance

  for(t in 1:time_steps){
    q[t,1]=sum(PI_1[t,])  # these plug into QC!
    q[t,2]=sum(PI_2[t,])
  }
  
# Probability that a fish that was observed in counter 1 (and 2) was at distance d

 for(t in 1:time_steps){
   for(d in 1:ndist){
     nu_1[t,d]=PI_1[t,d]/q[t,1]   
     nu_2[t,d]=PI_2[t,d]/q[t,2] 
   }
 }

# for(t in 1:time_steps){
#  for(d in 1:ndist){
#    nu_1[t,d]= 1/ndist #  PI_1[t,d]/q[t,1]   
#     nu_2[t,d]=1/ndist # PI_2[t,d]/q[t,2] 
#   }
# }

}"


data<-list(Xtot=counts_tot$tot, X=as.matrix(t(counts_tot[,2:3])),
           X_Long=as.matrix(t(tbl_80)),
           X_Short=as.matrix(t(tbl_40)),
           #distances=length(x),
           ndist= length(counts_tot$tot),
           distances=seq(2, 82.5, by=2.5),
           time_steps=nD
)

#sum(X_Short[t, d])


sum(X_Short)
sum(X[])


sum(X_Long)
  
#data<-list(Xtot=counts_tot$tot, X=as.matrix(t(counts_tot[,2:3])),
#           X_Long=matrix(1:2607, nrow = 33, ncol = 79),
#           X_Short= matrix(1:2607, nrow = 33, ncol = 79),
#           #distances=length(x),
#           ndist= length(x),
#           distances=seq(2, 82.5, by=2.5),
#           time_steps=nD
#)
#a = matrix(1:2607, nrow = 33, ncol = 79)

inits<-list(list(U=rep(10,nD)), list(U=rep(11,nD)))

var_names<- c("N", "a", "b", "a2", "b2", "mu_d1", "mu_d2", "w", "s_d1", "s_d2")

t1<-Sys.time();t1
run1 <- run.jags(ModelName,
                 monitor= var_names,data=data, inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =0,
                 modules = "mix",keep.jags.files=T,sample =200, adapt = 100,
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1)


#Jos ei, niin kannattaa tarkistaa yleisiä multinomijakauman ehtoja. Että 
#todennäköisyydet summautuvat ykköseen ja että X_Short vektorin summa on X jne.




