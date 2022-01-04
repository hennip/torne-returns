#Oho, tulikin tämmöinen. Koita kestää
#Samu


# Hajanaisia aatoksia:
# - distance d täytyy olla kalan etäisyys rannasta (esim. P[d,t])
# - havaitsemistn per etäisyys riippuu kuitenkin luotaimen etäisyydestä rannasta, dd
# - t mieluiten päivä, ellei ehdoton syy ottaa lyhyempi aika-askel
# - havaintomallin (kalojen lkm per päivä) dmulti oltava 4-ulotteinen, side=FI/SE ja window=40/80 
# - veden  korkeus vaikuttaa
#
#
#
#



model{
  
  ######################################################################################
  # Model for the migration dynamics
  # N[t]: true number of fish crossing the site at day t
  
  for(t in 1:time_steps){
    N[t]~dround(exp(U[t]),0)        # Independent priors as an example. Could be similar to Utsjoki smolt model
    U[t]~dnorm()
  }
  
  
  
  #####################################################################################
  # Model for the true distance distribution each day
  # P[d,t]: probability that a fish that crosses the site at day t crosses at distance d
  
  for(t in 1:time_steps){
    for(d in 1:distances){
      
      # P[d,t] needs to have some parametric function. Using the shape of a normal distribution here 
      # as an example
      
      #P_star[d,t]=exp(pow((d-mu_d[t])/s_d[t]),2)
      P_star[d,t]=exp(pow((d-mu_d1[t])/s_d1[t]),2)+w*exp(pow((d-mu_d2[t])/s_d2[t]),2)
      P[d,t]=P_star[d,t]/sum(P_star[,t])
      
    }
    
    # parameters of the function depend on water level, assuming linearity for the sake of an example
    
   # mu_d[t]=a+b*WL[t]
  }
  
  # priors 
  
#  a~dnorm()
#  b~dnorm()
  
  
  
  
  
  #####################################################################################
  # Obervation model for the daily number of fish observed in counters
  # X[t]: Total number of fish observed by both counters
  # XC[1:2,t]: Total number of fish observed by counters 1 and 2, respectively
  # Q[t]: probability that a fish passing the site becomes observed by one of the counters
  # QC[1:2,t]: probability that a fish that becomes observed, is observed by counter 1 and 2, respectively
  # q[1:2,t]: probability that a fish becomes observed by counters 1 and 2, respectively
  
  for( t in 1:time_steps){
    X[t]~dbin(Q[t],N[t])                     
    XC[1:2,t]~dmulti(QC[1:2,t],X[t])
    
    Q[t]=q[1,t]+q[2,t]
    QC[1,t]=q[1,t]/Q[t]
    QC[2,t]=q[2,t]/Q[t]
  }
  
  #####################################################################################
  # Observation models for the daily distance distributions in both counters
  # Counter = 1 ;long beam
  # Counter = 2 ;short beam
  
  # Observation probability as a function of distance for counter=1. Could also be a function of time (water level).
  # Using a simple cut off model as an example
  # pi_1[d,t]=probability that a fish that passes the site at distance d on day t becomes observed by counter 1
  # theta_1 = probability that the counter 1 is on
  # kappa_1 = maximum probability
  # gamma_1 = cut off distance
  
  
  for(d in 1:distances){
    for(t in 1:time_steps){
      pi_1[d,t]=theta_1*kappa_1*step(gamma_1-d)
    }
  }
  
  kappa_1~dbeta()  # Needs to be informative, might start with = 1
  theta_1=1/6 # assuming 10 minutes every hour
  gamma_1~dnorm()
  
  
  # Observation probability as a function of distance for counter=2. Could also be a function of time (water level).
  # Using a simple cut off model as an example, relating to the counter 1 values ( could also be independent models?)
  # pi_2[d,t]=probability that a fish that passes the site at distance d on day t becomes observed by counter 2
  # theta_2 = probability that the counter 2 is on
  # kappa_2 = ratio between counters 1 and 2 
  # gamma_2 = cut off distance
  
  
  for(d in 1:distances){
    for(t in 1:time_steps){
      pi_2[d,t]=theta_2*kappa_2*pi_1[d,t]*step(gamma_2-d)
    }
  }
  
  kappa_2~dgamma()
  theta_2=5/6 # assuming 50 minutes every hour
  gamma_2~dnorm()I(,gamma_1)  #   Short beam, cut off must be lower than for the long beam
  
  # Probability of observing a fish at distance d on day t, in counters 1 and 2
  
  for(d in 1:distances){
    for(t in 1:time_steps){
      PI_1[d,t]=P[d,t]*pi_1[d,t]   # Probability to cross * probablity to observe
      PI_2[d,t]=P[d,t]*pi_2[d,t]   # Probability to cross * probablity to observe
    }
  }
  
  # Probability of observing a fish in counters 1 and 2 at any distance
  
  for(t in 1:time_steps){
    q[1,t]=sum(PI_1[,t])  # these plug into QC!
    q[2,t]=sum(PI_2[,t])
  }
  
  # Probability that a fish that was observed in counter 1 (and 2) was at distance d
  
  for(d in 1:distances){
    for(t in 1:time_steps){
      nu_1[d,t]=P[d,t]/q[1,t]   
      nu_2[d,t]=P[d,t]/q[2,t] 
    }
  }
  
  # Observation model for daily observed distance frequencies in counters 1 and 2
  for(t in 1:time_steps){
    X_Long[t,1:distances]~dmulti(nu_1[1:distances,t],XC[1,t])
    X_Short[t,1:distances]~dmulti(nu_2[1:distances,t],XC[2,t])
  }
  
  
  
}
