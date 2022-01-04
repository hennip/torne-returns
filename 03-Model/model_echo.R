

M1<-"
model{
  
   ######################################################################################
   # Model for the migration dynamics
   # N[t]: true number of fish crossing the site at day t
   # 
    for(t in 1:time_steps){
   #   N[t]~dround(exp(U[t]),0)        # Independent priors as an example. Could be similar to Utsjoki smolt model
      N[t]<-round(exp(U[t]))
      U[t]~dunif(0.001,11)#dnorm()
    }
    
   
   
  # #####################################################################################
  # # Model for the true distance distribution each day
  # # P[d,t]: probability that a fish that crosses the site at day t crosses at distance d
  # Note! In here the distance distribution means the (total) distance from the shore 0-point
  
  # x<-seq(0,82, length.out=31 ) as a grid for (total) distance (from 0-point to the edge of the main stem)
  # Define l[t] in 1:9 for each day t stating the level of water grop
  # mu_d[l[t]] would define 
  # WH>78
  # 77.5<WH<78
  # 77.3<WH<77.5
  # 77<WH<77.3
  #At low water levels the prob dist is bi-modal
  
  # for(t in 1:time_steps){
  #   for(d in 1:distances){
  #     
  #     # P[d,t] needs to have some parametric function. Using the shape of a normal distribution here 
  #     # as an example
  #     
  #     P_star[d,t]=exp(pow((d-mu_d[t])/s_d[t]),2) 
  #     P[d,t]=P_star[d,t]/sum(P_star[,t])
  #     
  #   }
  #   
  #   # parameters of the function depend on water level, assuming linearity for the sake of an example
  #   
  #   mu_d[t]=a+b*WL[t]
  # }
  # 
  # # priors 
  # 
  # a~dnorm()
  # b~dnorm()
  # 
  # 
  # 
  
  
  #####################################################################################
  # Obervation model for the daily number of fish observed in counters
  # X[t]: Total number of fish observed by both counters
  # XC[1:2,t]: Total number of fish observed by counters 1 and 2, respectively
  # Q[t]: probability that a fish passing the site becomes observed by one of the counters
  # QC[1:2,t]: probability that a fish that becomes observed, is observed by counter 1 and 2, respectively
  # q[1:2,t]: probability that a fish becomes observed by counters 1 and 2, respectively
  
  for( t in 1:time_steps){
   Xtot[t]~dbin(Q[t],N[t])           
    
   X[1:2,t]~dmulti(QC[1:2,t],Xtot[t])

    # lb FI G, lb FI MSW, sb FI G, sb FI MSW, 
    # lb SE G, lb SE MSW, sb SE G, sb SE MSW 
    # for(i in 1:8){  
    #   X[i,t]
    # }
    
    
    Q[t]=q[1,t]+q[2,t]-0.001 # q's won't sum up to 1!
    QC[1,t]=q[1,t]/Q[t]
    QC[2,t]=q[2,t]/Q[t]
  }
  
  #####################################################################################
  # # Observation models for the daily distance distributions in both counters
  # # Counter = 1 ;long beam
  # # Counter = 2 ;short beam
  
  # Note! Observation probability depends on fish's distance from the device
  # This is different from true distance distribution that depends on the distance from the shore (0-point)
  # 
  # # Observation probability as a function of distance for counter=1. 
  # # Could also be a function of time (water level).
  # # Using a simple cut off model as an example
  # # pi_1[d,t]=probability that a fish that passes the site at distance d on day t becomes observed 
  # # by counter 1
  # # theta_1 = probability that the counter 1 is on
  # # kappa_1 = maximum probability
  # # gamma_1 = cut off distance
  # 
  # 
  # for(d in 1:distances){
  #   for(t in 1:time_steps){
  #     pi_1[d,t]=theta_1*kappa_1*step(gamma_1-d)
  #   }
  # }
  # 
  # kappa_1~dbeta()  # Needs to be informative, might start with = 1
  # theta_1=1/6 # assuming 10 minutes every hour
  # gamma_1~dnorm()
  # 
  # 
  # # Observation probability as a function of distance for counter=2. Could also be a function of time (water level).
  # # Using a simple cut off model as an example, relating to the counter 1 values ( could also be independent models?)
  # # pi_2[d,t]=probability that a fish that passes the site at distance d on day t becomes observed by counter 2
  # # theta_2 = probability that the counter 2 is on
  # # kappa_2 = ratio between counters 1 and 2 
  # # gamma_2 = cut off distance
  # 
  # 
  # for(d in 1:distances){
  #   for(t in 1:time_steps){
  #     pi_2[d,t]=theta_2*kappa_2*pi_1[d,t]*step(gamma_2-d)
  #   }
  # }
  # 
  # kappa_2~dgamma()
  # theta_2=5/6 # assuming 50 minutes every hour
  # gamma_2~dnorm()I(,gamma_1)  #   Short beam, cut off must be lower than for the long beam
  # 
  # # Probability of observing a fish at distance d on day t, in counters 1 and 2
  # 
  # for(d in 1:distances){
  #   for(t in 1:time_steps){
  #     PI_1[d,t]=P[d,t]*pi_1[d,t]   # Probability to cross * probablity to observe
  #     PI_2[d,t]=P[d,t]*pi_2[d,t]   # Probability to cross * probablity to observe
  #   }
  # }
  # 
  
  # Probability of observing a fish in counters 1 and 2 at any distance
  
  for(t in 1:time_steps){
    q[1,t]~dbeta(2,2)
    q[2,t]~dbeta(2,2)
    #q[1,t]=sum(PI_1[,t])  # these plug into QC!
    #q[2,t]=sum(PI_2[,t])
  }
  
  # # Probability that a fish that was observed in counter 1 (and 2) was at distance d
  # 
  # for(d in 1:distances){
  #   for(t in 1:time_steps){
  #     nu_1[d,t]=P[d,t]/q[1,t]   
  #     nu_2[d,t]=P[d,t]/q[2,t] 
  #   }
  # }
  # 
  # # Observation model for daily observed distance frequencies in counters 1 and 2
  # for(t in 1:time_steps){
  #   X_Long[t,1:distances]~dmulti(nu_1[1:distances,t],X[1,t])
  #   X_Short[t,1:distances]~dmulti(nu_2[1:distances,t],X[2,t])
  # }
  # 
  # 
  
}"

modelName<-"echo"


Mname<-str_c("03-Model/",modelName, ".txt")
cat(M1,file=Mname)

# Select data
# =================================
#dat<-dat_msw
#dataName<-"MSW"

# Years to include
#years<-c(2002:2014) # 13 years 

# Number of days to include
#n_days<-61 # june & july

# Computer name
#compName<-"turd010"#"ould017"

#df<-smolts_data_to_jags(dat, years, n_days)

data<-list(
  Xtot=df_tot$tot,
  X=as.matrix(t(df_tot[,2:3])),
  #X_Long=tbl_80,
  #X_Short=tbl_40,
  #distances=length(x),
  time_steps=nD
)


inits<-list(list(U=rep(10,nD)#,zN=array(1, dim=c(61,data$nYears))
                 ),
            list(U=rep(10.1,nD)))

var_names<-c(
#"N", 
  "q"
)


#nb of samples = samples * thin, burnin doesn't take into account thin
# sample on tässä lopullinen sample, toisin kuin rjagsissa!!!

run0<-run.jags(M1, 
         monitor= var_names,data=data,inits = inits,
         n.chains = 2, method = 'parallel', thin=1, burnin =0, 
         modules = "mix",keep.jags.files=F,sample =1000, adapt = 100, 
         progress.bar=TRUE)
    
summary(run0)     

# t1<-Sys.time();t1
# run1 <- run.jags(M1, 
#                  monitor= var_names,data=data,inits = inits,
#                  n.chains = 2, method = 'parallel', thin=300, burnin =0, 
#                  modules = "mix",keep.jags.files=T,sample =1000, adapt = 100, 
#                  progress.bar=TRUE)
# t2<-Sys.time()
# difftime(t2,t1)
# # 7d
# run<-run1
# save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))
# 
# t1<-Sys.time();t1
# run2 <- extend.jags(run1, combine=T, sample=4000, thin=300, keep.jags.files=T)
# t2<-Sys.time()
# difftime(t2,t1)
# #3.3d?
# run<-run2
# save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))
# 
# t1<-Sys.time();t1
# run3 <- extend.jags(run2, combine=T, sample=4000, thin=300, keep.jags.files=T)
# t2<-Sys.time()
# difftime(t2,t1)
# run<-run3
# save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))
# 
# t1<-Sys.time();t1
# run4 <- extend.jags(run3, combine=T, sample=4000, thin=300, keep.jags.files=T)
# t2<-Sys.time()
# difftime(t2,t1)
# run<-run4
# save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))
# 
# t1<-Sys.time();t1
# run5 <- extend.jags(run4, combine=T, sample=4000, thin=300, keep.jags.files=T)
# t2<-Sys.time()
# difftime(t2,t1)
# run<-run5
# save(run, file=str_c(pathOut,modelName,"_",dataName,"_run_",compName,".RData"))
