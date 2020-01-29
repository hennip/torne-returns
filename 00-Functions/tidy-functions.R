boxplot.jags.df<-function(mcmc.chains, name, X){ # chain object, variable name, values to x-axis
  # note: length of x and dim variable need to match
  Q5<-c();Q25<-c();Q50<-c();Q75<-c();Q95<-c()
  n<-length(X)
  
  for(i in 1:n){
    y <- as.mcmc(mcmc.chains[,paste(sep="",name,"[",i,"]")][[1]])
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

boxplot.jags.df2<-function(mcmc.chains, name1, name2, X){ # chain object, variable name, values to x-axis
  # note: length of x and dim variable need to match
  #mcmc.chains<-chains
  #name1<-"N["
  #name2<-"3]"
  #X<-1:61
  Q5<-c();Q25<-c();Q50<-c();Q75<-c();Q95<-c()
  n<-length(X)
  
  for(i in 1:n){
  #i<-1
      y <- as.mcmc(mcmc.chains[,paste(sep="",name1,i,",",name2)][[1]])
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

