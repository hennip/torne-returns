
# simuloidaan dataa jonka avulla voidaan sovittaa priorit 
# virtaaman vaikutukselle havaitsemistodennakoisyyteen

# Panun expert-info 16.6.17:
# Mahdolliset arvot välillä [0.9,0.3], 
# matalilla virtaamilla (<20m3/s) korkea tn tulla nähdyksi(0.75-0.9), 
# tästä verkkaisesti laskee (ei romahda) minimiin, jossa kuitenkin 
# suuri epävarmuus (0.3-0.6), jakauma ennemmin tasainen kuin huipukas.
# Virtaamilla 50-60 m3/s olosuhteet havaita ovat selkeästi huonot.
# Kun virtaama nousee 10m3/s->60m3/s, veden korkeus nousee metrin.

# Konstan expert-info:
# Kun vesi on korkealla (>78.5m), kalat kulkevat lähellä rantaa missä vastus
# on pienin -> Korkealla vedellä havaitsemistn on suurimmillaan
#  Kun vedenkorkeus on 79 m ei keskiuomaan asti nähdä ollenkaan, toisaalta silloin
# siellä ei myöskään kulje kaloja
# 86m ruotsin parvekkeelta keskiuoman reunaan
# 82m suomen rantapisteestä keskiuoman reunaan
# 78m vedenkorkeus ei enää jarruta kalojen kulkua uoman keskellä
# 77.5m vedenkorkeuden jälkeen vedenkorkeuden lasku ei enää vaikuta
# (ts. tällöin havaitsemistodennäköisyyden minimitaso on saavutettu)

# Tarvitaan ekspert-näkemys sille minkä tasoinen havaitsemistn voi olla
# - huonoimmillaan
# -parhaimmillaan
# Nämä realistinen epävarmuuden haarukka huomioiden.

# Havaitsemismalli tehtävä erikseen suomen ja ruotsin puolisille luotaimille, 
# sillä ranta syvenee eri tavalla? 

# Logit-käyrän sovittaminen ei ehkä tähän sovi kovin hyvin, 
# kokeillaan ennemmin paloittaista mallia 

# 1: Maksimihavaittavuus kun veden korkeus on >=78.5m (?)
# 2: Minimihavaittavuus kun veden korkeus on <77.5m (?)
# 3: Vaihtuva havaittavuus ym välillä, tn nousee lineaarisesti 
# vedenkorkeuden noustessa. Epävarmuuden pitänee olla matalalla vedenkorkeudella
# suurempaa, kuin korkealla vedellä. 


# Sämplätään kahdesta rajapisteestä kuvitellun epävarmuuden mukaisesti
# sovitetaan välille logit-lineaarinen malli
x<-c(rbeta(100,10,100),rbeta(100,100,10))
y<-c(rep(77.5,100),rep(78.5,100))
x2<-log(x/(1-x))
M2<-"
model{

for(i in 1:100){
  x2[i]~dnorm(mu[i],tau)
  mu[i]<-alpha+beta*y[i]
 
}
alpha~dunif(0,1)
beta~dunif(0,1)
tau<-1/(sd*sd)
sd~dlnorm(-0.23,1)
sdX~dlnorm(-0.23,1)

}"

cat(M2,file="prior-obs.txt")

data<-list(x2=x2, y=y)
#inits<-list(alpha=, beta=)

system.time(jm<-jags.model('prior-obs.txt',#inits=inits,
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "alpha","beta", "sd", "sdX"
                                  ),
                                  n.iter=5000,
                                  thin=1))

chainsM<-chains1
summary(chainsM)

# 
#       Mean        SD  Naive SE Time-series SE
# alpha 0.0246881 0.0245099 2.451e-04      5.624e-04
# beta  0.0003271 0.0003293 3.293e-06      6.629e-06
# sd    2.4443548 0.1741454 1.741e-03      2.385e-03

mu<-0.025
sd<-0.0245
tau<-1/(sd*sd);tau

mu<-0.0003271
sd<-0.0003293
tau<-1/(sd*sd);tau

mu<-2.4
sd<-0.17
cv<-sd/mu
tau<-1/log(cv*cv+1)
M<-log(mu)-0.5/tau
tau;M


wh<-seq(77,80, by=0.1)
nwh<-length(wh)

whmin<-77.5
whmax<-78.5
# x<-c(77,78,79)
# #whmin-x
# ifelse((whmin-x)>0,1,0)
# (1-(ifelse((whmin-x)>0,1,0)))*(1-(ifelse((x-whmax)>0,1,0)))
# #x-whmax
# ifelse((x-whmax)>0,1,0)

M2<-"
model{

pmin~dbeta(10,100)
pmax~dbeta(100,10)
a~dnorm(0.025,10000)
b~dnorm(0.003271,10000)



for(i in 1:n){

logit(pobs[i])<-logit_pobs[i]
logit_pobs[i]~dnorm(mu[i],tau)
  mu[i]<-  step(whmin-wh[i])*pmin +# min
        (1-step(whmin-wh[i]))*(1-step(wh[i]-whmax))*(a+b*wh[i])+ # mid
        step(wh[i]-whmax)*pmax #max
}
tau<-1/(sd*sd)
sd<-0.001#~dlnorm(0.87,200)

}"

cat(M2,file="prior-obs.txt")


whmin<-77.5
whmax<-78.5
wh<-seq(77,79, by=0.05)
nwh<-length(wh)
data<-list(wh=wh, n=nwh, whmin=whmin, whmax=whmax)
system.time(jm<-jags.model('prior-obs.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "a","b", #"sd",
                                    "pobs"
                                  ),
                                  n.iter=5000,
                                  thin=1))

chainsM<-chains1
summary(chainsM)














#wh<-seq(-100,100, by=1)
#wh<-seq(0,1, by=0.01)
wh<-seq(77,80, by=0.1)
nwh<-length(wh)

a<-0.00001
b<-0.05
mu<-c();P<-c();p<-c();sd<-c()
sd<-0.001
for(i in 1:nwh){
  #mu[i]<-a+b*(wh[i]*200-100) # wh<-seq(0,1, by=0.01)
  mu[i]<-a+b*(wh[i]*66.7-5233)
  P[i]<-rnorm(1,mu[i],sd)
  p[i]<-0.6*(exp(P[i])/(1+exp(P[i]))) +0.4
  #p[i]<-0.6*(exp(P[i])/(1+exp(P[i]))) +0.3
}

df<-as.tibble(cbind(wh,p))

ggplot(df) + 
  geom_point(aes(wh, p))+
  coord_cartesian(ylim=c(0,1))+
  labs(title=paste(sep="","a=",a," b=",b))


# Sovitetaan P:t edellisestä ja estimoidaan aB, bB ja sdB

wh<-seq(77,80, by=0.1)
nwh<-length(wh)

M2<-"
model{
for(i in 1:n){
#p[i]<-0.6*p2[i]+0.3
#logit(p2[i])<-P[i]
P[i]~dnorm(muB[i],tauB)
muB[i]<-aB+bB*(wh[i]*66.7-5233)
}
tauB<-1/pow(sdB,2)

sdB<-0.7
#aB~dnorm(5,1)
#bB~dlnorm(log(1)-0.5/tau_bB,tau_bB)
#cv_bB<-0.3
#tau_bB<-1/log(cv_bB*cv_bB+1)

aB~dnorm(1,0.01)
bB~dlnorm(0.1,1)
#sdB~dlnorm(1,0.1)
#sdB~dlnorm(log(0.7)-0.5/tau_sdB,tau_sdB)
#tau_sdB<-1
}"

cat(M2,file="prior-obs.txt")

data<-list(wh=wh,P=P, n=nwh)

system.time(jm<-jags.model('prior-obs.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "aB","bB", "sdB"
                                  ),
                                  n.iter=5000,
                                  thin=1))

chainsM<-chains1
summary(chainsM)



# Sitten katsotaan millaista matskua saadut priorit tuottaisivat

# odotusarvot ja hajonnat edellisesta ajosta
muaB<-summary(chainsM[,"aB"])$statistics[1]
sdaB<-summary(chainsM[,"aB"])$statistics[2]
tauaB<-1/(sdaB*sdaB)
muaB;tauaB

mubB<-summary(chainsM[,"bB"])$statistics[1]
sdbB<-summary(chainsM[,"bB"])$statistics[2]
cvbB<-sdbB/mubB
taubB<-1/log(cvbB*cvbB+1)
MbB<-log(mubB)-0.5/taubB
MbB;taubB

musdB<-summary(chainsM[,"sdB"])$statistics[1]
sdsdB<-summary(chainsM[,"sdB"])$statistics[2]
cvsdB<-sdsdB/musdB
tausdB<-1/log(cvsdB*cvsdB+1)
MsdB<-log(musdB)-0.5/tausdB
MsdB;tausdB

wh<-seq(77,80, by=0.1)
nwh<-length(wh)

M2<-"
model{
for(i in 1:n){
p[i]<-0.6*p2[i]+0.4
logit(p2[i])<-P[i]
P[i]~dnorm(muB[i],tauB)
muB[i]<-aB+bB*(wh[i]*66.7-5233)
}
tauB<-1/pow(sdB,2)

aB~dnorm(mu.aB,t.aB)
bB~dlnorm(M.bB,T.bB)
sdB~dlnorm(M.sdB,T.sdB)

# aB~dnorm(2.9,60)
# bB~dlnorm(-2.6,984)
# sdB~dlnorm(-0.23,210)

}"

cat(M2,file="prior-obs.txt")

data<-list( 
   mu.aB=muaB,t.aB=tauaB,
   M.bB=MbB, T.bB=taubB,
   M.sdB=MsdB, T.sdB=tausdB,
  wh=wh, n=nwh
)

system.time(jm<-jags.model('prior-obs.txt',
                           n.adapt=100,data=data,n.chains=2))


system.time(chains1<-coda.samples(jm,
                                  variable.names=c(
                                    "p"
                                  ),
                                  n.iter=5000,
                                  thin=1))


df<-boxplot.jags.df(chains1,"p",wh)
df<-as.tibble(df)
df<-filter(df, x>0)

ggplot(df, aes(x, group=x))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  coord_cartesian(ylim=c(0,1))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  geom_vline(xintercept = c(77.49,78.51), color="red")

wfilter(df, x==10 | x==20 |x==50 |x==60)
