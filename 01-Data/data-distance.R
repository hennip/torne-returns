#source("01-Data/data-salmon.r")

# In 2019 distance from shore is available. In earlier years only distance from the 
# device.

# How we can use the distance information to evaluate circumstances
# in which a certain proportion passes the site unobserved (via the main channel)

dat19<-read_xlsx(str_c(pathIn,"Tornionjoki 2019/Kaikki kalat 2019.xlsx"),
               sheet="Kaikki kalat 2019", na="")%>%
  filter(lohi==1)%>% # Remember to delete those that go down!
  mutate(MSW=if_else(msv==T, 1,0, missing=0))%>% 
  mutate(DistShore=if_else(is.na(`Luotaimen etäisyys rantapenkasta FIN`)==T,
                           `Luotaimen etäisyys rantapenkasta SWE`,
                           `Luotaimen etäisyys rantapenkasta FIN`, missing=0))%>% 
  rename(WHeight=`vedenkorkeus pello`)%>%
  mutate(DistTot=Distance+DistShore)%>%
  mutate(L=`L(cm)`)%>%
  mutate(y=year(Date),
         mon=month(Date),
         d=day(Date),
         h=hour(Time),
         m=minute(Time),
         s=second(Time))%>%
  mutate(dttm=make_datetime(year=y, month=mon,day=d,hour=h, min=m, sec=s))%>%
  select(dttm,Side,DistTot,L,MSW, Window,WHeight, 
         Dir, DistShore, Distance)

# How to spot salmon seen from both sides?
# ==========

# Select a limit for water height above which doubles may occur
# Should we check only window 80?
dat<-dat19%>%
  filter(WHeight>78.5)%>%
  filter(Window==80)

dim(filter(dat, Side=="FIN"))
#[1] 252   18
dim(filter(dat, Side=="SWE"))
#[1] 646   18

# dttm2:een pitää määrittää myös minimiaika mikä siirtymiseen vähintään kuluu!
SE<-dat%>%filter(Side=="SWE")%>%
  mutate(dttm2=dttm-dminutes(1))%>%
  mutate(limit1=L-1, limit2=L+1) # or whatever range in size could be interpreted as the same fish

FI<-dat%>%filter(Side=="FIN")%>%
  mutate(dttm2=dttm+dminutes(1))%>%
  mutate(limit1=L-1, limit2=L+1)



t1<-Sys.time()
double<-c(); x<-c();
tdiff<-c();ldiff<-c()
for(i in 1:dim(FI)[1]){
  #i<-1
  tmp<-0
  intFI<-interval(FI$dttm[i], FI$dttm2[i])
  for(j in 1:dim(SE)[1]){
    #j<-1
    if(SE$L[j]>FI$limit1[i] & SE$L[j]<FI$limit2[i]){
      if(SE$dttm[j] %within% intFI){
        double[i]<-1
        x[i]<-j
        tdiff[i]<-difftime(SE$dttm[j],FI$dttm[i])
        ldiff[i]<-FI$L[i]-SE$L[j]
        tmp<-1
      } 
    }
  }
  if(tmp==0){double[i]<-NA; x[i]<-NA;ldiff[i]<-NA; tdiff[i]<-NA}
}
t2<-Sys.time()
t2-t1
sum(double,na.rm=T)
# 283

tmp<-FI%>%mutate(double=double, x=x, ldiff=ldiff, tdiff=tdiff)
filter(tmp, is.na(double)==F)%>%select(x,ldiff,tdiff, everything())

SE[98,]
SE[235,]
SE[415,]



t1<-Sys.time()
double<-c(); x<-c();
tdiff<-c();ldiff<-c()
for(i in 1:dim(SE)[1]){
  #i<-1
  tmp<-0
  intSE<-interval(SE$dttm2[i], SE$dttm[i])
  for(j in 1:dim(FI)[1]){
    #j<-1
    if(FI$L[j]>SE$limit1[i] & FI$L[j]<SE$limit2[i]){
      if(FI$dttm[j] %within% intSE){
        double[i]<-1
        x[i]<-j
        tdiff[i]<-difftime(FI$dttm[j],SE$dttm[i])
        ldiff[i]<-SE$L[i]-FI$L[j]
        tmp<-1
      } 
    }
  }
  if(tmp==0){double[i]<-NA; x[i]<-NA;ldiff[i]<-NA; tdiff[i]<-NA}
}
t2<-Sys.time()
t2-t1
sum(double,na.rm=T)
# 283

SE<-SE%>%mutate(double=double, x=x, ldiff=ldiff, tdiff=tdiff)
filter(SE, is.na(double)==F)%>%select(x,ldiff,tdiff, everything())

FI[70,]
FI[122,]
FI[176,]













D19<-dat19%>%
  filter(Dir=="Up")

View(D19)

datFI<-D19%>%filter(Side=="FIN")
datSE<-D19%>%filter(Side=="SWE")

# Fish's distance from the device
ggplot(D19)+
  geom_point(aes(y=Distance, x=WHeight, col=Date), alpha=0.05)+ 
  facet_grid(MSW~Side)

# Device's distance from the shore
ggplot(D19)+
  geom_point(aes(y=DistShore, x=WHeight), alpha=0.05, col=my_palette[1])+ 
  facet_wrap(~Side)

# Fish's distance from the shore
ggplot(D19)+
  geom_point(aes(y=DistTot, x=WHeight, col=Date), alpha=0.05#, col=my_palette[1]
  )+ 
  facet_grid(MSW~Side)




FI[16,]



t1<-Sys.time()
d2<-c(); timediff2<-c();ldiff2<-c()
for(i in 1:dim(FI)[1]){
  #
  i<-1
  intFI<-interval(FI$Time[i], FI$Time2[i])
  for(j in 1:dim(SE)[1]){
    #
    j<-1
    if(SE$L[j]>FI$limit1[i] & SE$L[j]<FI$limit2[i]){
      if(SE$Time[j] %within% intFI){
        d2[i]<-1
        timediff[i]<-SE$Time[j]-FI$Time[i]
        ldiff[i]<-SE$L[j]-FI$L[i]
      }  
    }
    
  }
}
t2<-Sys.time()
t2-t1
sum(d2,na.rm=T)
summary(timediff2)
summary(ldiff2)






View(tmp)

tmp
#https://rdrr.io/cran/lubridate/man/within-interval.html


# # Aggregate daily counts (FI/SE side, Grilse/MSW)
# dat<-D19%>%
#   group_by(Date, Side, MSW)%>%
#   summarize(n=n())%>%
#   # mutate(Year=year(as.POSIXct(Date)))%>%
#   # mutate(Day=day(as.POSIXct(Date)))%>%
#   # mutate(Month=month(as.POSIXct(Date)))%>%  
# mutate(Year=2019)%>%
#      mutate(ydate=yday(as.POSIXct(Date)))%>%  
#   select(Year,ydate,n, Side, MSW)
# 
# View(dat)  

# dat<-D19%>%filter(Side=="FIN", MSW==1)
# 
# ggplot(dat, aes(x=ydate))+
#   geom_histogram(bins=200)+
#  facet_grid(Side~MSW)

dat_test<-datFI%>%filter(WHeight<79)
#dat_test<-datFI
v1<-dat_test$DistShore
v2<-dat_test$WHeight
m1<-lm(v1~v2)
summary(m1)
plot(v1~v2)
abline(m1)

m2<-lm(datSE$DistShore~datSE$WHeight)
summary(m2)
plot(datSE$DistShore~datSE$WHeight)
abline(m2)


D18<-read_xlsx(path="H:/Projects/torne-returns/data/orig/Tornionjoki 2018/Kaikki kalat 2018.xlsx",
               sheet="Kaikki kalat 2018", na="")%>%
  mutate(year=2018)%>%
  filter(lohi==1)%>%
  mutate(MSW=if_else(msv==T, 1,0, missing=0))%>% 
  rename(WHeight=`vedenkorkeus pello`)%>%
  mutate(DistShore=if_else(Side=="FIN",
                           summary(m1)$coefficients[1]+WHeight*summary(m1)$coefficients[2], 
                           summary(m2)$coefficients[1]+WHeight*summary(m2)$coefficients[2], 
                           missing=0))%>%
  select(Dir, Distance, Date,year, Side, Window, Hour, WHeight, MSW, DistShore)



dat<-full_join(D18, D19)

ggplot(dat)+
  geom_point(aes(y=DistShore, x=WHeight, col=year), alpha=0.05)+ 
  facet_wrap(~Side)





ggplot(dat)+
  geom_point(aes(y=DistTot, x=WHeight), alpha=0.05, col=my_palette[1])+ 
  facet_grid(MSW~Side)



ggplot(datFI)+
  geom_point(aes(y=DistTotFI, x=WHeight))

ggplot(datSE)+
  geom_point(aes(y=DistTotSE, x=WHeight))+ 
  facet_wrap(~MSW)

tmp<-D19%>%filter(MSW==1)


