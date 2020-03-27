#source("01-Data/data-salmon.r")

# In 2019 distance from shore is available. In earlier years only distance 
# from the device.

# How we can use the distance information to evaluate circumstances
# in which a certain proportion passes the site unobserved (via the main channel)

dat19<-read_xlsx(str_c(pathIn,"Tornionjoki 2019/Kaikki kalat 2019.xlsx"),
               sheet="Kaikki kalat 2019", na="")%>%
  filter(lohi==1)%>%
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
         Dir, DistShore, Distance, Date)

# How to remove salmon seen from both sides?
# =========================================

# Select a limit for water height above which doubles may occur
# Should we check only window 80?
dat_w80_low<-dat19%>%
  filter(Window==80)%>%
#  filter(WHeight<78.5)%>%
  filter(Dir=="Up")

dim(filter(dat_w80_low, Side=="FIN"))
#[1] 2578 11
dim(filter(dat_w80_low, Side=="SWE"))
#[1] 3332 11

lx<-10 # allowed diff in length

SE<-dat_w80_low%>%filter(Side=="SWE")%>%
  mutate(dttm2=dttm-dminutes(1))%>%
  mutate(limit1=L-lx, limit2=L+lx) # or whatever range in size could be interpreted as the same fish

FI<-dat_w80_low%>%filter(Side=="FIN")%>%
  mutate(dttm2=dttm+dminutes(1))%>%
  mutate(limit1=L-lx, limit2=L+lx)



t1<-Sys.time()
dbl<-c(); x<-c();tdiff<-c();ldiff<-c()
for(i in 1:dim(FI)[1]){
  #i<-1
  tmp<-0
  intFI<-interval(FI$dttm[i], FI$dttm2[i])
  for(j in 1:dim(SE)[1]){
    #j<-1
    if(SE$L[j]>FI$limit1[i] & SE$L[j]<FI$limit2[i]){
      if(SE$dttm[j] %within% intFI){
        dbl[i]<-1
        x[i]<-j
        tdiff[i]<-difftime(SE$dttm[j],FI$dttm[i])
        ldiff[i]<-FI$L[i]-SE$L[j]
        tmp<-1
      } 
    }
  }
  if(tmp==0){dbl[i]<-NA; x[i]<-NA;ldiff[i]<-NA; tdiff[i]<-NA}
}
t2<-Sys.time()
t2-t1
sum(dbl,na.rm=T)
# 3

tmp<-FI%>%
  mutate(dbl=dbl, x=x, ldiff=ldiff, tdiff=tdiff)%>%
  filter(is.na(dbl)==F,tdiff>29)%>%
  select(x,dbl,ldiff,tdiff, everything())

# Remove doubles that are counted twice or more.

tmp2<-distinct(tmp,x,.keep_all = TRUE)
View(tmp2)

 ggplot(tmp2, aes(x=WHeight))+
   geom_histogram(bins=30)
  #facet_grid(Side~MSW)
 ggplot(tmp2, aes(x=dttm))+
   geom_histogram(bins=30)
 


x2<-tmp2$x

y<-SE[x2[1],]
for(i in 2:length(x2)){
  y<-full_join(y,SE[x2[i],])
  print(i)
}
y<-mutate(y, x=x2)

#SE[98,]
#SE[235,]
#SE[415,]


dim(FI)

FI_dblrm<-FI%>%
  mutate(dbl=dbl, tdiff=tdiff)%>%
  select(tdiff, dbl, everything())%>%
  filter(is.na(tdiff)| tdiff<4)%>% # Condition for tdiff that is too small to be double
select(-limit1, -limit2,-tdiff,-dbl, -dttm2)
dim(FI_dblrm)

SE<-SE%>%select(-limit1, -limit2, -dttm2)

dat_w80_low_dblrm<-full_join(FI_dblrm, SE)

dat_w80_high<-dat19%>%
  filter(Window==80)%>%
  filter(WHeight>=78)

dat_w40<-dat19%>%
  filter(Window==40)

dat19_dblrm<-full_join(dat_w40, dat_w80_high)%>%
  full_join(dat_w80_low_dblrm)

dim(dat19)[1]-
dim(dat19_dblrm)[1]


# Sum daily number of salmon going downstream

(dat19_dwn<-dat19%>%
  filter(Dir=="Down"))

# zero. Onko kyse siitä ettei alaspäin meneviä kaloja ole helppo määrittää?
# Osalla kaloista on niin paljon pituutta etteivät voi olla muita kuin lohia (?)



## 
# ===================

D19<-dat19%>%
  filter(Dir=="Up")

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

m2<-lm(datFI$DistShore~datFI$WHeight)
summary(m2)
plot(datFI$DistShore~datFI$WHeight)
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


