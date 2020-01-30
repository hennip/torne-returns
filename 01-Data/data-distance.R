#source("01-Data/data-salmon.r")

# In 2019 distance from shore is available. In earlier years only distance from the 
# device.

# How we can use the distance information to evaluate circumstances
# in which a certain proportion passes the site unobserved (via the main channel)

D19<-read_xlsx(path="H:/Projects/torne-returns/data/orig/Tornionjoki 2019/Kaikki kalat 2019.xlsx",
               sheet="Kaikki kalat 2019", na="")%>%
  mutate(year=2019)%>%
  filter(lohi==1)%>%
  filter(Dir=="Up")%>%
  mutate(MSW=if_else(msv==T, 1,0, missing=0))%>% 
  mutate(DistShore=if_else(is.na(`Luotaimen etäisyys rantapenkasta FIN`)==T,
                           `Luotaimen etäisyys rantapenkasta SWE`,
                           `Luotaimen etäisyys rantapenkasta FIN`, missing=0))%>% 
  rename(WHeight=`vedenkorkeus pello`)%>%
  mutate(DistTot=Distance+DistShore)%>%
  select(Dir, Distance, Date, year, Side, Window, Hour, WHeight, MSW, DistShore, DistTot)

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

# Aggregate daily counts (FI/SE side, Grilse/MSW)
dat<-D19%>%
  group_by(Date, Side, MSW)%>%
  summarize(n=n())%>%
  mutate(Year=year(as.POSIXct(Date)))%>%
  mutate(Day=day(as.POSIXct(Date)))%>%
  mutate(Month=month(as.POSIXct(Date)))%>%  
  select(Year,Month,Day,n, Side, MSW)

View(dat)  

dat<-D19%>%filter(Side=="FIN", MSW==1)

ggplot(dat, aes(x=Date))+
  geom_histogram(bins=120)
# facet_grid(Side~MSW)

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


