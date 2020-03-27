# Kalakohtainen data 
###############################################

source("00-Functions/packages-and-paths.r")

D14<-read_xlsx(str_c(pathIn,"Tornionjoki 2014/Kaikki kalat 2014_uusin_18.12.2014.xlsx"),
               sheet="Sheet1", na=c("","-1.$" ))%>%
  mutate(year=2014)%>%
  mutate(ydate=yday(as.POSIXct(Date)))


D15<-read_xlsx(str_c(pathIn,"Tornionjoki 2015/Kaikki kalat 2015.xlsx"),
               sheet="Sheet1", na=c("","-1.$" ))%>%
  mutate(year=2015)%>%
  mutate(ydate=yday(as.POSIXct(Date)))


D16<-read_xlsx(str_c(pathIn,"Tornionjoki 2016/Kaikki kalat 2016.xlsx"),
               sheet="Sheet1", na=c("","-1.$" ))%>%
  mutate(year=2016)%>%
  mutate(ydate=yday(as.POSIXct(Date)))
  #   mutate(Year=year(as.POSIXct(Date)))%>%
  # mutate(Day=day(as.POSIXct(Date)))%>%
  # mutate(Month=month(as.POSIXct(Date)))


#https://stackoverflow.com/questions/29974535/dates-with-month-and-day-in-time-series-plot-in-ggplot2-with-facet-for-years/47901935

D17<-read_xlsx(str_c(pathIn,"Tornionjoki 2017/Kaikki kalat 2017.xlsx"),
               sheet="Kaikki kalat 2017", na="")%>%
  mutate(year=2017)%>%
  mutate(ydate=yday(as.POSIXct(Date)))

  
D18<-read_xlsx(str_c(pathIn,"Tornionjoki 2018/Kaikki kalat 2018.xlsx"),
               sheet="Kaikki kalat 2018", na=c("","-1.$" ))%>%
  mutate(year=2018)%>%
  mutate(ydate=yday(as.POSIXct(Date)))


D19<-read_xlsx(str_c(pathIn,"Tornionjoki 2019/Kaikki kalat 2019.xlsx"),
               sheet="Kaikki kalat 2019", na="")%>%
  mutate(year=2019)%>%
  mutate(ydate=yday(as.POSIXct(Date)))


dat<-full_join(D15,D16)%>%
  #full_join(D16)%>%
  full_join(D17)%>%
  full_join(D18)%>%
  full_join(D19)

# Plot daily counts  
df<-dat%>%
  group_by(Date)%>%
  summarise(n=n())
  
ggplot(dat, aes(x=ydate))+
  geom_histogram(bins=300)+
  facet_wrap(~year)
##########

# Remove salmon seen at both sides

dat_all<-dat%>%
  filter(lohi==1)%>%
  mutate(MSW=if_else(msv==T, 1,0, missing=0))%>% 
  rename(WHeight=`vedenkorkeus pello`)%>%
  mutate(L=`L(cm)`)%>%
  mutate(y=year(Date),
         mon=month(Date),
         d=day(Date),
         h=hour(Time),
         m=minute(Time),
         s=second(Time))%>%
  mutate(dttm=make_datetime(year=y, month=mon,day=d,hour=h, min=m, sec=s))%>%
  select(dttm,Side,L,MSW, Window,WHeight,Dir, Distance)

# Select a limit for water height above which doubles may occur
# Assume doubles seen only when window==80
dat_w80_low<-dat_all%>%
  filter(Window==80)%>%
  filter(WHeight<78)%>%
  filter(Dir=="Up")

dim(filter(dat_w80_low, Side=="FIN"))
dim(filter(dat_w80_low, Side=="SWE"))

SE<-dat_w80_low%>%filter(Side=="SWE")%>%
  mutate(dttm2=dttm-dminutes(1))%>%
  mutate(limit1=L-1, limit2=L+1) # or whatever range in size could be interpreted as the same fish

FI<-dat_w80_low%>%filter(Side=="FIN")%>%
  mutate(dttm2=dttm+dminutes(1))%>%
  mutate(limit1=L-1, limit2=L+1)



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

tmp<-FI%>%mutate(dbl=dbl, x=x, ldiff=ldiff, tdiff=tdiff)
View(filter(tmp, is.na(dbl)==F)%>%
  select(x,dbl,ldiff,tdiff, everything()))

filter(tmp, tdiff>0)

#SE[98,]
#SE[235,]
#SE[415,]


dim(FI)

FI_dblrm<-FI%>%
  mutate(dbl=dbl, tdiff=tdiff)%>%
  select(tdiff, dbl, everything())%>%
  filter(is.na(tdiff)| tdiff<1)%>% # Condition for tdiff that is too small to be double
  select(-limit1, -limit2,-tdiff,-dbl, -dttm2)
dim(FI_dblrm)

SE<-SE%>%select(-limit1, -limit2, -dttm2)

dat_w80_low_dblrm<-full_join(FI_dblrm, SE)

dat_w80_high<-dat19%>%
  filter(Window==80)%>%
  filter(WHeight<=78.5)

dat_w40<-dat19%>%
  filter(Window==40)

dat19_dblrm<-full_join(dat_w40, dat_w80_high)%>%
  full_join(dat_w80_low_dblrm)

dim(dat19_dblrm)
dim(dat19)



