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

  
df<-dat%>%
  group_by(Date)%>%
  summarise(n=n())
  
#windows()
ggplot(dat, aes(x=ydate))+
  geom_histogram(bins=300)+
  facet_wrap(~year)

filter(lohi==1)%>%
  filter(Dir=="Up")%>%
  mutate(MSW=if_else(msv==T, 1,0, missing=0))%>% 
  mutate(DistShore=if_else(is.na(`Luotaimen etäisyys rantapenkasta FIN`)==T,
                           `Luotaimen etäisyys rantapenkasta SWE`,
                           `Luotaimen etäisyys rantapenkasta FIN`, missing=0))%>% 
  rename(WHeight=`vedenkorkeus pello`)%>%
  mutate(DistTot=Distance+DistShore)%>%
  select(Dir, Distance, Date, year, Side, Window, Hour, WHeight, MSW, DistShore, DistTot)
