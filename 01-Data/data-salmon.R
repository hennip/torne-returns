# Kalakohtainen data 
###############################################

source("00-Functions/packages-and-paths.r")

# set running number from 17th May to 12th Sept

D16<-read_xlsx(str_c(pathIn,"Tornionjoki 2016/Kaikki kalat 2016.xlsx"),
               sheet="Sheet1", na="")%>%
  mutate(year=2016)%>%
  mutate(Year=year(as.POSIXct(Date)))%>%
  mutate(Day=day(as.POSIXct(Date)))%>%
  mutate(Month=month(as.POSIXct(Date)))%>%  
  mutate(Dayr=Day-16+(Month-5)*30)

test<-D16%>%select(Date, Dayr)
View(test)

ggplot(test, aes(x=Dayr))+
  geom_histogram(bins=30)#%>%
#facet_wrap(~year)

#https://stackoverflow.com/questions/29974535/dates-with-month-and-day-in-time-series-plot-in-ggplot2-with-facet-for-years/47901935

D17<-read_xlsx(str_c(pathIn,"Tornionjoki 2017/Kaikki kalat 2017.xlsx"),
               sheet="Kaikki kalat 2017", na="")%>%
  mutate(year=2017)
  
D18<-read_xlsx(str_c(pathIn,"Tornionjoki 2018/Kaikki kalat 2018.xlsx"),
               sheet="Kaikki kalat 2018", na=c("","-1.$" ))%>%
  mutate(year=2018)

D19<-read_xlsx(str_c(pathIn,"Tornionjoki 2019/Kaikki kalat 2019.xlsx"),
               sheet="Kaikki kalat 2019", na="")%>%
  mutate(year=2019)

dat<-full_join(D16,D17)%>%
  full_join(D18)%>%
  full_join(D19)

max(D16$Date)


ggplot(dat, aes(x=Date))+
  geom_histogram(bins=600)#%>%
  #facet_wrap(~year)

filter(lohi==1)%>%
  filter(Dir=="Up")%>%
  mutate(MSW=if_else(msv==T, 1,0, missing=0))%>% 
  mutate(DistShore=if_else(is.na(`Luotaimen etäisyys rantapenkasta FIN`)==T,
                           `Luotaimen etäisyys rantapenkasta SWE`,
                           `Luotaimen etäisyys rantapenkasta FIN`, missing=0))%>% 
  rename(WHeight=`vedenkorkeus pello`)%>%
  mutate(DistTot=Distance+DistShore)%>%
  select(Dir, Distance, Date, year, Side, Window, Hour, WHeight, MSW, DistShore, DistTot)
