# Annual data from 1.6. - 31.8. (92 days)
###############################################

#ColNames<-c(
#  "Frame","Dir","Distance","Theta","Length","Aspect","Time","Date","Side","Window","Hour",
#  "cm5","Round1","Round2","lohi","WHeight","WHeightRound","MSW","TempKK",
#  "DistanceRound","DistanceRound2", "LengthRound","DistShoreFI","DistShoreSE","Grilse",
#  "80mRangeEndFI","80mRangeEndSE")

D17<-read_xlsx(path="H:/Projects/torne-returns/data/orig/Tornionjoki 2017/Kaikki kalat 2017.xlsx",
               sheet="Kaikki kalat 2017", na="")%>%
  mutate(year=2017)
  
D18<-read_xlsx(path="H:/Projects/torne-returns/data/orig/Tornionjoki 2018/Kaikki kalat 2018.xlsx",
               sheet="Kaikki kalat 2018", na="")%>%
  mutate(year=2018)

D19<-read_xlsx(path="H:/Projects/torne-returns/data/orig/Tornionjoki 2019/Kaikki kalat 2019.xlsx",
               sheet="Kaikki kalat 2019", na="")%>%
  mutate(year=2019)

  
  
  filter(lohi==1)%>%
  mutate(MSW=if_else(msv==T, 1,0, missing=0))%>% 
  rename(WHeight=`vedenkorkeus pello`,
         DistShoreFI=`Luotaimen etäisyys rantapenkasta FIN`,
         DistShoreSE=`Luotaimen etäisyys rantapenkasta SWE`)

datFI<-D19%>%filter(Side=="FIN")%>%
  mutate(DistTot=Distance+DistShoreFI)

datSE<-D19%>%filter(Side=="SWE")%>%
  mutate(DistTot=Distance+DistShoreSE)

dat<-full_join(datFI,datSE)

ggplot(dat)+
  geom_point(aes(y=DistTot, x=WHeight), alpha=0.05, col=my_palette[1])+ 
  facet_grid(MSW~Side)



ggplot(datFI)+
  geom_point(aes(y=DistTotFI, x=WHeight))

ggplot(datSE)+
  geom_point(aes(y=DistTotSE, x=WHeight))+ 
  facet_wrap(~MSW)

tmp<-D19%>%filter(MSW==1)
View(tmp)

dat<-D19%>%filter(Dir=="Up")%>%
  select("Distance", "Date", "Side", "MSW", "WHeight" , "Window", "Grilse")







# Smolts
# =================
ColNames<-c("smolts", "empty", "n_schools", "school_size")
Day<-c(c(1:30), c(1:31), c(1:31))
Month<-c(rep(6,30), rep(7,31), rep(8,31))


D02<-read_xls(str_c(pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2002.xls"),
              sheet=1, na="", 
              range="B5:F60", col_names=c("smolts", "prop%", "n_schools", "prop2%","school_size"))
tmp<-array(NA, dim=c(6,5));colnames(tmp)<-colnames(D02)
D02<-rbind(tmp,D02) #1.6.-6.6. missing
tmp<-array(0, dim=c(30,5));colnames(tmp)<-colnames(D02)
D02<-rbind(D02, tmp) #2.8.-31.8. missing but replace with zeros
D02<-D02%>% mutate(day=c(1:92))%>%
  mutate(Year=2002)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D03<-read_xls(str_c(pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2003.xls"),
              sheet=1, na="", 
              range="Z11:AD71", col_names=c("smolts", "empty", "prop%", "n_schools", "school_size"))
tmp<-array(0, dim=c(31,5));colnames(tmp)<-colnames(D03)
D03<-rbind(D03, tmp) #1.8.-31.8. missing but replace with zeros
D03<-D03%>% mutate(day=c(1:92))%>%
  mutate(Year=2003)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)


D04<-read_xls(str_c(pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2004.xls"),
              sheet=1, na="", 
              range="Z8:AD68", col_names=c("smolts", "empty", "prop%", "n_schools", "school_size"))
tmp<-array(0, dim=c(31,5));colnames(tmp)<-colnames(D04)
D04<-rbind(D04, tmp) #1.8.-31.8. missing but replace with zeros
D04<-D04%>% mutate(day=c(1:92))%>%
  mutate(Year=2004)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D05<-read_xls(str_c(pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2005.xls"),
              sheet=1, na="", 
              range="Z10:AC70", col_names=ColNames)
D05[23,]<-rep(NA,4) # 23.6. 00-09 missing
tmp<-array(0, dim=c(31,4));colnames(tmp)<-colnames(D05)
D05<-rbind(D05, tmp) #1.8.-31.8. missing but replace with zeros
D05<-D05 %>% mutate(day=c(1:92))%>%
  mutate(Year=2005)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)
#View(D05)

D06<-read_xls(str_c(pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2006.xls"),
              sheet=1, na="", range="Z23:AD114", 
              col_names=c("smolts", "empty", "prop%", "n_schools", "school_size"))%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2006)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D07<-read_xls(str_c(pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2007.xls"),
              sheet=1, na="", range="Z8:AC94", 
              col_names=ColNames)
tmp<-array(0, dim=c(5,4));colnames(tmp)<-colnames(D07)
D07<-rbind(tmp, D07) #1.6.-5.6. missing but replace with zeros
D07<-D07 %>%   
  mutate(day=c(1:92))%>%
  mutate(Year=2007)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D08<-read_xls(str_c(pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2008.xls"),
              sheet=1, na="", range="Z25:AC116",col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2008)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D09<-read_xls(str_c(pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2009.xls"),
              sheet=1, na="", range="Z16:AC107",col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2009)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D10<-read_xls(str_c(pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2010.xls"),
              sheet=1, na="", range="Z16:AC107",col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2010)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D11<-read_xlsx(str_c(pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2011.xlsx"),
               sheet=1, na="", range="Z16:AC107", col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2011)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D12<-read_xlsx(str_c(pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2012.xlsx"),
               sheet=1, na="", range="Z8:AC99", col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2012)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D13<-read_xlsx(str_c(pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2013.xlsx"),
               sheet=1, na="", range="Z9:AC100",col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2013)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)

D14<-read_xlsx(str_c(pathIn2,"UTSJOKI VIDEODATA/Utsjoki_smoltit 2014.xlsx"),
               sheet=1, na="", range="Z8:AC99", col_names=ColNames)%>% 
  mutate(day=c(1:92))%>%
  mutate(Year=2014)%>%
  mutate(Day=Day)%>%
  mutate(Month=Month)%>%
  select(Year,Month,Day,day,smolts, school_size)


dat_smolts<-
  D02%>%full_join(D03, by=NULL)%>%
  full_join(D04, by=NULL)%>%
  full_join(D05, by=NULL)%>%
  full_join(D06, by=NULL)%>%
  full_join(D07, by=NULL)%>%
  full_join(D08, by=NULL)%>%
  full_join(D09, by=NULL)%>%
  full_join(D10, by=NULL)%>%
  full_join(D11, by=NULL)%>%
  full_join(D12, by=NULL)%>%
  full_join(D13, by=NULL)%>%
  full_join(D14, by=NULL)%>% 
  
  # What should schools be when smolts==0 ?
  # mutate(schools=if_else(smolts==0, NA_real_, school_size))%>%
  #mutate(schools=if_else(smolts==0, 1, school_size))%>%
  mutate(schools=if_else(smolts==0, 0.001, school_size))%>%
  
  # What should schools be when smolts==NA
  # mutate(schools=if_else(is.na(smolts)==T, 1, schools))%>%
  mutate(schools=if_else(is.na(smolts)==T, NA_real_, schools))%>%
  
  select(Year,Month,Day,day,smolts, schools)
#View(dat_smolts)

# Flow
# ============
dat_flow<-read_xlsx(str_c(pathIn2,"UTSJOKI VIRTAAMADATA/Virtaama_Patoniva 1963-2014.xlsx"),
                    sheet="Patoniva_virtaama_1963-2014", skip=1,
                    col_names = c("Day", "Month", "Year", "flow")) %>%
  filter(Year>2001)%>%
  filter(Month==6 | Month==7 | Month==8)

#View(filter(dat_flow, Year==2012))

# Temperature
# ============

T03<-read_xls(str_c(pathIn2,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2003.xls"),
              sheet = "Utsjoki_raakadata", range="I8:J3074",
              col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T04<-read_xls(str_c(pathIn2,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2004_cam4.xls"),
              sheet = "Min-max lampotilat", range="A4:B1478",
              col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

# Note! Some weird logger points removed from 2005 data (15.6. & 21.6.)
T05<-read_xls(str_c(pathIn2,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2005_cam4.xls"),
              skip=36, col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T06<-read_xls(str_c(pathIn2,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2006.xls"),
              range="A36:B1834", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T07<-read_xls(str_c(pathIn2,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2007.xls"),
              range="A3:B2244",col_names=c("Date", "Temp"))%>%
  mutate(Date=date(parse_datetime(Date, "%d. %B %Y %H:%M")))

T08<-read_xls(str_c(pathIn2,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2008.xls"),
              sheet="UTS2008", range="A35:B3060", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(parse_datetime(Date, "%d. %B %Y %H:%M")))

T09<-read_xls(str_c(pathIn2,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2009.xls"),
              sheet="UTS2009", range="A34:E3928",
              col_names=c("Day", "month", "Year","time", "Temp"))%>%
  select(Day, month, Year, Temp)%>%
  mutate(Month=if_else(
    month=="June", 6, if_else(
      month=="July", 7, if_else(
        month=="August", 8, NA_real_))))%>%
  filter(is.na(Month)==F)%>%
  select(-month)%>%
  group_by(Year,Month,Day)%>%
  summarize(meanTemp=mean(Temp))

T10<-read_xls(str_c(pathIn2,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2010.xls"),
              range="A36:B1834", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T11<-read_xlsx(str_c(pathIn2,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2011.xlsx"),
               sheet="Sheet1", range="B6:C4040", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T12<-read_xlsx(str_c(pathIn2,"UTSJOKI_VEDEN LAMPO/Temperature_Utsjoki2012.xlsx"),
               sheet="Sheet1", range="B6:C4612", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T13<-read_xlsx(str_c(pathIn2,
                     "UTSJOKI_VEDEN LAMPO/Utsjoki_veden lampo_2013-2014.xlsx"),
               sheet="Sheet1", range="B6:C8749", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

T14<-read_xlsx(str_c(pathIn2,
                     "UTSJOKI_VEDEN LAMPO/Utsjoki_veden lampo_2014-2015.xlsx"),
               sheet="Sheet1", range="B7:C8634", col_names=c("Date", "Temp"))%>%
  mutate(Date=date(as.POSIXct(Date)))

#View(T14)

dat_temp<-T03%>%
  full_join(T04, by=NULL)%>%
  full_join(T05, by=NULL)%>%
  full_join(T06, by=NULL)%>%
  full_join(T07, by=NULL)%>%
  full_join(T08, by=NULL)%>%
  full_join(T10, by=NULL)%>%
  full_join(T11, by=NULL)%>%
  full_join(T12, by=NULL)%>%
  full_join(T13, by=NULL)%>%
  full_join(T14, by=NULL)%>%
  group_by(Date)%>%
  summarize(meanTemp=mean(Temp))%>%
  mutate(Year=year(as.POSIXct(Date)))%>%
  mutate(Day=day(as.POSIXct(Date)))%>%
  mutate(Month=month(as.POSIXct(Date)))%>%  
  select(Year,Month,Day,meanTemp)%>%
  full_join(T09, by=NULL)%>%
  filter(Month==6 | Month==7 | Month==8)

dat_all<-dat_smolts%>%
  full_join(dat_flow, by=NULL)%>%
  full_join(dat_temp, by=NULL)


#View(filter(dat_all, is.na(meanTemp)==T, Month<8))
#View(filter(dat_all, is.na(flow)==T, Month<8))

