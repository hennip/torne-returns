source("00-Functions/packages-and-paths.r")


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
         Dir, DistShore, Distance, Date, h,m)


df<-dat19%>%
  filter(is.na(DistShore)==F)%>%
  filter(Side=="FIN", Dir=="Up")%>%
  arrange(dttm)%>%
  filter(MSW==1)
df



max40<-df%>%filter(Window==40)%>%
  summarise(x=max(Distance))

max80<-df%>%filter(Window==80)%>%
  summarise(x=max(Distance))
max40;max80

# Daily total counts per window and in total
# ==========================================
df40<-df%>%
  filter(Window==40)%>%
  group_by(Date)%>%
  tally()%>%
  mutate(w40=n)%>%
  select(-n)

df80<-df%>%
  filter(Window==80)%>%
  group_by(Date)%>%
  tally()%>%
  mutate(w80=n)%>%
  select(-n)

df_tot<-full_join(df40,df80, key=Date)%>%
  mutate(w40=ifelse(is.na(w40)==T, 0, w40))%>%
  mutate(w80=ifelse(is.na(w80)==T, 0, w80))%>%
  mutate(tot=w40+w80)
df_tot
#View(df_tot)


# Daily frequency distributions per distance
# ==========================================


# Sequence of (total) distances:
#x<-seq(0,82, length.out=71 )
x<-seq(0,82, by=2.5 )
#x<-seq(0,82, length.out=31 )
x
length(x)
dg_limits<-tibble(
  DistGroup=1:length(x),
  l1=x, 
  l2=c(x[2:length(x)],NA))

# Define (total) distance group for each indiv
dist_g<-c()
for(i in 1:(dim(df)[1])){
  for(j in 1:(length(x)-1)){
  if(df$DistTot[i]>=x[j] & df$DistTot[i]<x[j+1]){
    dist_g[i]<-j
  }
}
}  
length(dist_g)  
dim(df)

df2<-df%>%
  mutate(DistGroup=dist_g)%>%
  select(Date, DistTot, DistGroup, everything())
df2

# remove observations beyond 82m 
#df2%>%filter(is.na(DistGroup)==T)
df2<-df2%>%filter(is.na(DistGroup)==F)

dat19%>%summarise(min(Date),max(Date))
df2%>%summarise(min(Date),max(Date))

#Days<-seq(as.Date("2019-05-17"),as.Date("2019-09-15"), by=1) #obs in total 2019 data
Days<-seq(as.Date("2019-06-04"),as.Date("2019-08-21"), by=1)#obs in FIN MSW 2019 data
nD<-length(Days)

# Daily counts at a specific distance
dfX<-tibble(DistGroup=1:length(x))
tbl<-NULL
for(i in 1:nD){
  #i<-2
    tmp<-df2%>%
      #filter(Window==40)%>%
      filter(Window==80)%>%
      filter(Date==Days[i])%>%
  group_by(DistGroup)%>%
    tally()
    
tmp2<-full_join(dfX,tmp,key="DistGroup")

ifelse(i==1, tbl<-tmp2[,2],tbl<-cbind(tbl,tmp2[,2]))

}
dim(tbl)
# Replace NA's with 0's
for(i in 1:dim(tbl)[1]){
  for(j in 1:dim(tbl)[2]){
  if(is.na(tbl[i,j])==T){tbl[i,j]<-0}   
}
}

names(tbl)<-c(1:nD)
#sum(tbl,na.rm=T)
tbl_40<-tbl
tbl_80<-tbl
sum(tbl_40,na.rm=T)
sum(tbl_80,na.rm=T)
#View(tbl_40)

# Define true distance distribution for low water situation
tmpX<-df2%>%filter(WHeight<77.3)%>%
  mutate(DS2=as.factor(DistShore))%>%
  select(DS2, everything())

levels(tmpX$DS2)

#View(tmpX%>%filter(DS2=="23.7"))


# Treat these distances from the shore as the same 
tmpX<-tmpX%>%mutate(DistShore2=ifelse(DistShore==23.7,23.5, DistShore))%>%
  mutate(DS2=as.factor(DistShore2))%>%
  select(DS2, everything())

levels(tmpX$DS2)


# Count in W40/W80,
# colours indicate observations from specific device distance from shore
ggplot(tmpX, aes(x=DistTot, col=DS2, fill=DS2, alpha=0.1))+
  #geom_density()+
  #geom_histogram(bins=40, position = "identity")+
  geom_histogram(bins=33)+
  xlim(0,82)+
  geom_vline(xintercept=tmpX$DistShore, color="black")+
  facet_wrap(~Window, scales="free")

# Same in numbers
counts<-tmpX%>%
  group_by(DistGroup)%>%
  tally()

counts<-inner_join(dg_limits,counts, key=DistGroup)

counts1<-counts%>%filter(l2<=40)
counts2<-counts%>%filter(l2>40)


ggplot(tmpX%>%filter(Window==40), aes(x=DistTot, col=DS2, fill=DS2, alpha=0.1))+
  geom_histogram(bins=30, position = "identity")+
  xlim(0,80)+
  facet_wrap(~DS2)

ggplot(tmpX%>%filter(Window==80), aes(x=DistTot, col=DS2, fill=DS2, alpha=0.1))+
  geom_histogram(bins=30, position = "identity")+
  xlim(0,80)+
  facet_wrap(~DS2)


#ds<-c("32.7","28.7","25.5","23.5","22.6")
ds<-c("22.6","23.5","25.5","28.7","32.7")

#View(tmpX%>%filter(DistShore==22.6))

# Nice but you can't join these 
# X<-NULL
# for(i in 1:5){
#   #i<-1
#   X[[i]]<-tmpX%>%filter(DS2==ds[i])%>%
#     group_by(DistGroup)%>%
#     summarise(n = n(),
#               muwh=mean(WHeight, na.rm=T), 
#               sdwh=round(sd(WHeight, na.rm =) ,2)) %>%
#   mutate(freq = round(n / sum(n),2))
# }
# 
# full_join(X[[1]],X[[2]], key=DistGroup)
  


X1<-tmpX%>%filter(DS2==ds[1])%>%
  group_by(DistGroup)%>%
  summarise(n1 = n(), muwh1=mean(WHeight), sdwh1=round(sd(WHeight),2)) %>%
  mutate(freq1 = round(n1 / sum(n1),2))%>%
  select(DistGroup,n1, freq1)

X2<-tmpX%>%filter(DS2==ds[2])%>%
  group_by(DistGroup)%>%
  summarise(n2 = n(), muwh2=mean(WHeight), sdwh2=round(sd(WHeight),2)) %>%
  mutate(freq2 = round(n2 / sum(n2),2))%>%
  select(DistGroup,n2, freq2)

X3<-tmpX%>%filter(DS2==ds[3])%>%
  group_by(DistGroup)%>%
  summarise(n3 = n(), muwh3=mean(WHeight), sdwh3=round(sd(WHeight),2)) %>%
  mutate(freq3 = round(n3 / sum(n3),2))%>%
  select(DistGroup,n3, freq3)

X4<-tmpX%>%filter(DS2==ds[4])%>%
  group_by(DistGroup)%>%
  summarise(n4 = n(), muwh4=mean(WHeight), sdwh4=round(sd(WHeight),2)) %>%
  mutate(freq4 = round(n4 / sum(n4),2))%>%
  select(DistGroup,n4, freq4)

X5<-tmpX%>%filter(DS2==ds[5])%>%
  group_by(DistGroup)%>%
  summarise(n5 = n(), muwh5=mean(WHeight), sdwh5=round(sd(WHeight),2)) %>%
  mutate(freq5 = round(n5 / sum(n5),2))%>%
  select(DistGroup,n5, freq5)

df4<-inner_join(dg_limits,df3, key=DistGroup)

View(full_join(X1,X2, key=DistGroup)%>%
  full_join(X3, key=DistGroup)%>%
  full_join(X4, key=DistGroup)%>%
  full_join(X5, key=DistGroup)%>%
  inner_join(tmp, key=DistGroup)%>%
    arrange(DistGroup)%>%
    select(DistGroup, l1, l2, everything()))




tmpZ<-df2%>%
  mutate(DS2=as.factor(DistShore))%>%
  select(DS2, everything())

cbind(c(1:length(levels(tmpZ$DS2))),as.numeric(levels(tmpZ$DS2)))


# P[d,t]: probability that a fish that crosses the site at day t crosses at distance d

# Distance distributions
############################
# FIN/SWE side with short/long window
# 0=Grilse, 1=MSW 

WHseq<-seq(77.3,78.7, by=0.2)
WHseq1<-c(77, WHseq, 79.5)

# WH>78
# 77.5<WH<78
# 77.3<WH<77.5
# 77<WH<77.3

# Ehkä niin että sijainnin jakauma painuu lähemmäs rantaa jotenkin lineaarisemmin (?) vedenkorkeuden noustessa
# Mutta havaitsemistn:n jakauma vedenkorkeuden suhteen voisi olla karkeammin jaoteltu esim. 4:ään vaihtoehtoon
# Vai meneekö monimutkaiseksi, pitäisikö olla samat luokat?
# Ja onko molemmat käsiteltävä kaksihuippuisina matalalla vedellä, vai pelkästään sijainnin jakauma?
# Mistä sen tietää?

WHseq1<-c(77,77.3,77.5,78,79.5)

#FIN
##########################
for(i in 1:(length(WHseq1)-1)){
  #i<-1
  tmp<-dat19%>%
    filter(is.na(DistShore)==F)%>%
    filter(Side=="FIN", WHeight>WHseq1[i] & WHeight<=WHseq1[i+1], Dir=="Up")%>%
    arrange(dttm)
  
  dat80<-tmp%>%filter(Window==80)
  dat40<-tmp%>%filter(Window==40)%>%
    filter(m>=40 & m<50)# tunnin 2. viimeiset 10 min
  
  dfX<-full_join(dat80,dat40)%>%
    mutate(Window2=as.factor(Window))%>%
  mutate(MSW=1)
  
  print(
    ggplot(dfX, aes(x=DistTot, fill=Window2, col=Window2))+
      geom_histogram(bins=30, alpha=0.2, position = "identity")+
      xlim(0,110)+
      #ylim(0,300)+
      geom_vline(xintercept=dfX$DistShore, color="black")+
      geom_vline(xintercept=82, color="blue")+
      #geom_vline(xintercept=as.numeric(max40), color="red")+
      #geom_vline(xintercept=as.numeric(max80), color="red")+
      labs(x="Distance from shore", title=str_c("FIN, water height ",WHseq1[i],"-",WHseq1[i+1]))#+
      #facet_wrap(~MSW)
    
  )
}

i<-1
tmp<-dat19%>%
  filter(is.na(DistShore)==F)%>%
  filter(Side=="FIN", WHeight>WHseq1[i] & WHeight<=WHseq1[i+1], Dir=="Up")%>%
  arrange(dttm)

dat80<-tmp%>%filter(Window==80)
dat40<-tmp%>%filter(Window==40)%>%
  filter(m>=40 & m<50)# tunnin 2. viimeiset 10 min

dfX<-full_join(dat80,dat40)%>%
  mutate(Window2=as.factor(Window))%>%
  mutate(MSW=1)

p<-ggplot(dfX, aes(x=DistTot, fill=Window2, col=Window2))+
    geom_histogram(bins=30, alpha=0.2, position = "identity")+
    xlim(0,110)+
    geom_vline(xintercept=dfX$DistShore, color="black")+
    geom_vline(xintercept=82, color="blue")+
    labs(x="Distance from shore", title=str_c("FIN, water height ",WHseq1[i],"-",WHseq1[i+1]))#+

pg1 <- ggplot_build(p)
pg1$data[[1]][,4][1:30] #w40
pg1$data[[1]][,4][31:55] #w40


ggplot(tmp, aes(x=DistShore))+
  geom_histogram(bins=70)





df3<-df2%>%
  group_by(Date,DistGroup)%>%
  group_by(DistGroup)%>%
  tally()
#View(df3)

tmp<-tibble(
  DistGroup=1:length(x),
  l1=x, 
  l2=c(x[2:length(x)],NA))

df4<-inner_join(tmp,df3, key=DistGroup)
View(df4)

df4%>%
  group_by(Date)%>%
  


# Mean water height per day
df%>%
  group_by(Date)%>%
summarise(mean_wh=mean(WHeight))




