
dat19<-read_xlsx(str_c(pathIn,"Tornionjoki 2019/Kaikki kalat 2019.xlsx"),
                 sheet="Kaikki kalat 2019", na="")%>%
  
#dat19<-read_xlsx(str_c(pathIn,"Kaikki kalat 2019.xlsx"),
#                 sheet="Kaikki kalat 2019", na="")%>%
  filter(lohi==1)%>%
  mutate(MSW=if_else(msv==T, 1,0, missing=0))%>% 
  mutate(DistShore=if_else(is.na(`Luotaimen etäisyys rantapenkasta FIN`)==T,
                           `Luotaimen etäisyys rantapenkasta SWE`,
                           `Luotaimen etäisyys rantapenkasta FIN`, missing=NULL))%>% 
  #mutate(DistShore=if_else(is.na(`Luotaimen et?isyys rantapenkasta FIN`)==T,
  #                         `Luotaimen et?isyys rantapenkasta SWE`,
  #                         `Luotaimen et?isyys rantapenkasta FIN`, missing=0))%>% 
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


# Pick first only the MSW salmon at FIN side
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

counts_tot<-full_join(df40,df80, key=Date)%>%
  mutate(w40=ifelse(is.na(w40)==T, 0, w40))%>%
  mutate(w80=ifelse(is.na(w80)==T, 0, w80))%>%
  mutate(tot=w40+w80)
counts_tot
View(counts_tot)

##################################
###################################
#OMAT

#X=counts_tot$tot
#XC=as.matrix(t(counts_tot[,2:3])

#X_1 = counts_tot$w80 
#X_2 = counts_tot$w40 

#XC=as.matrix(X_1)
#XC2=as.matrix(X_2)

#XC=cbind(X_1,X_2)


#length(X)

##################################
##################################

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

# remove observations beyond 82m (-> NA for DistGroup)
#df2%>%filter(is.na(DistGroup)==T)
df2<-df2%>%filter(is.na(DistGroup)==F)

dat19%>%summarise(min(Date),max(Date))
df2%>%summarise(min(Date),max(Date))

#Days<-seq(as.Date("2019-05-17"),as.Date("2019-09-15"), by=1) #obs in total 2019 data
Days<-seq(as.Date("2019-06-04"),as.Date("2019-08-21"), by=1)#obs in FIN MSW 2019 data
nD<-length(Days)

# Daily counts at a specific distance, per Window 40 and Window 80
# 33xnD matrix, where 33 is number of distance groups and nD is the number of days
for(g in 1:2){
  ifelse(g==1,w<-40, w<-80)
  
  dfX<-tibble(DistGroup=1:length(x))
  tbl<-NULL
  for(i in 1:nD){
    #i<-2
    tmp<-df2%>%
      filter(Window==w)%>%
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
  ifelse(g==1,tbl_40<-tbl,tbl_80<-tbl)
  
}

sum(tbl_40,na.rm=T)
sum(tbl_80,na.rm=T)
#View(tbl_40)

