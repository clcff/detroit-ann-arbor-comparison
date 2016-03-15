library(Quandl)
library(tseries)
library(timeSeries)
library(forecast)
library("zoo")
library(quantmod)

#Quandl authetication key

Quandl.auth("xsPzjKeHjtLjjjEpLgYi")

#list of stocks in the index and market cap calculation:

ORCLF<-Quandl("DMDRN/ORCL_FLOAT")
GPSF<-Quandl("DMDRN/GPS_FLOAT")
RLF<-Quandl("DMDRN/RL_FLOAT")
SNIF<-Quandl("DMDRN/SNI_FLOAT")
DKSF<-Quandl("DMDRN/DKS_FLOAT")
WNRF<-Quandl("DMDRN/WNR_FLOAT")
SYNTF<-Quandl("DMDRN/SYNT_FLOAT")
URBNF<-Quandl("DMDRN/URBN_FLOAT")
SEBF<-Quandl("DMDRN/SEB_FLOAT")
GHCF<-Quandl("DMDRN/GHC_FLOAT")
#CALMF<-Quandl("DMDRN/CALM_FLOAT")
SBGIF<-Quandl("DMDRN/SBGI_FLOAT")
SWFTF<-Quandl("DMDRN/SWFT_FLOAT")
#GMEDF<-Quandl("DMDRN/GMED_FLOAT")
AHGPF<-Quandl("DMDRN/AHGP_FLOAT")
GESF<-Quandl("DMDRN/GES_FLOAT")
WSTCF<-Quandl("DMDRN/WSTC_FLOAT")
WERNF<-Quandl("DMDRN/WERN_FLOAT")
BKEF<-Quandl("DMDRN/BKE_FLOAT")
USNAF<-Quandl("DMDRN/USNA_FLOAT")
TTECF<-Quandl("DMDRN/TTEC_FLOAT")
WMKF<-Quandl("DMDRN/WMK_FLOAT")
AMKRF<-Quandl("DMDRN/AMKR_FLOAT")
HYF<-Quandl("DMDRN/AMKR_FLOAT")
NHCF<-Quandl("DMDRN/NHC_FLOAT")
TPCF<-Quandl("DMDRN/TPC_FLOAT")
QSIIF<-Quandl("DMDRN/QSII_FLOAT")
FLWSF<-Quandl("DMDRN/FLWS_FLOAT")
MRTNF<-Quandl("DMDRN/MRTN_FLOAT")
UACLF<-Quandl("DMDRN/UACL_FLOAT")
PCCCF<-Quandl("DMDRN/PCCC_FLOAT")

#time series of each component:


spy<-Quandl("GOOG/NYSE_SPY")

ORCLP<-Quandl("GOOG/NASDAQ_ORCL")
GPSP<-Quandl("GOOG/NYSE_GPS")
RLP<-Quandl("GOOG/NYSE_RL")
SNIP<-Quandl("GOOG/NYSE_SNI")
DKSP<-Quandl("GOOG/NYSE_DKS")
WNRP<-Quandl("GOOG/NYSE_WNR")
SYNTP<-Quandl("GOOG/NASDAQ_SYNT")
URBNP<-Quandl("GOOG/NASDAQ_URBN")
SEBP<-Quandl("GOOG/AMEX_SEB")
GHCP<-Quandl("GOOG/NYSE_GHC")
SBGIP<-Quandl("GOOG/NASDAQ_SBGI")
SWFTP<-Quandl("GOOG/NYSE_SWFT")
AHGPP<-Quandl("GOOG/NASDAQ_AHGP")
GESP<-Quandl("GOOG/NYSE_GES")
WSTCP<-Quandl("GOOG/NASDAQ_WSTC")
WERNP<-Quandl("GOOG/NASDAQ_WERN")
BKEP<-Quandl("GOOG/NYSE_BKE")
USNAP<-Quandl("GOOG/NYSE_USNA")
TTECP<-Quandl("GOOG/NASDAQ_TTEC")
WMKP<-Quandl("GOOG/NYSE_WMK")
AMKRP<-Quandl("GOOG/NASDAQ_AMKR")
HYP<-Quandl("GOOG/NYSE_HY")
NHCP<-Quandl("GOOG/AMEX_NHC")
TPCP<-Quandl("GOOG/NYSE_TPC")
QSIIP<-Quandl("GOOG/NASDAQ_QSII")
FLWSP<-Quandl("GOOG/NASDAQ_FLWS")
MRTNP<-Quandl("GOOG/NASDAQ_MRTN")
UACLP<-Quandl("GOOG/NASDAQ_UACL")
PCCCP<-Quandl("GOOG/NASDAQ_PCCC")


# initial date of the model:

#databeg<- ORCLF[4,1]
databeg<- tail(ORCLF,1)
#View(ORCLF)
databeg<- databeg[,1]
print("start date:")
print(databeg)

# Spy will be considered in the same date for sake of comparison:

p0<-ts(subset(spy, spy$Date>=databeg))
x<- head(p0,10)
#View(x)
y<- tail(p0,10)
#View(y)
p0<- p0[,5]/lag(p0[,5],1)-1
p0[is.na(p0)] <- 0


# subseting all stocks to start after the initial date defined above 
#and calculate % change:

p1<-ts(subset(ORCLP, ORCLP$Date>=databeg))
p1<- p1[,5]/lag(p1[,5],1)-1


p2<-ts(subset(GPSP, GPSP$Date>=databeg))
p2<- p2[,5]/lag(p2[,5],1)-1


p3<-ts(subset(RLP, RLP$Date>=databeg))
p3<- p3[,5]/lag(p3[,5],1)-1


p4<-ts(subset(SNIP, SNIP$Date>=databeg))
p4<- p4[,5]/lag(p4[,5],1)-1


p5<-ts(subset(DKSP, DKSP$Date>=databeg))
p5<- p5[,5]/lag(p5[,5],1)-1


p6<-ts(subset(WNRP, WNRP$Date>=databeg))
p6<- p6[,5]/lag(p6[,5],1)-1


p7<-ts(subset(SYNTP, SYNTP$Date>=databeg))
p7<- p7[,5]/lag(p7[,5],1)-1


p8<-ts(subset(URBNP, URBNP$Date>=databeg))
p8<- p8[,5]/lag(p8[,5],1)-1


p9<-ts(subset(SEBP, SEBP$Date>=databeg))
p9<- p9[,5]/lag(p9[,5],1)-1


p10<-ts(subset(GHCP, GHCP$Date>=databeg))
p10<- p10[,5]/lag(p10[,5],1)-1


p11<-ts(subset(SBGIP, SBGIP$Date>=databeg))
p11<- p11[,5]/lag(p11[,5],1)-1


p12<-ts(subset(SWFTP, SWFTP$Date>=databeg))
p12<- p12[,5]/lag(p12[,5],1)-1


p13<-ts(subset(AHGPP, AHGPP$Date>=databeg))
p13<- p13[,5]/lag(p13[,5],1)-1

p14<-ts(subset(GESP, GESP$Date>=databeg))
p14<- p14[,5]/lag(p14[,5],1)-1


p15<-ts(subset(WSTCP, WSTCP$Date>=databeg))
p15<- p15[,5]/lag(p15[,5],1)-1


p16<-ts(subset(WERNP, WERNP$Date>=databeg))
p16<- p16[,5]/lag(p16[,5],1)-1


p17<-ts(subset(BKEP, BKEP$Date>=databeg))
p17<- p17[,5]/lag(p17[,5],1)-1


p18<-ts(subset(USNAP, USNAP$Date>=databeg))
p18<- p18[,5]/lag(p18[,5],1)-1


p19<-ts(subset(TTECP, TTECP$Date>=databeg))
p19<- p19[,5]/lag(p19[,5],1)-1

p20<-ts(subset(WMKP, WMKP$Date>=databeg))
p20<- p20[,5]/lag(p20[,5],1)-1

p21<-ts(subset(AMKRP, AMKRP$Date>=databeg))
p21<- p21[,5]/lag(p21[,5],1)-1

p22<-ts(subset(HYP, HYP$Date>=databeg))
p22<- p22[,5]/lag(p22[,5],1)-1

p23<-ts(subset(NHCP, NHCP$Date>=databeg))
p23<- p23[,5]/lag(p23[,5],1)-1

p24<-ts(subset(TPCP, TPCP$Date>=databeg))
p24<- p24[,5]/lag(p24[,5],1)-1

p25<-ts(subset(QSIIP, QSIIP$Date>=databeg))
p25<- p25[,5]/lag(p25[,5],1)-1

p26<-ts(subset(FLWSP, FLWSP$Date>=databeg))
p26<- p26[,5]/lag(p26[,5],1)-1

p27<-ts(subset(MRTNP, MRTNP$Date>=databeg))
p27<- p27[,5]/lag(p27[,5],1)-1

p28<-ts(subset(UACLP, UACLP$Date>=databeg))
p28<- p28[,5]/lag(p28[,5],1)-1

p29<-ts(subset(PCCCP, PCCCP$Date>=databeg))
p29<- p29[,5]/lag(p29[,5],1)-1

changes<-cbind(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,
               p12,p13,p14,p15,p16,p17,p18,p19,p20,
               p21,p22,p23,p24,p25,p26,p27,p28,p29)

#clean up any possible na errors:

changes[is.na(changes)] <- 0

#View(changes)
d<-dim(changes)


#creating a market cap matrix over time for all stocks in the index:

n<-dim(ORCLF)[1]
n2<-dim(ORCLP)[1]
nn<-n+1
for(j in 1:n2){
  
    ORCLP$MC[j]<- 0
  } 


for (i in 1:n)   {
    i<-nn-i
    for(j in 1:n2){

              if(ORCLP$Date[j]>=ORCLF$Date[i]){
               ORCLP$MC[j]<- ORCLF[i,2]*ORCLP$Close[j]
              } 
              else {ORCLP$MC[j]<-ORCLP$MC[j]}      
          }
        
     }

ORCLP$Date<- as.Date(ORCLP$Date)
#View(ORCLP$Date)

n<-dim(GPSF)[1]
n2<-dim(GPSP)[1]
nn<-n+1
sub<- data.frame(0,0)
for(j in 1:n2){
  
  GPSP$MC[j]<- 0
} 


for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    
    if(GPSP$Date[j]>=GPSF$Date[i]){
      GPSP$MC[j]<- GPSF[i,2]*GPSP$Close[j]
    } 
    else {GPSP$MC[j]<-GPSP$MC[j]}      
  }
  
}
GPSP$Date<- as.Date(GPSP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
#combo<- cbind(GPSP$Date,GPSP$MC, ORCLP$MC)
combo<- cbind(ORCLP$MC, GPSP$MC)

#View(combo[,1])

n<-dim(RLF)[1]
n2<-dim(RLP)[1]
nn<-n+1
for(j in 1:n2){
  
  RLP$MC[j]<- 0
} 


for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    
    if(RLP$Date[j]>=RLF$Date[i]){
      RLP$MC[j]<- RLF[i,2]*RLP$Close[j]
    } 
    else {RLP$MC[j]<-RLP$MC[j]}      
  }
  
}
RLP$Date<- as.Date(RLP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, RLP$MC)

n<-dim(SNIF)[1]
n2<-dim(SNIP)[1]
nn<-n+1
for(j in 1:n2){
  
  SNIP$MC[j]<- 0
} 


for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    
    if(SNIP$Date[j]>=SNIF$Date[i]){
      SNIP$MC[j]<- SNIF[i,2]*SNIP$Close[j]
    } 
    else {SNIP$MC[j]<-SNIP$MC[j]}      
  }
  
}
SNIP$Date<- as.Date(SNIP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, SNIP$MC)

n<-dim(DKSF)[1]
n2<-dim(DKSP)[1]
nn<-n+1
for(j in 1:n2){
  
  DKSP$MC[j]<- 0
} 


for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    
    if(DKSP$Date[j]>=DKSF$Date[i]){
      DKSP$MC[j]<- DKSF[i,2]*DKSP$Close[j]
    } 
    else {DKSP$MC[j]<-DKSP$MC[j]}      
  }
  
}
DKSP$Date<- as.Date(DKSP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, DKSP$MC)

n<-dim(WNRF)[1]
n2<-dim(WNRP)[1]
nn<-n+1
for(j in 1:n2){
  
  WNRP$MC[j]<- 0
} 


for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    
    if(WNRP$Date[j]>=WNRF$Date[i]){
      WNRP$MC[j]<- WNRF[i,2]*WNRP$Close[j]
    } 
    else {WNRP$MC[j]<-WNRP$MC[j]}      
  }
  
}
WNRP$Date<- as.Date(WNRP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, WNRP$MC)



n<-dim(SYNTF)[1]
n2<-dim(SYNTP)[1]
nn<-n+1
for(j in 1:n2){
  
  SYNTP$MC[j]<- 0
} 


for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    
    if(SYNTP$Date[j]>=SYNTF$Date[i]){
      SYNTP$MC[j]<- SYNTF[i,2]*SYNTP$Close[j]
    } 
    else {SYNTP$MC[j]<-SYNTP$MC[j]}      
  }
  
}
SYNTP$Date<- as.Date(SYNTP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, SYNTP$MC)


n<-dim(URBNF)[1]
n2<-dim(URBNP)[1]
nn<-n+1
for(j in 1:n2){
   URBNP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(URBNP$Date[j]>=URBNF$Date[i]){
      URBNP$MC[j]<- URBNF[i,2]*URBNP$Close[j]
    } 
    else {URBNP$MC[j]<-URBNP$MC[j]}      
  }
}
URBNP$Date<- as.Date(URBNP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, URBNP$MC)


n<-dim(SEBF)[1]
n2<-dim(SEBP)[1]
nn<-n+1
for(j in 1:n2){
  SEBP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(SEBP$Date[j]>=SEBF$Date[i]){
      SEBP$MC[j]<- SEBF[i,2]*SEBP$Close[j]
    } 
    else {SEBP$MC[j]<-SEBP$MC[j]}      
  }
}
SEBP$Date<- as.Date(SEBP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, SEBP$MC)


n<-dim(GHCF)[1]
n2<-dim(GHCP)[1]
nn<-n+1
for(j in 1:n2){
  GHCP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(GHCP$Date[j]>=GHCF$Date[i]){
      GHCP$MC[j]<- GHCF[i,2]*GHCP$Close[j]
    } 
    else {GHCP$MC[j]<-GHCP$MC[j]}      
  }
}
GHCP$Date<- as.Date(GHCP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, GHCP$MC)


n<-dim(SBGIF)[1]
n2<-dim(SBGIP)[1]
nn<-n+1
for(j in 1:n2){
  SBGIP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(SBGIP$Date[j]>=SBGIF$Date[i]){
      SBGIP$MC[j]<- SBGIF[i,2]*SBGIP$Close[j]
    } 
    else {SBGIP$MC[j]<-SBGIP$MC[j]}      
  }
}
SBGIP$Date<- as.Date(SBGIP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, SBGIP$MC)

n<-dim(SWFTF)[1]
n2<-dim(SWFTP)[1]
nn<-n+1
for(j in 1:n2){
  SWFTP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(SWFTP$Date[j]>=SWFTF$Date[i]){
      SWFTP$MC[j]<- SWFTF[i,2]*SWFTP$Close[j]
    } 
    else {SWFTP$MC[j]<-SWFTP$MC[j]}      
  }
}
SWFTP$Date<- as.Date(SWFTP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, SWFTP$MC)


n<-dim(AHGPF)[1]
n2<-dim(AHGPP)[1]
nn<-n+1
for(j in 1:n2){
  AHGPP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(AHGPP$Date[j]>=AHGPF$Date[i]){
      AHGPP$MC[j]<- AHGPF[i,2]*AHGPP$Close[j]
    } 
    else {AHGPP$MC[j]<-AHGPP$MC[j]}      
  }
}
AHGPP$Date<- as.Date(AHGPP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, AHGPP$MC)


n<-dim(GESF)[1]
n2<-dim(GESP)[1]
nn<-n+1
for(j in 1:n2){
  GESP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(GESP$Date[j]>=GESF$Date[i]){
      GESP$MC[j]<- GESF[i,2]*GESP$Close[j]
    } 
    else {GESP$MC[j]<-GESP$MC[j]}      
  }
}
GESP$Date<- as.Date(GESP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, GESP$MC)


n<-dim(WSTCF)[1]
n2<-dim(WSTCP)[1]
nn<-n+1
for(j in 1:n2){
  WSTCP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(WSTCP$Date[j]>=WSTCF$Date[i]){
      WSTCP$MC[j]<- WSTCF[i,2]*WSTCP$Close[j]
    } 
    else {WSTCP$MC[j]<-WSTCP$MC[j]}      
  }
}
WSTCP$Date<- as.Date(WSTCP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, WSTCP$MC)


n<-dim(WERNF)[1]
n2<-dim(WERNP)[1]
nn<-n+1
for(j in 1:n2){
  WERNP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(WERNP$Date[j]>=WERNF$Date[i]){
      WERNP$MC[j]<- WERNF[i,2]*WERNP$Close[j]
    } 
    else {WERNP$MC[j]<-WERNP$MC[j]}      
  }
}
WERNP$Date<- as.Date(WERNP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, WERNP$MC)


n<-dim(BKEF)[1]
n2<-dim(BKEP)[1]
nn<-n+1
for(j in 1:n2){
  BKEP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(BKEP$Date[j]>=BKEF$Date[i]){
      BKEP$MC[j]<- BKEF[i,2]*BKEP$Close[j]
    } 
    else {BKEP$MC[j]<-BKEP$MC[j]}      
  }
}
BKEP$Date<- as.Date(BKEP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, BKEP$MC)


n<-dim(USNAF)[1]
n2<-dim(USNAP)[1]
nn<-n+1
for(j in 1:n2){
  USNAP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(USNAP$Date[j]>=USNAF$Date[i]){
      USNAP$MC[j]<- USNAF[i,2]*USNAP$Close[j]
    } 
    else {USNAP$MC[j]<-USNAP$MC[j]}      
  }
}
USNAP$Date<- as.Date(USNAP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, USNAP$MC)
#View(combo)

n<-dim(TTECF)[1]
n2<-dim(TTECP)[1]
nn<-n+1
for(j in 1:n2){
  TTECP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(TTECP$Date[j]>=TTECF$Date[i]){
      TTECP$MC[j]<- TTECF[i,2]*TTECP$Close[j]
    } 
    else {TTECP$MC[j]<-TTECP$MC[j]}      
  }
}
TTECP$Date<- as.Date(TTECP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, TTECP$MC)
#View(combo)

n<-dim(WMKF)[1]
n2<-dim(WMKP)[1]
nn<-n+1
for(j in 1:n2){
  WMKP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(WMKP$Date[j]>=WMKF$Date[i]){
      WMKP$MC[j]<- WMKF[i,2]*WMKP$Close[j]
    } 
    else {WMKP$MC[j]<-WMKP$MC[j]}      
  }
}
WMKP$Date<- as.Date(WMKP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, WMKP$MC)
#View(combo)

n<-dim(AMKRF)[1]
n2<-dim(AMKRP)[1]
nn<-n+1
for(j in 1:n2){
  AMKRP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(AMKRP$Date[j]>=AMKRF$Date[i]){
      AMKRP$MC[j]<- AMKRF[i,2]*AMKRP$Close[j]
    } 
    else {AMKRP$MC[j]<-AMKRP$MC[j]}      
  }
}
AMKRP$Date<- as.Date(AMKRP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, AMKRP$MC)
#View(combo)

n<-dim(HYF)[1]
n2<-dim(HYP)[1]
nn<-n+1
for(j in 1:n2){
  HYP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(HYP$Date[j]>=HYF$Date[i]){
      HYP$MC[j]<- HYF[i,2]*HYP$Close[j]
    } 
    else {HYP$MC[j]<-HYP$MC[j]}      
  }
}
HYP$Date<- as.Date(HYP$Date)
#combo<- merge(GPSP, ORCLP, by="Date")
combo<- cbind(combo, HYP$MC)
#View(combo)

n<-dim(NHCF)[1]
n2<-dim(NHCP)[1]
nn<-n+1
for(j in 1:n2){
  NHCP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(NHCP$Date[j]>=NHCF$Date[i]){
      NHCP$MC[j]<- NHCF[i,2]*NHCP$Close[j]
    } 
    else {NHCP$MC[j]<-NHCP$MC[j]}      
  }
}
NHCP$Date<- as.Date(NHCP$Date)
combo<- cbind(combo, NHCP$MC)

n<-dim(TPCF)[1]
n2<-dim(TPCP)[1]
nn<-n+1
for(j in 1:n2){
  TPCP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(TPCP$Date[j]>=TPCF$Date[i]){
      TPCP$MC[j]<- TPCF[i,2]*TPCP$Close[j]
    } 
    else {TPCP$MC[j]<-TPCP$MC[j]}      
  }
}
TPCP$Date<- as.Date(TPCP$Date)
combo<- cbind(combo, TPCP$MC)

n<-dim(QSIIF)[1]
n2<-dim(QSIIP)[1]
nn<-n+1
for(j in 1:n2){
  QSIIP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(QSIIP$Date[j]>=QSIIF$Date[i]){
      QSIIP$MC[j]<- QSIIF[i,2]*QSIIP$Close[j]
    } 
    else {QSIIP$MC[j]<-QSIIP$MC[j]}      
  }
}
QSIIP$Date<- as.Date(QSIIP$Date)
combo<- cbind(combo, QSIIP$MC)

n<-dim(FLWSF)[1]
n2<-dim(FLWSP)[1]
nn<-n+1
for(j in 1:n2){
  FLWSP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(FLWSP$Date[j]>=FLWSF$Date[i]){
      FLWSP$MC[j]<- FLWSF[i,2]*FLWSP$Close[j]
    } 
    else {FLWSP$MC[j]<-FLWSP$MC[j]}      
  }
}
FLWSP$Date<- as.Date(FLWSP$Date)
combo<- cbind(combo, FLWSP$MC)

n<-dim(MRTNF)[1]
n2<-dim(MRTNP)[1]
nn<-n+1
for(j in 1:n2){
  MRTNP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(MRTNP$Date[j]>=MRTNF$Date[i]){
      MRTNP$MC[j]<- MRTNF[i,2]*MRTNP$Close[j]
    } 
    else {MRTNP$MC[j]<-MRTNP$MC[j]}      
  }
}
MRTNP$Date<- as.Date(MRTNP$Date)
combo<- cbind(combo, MRTNP$MC)

n<-dim(UACLF)[1]
n2<-dim(UACLP)[1]
nn<-n+1
for(j in 1:n2){
  UACLP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(UACLP$Date[j]>=UACLF$Date[i]){
      UACLP$MC[j]<- UACLF[i,2]*UACLP$Close[j]
    } 
    else {UACLP$MC[j]<-UACLP$MC[j]}      
  }
}
UACLP$Date<- as.Date(UACLP$Date)
combo<- cbind(combo, UACLP$MC)

n<-dim(PCCCF)[1]
n2<-dim(PCCCP)[1]
nn<-n+1
for(j in 1:n2){
  PCCCP$MC[j]<- 0
} 
for (i in 1:n)   {
  i<-nn-i
  for(j in 1:n2){
    if(PCCCP$Date[j]>=PCCCF$Date[i]){
      PCCCP$MC[j]<- PCCCF[i,2]*PCCCP$Close[j]
    } 
    else {PCCCP$MC[j]<-PCCCP$MC[j]}      
  }
}
PCCCP$Date<- as.Date(PCCCP$Date)
combo<- cbind(combo, PCCCP$MC)


#Create a matrix of weights for each stock over time


weights<- rowSums(combo)
#View(weights)
n<- dim(combo)[2]
n2<- length(weights)


weightmatrix<- matrix(nrow=n2, ncol=n)

for (i in 1:n){
      for(j in 1:n2){
        weightmatrix[j,i]<-combo[j,i]/weights[j]
      }
}
#View(weightmatrix)
cap<- 0.075
K<-1
for (k in 1:4){
  for (i in 1:n){
      for(j in 1:n2){
        if(weightmatrix[j,i] > cap){weightmatrix[j,i]<-cap}
    }
  }
  weights<-rowSums(weightmatrix)

  for (i in 1:n){
      for(j in 1:n2){
        weightmatrix[j,i]<-weightmatrix[j,i]/weights[j]
    }
  }
  k<- k+1
}

weightmatrix<- head(weightmatrix, d[1] )

View(weightmatrix)
d2<- dim(weightmatrix)[1]


#claculating the result percentual changes & weitght matrix added row.

#print(d2)
verify<-rowSums(weightmatrix)
#View(verify)
result<- changes*weightmatrix
index<- rowSums(result)


#basic quantitative calculations:

variance<-var(index)
indexacum<- index+1

print("Variance index:")
print(variance)

print("Variance Spy:")
variancespy<-var(p0)
print(variancespy)
spyacum<- p0+1


dd2<-length(indexacum)
dimspy<- length(spyacum)

#print(dimspy)
indexcor<-index[-dd2]
k<-dd2-1
k2<-dimspy-1
indexcor<-indexcor[-k]
DD3<-length(indexcor)
#print(DD3)



corr<- cor(indexcor, p0)
cova<- cov(indexcor, p0)
print("correlation index x spy:")
print(corr)
print("covariance index x spy:")
print(cova)

test2<- tail(indexacum,1000)
indexfinal<- indexacum
#View(test2)
indexfinal[dd2]<-1
#test2<- tail(indexfinal,1000)
#View(test2)

for(i in 1:k){
       n<- dd2-i
#      n2<-dd2-i+1
       indexfinal[n]<- prod(indexacum[k:n])
      }

spyfinal<- spyacum
spyfinal[dimspy]<- 1


for(i in 1:k2){
      n<- dimspy-i
      spyfinal[n]<- prod(spyacum[k2:n])
}

newspyfinal<- head(spyfinal, 1650)
newindexfinal<- head(indexfinal, 1650)

#View(spyfinal)
spyfinal<- rev(spyfinal)


#plot the chart:

#test<- tail(indexfinal,1000)
#View(test)
indexfinal<- rev(indexfinal)
plot(indexfinal,col="red", type="l")
lines(spyfinal, col="blue", type="l")




