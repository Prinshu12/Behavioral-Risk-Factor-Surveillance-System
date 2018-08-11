
library(readxl)
library(Hmisc)
library(rpart)
library(rpart.plot)
#library(rattle)
library(plyr)
library(VIM)
library(beepr)

BRFSS_2014<-read.csv(file.choose(),header=T)

Col_2014<- read.csv(file.choose(),header=T)

row.names(Col_2014)<-Col_2014[,1]

BRFSS_2014<-BRFSS_2014[,noquote(ifelse(substr(noquote(colnames(BRFSS_2014)),1,2)=='X_',substr(noquote(colnames(BRFSS_2014)),2,nchar(noquote(colnames(BRFSS_2014)))),noquote(colnames(BRFSS_2014)))) %in% noquote(row.names(Col_2014))]

dim(BRFSS_2014)

summary(BRFSS_2014)

colnames(BRFSS_2014)<- ifelse(substr(noquote(colnames(BRFSS_2014)),1,2)=='X_',substr(noquote(colnames(BRFSS_2014)),3,nchar(noquote(colnames(BRFSS_2014)))),noquote(colnames(BRFSS_2014)))

colnames(BRFSS_2014)

BRFSS_2014<-BRFSS_2014[ , order(colnames(BRFSS_2014))]

head(BRFSS_2014)

#####################################

BRFSS_2013<-read.csv(file.choose(),header=T)

BRFSS_2013<-BRFSS_2013[,noquote(ifelse(substr(noquote(colnames(BRFSS_2013)),1,2)=='X_',substr(noquote(colnames(BRFSS_2013)),2,nchar(noquote(colnames(BRFSS_2013)))),noquote(colnames(BRFSS_2013)))) %in% noquote(row.names(Col_2014))]

dim(BRFSS_2013)

summary(BRFSS_2013)

colnames(BRFSS_2013)<- ifelse(substr(noquote(colnames(BRFSS_2013)),1,2)=='X_',substr(noquote(colnames(BRFSS_2013)),3,nchar(noquote(colnames(BRFSS_2013)))),noquote(colnames(BRFSS_2013)))

colnames(BRFSS_2013)

BRFSS_2013<-BRFSS_2013[ , order(colnames(BRFSS_2013))]

head(BRFSS_2013)

#####################################

BRFSS_2012<-read.csv(file.choose(),header=T)

BRFSS_2012<-BRFSS_2012[,noquote(ifelse(substr(noquote(colnames(BRFSS_2012)),1,2)=='X_',substr(noquote(colnames(BRFSS_2012)),2,nchar(noquote(colnames(BRFSS_2012)))),noquote(colnames(BRFSS_2012)))) %in% noquote(row.names(Col_2014))]

dim(BRFSS_2012)

summary(BRFSS_2012)

colnames(BRFSS_2012)<- ifelse(substr(noquote(colnames(BRFSS_2012)),1,2)=='X_',substr(noquote(colnames(BRFSS_2012)),3,nchar(noquote(colnames(BRFSS_2012)))),noquote(colnames(BRFSS_2012)))

colnames(BRFSS_2012)

BRFSS_2012<-BRFSS_2012[ , order(colnames(BRFSS_2012))]

head(BRFSS_2012)

#####################################

BRFSS_2011<-read.csv(file.choose(),header=T)

BRFSS_2011<-BRFSS_2011[,noquote(ifelse(substr(noquote(colnames(BRFSS_2011)),1,2)=='X_',substr(noquote(colnames(BRFSS_2011)),2,nchar(noquote(colnames(BRFSS_2011)))),noquote(colnames(BRFSS_2011)))) %in% noquote(row.names(Col_2014))]

dim(BRFSS_2011)

summary(BRFSS_2011)

colnames(BRFSS_2011)<- ifelse(substr(noquote(colnames(BRFSS_2011)),1,2)=='X_',substr(noquote(colnames(BRFSS_2011)),3,nchar(noquote(colnames(BRFSS_2011)))),noquote(colnames(BRFSS_2011)))

colnames(BRFSS_2011)

BRFSS_2011<-BRFSS_2011[ , order(colnames(BRFSS_2011))]

head(BRFSS_2011)

####################################

BRFSS_2015<-read.csv(file.choose(),header=T)

BRFSS_2015<-BRFSS_2015[,noquote(ifelse(substr(noquote(colnames(BRFSS_2015)),1,2)=='X_',substr(noquote(colnames(BRFSS_2015)),2,nchar(noquote(colnames(BRFSS_2015)))),noquote(colnames(BRFSS_2015)))) %in% noquote(row.names(Col_2014))]

dim(BRFSS_2015)

summary(BRFSS_2015)

colnames(BRFSS_2015)<- ifelse(substr(noquote(colnames(BRFSS_2015)),1,2)=='X_',substr(noquote(colnames(BRFSS_2015)),3,nchar(noquote(colnames(BRFSS_2015)))),noquote(colnames(BRFSS_2015)))

colnames(BRFSS_2015)

BRFSS_2015<-BRFSS_2015[ , order(colnames(BRFSS_2015))]

head(BRFSS_2015)

####################################

BRFSS<-rbind(BRFSS_2011,BRFSS_2012,BRFSS_2013,BRFSS_2014,BRFSS_2015)

dim(BRFSS)

BRFSS1<-BRFSS

#AGE65YR
BRFSS1$AGE65YR<- ifelse(BRFSS1$AGE65YR==1,"Yes",
                         ifelse(BRFSS1$AGE65YR==2,"No",""))

#ASTHMS1
BRFSS1$ASTHMS1<- ifelse(BRFSS1$ASTHMS1==1,"current",
                        ifelse(BRFSS1$ASTHMS1==2,"former",
                               ifelse(BRFSS1$ASTHMS1==3,"never","")))

#BMI5
BRFSS1$BMI5<- BRFSS1$BMI5/100

#CASTHM1
BRFSS1$CASTHM1<- ifelse(BRFSS1$CASTHM1==1,"No",
                        ifelse(BRFSS1$CASTHM1==2,"Yes",""))

#CHLDCNT
BRFSS1$CHLDCNT<- ifelse(BRFSS1$CHLDCNT==1,"no child",
                        ifelse(BRFSS1$CHLDCNT==2,"1",
                               ifelse(BRFSS1$CHLDCNT==3,"2",
                                      ifelse(BRFSS1$CHLDCNT==4,"3",
                                             ifelse(BRFSS1$CHLDCNT==5,"4",
                                                    ifelse(BRFSS1$CHLDCNT==6,"5",""))))))

#CLLCPWT
BRFSS1$CLLCPWT<- NULL

#DRDXAR1
BRFSS1$DRDXAR1<- ifelse(BRFSS1$DRDXAR1==1,"Yes",
                        ifelse(BRFSS1$DRDXAR1==2,"No",""))

#EDUCAG
BRFSS1$EDUCAG<- ifelse(BRFSS1$EDUCAG==1,"no High School",
                       ifelse(BRFSS1$EDUCAG==2,"High School Grad",
                              ifelse(BRFSS1$EDUCAG==3,"Attended College","College Grad")))

#HCVU651
BRFSS1$HCVU651<- ifelse(BRFSS1$HCVU651==1,"Yes",
                        ifelse(BRFSS1$HCVU651==2,"No",""))

#INCOMG
BRFSS1$INCOMG<- ifelse(BRFSS1$INCOMG==1,"less than 15000",
                       ifelse(BRFSS1$INCOMG==2,"15-25",
                              ifelse(BRFSS1$INCOMG==3,"25-35",
                                     ifelse(BRFSS1$INCOMG==4,"35-50",
                                            ifelse(BRFSS1$INCOMG==5,"greater than 50000","")))))

#LLCPWT
BRFSS1$LLCPWT<- NULL

#LTASTH1
BRFSS1$LTASTH1<- ifelse(BRFSS1$LTASTH1==1,"No",
                       ifelse(BRFSS1$LTASTH1==2,"Yes",""))

#PNEUMO2
BRFSS1$PNEUMO2<- ifelse(BRFSS1$PNEUMO2==1,"Yes",
                        ifelse(BRFSS1$PNEUMO2==2,"No",""))

#PSU
BRFSS1$PSU<- NULL

#PNEUMO2
BRFSS1$PNEUMO2<- NULL

#RFBING5
BRFSS1$RFBING5<- ifelse(BRFSS1$RFBING5==1,"No",
                        ifelse(BRFSS1$RFBING5==2,"Yes",""))

#RFBMI5
BRFSS1$RFBMI5<- ifelse(BRFSS1$RFBMI5==1,"BMI =<25",
                       ifelse(BRFSS1$RFBMI5==2,"BMI >25",""))

#RFHLTH
BRFSS1$RFHLTH<- ifelse(BRFSS1$RFHLTH==1,"good or better health",
                       ifelse(BRFSS1$RFHLTH==2,"fair or poor helath",""))

#RFSEAT2
BRFSS1$RFSEAT2<- ifelse(BRFSS1$RFSEAT2==1,"always wear seat belt",
                        ifelse(BRFSS1$RFSEAT2==2,"seldom or never",""))

#RFSEAT3
BRFSS1$RFSEAT3<- NULL

#RFSMOK3
BRFSS1$RFSMOK3<- ifelse(BRFSS1$RFSMOK3==1,"No",
                        ifelse(BRFSS1$RFSMOK3==2,"Yes",""))

#SMOKER3
BRFSS1$SMOKER3<- ifelse(BRFSS1$SMOKER3==1,"current/smokes everyday",
                                        ifelse(BRFSS1$SMOKER3==2,"current/smokes somedays",
                                               ifelse(BRFSS1$SMOKER3==3,"former",
                                                      ifelse(BRFSS1$SMOKER3==4,"never",""))))

#STATE
BRFSS1$STATE<- ifelse(BRFSS1$STATE==1,"Alabama",
                      ifelse(BRFSS1$STATE==2,"Alaska",
                             ifelse(BRFSS1$STATE==4,"Arizona",
                                    ifelse(BRFSS1$STATE==5,"Arkansas",
                                           ifelse(BRFSS1$STATE==6,"California",
                                                  ifelse(BRFSS1$STATE==8,"Colorado",
                                                         ifelse(BRFSS1$STATE==9,"Connecticut",
                                                                ifelse(BRFSS1$STATE==10,"Delaware",
                                                                       ifelse(BRFSS1$STATE==11,"District of Columbia",
                                                                              ifelse(BRFSS1$STATE==12,"Florida",
                                                                                     ifelse(BRFSS1$STATE==13,"Georgia",
                                                                                            ifelse(BRFSS1$STATE==15,"Hawaii",
                                                                                                   ifelse(BRFSS1$STATE==16,"Idaho",
                                                                                                          ifelse(BRFSS1$STATE==17,"Illinois",
                                                                                                                 ifelse(BRFSS1$STATE==18,"Indiana",
                                                                                                                        ifelse(BRFSS1$STATE==19,"Iowa",
                                                                                                                               ifelse(BRFSS1$STATE==20,"Kansas",
                                                                                                                                      ifelse(BRFSS1$STATE==21,"Kentucky",
                                                                                                                                             ifelse(BRFSS1$STATE==22,"Louisiana",
                                                                                                                                                    ifelse(BRFSS1$STATE==23,"Maine",
                                                                                                                                                           ifelse(BRFSS1$STATE==24,"Maryland",
                                                                                                                                                                  ifelse(BRFSS1$STATE==25,"Massachusetts",
                                                                                                                                                                         ifelse(BRFSS1$STATE==26,"Michigan",
                                                                                                                                                                                ifelse(BRFSS1$STATE==27,"Minnesota",
                                                                                                                                                                                       ifelse(BRFSS1$STATE==28,"Mississippi",
                                                                                                                                                                                              ifelse(BRFSS1$STATE==29,"Missouri",
                                                                                                                                                                                                     ifelse(BRFSS1$STATE==30,"Montana",
                                                                                                                                                                                                            ifelse(BRFSS1$STATE==31,"Nebraska",
                                                                                                                                                                                                                   ifelse(BRFSS1$STATE==32,"Nevada",
                                                                                                                                                                                                                          ifelse(BRFSS1$STATE==33,"New Hampshire",
                                                                                                                                                                                                                                 ifelse(BRFSS1$STATE==34,"New Jersey",
                                                                                                                                                                                                                                        ifelse(BRFSS1$STATE==35,"New Mexico",
                                                                                                                                                                                                                                               ifelse(BRFSS1$STATE==36,"New York",
                                                                                                                                                                                                                                                      ifelse(BRFSS1$STATE==37,"North Carolina",
                                                                                                                                                                                                                                                             ifelse(BRFSS1$STATE==38,"North Dakota",
                                                                                                                                                                                                                                                                    ifelse(BRFSS1$STATE==39,"Ohio",
                                                                                                                                                                                                                                                                           ifelse(BRFSS1$STATE==40,"Oklahoma",
                                                                                                                                                                                                                                                                                  ifelse(BRFSS1$STATE==41,"Oregon ",""
                                                                                                                                                                                                                                                                                  ))))))))))))))))))))))))))))))))))))))

#STRWT
BRFSS1$STRWT<- NULL

#STSTR
BRFSS1$STSTR<- NULL

#TOTINDA
BRFSS1$TOTINDA<- ifelse(BRFSS1$TOTINDA==1,"Yes",
                        ifelse(BRFSS1$TOTINDA==2,"No",""))

#WT2RAKE
BRFSS1$WT2RAKE<- NULL

#ADDEPEV2
BRFSS1$ADDEPEV2<- ifelse(BRFSS1$ADDEPEV2==1,"Yes",
                         ifelse(BRFSS1$ADDEPEV2==2,"No",""))

#ALCDAY5 - Convert days per week to approx days per month considering 30 days in a month.
BRFSS1$ALCDAY5<- ifelse(BRFSS1$ALCDAY5 %in% c(100:199), round((BRFSS1$ALCDAY5-100)*4.29,digits=0),
                        ifelse(BRFSS1$ALCDAY5 %in% c(200:299),BRFSS1$ALCDAY5-200,
                               ifelse(BRFSS1$ALCDAY5 ==888,0,"")))

#ASACTLIM
BRFSS1$ASACTLIM<- ifelse(BRFSS1$ASACTLIM<=365,BRFSS1$ASACTLIM,
                         ifelse(BRFSS1$ASACTLIM==888,0,""))

#ASATTACK
BRFSS1$ASATTACK<- ifelse(BRFSS1$ASATTACK==1,"Yes",
                         ifelse(BRFSS1$ASATTACK==2,"No",""))

#ASDRVIST
BRFSS1$ASDRVIST<- ifelse(BRFSS1$ASDRVIST<=87,BRFSS1$ASDRVIST,
                         ifelse(BRFSS1$ASDRVIST==88,0,""))

#ASERVIST
BRFSS1$ASERVIST<- ifelse(BRFSS1$ASERVIST<=87,BRFSS1$ASERVIST,
                         ifelse(BRFSS1$ASERVIST==88,0,""))

#ASINHALR
BRFSS1$ASINHALR<- ifelse(BRFSS1$ASINHALR==1,"1-4",
                         ifelse(BRFSS1$ASINHALR==2,"5-14",
                                ifelse(BRFSS1$ASINHALR==3,"15-29",
                                       ifelse(BRFSS1$ASINHALR==4,"30-59",
                                              ifelse(BRFSS1$ASINHALR==5,"60-99",
                                                     ifelse(BRFSS1$ASINHALR==6,">=100",
                                                            ifelse(BRFSS1$ASINHALR==8,"0","")))))))

#ASTHMA3
BRFSS1$ASTHMA3<- ifelse(BRFSS1$ASTHMA3==1,"Yes",
                        ifelse(BRFSS1$ASTHMA3==2,"No",""))

#ASTHMAGE
BRFSS1$ASTHMAGE<- ifelse(BRFSS1$ASTHMAGE==97,"<=10",
                         ifelse(BRFSS1$ASTHMAGE<97,BRFSS1$ASTHMAGE,""))

#ASTHMED3
BRFSS1$ASTHMED3<- ifelse(BRFSS1$ASTHMED3==1,"1-14",
                         ifelse(BRFSS1$ASTHMED3==2,"15-24",
                                ifelse(BRFSS1$ASTHMED3==3,"25-30",
                                       ifelse(BRFSS1$ASTHMED3==8,"0",""))))

#CADULT
BRFSS1$CADULT<- ifelse(BRFSS1$CADULT==1,"Yes/Male",
                       ifelse(BRFSS1$CADULT==2,"Yes/Female",""))

#CASTHDX2
BRFSS1$CASTHDX2<- ifelse(BRFSS1$CASTHDX2==1,"Yes",
                         ifelse(BRFSS1$CASTHDX2==2,"No",""))

#CASTHNO2
BRFSS1$CASTHNO2<- ifelse(BRFSS1$CASTHNO2==1,"Yes",
                         ifelse(BRFSS1$CASTHNO2==2,"No",""))

#CELLFON2
BRFSS1$CELLFON2<- ifelse(BRFSS1$CELLFON2==1,"Yes","")

#CHCKIDNY
BRFSS1$CHCKIDNY<- ifelse(BRFSS1$CHCKIDNY==1,"Yes",
                         ifelse(BRFSS1$CHCKIDNY==2,"No",""))

#CHCOCNCR
BRFSS1$CHCOCNCR<- ifelse(BRFSS1$CHCOCNCR==1,"Yes",
                         ifelse(BRFSS1$CHCOCNCR==2,"No",""))

#CHCSCNCR
BRFSS1$CHCSCNCR<- ifelse(BRFSS1$CHCOCNCR==1,"Yes",
                         ifelse(BRFSS1$CHCOCNCR==2,"No",""))

#CHECKUP1
BRFSS1$CHECKUP1<- ifelse(BRFSS1$CHECKUP1==1,"within past year",
                         ifelse(BRFSS1$CHECKUP1==2,"within past 2 years",
                                ifelse(BRFSS1$CHECKUP1==3,"within past 5 years",
                                       ifelse(BRFSS1$CHECKUP1==4,"5 or more years before",
                                              ifelse(BRFSS1$CHECKUP1==8,"never","")))))

#CHILDREN
BRFSS1$CHILDREN<- ifelse(BRFSS1$CHILDREN<=87,BRFSS1$CHILDREN,
                         ifelse(BRFSS1$CHILDREN==88,"0",""))

#CHKHEMO3
BRFSS1$CHKHEMO3<- ifelse(BRFSS1$CHKHEMO3<=76,BRFSS1$CHKHEMO3,
       ifelse(BRFSS1$CHKHEMO3==88,"0",
              ifelse(BRFSS1$CHKHEMO3==98,"never heard of the test","")))

#CPDEMO1
BRFSS1$CPDEMO1<- ifelse(BRFSS1$CPDEMO1==1,"Yes",
                        ifelse(BRFSS1$CPDEMO1==2,"No",""))

#CSTATE
BRFSS1$CSTATE<- ifelse(BRFSS1$CSTATE==1,"Yes",
                       ifelse(BRFSS1$CSTATE==2,"No",""))

#CTELENUM
BRFSS1$CTELENUM<- NULL

#CTELNUM1
BRFSS1$CTELNUM1<- NULL

#CVDCRHD4
BRFSS1$CVDCRHD4<- ifelse(BRFSS1$CVDCRHD4==1,"Yes",
                         ifelse(BRFSS1$CVDCRHD4==2,"No",""))

#CVDINFR4
BRFSS1$CVDINFR4<- ifelse(BRFSS1$CVDINFR4==1,"Yes",
                         ifelse(BRFSS1$CVDINFR4==2,"No",""))

#CVDSTRK3
BRFSS1$CVDSTRK3<- ifelse(BRFSS1$CVDSTRK3==1,"Yes",
                         ifelse(BRFSS1$CVDSTRK3==2,"No",""))

#DIABAGE2
BRFSS1$DIABAGE2<- ifelse(BRFSS1$DIABAGE2<=97,BRFSS1$DIABAGE2,"")

#DIABEDU
BRFSS1$DIABEDU<- ifelse(BRFSS1$DIABEDU==1,"Yes",
                        ifelse(BRFSS1$DIABEDU==2,"No",""))

#DIABETE3
BRFSS1$DIABETE3<- ifelse(BRFSS1$DIABETE3==1,"Yes",
                         ifelse(BRFSS1$DIABETE3==2,"Yes/female during pregnancy",
                                ifelse(BRFSS1$DIABETE3==3,"No",
                                       ifelse(BRFSS1$DIABETE3==2,"Borderline diabetes/No current diabetes",""))))

#DIABEYE
BRFSS1$DIABEYE<- ifelse(BRFSS1$DIABEYE==1,"Yes",
                        ifelse(BRFSS1$DIABEYE==2,"No",""))

#DISPCODE
BRFSS1$DISPCODE<- NULL

#DOCTDIAB
BRFSS1$DOCTDIAB<- ifelse(BRFSS1$DOCTDIAB==88,0,
                         ifelse(BRFSS1$DOCTDIAB<=76,BRFSS1$DOCTDIAB,""))

#DRNK3GE5
BRFSS1$DRNK3GE5<- ifelse(BRFSS1$DRNK3GE5==88,0,
                         ifelse(BRFSS1$DRNK3GE5<=76,BRFSS1$DRNK3GE5,""))

#DRNKANY5
BRFSS1$DRNKANY5<- ifelse(BRFSS1$DRNKANY5==1,"Yes",
                         ifelse(BRFSS1$DRNKANY5==2,"No",""))

#DROCDY3_
BRFSS1$DROCDY3_<- NULL

#EDUCA
BRFSS1$EDUCA<-ifelse(BRFSS1$EDUCA==1,"never attended school",
                     ifelse(BRFSS1$EDUCA==2,"1-8 elementary",
                            ifelse(BRFSS1$EDUCA==3,"9-11 high school",
                                   ifelse(BRFSS1$EDUCA==4,"grade 12 or higher",
                                          ifelse(BRFSS1$EDUCA==5,"college year 1-3",
                                                 ifelse(BRFSS1$EDUCA==6,"college year 4 or graduate",""))))))

#EMTSUPRT
BRFSS1$EMTSUPRT<- ifelse(BRFSS1$EMTSUPRT==1,"always",
                         ifelse(BRFSS1$EMTSUPRT==2,"usually",
                                ifelse(BRFSS1$EMTSUPRT==3,"sometimes",
                                       ifelse(BRFSS1$EMTSUPRT==4,"rarely",
                                              ifelse(BRFSS1$EMTSUPRT==5,"never","")))))

#EXERANY2
BRFSS1$EXERANY2<- ifelse(BRFSS1$EXERANY2==1,"Yes",
                         ifelse(BRFSS1$EXERANY2==2,"No",""))

#EYEEXAM
BRFSS1$EYEEXAM<- ifelse(BRFSS1$EYEEXAM==1,"within past one month",
                        ifelse(BRFSS1$EYEEXAM==2,"within past year",
                               ifelse(BRFSS1$EYEEXAM==3,"within past 2 years",
                                      ifelse(BRFSS1$EYEEXAM==4,"2 or more years",
                                             ifelse(BRFSS1$EYEEXAM==8,"never","")))))

#FEETCHK
BRFSS1$FEETCHK<- ifelse(BRFSS1$FEETCHK==88,0,
                        ifelse(BRFSS1$FEETCHK<=76,BRFSS1$DOCTDIAB,""))

#FEETCHK2
BRFSS1$FEETCHK2<- ifelse(BRFSS1$FEETCHK2 %in% c(100:199), round((BRFSS1$FEETCHK2-100)*365,digits=0),
                         ifelse(BRFSS1$FEETCHK2 %in% c(200:299),round((BRFSS1$FEETCHK2-200)*(365/7),digits=0),
                                ifelse(BRFSS1$FEETCHK2 %in% c(300:399),round((BRFSS1$FEETCHK2-300)*12,digits=0),
                                       ifelse(BRFSS1$FEETCHK2 %in% c(400:499),BRFSS1$FEETCHK2,
                                              ifelse(BRFSS1$FEETCHK2 ==888,0,"")))))

#AGE_G
BRFSS1$AGE_G<- ifelse(BRFSS1$AGE_G ==1, "18-24",
                      ifelse(BRFSS1$AGE_G ==2,"25-34",
                             ifelse(BRFSS1$AGE_G ==3,"35-44",
                                    ifelse(BRFSS1$AGE_G ==4,"45-54",
                                           ifelse(BRFSS1$AGE_G ==5,"55-64",">=65")))))

#AGEG5YR
BRFSS1$AGEG5YR<- ifelse(BRFSS1$AGEG5YR ==1, "18-24",
                        ifelse(BRFSS1$AGEG5YR ==2,"25-29",
                               ifelse(BRFSS1$AGEG5YR ==3,"30-34",
                                      ifelse(BRFSS1$AGEG5YR ==4,"35-39",
                                             ifelse(BRFSS1$AGEG5YR ==5,"40-44",
                                                    ifelse(BRFSS1$AGEG5YR ==6,"44-49",
                                                           ifelse(BRFSS1$AGEG5YR ==7,"50-54",
                                                                  ifelse(BRFSS1$AGEG5YR ==8,"55-59",
                                                                         ifelse(BRFSS1$AGEG5YR ==9,"60-64",
                                                                                ifelse(BRFSS1$AGEG5YR ==10,"65-69",
                                                                                       ifelse(BRFSS1$AGEG5YR ==11,"70-74",
                                                                                              ifelse(BRFSS1$AGEG5YR ==12,"75-79",
                                                                                                     ifelse(BRFSS1$AGEG5YR ==13,">=80","")))))))))))))

#AIDTST3
BRFSS1$AIDTST3<- ifelse(BRFSS1$AIDTST3==1,"Yes",
                        ifelse(BRFSS1$AIDTST3==2,"No",""))

#BMI5CAT
BRFSS1$BMI5CAT<- ifelse(BRFSS1$BMI5CAT==1,"Underweight (BMI<18.50)",
                        ifelse(BRFSS1$BMI5CAT==2,"Normal Weight (18.50<=BMI< 25.00)",
                               ifelse(BRFSS1$BMI5CAT==3,"Overweight (25.00<=BMI<30.00)",
                                      ifelse(BRFSS1$BMI5CAT==4,"Obese (30.00<=BMI<99.99)",""))))

#ASNOSLEP
BRFSS1$ASNOSLEP<- ifelse(BRFSS1$ASNOSLEP ==1, "1-2",
                         ifelse(BRFSS1$ASNOSLEP ==2,"3-4",
                                ifelse(BRFSS1$ASNOSLEP ==3,"5",
                                       ifelse(BRFSS1$ASNOSLEP ==4,"6-10",
                                              ifelse(BRFSS1$ASNOSLEP ==5,">=10",
                                                     ifelse(BRFSS1$ASNOSLEP ==8,"0",""))))))

#ASRCHKUP
BRFSS1$ASRCHKUP<- ifelse(BRFSS1$ASRCHKUP==88,0,
                         ifelse(BRFSS1$ASRCHKUP<=86,BRFSS1$ASRCHKUP,""))

#ASTHNOW
BRFSS1$ASTHNOW<- ifelse(BRFSS1$ASTHNOW==1,"Yes",
                        ifelse(BRFSS1$ASTHNOW==2,"No",""))

#ASYMPTOM
BRFSS1$ASYMPTOM<- ifelse(BRFSS1$ASYMPTOM ==1, "less than 1 in a week",
                         ifelse(BRFSS1$ASYMPTOM ==2,"1-2/week",
                                ifelse(BRFSS1$ASYMPTOM ==3,"3-6/week",
                                       ifelse(BRFSS1$ASYMPTOM ==4,"everyday but not all the time",
                                              ifelse(BRFSS1$ASYMPTOM ==5,"everyday all the time",
                                                     ifelse(BRFSS1$ASYMPTOM ==8,"never",""))))))

#AVEDRNK2
BRFSS1$AVEDRNK2<- ifelse(BRFSS1$AVEDRNK2<=76,BRFSS1$ASRCHKUP,"")

#BLDSTOOL
BRFSS1$BLDSTOOL<- ifelse(BRFSS1$BLDSTOOL==1,"Yes",
                         ifelse(BRFSS1$BLDSTOOL==2,"No",""))

#BLDSUGAR
BRFSS1$BLDSUGAR<- ifelse(BRFSS1$BLDSUGAR %in% c(100:199), round((BRFSS1$BLDSUGAR-100)*365,digits=0),
                         ifelse(BRFSS1$BLDSUGAR %in% c(200:299),round((BRFSS1$BLDSUGAR-200)*(365/7),digits=0),
                                ifelse(BRFSS1$BLDSUGAR %in% c(300:399),round((BRFSS1$BLDSUGAR-300)*12,digits=0),
                                       ifelse(BRFSS1$BLDSUGAR %in% c(400:499),BRFSS1$BLDSUGAR,
                                              ifelse(BRFSS1$BLDSUGAR ==888,0,"")))))



#FLSHTMY2
BRFSS1$FLSHTMY2<-ifelse(BRFSS1$FLSHTMY2==777777,"never",
                        ifelse(BRFSS1$FLSHTMY2<999999,paste(substr(BRFSS1$FLSHTMY2, 1, ifelse(nchar(BRFSS1$FLSHTMY2)==6,2,1)),"-",substr(BRFSS1$FLSHTMY2, ifelse(nchar(BRFSS1$FLSHTMY2)==6,3,2), nchar(BRFSS1$FLSHTMY2))),""))

#GENHLTH
BRFSS1$GENHLTH<-ifelse(BRFSS1$GENHLTH==1,"Excellent",
                       
                       ifelse(BRFSS1$GENHLTH==2,"Very Good",
                              
                              ifelse(BRFSS1$GENHLTH==3,"Good",
                                     ifelse(BRFSS1$GENHLTH==4,"Fair",
                                            ifelse(BRFSS1$GENHLTH==5,"poor","")))))


#HADHYST2
BRFSS1$HADHYST2<-ifelse(BRFSS1$HADHYST2==1,"Yes",
                        ifelse(BRFSS1$HADHYST2==2,"No",""))

#HADMAM
BRFSS1$HADMAM<-ifelse(BRFSS1$HADMAM==1,"Yes",
                      ifelse(BRFSS1$HADMAM==2,"No",""))

#HADPAP2
BRFSS1$HADPAP2<-ifelse(BRFSS1$HADPAP2==1,"Yes",
                       ifelse(BRFSS1$HADPAP2==2,"No",""))


#HADSGCO1
BRFSS1$HADSGCO1<-ifelse(BRFSS1$HADSGCO1==1,"Sigmoidal",
                        ifelse(BRFSS1$HADSGCO1==2,"Colonoscopy",""))

#HADSIGM3
BRFSS1$HADSIGM3<-ifelse(BRFSS1$HADSIGM3==1,"Yes",
                        ifelse(BRFSS1$HADSIGM3==2,"No",""))

#HAVARTH3
BRFSS1$HAVARTH3<-ifelse(BRFSS1$HAVARTH3==1,"Yes",
                        ifelse(BRFSS1$HAVARTH3==2,"No",""))

#HEIGHT3
BRFSS1$HEIGHT3<-ifelse(BRFSS1$HEIGHT3<=800,((as.numeric(substr(BRFSS1$HEIGHT3,1,1))*12) +  as.numeric(substr(BRFSS1$HEIGHT3,2,3)))*2.54,
                       ifelse(BRFSS1$HEIGHT3 %in% c(9000:9998),substr(BRFSS1$HEIGHT3,2,4),""))

#HIVTST6
BRFSS1$HIVTST6<-ifelse(BRFSS1$HIVTST6==1,"Yes",
                       ifelse(BRFSS1$HIVTST6==2,"No",""))

#HIVTSTD3
BRFSS1$HIVTSTD3-ifelse(BRFSS1$HIVTSTD3<777777,paste(substr(BRFSS1$HIVTSTD3, 1, ifelse(nchar(BRFSS1$HIVTSTD3)==6,2,1)),"-",substr(BRFSS1$HIVTSTD3, ifelse(nchar(BRFSS1$HIVTSTD3)==6,3,2), nchar(BRFSS1$HIVTSTD3))),"")

#HLTHPLN1
BRFSS1$HLTHPLN1<- ifelse(BRFSS1$HLTHPLN1==1,"Yes",
                         ifelse(BRFSS1$HLTHPLN1==2,"No",""))


#HOWLONG
BRFSS1$HOWLONG<- ifelse(BRFSS1$HLTHPLN1==1,"within 1 year",
                        ifelse(BRFSS1$HLTHPLN1==2,"1-2 years",
                               ifelse(BRFSS1$HLTHPLN1==2,"2-3 years",
                                      ifelse(BRFSS1$HLTHPLN1==2,"3-5 years",
                                             ifelse(BRFSS1$HLTHPLN1==2,"before 5 years","")))))

#HPVADSHT
BRFSS1$HPVADSHT<- ifelse(BRFSS1$HPVADSHT==3,"all shots taken",
                         ifelse(BRFSS1$HPVADSHT<=2,paste(BRFSS1$HPVADSHT,"shots taken"),""))

#HPVADVC2
BRFSS1$HPVADVC2<- ifelse(BRFSS1$HPVADVC2==1,"Yes",
                         ifelse(BRFSS1$HPVADVC2==2,"No",""))

#HTIN4
BRFSS1$HTIN4<- NULL

#HTM4
BRFSS1$HTM4<- NULL

#IDATE
BRFSS1$IDATE<- NULL

#IMFVPLAC
BRFSS1$IMFVPLAC<-ifelse(BRFSS1$IMFVPLAC==1,"Doctor's office/Health Management Organization",
                        
                        ifelse(BRFSS1$IMFVPLAC==2,"A health department",
                               
                               ifelse(BRFSS1$IMFVPLAC==3,"Another type of clinic or health center",
                                      ifelse(BRFSS1$IMFVPLAC==4,"A senior,recreation or community center",
                                             ifelse(BRFSS1$IMFVPLAC==5,"A store",
                                                    ifelse(BRFSS1$IMFVPLAC==6,"A hospital(e.g. in patient)",
                                                           ifelse(BRFSS1$IMFVPLAC==7,"An emergency room",
                                                                  ifelse(BRFSS1$IMFVPLAC==8,"Workplace",
                                                                         ifelse(BRFSS1$IMFVPLAC==9,"Some other kind of place",
                                                                                ifelse(BRFSS1$IMFVPLAC==10,"Received vaccination in Canada/Mexico",
                                                                                       ifelse(BRFSS1$IMFVPLAC==11,"A school","")))))))))))






#INCOME2
BRFSS1$INCOME2<-ifelse(BRFSS1$INCOME2==1,"Less than 10,000",
                       ifelse(BRFSS1$INCOME2==2,"Less than 15,000",
                              ifelse(BRFSS1$INCOME2==3,"Less than 20,000",
                                     ifelse(BRFSS1$INCOME2==4,"Less than 25,000",
                                            ifelse(BRFSS1$INCOME2==5,"Less than 35,000",
                                                   ifelse(BRFSS1$INCOME2==6,"Less than 50,000",
                                                          ifelse(BRFSS1$INCOME2==7,"Less than 75,000",
                                                                 ifelse(BRFSS1$INCOME2==8,"75,000 or more",""))))))))


#INSULIN
BRFSS1$INSULIN<-ifelse(BRFSS1$INSULIN==1,"Yes",
                       ifelse(BRFSS1$INSULIN==2,"No",""))


#IYEAR
BRFSS1$IYEAR<-NULL


#LANDLINE
BRFSS1$LANDLINE<-ifelse(BRFSS1$LANDLINE==1,"Yes",
                        ifelse(BRFSS1$LANDLINE==2," No",""))

#LASTPAP2
BRFSS1$LASTPAP2<-ifelse(BRFSS1$LASTPAP2==1,"Within 1 year",
                        ifelse(BRFSS1$LASTPAP2==2,"1 -2 years",
                               ifelse(BRFSS1$LASTPAP2==3,"2-3 years",
                                      ifelse(BRFSS1$LASTPAP2==4,"3-5 years",
                                             ifelse(BRFSS1$LASTPAP2==5,"before 5 years","")))))


#LASTSIG3
BRFSS1$LASTSIG3<-ifelse(BRFSS1$LASTSIG3==1,"Within 1 year",
                        ifelse(BRFSS1$LASTSIG3==2,"1-2 years",
                               ifelse(BRFSS1$LASTSIG3==3,"2-3 years",
                                      ifelse(BRFSS1$LASTSIG3==4,"3-5 years",
                                             ifelse(BRFSS1$LASTSIG3==5,"5-10 years",
                                                    ifelse(BRFSS1$LASTSIG3==6,"before 10 years",""))))))

#LASTSMK2
BRFSS1$LASTSMK2<-ifelse(BRFSS1$LASTSMK2==1,"Everyday",
                        ifelse(BRFSS1$LASTSMK2==2,"Somedays",
                               ifelse(BRFSS1$LASTSMK2==3,"Not at all","")))


#LENGEXAM
BRFSS1$LENGEXAM<-ifelse(BRFSS1$LENGEXAM==1,"Within 1 year",
                        ifelse(BRFSS1$LENGEXAM==2,"1 -2 years",
                               ifelse(BRFSS1$LENGEXAM==3,"2-3 years",
                                      ifelse(BRFSS1$LENGEXAM==4,"3-5 years",
                                             ifelse(BRFSS1$LENGEXAM==5,"before 5 years","")))))


#LSATISFY
BRFSS1$LSATISFY<-ifelse(BRFSS1$LSATISFY==1,"Very satisfied",
                        ifelse(BRFSS1$LSATISFY==2,"satisfied",
                               ifelse(BRFSS1$LSATISFY==3,"Dissatisfied",
                                      ifelse(BRFSS1$LSATISFY==4,"Very dissatisfied",""))))


#LSTBLDS3
BRFSS1$LSTBLDS3<-ifelse(BRFSS1$LSTBLDS3==1,"Within 1 year",
                        ifelse(BRFSS1$LSTBLDS3==2,"1 -2 years",
                               ifelse(BRFSS1$LSTBLDS3==3,"2-3 years",
                                      ifelse(BRFSS1$LSTBLDS3==4,"3-5 years",
                                             ifelse(BRFSS1$LSTBLDS3==5,"before 5 years","")))))


#MARITAL
BRFSS1$MARITAL<-ifelse(BRFSS1$MARITAL==1,"Married",
                       ifelse(BRFSS1$MARITAL==2,"Divorced",
                              ifelse(BRFSS1$MARITAL==3,"Widowed",
                                     ifelse(BRFSS1$MARITAL==4,"Separated",
                                            ifelse(BRFSS1$MARITAL==5,"Never married",
                                                   ifelse(BRFSS1$MARITAL==6,"A member of an unmarried couple",""))))))



#MAXDRNKS
BRFSS1$MAXDRNKS<-ifelse(BRFSS1$MAXDRNKS<=76,BRFSS1$MAXDRNKS,"")


#MEDCOST
BRFSS1$MEDCOST<-ifelse(BRFSS1$MEDCOST==1,"Yes",
                       ifelse(BRFSS1$MEDCOST==2,"No",""))

#MENTHLTH
BRFSS1$MENTHLTH<-ifelse(BRFSS1$MENTHLTH<=30,BRFSS1$MENTHLTH,
                        ifelse(BRFSS1$MENTHLTH==88,0,""))


#MSCODE
BRFSS1$MSCODE<-ifelse(BRFSS1$MSCODE==1,"In city center",
                      ifelse(BRFSS1$MSCODE==2,"outside city center but inside county",
                             ifelse(BRFSS1$MSCODE==3,"Inside the suburban county",
                                    ifelse(BRFSS1$MSCODE==5,"Not in an MSA",""))))


#NUMADULT
BRFSS1$NUMADULT<-ifelse(BRFSS1$NUMADULT<=99,BRFSS1$NUMADULT,"")


#NUMHHOL2
BRFSS1$NUMHHOL2<-ifelse(BRFSS1$NUMADULT==1,"Yes",
                        ifelse(BRFSS1$NUMADULT==2,"No",""))

#NUMMEN
BRFSS1$NUMMEN<-ifelse(BRFSS1$NUMMEN<=99,BRFSS1$NUMMEN,"")


#NUMPHON2
BRFSS1$NUMPHON2<-NULL


#NUMWOMEN
BRFSS1$NUMWOMEN<-ifelse(BRFSS1$NUMWOMEN<=99,BRFSS1$NUMWOMEN,"")


#PAINACT2
BRFSS1$PAINACT2<-NULL


#PDIABTST
BRFSS1$PDIABTST<-ifelse(BRFSS1$PDIABTST==1,"Yes",
                        ifelse(BRFSS1$PDIABTST==2,"No",""))


#PERSDOC2
BRFSS1$PERSDOC2<-ifelse(BRFSS1$PERSDOC2==1,"YEes, only 1",
                        ifelse(BRFSS1$PERSDOC2==2,"More than 1",
                               ifelse(BRFSS1$PERSDOC2==3,"no","")))

#PHYSHLTH
BRFSS1$PHYSHLTH<-ifelse(BRFSS1$PHYSHLTH<=30,BRFSS1$PHYSHLTH,
                        ifelse(BRFSS1$PHYSHLTH==88,0,""))

#PNEUVAC3
BRFSS1$PNEUVAC3<-ifelse(BRFSS1$PNEUVAC3==1,"Yes",
                        ifelse(BRFSS1$PNEUVAC3==2,"No",""))


#POORHLTH
BRFSS1$POORHLTH<-ifelse(BRFSS1$POORHLTH<=30,BRFSS1$POORHLTH,
                        ifelse(BRFSS1$POORHLTH==88,0,""))


#PREDIAB1
BRFSS1$PREDIAB1<-ifelse(BRFSS1$PREDIAB1==1,"Yes",
                        ifelse(BRFSS1$PREDIAB1==2,"Yes,during pregnency",
                               ifelse(BRFSS1$PREDIAB1==3,"No","")))


#PREGNANT
BRFSS1$PREGNANT<-ifelse(BRFSS1$PREGNANT==1,"Yes",
                        ifelse(BRFSS1$PREGNANT==2,"No",""))


#PROFEXAM
BRFSS1$PROFEXAM<-ifelse(BRFSS1$PROFEXAM==1,"Yes",
                        ifelse(BRFSS1$PROFEXAM==2,"No",""))

#PSATEST1
BRFSS1$PSATEST1<-ifelse(BRFSS1$PSATEST1==1,"Yes",
                        ifelse(BRFSS1$PSATEST1==2,"No",""))


#PSATIME
BRFSS1$PSATIME<-ifelse(BRFSS1$PSATIME==1,"Within 1 year",
                       ifelse(BRFSS1$PSATIME==2,"1 -2 years",
                              ifelse(BRFSS1$PSATIME==3,"2-3 years",
                                     ifelse(BRFSS1$PSATIME==4,"3-5 years",
                                            ifelse(BRFSS1$PSATIME==5,"before 5 years","")))))


#PVTRESD2
BRFSS1$PVTRESD2<-ifelse(BRFSS1$PVTRESD2==1,"Yes",
                        ifelse(BRFSS1$PVTRESD2==2,"No",""))


#QLACTLM2
BRFSS1$QLACTLM2<-ifelse(BRFSS1$QLACTLM2==1,"Yes",
                        ifelse(BRFSS1$QLACTLM2==2,"No",""))


#QLHLTH2
BRFSS1$QLHLTH2<-NULL


#QLMENTL2
BRFSS1$QLMENTL2<-NULL


#QLSTRES2
BRFSS1$QLSTRES2<-NULL


#QSTLANG
BRFSS1$QSTLANG<-ifelse(BRFSS1$QSTLANG==1,"English",
                       ifelse(BRFSS1$QSTLANG==2,"Spanish",
                              ifelse(BRFSS1$QSTLANG>=3,"other","")))


#RCSGENDR
BRFSS1$RCSGENDR<-ifelse(BRFSS1$RCSGENDR==1,"Boy",
                        ifelse(BRFSS1$RCSGENDR==2,"Girl",""))


#RCSRLTN2
BRFSS1$RCSRLTN2<-ifelse(BRFSS1$RCSRLTN2==1,"Parent",
                        ifelse(BRFSS1$RCSRLTN2==2,"Grandparent",
                               ifelse(BRFSS1$RCSRLTN2==3,"Foster Parent or Guardian",
                                      ifelse(BRFSS1$RCSRLTN2==4,"Sibling",
                                             ifelse(BRFSS1$RCSRLTN2==5,"Other relative",
                                                    ifelse(BRFSS1$RCSRLTN2==6,"Not related in any way",
                                                           ifelse(BRFSS1$RCSRLTN2==7,"Don't know not sure","")))))))



#RENTHOM1
BRFSS1$RENTHOM1<-ifelse(BRFSS1$RENTHOM1==1,"Parent",
                        ifelse(BRFSS1$RENTHOM1==2,"Own",
                               ifelse(BRFSS1$RENTHOM1==3,"Rent",
                                      ifelse(BRFSS1$RENTHOM1==4,"Other Arrangement",""))))


#SCNTLPAD
BRFSS1$SCNTLPAD<-ifelse(BRFSS1$SCNTLPAD==1,"By Salary",
                        ifelse(BRFSS1$SCNTLPAD==2,"Paid by hour",
                               ifelse(BRFSS1$SCNTLPAD==3,"Paid by job/task(commission)",
                                      ifelse(BRFSS1$SCNTLPAD==4,"Paid some other way",""))))



#SCNTLWK1
BRFSS1$SCNTLWK1<- ifelse(BRFSS1$SCNTLWK1==98,0,
                         ifelse(BRFSS1$SCNTLWK1<=96,BRFSS1$SCNTLWK1,""))


#SCNTPAID
BRFSS1$SCNTPAID<-NULL


#SCNTWRK1
BRFSS1$SCNTWRK1<-NULL


#SEATBELT
BRFSS1$SEATBELT<-ifelse(BRFSS1$SEATBELT==1,"always",
                        ifelse(BRFSS1$SEATBELT==2,"nearly always",
                               ifelse(BRFSS1$SEATBELT==3,"sometimes",
                                      ifelse(BRFSS1$SEATBELT==4,"seldom",
                                             ifelse(BRFSS1$SEATBELT==5,"never",
                                                    ifelse(BRFSS1$SEATBELT==8,"Never drive or ride a car",""))))))


#SEQNO
BRFSS1$SEQNO<-NULL


#SEX
BRFSS1$SEX<-ifelse(BRFSS1$SEX==1,"Male",
                   ifelse(BRFSS1$SEX==2,"Female",""))



#SMOKDAY2
BRFSS1$SMOKDAY2<-ifelse(BRFSS1$SMOKDAY2==1,"Everyday",
                        ifelse(BRFSS1$SMOKDAY2==2,"Somedays",
                               ifelse(BRFSS1$SMOKDAY2==3,"Not at all","")))



#SMOKE100
BRFSS1$SMOKE100<-ifelse(BRFSS1$SMOKE100==1,"Yes",
                        ifelse(BRFSS1$SMOKE100==2,"No",""))



#STOPSMK2
BRFSS1$STOPSMK2<-ifelse(BRFSS1$STOPSMK2==1,"Yes",
                        ifelse(BRFSS1$STOPSMK2==2,"No",""))


#USEEQUIP
BRFSS1$USEEQUIP<-ifelse(BRFSS1$USEEQUIP==1,"Yes",
                        ifelse(BRFSS1$USEEQUIP==2,"No",""))


#USENOW3
BRFSS1$USENOW3<-ifelse(BRFSS1$USENOW3==1,"Everyday",
                       ifelse(BRFSS1$USENOW3==2,"Somedays",
                              ifelse(BRFSS1$USENOW3==3,"Not at all","")))



#VETERAN3
BRFSS1$VETERAN3<-ifelse(BRFSS1$VETERAN3==1,"Yes",
                        ifelse(BRFSS1$VETERAN3==2,"No",""))


#WEIGHT2
BRFSS1$WEIGHT2<-ifelse(BRFSS1$WEIGHT2<=1000,round((as.numeric(BRFSS1$WEIGHT2)*0.453592),digits=0),
                       ifelse(BRFSS1$WEIGHT2 %in% c(9000:9998),BRFSS1$WEIGHT2-9000,""))

#WTKG3
BRFSS1$WTKG3<-NULL




nrow(BRFSS1[is.na(BRFSS1$DIABEYE)==FALSE,])
BRFSS1$FLSHTMY2

#Finding number of row which are having null values
nrow(BRFSS1)
count(BRFSS$EDUCAG)

dim(BRFSS1)
names(BRFSS1)
head(BRFSS1$SEX)
#null_count<- sum(is.na(BRFSS1$))
#null_count
cancer.df<-subset(BRFSS1,BRFSS1$CHCOCNCR =='Yes' | BRFSS1$CHCOCNCR =='No')
nrow(cancer.df)
head(cancer.df)
cancer.df[cancer.df==""]<-NA
count(cancer.df$SEX)

names(cancer.df)
select_column<-c('AGEG5YR','AVEDRNK2','BLDSTOOL','BMI5','CHCOCNCR','CHECKUP1','EDUCAG',
                 'HADPAP2','HADSIGM3','HOWLONG','LASTPAP2','LENGEXAM','LSTBLDS3','PDIABTST','PROFEXAM',
                 'PSATEST1','PSATIME','SEX','SMOKDAY2','TOTINDA','USENOW3')

select_column
t.cancer.df<-cancer.df[select_column]
names(t.cancer.df)
dim(t.cancer.df)
temp<-colSums(is.na(t.cancer.df))
temp
beep("sword")
temp[temp<2000000]
col<-names(temp[temp<2000000])
col
t.cancer.df<-t.cancer.df[col]
head(t.cancer.df)

t.cancer.df<- na.omit(t.cancer.df)
nrow(t.cancer.df)
head(t.cancer.df)
summary(t.cancer.df)
dim(t.cancer.df)


total_cancer<-subset(BRFSS1,BRFSS1$CHCOCNCR=='Yes')
nrow(total_cancer)
dim(total_cancer)
count(total_cancer$SEX)
total_cancer<-t.cancer.df
count(total_cancer[c("SEX","SMOKDAY2")])

########################################
#Randomize Data
########################################
#Randomize data
set.seed(42)
rand <- runif(nrow(t.cancer.df)) 
cancer.rand.df <- t.cancer.df[order(rand), ]
nrow(cancer.rand.df)
#col
head(cancer.rand.df$WEIGHT2)
cancer.rand.df$WEIGHT2<-as.numeric(cancer.rand.df$WEIGHT2)
cancer.rand.df$AVEDRNK2<-as.numeric(cancer.rand.df$AVEDRNK2)
#cancer.rand.df$PHYSHLTH<-as.numeric(cancer.rand.df$PHYSHLTH)
#cancer.rand.df$BMI5<-as.numeric(cancer.rand.df$BMI5)
#cancer.rand.df$AVEDRNK2<-as.numeric(cancer.rand.df$AVEDRNK2)

#Partition data
sub<-sample(nrow(cancer.rand.df),floor(nrow(cancer.rand.df)*0.6))
cancer.train.df <- cancer.rand.df[sub, ]
cancer.test.df<-cancer.rand.df[-sub, ]
nrow(cancer.train.df)
nrow(cancer.test.df)
head(cancer.train.df)
dim(cancer.train.df)

#Build decision tree
cancer.train.tree <- rpart(CHCOCNCR ~ ., data = cancer.train.df , method = "class",control=rpart.control(cp=0.000001,maxdepth=7))
cancer.train.tree
summary(cancer.train.tree)
rpart.plot(cancer.train.tree)

#First Look at Train
cancer.train.df$pred <- predict(cancer.train.tree, cancer.train.df, type = "class") #create a prediction using our tree
table(Actual = cancer.train.df$CHCOCNCR, Predicted = cancer.train.df$pred) #create a confusion matrix

cancer.train.df$correct <- cancer.train.df$CHCOCNCR == cancer.train.df$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
traincorrectcount <- length(which(cancer.train.df$correct))
trainincorrectcount <- nrow(cancer.train.df) - traincorrectcount
trainerrorrate <- trainincorrectcount/nrow(cancer.train.df)
trainaccuracy <- 1-trainerrorrate
trainaccuracy

#First Look at Test
cancer.test.df$pred <- predict(cancer.train.tree, cancer.test.df, type = "class") #create a prediction using our tree
table(Actual = cancer.test.df$CHCOCNCR, Predicted = cancer.test.df$pred) #create a confusion matrix

cancer.test.df$correct <- cancer.test.df$CHCOCNCR == cancer.test.df$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
testcorrectcount <- length(which(cancer.test.df$correct))
testincorrectcount <- nrow(cancer.test.df) - testcorrectcount
testerrorrate <- testincorrectcount/nrow(cancer.test.df)
testaccuracy <- 1-testerrorrate
testaccuracy

typeof(cancer.test.df$pred)
typeof(cancer.test.df$CHCOCNCR)


#CHCOCNCR
cancer.test.df$CHCOCNCR<- ifelse(cancer.test.df$CHCOCNCR=="Yes",1,0)
cancer.test.df$pred<-ifelse(cancer.test.df$pred=="Yes",1,0)
head(cancer.test.df$CHCOCNCR)
class(cancer.test.df$pred)
unique(cancer.test.df$CHCOCNCR)
unique(cancer.test.df$pred)
#Lift Chart
library(gains)
gain <- gains(cancer.test.df$CHCOCNCR, cancer.test.df$pred, groups=length(cancer.test.df$pred))
gain$cume.pct.of.total
head(cancer.test.df$CHCOCNCR)
count(cancer.test.df$CHCOCNCR)
dim(cancer.test.df)[1]
names(gain)
plot(c(0,gain$cume.pct.of.total*sum(cancer.test.df$CHCOCNCR==1))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(cancer.test.df$CHCOCNCR==1))~c(0, dim(cancer.test.df)[1]),col="red", lty=2)

count(cancer.test.df$AGE_G)
head(cancer.test.df$AGE_G)

###########################################################################################
# Functions
###########################################################################################

### Test Model Performance - Creates a 2X2 confusion matrix and associated metrics
testModelPerformance <- function(model, dataset, target, prediction) {
  if(missing(prediction))
  {
    print("here")
    dataset$pred <- predict(model, dataset, type = "class")
  }
  else
  {
    print("here2")
    dataset$pred <- prediction
  }
  
  writeLines("PERFORMANCE EVALUATION FOR")
  writeLines(paste("Model:", deparse(substitute(model))))
  writeLines(paste("Target:", deparse(substitute(target))))
  
  writeLines("\n\nConfusion Matrix:")
  confMatrix <- table(Actual = target, Predicted = dataset$pred)
  truePos <- confMatrix[2,2]
  falseNeg <- confMatrix[2,1]
  falsePos <- confMatrix[1,2]
  trueNeg <- confMatrix[1,1]
  print(confMatrix)
  writeLines("\n\n")
  
  accuracy <- (truePos + trueNeg)/(truePos + falseNeg + falsePos + trueNeg)
  sensitivity <- truePos/(truePos + falseNeg)
  specificity <- trueNeg/(falsePos + trueNeg)
  falsePosRate <- falsePos/(falsePos + trueNeg)
  falseNegRate <- falseNeg/(truePos + falseNeg)
  precision <- truePos/(truePos + falsePos)
  
  writeLines(paste("Accuracy:", round(accuracy, digits = 4)))
  writeLines(paste("Sensitivity:", round(sensitivity, digits = 4)))
  writeLines(paste("Specificity:", round(specificity, digits = 4)))
  writeLines(paste("Precision:", round(precision, digits = 4)))
  
  writeLines(paste("False Positive Rate:", round(falsePosRate, digits = 4)))
  writeLines(paste("False Negative Rate:", round(falseNegRate, digits = 4)))
  
  
  dataset
}

#Evaluate tree performance
cancer_train <- testModelPerformance(cancer.train.tree, cancer.train.df, cancer.train.df$CHCOCNCR)
cancer_test <- testModelPerformance(cancer.train.tree, cancer.test.df, cancer.test.df$CHCOCNCR)

count(cancer.test.df$LASTPAP2)
beep("sword")

TEST<-cancer.rand.df

#Calculating percentage of cervical cancer patient through PAP Test confirmation
cervical_cancer<- subset(TEST,TEST$CHCOCNCR=='Yes')
cervical_cancer<-subset(cervical_cancer,cervical_cancer$HADPAP2=='Yes')
nrow(cervical_cancer)
head(cervical_cancer[c('CHCOCNCR','HADPAP2')])
percent_cervical_cancer_patient=(nrow(cervical_cancer)/nrow(TEST))*100
percent_cervical_cancer_patient
count(cervical_cancer$SEX)

#Calculating percentage of  Anus cancer patient through Sigmoidal / Colonscopy Test confirmation
anus_cancer<-subset(TEST,TEST$CHCOCNCR=='Yes')
anus_cancer<-subset(anus_cancer,anus_cancer$HADSIGM3=='Yes')
nrow(anus_cancer)
head(anus_cancer[c('CHCOCNCR','HADSIGM3')])
percent_anus_cancer_patient=(nrow(anus_cancer)/nrow(TEST))*100
percent_anus_cancer_patient
count(anus_cancer$SEX)
#Calculating percentage of Breast cancer patient through clinical examination confirmation
breast_cancer<-subset(TEST,TEST$CHCOCNCR=='Yes')
breast_cancer<-subset(breast_cancer,breast_cancer$PROFEXAM=='Yes')
nrow(breast_cancer)
head(breast_cancer[c('CHCOCNCR','PROFEXAM')])
percent_breast_cancer_patient=(nrow(breast_cancer)/nrow(TEST))*100
percent_breast_cancer_patient
count(breast_cancer$SEX)
#Calculating percentage of prostrate cancer patient through PSA test confirmation

prostrate_cancer<-subset(TEST,TEST$CHCOCNCR=='Yes')
prostrate_cancer<-subset(prostrate_cancer,prostrate_cancer$PSATEST1=='Yes')
nrow(prostrate_cancer)
head(prostrate_cancer[c('CHCOCNCR','PSATEST1')])
percent_prostrate_cancer_patient=(nrow(prostrate_cancer)/nrow(TEST))*100
percent_prostrate_cancer_patient
count(prostrate_cancer$SEX)

#Calculating percentage of prostrate cancer patient through PSA test confirmation

skin_cancer<-subset(TEST,TEST$CHCOCNCR=='Yes')
skin_cancer<-subset(skin_cancer,skin_cancer$CHCSCNCR=='Yes')
nrow(skin_cancer)
head(skin_cancer[c('CHCOCNCR','CHCSCNCR')])
percent_skin_cancer_patient=(nrow(skin_cancer)/nrow(TEST))*100
percent_skin_cancer_patient
count(skin_cancer$SEX)
count(TEST$CHCSCNCR)

dim(TEST)
percent_anus_cancer_patient
percent_breast_cancer_patient
percent_cervical_cancer_patient
percent_prostrate_cancer_patient
percent_skin_cancer_patient


######
tobacco_user<-subset(TEST,TEST$USENOW3=='Yes')
nrow(tobacco_user)
tobacco_cancer<-subset(tobacco_cancer,tobacco_cancer$CHCSCNCR=='Yes')
nrow(skin_cancer)
head(skin_cancer[c('CHCOCNCR','CHCSCNCR')])
percent_skin_cancer_patient=(nrow(skin_cancer)/nrow(TEST))*100
percent_skin_cancer_patient
count(skin_cancer$SEX)
count(TEST$CHCSCNCR)


total_cancer<-subset(TEST,TEST$CHCOCNCR=='Yes')
nrow(total_cancer)
percent_total_cancer_patient<-(nrow(total_cancer)/nrow(TEST))*100
percent_total_cancer_patient