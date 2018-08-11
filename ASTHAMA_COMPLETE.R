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


BRFSS11<-BRFSS_2011

#AGE65YR
BRFSS11$AGE65YR<- ifelse(BRFSS11$AGE65YR==1,"Yes",
                         ifelse(BRFSS11$AGE65YR==2,"No",""))

#ASTHMS1
BRFSS11$ASTHMS1<- ifelse(BRFSS11$ASTHMS1==1,"current",
                        ifelse(BRFSS11$ASTHMS1==2,"former",
                               ifelse(BRFSS11$ASTHMS1==3,"never","")))

#BMI5
BRFSS11$BMI5<- BRFSS11$BMI5/100

#CASTHM1
BRFSS11$CASTHM1<- ifelse(BRFSS11$CASTHM1==1,"No",
                        ifelse(BRFSS11$CASTHM1==2,"Yes",""))

#CHLDCNT
BRFSS11$CHLDCNT<- ifelse(BRFSS11$CHLDCNT==1,"no child",
                        ifelse(BRFSS11$CHLDCNT==2,"1",
                               ifelse(BRFSS11$CHLDCNT==3,"2",
                                      ifelse(BRFSS11$CHLDCNT==4,"3",
                                             ifelse(BRFSS11$CHLDCNT==5,"4",
                                                    ifelse(BRFSS11$CHLDCNT==6,"5",""))))))

#CLLCPWT
BRFSS11$CLLCPWT<- NULL

#DRDXAR1
BRFSS11$DRDXAR1<- ifelse(BRFSS11$DRDXAR1==1,"Yes",
                        ifelse(BRFSS11$DRDXAR1==2,"No",""))

#EDUCAG
BRFSS11$EDUCAG<- ifelse(BRFSS11$EDUCAG==1,"no High School",
                       ifelse(BRFSS11$EDUCAG==2,"High School Grad",
                              ifelse(BRFSS11$EDUCAG==3,"Attended College","College Grad")))

#HCVU651
BRFSS11$HCVU651<- ifelse(BRFSS11$HCVU651==1,"Yes",
                        ifelse(BRFSS11$HCVU651==2,"No",""))

#INCOMG
BRFSS11$INCOMG<- ifelse(BRFSS11$INCOMG==1,"less than 15000",
                       ifelse(BRFSS11$INCOMG==2,"15-25",
                              ifelse(BRFSS11$INCOMG==3,"25-35",
                                     ifelse(BRFSS11$INCOMG==4,"35-50",
                                            ifelse(BRFSS11$INCOMG==5,"greater than 50000","")))))

#LLCPWT
BRFSS11$LLCPWT<- NULL

#LTASTH1
BRFSS11$LTASTH1<- ifelse(BRFSS11$LTASTH1==1,"No",
                       ifelse(BRFSS11$LTASTH1==2,"Yes",""))

#PNEUMO2
BRFSS11$PNEUMO2<- ifelse(BRFSS11$PNEUMO2==1,"Yes",
                        ifelse(BRFSS11$PNEUMO2==2,"No",""))

#PSU
BRFSS11$PSU<- NULL

#PNEUMO2
BRFSS11$PNEUMO2<- NULL

#RFBING5
BRFSS11$RFBING5<- ifelse(BRFSS11$RFBING5==1,"No",
                        ifelse(BRFSS11$RFBING5==2,"Yes",""))

#RFBMI5
BRFSS11$RFBMI5<- ifelse(BRFSS11$RFBMI5==1,"BMI =<25",
                       ifelse(BRFSS11$RFBMI5==2,"BMI >25",""))

#RFHLTH
BRFSS11$RFHLTH<- ifelse(BRFSS11$RFHLTH==1,"good or better health",
                       ifelse(BRFSS11$RFHLTH==2,"fair or poor helath",""))

#RFSEAT2
BRFSS11$RFSEAT2<- ifelse(BRFSS11$RFSEAT2==1,"always wear seat belt",
                        ifelse(BRFSS11$RFSEAT2==2,"seldom or never",""))

#RFSEAT3
BRFSS11$RFSEAT3<- NULL

#RFSMOK3
BRFSS11$RFSMOK3<- ifelse(BRFSS11$RFSMOK3==1,"No",
                        ifelse(BRFSS11$RFSMOK3==2,"Yes",""))

#SMOKER3
BRFSS11$SMOKER3<- ifelse(BRFSS11$SMOKER3==1,"current/smokes everyday",
                                        ifelse(BRFSS11$SMOKER3==2,"current/smokes somedays",
                                               ifelse(BRFSS11$SMOKER3==3,"former",
                                                      ifelse(BRFSS11$SMOKER3==4,"never",""))))

#STATE
BRFSS11$STATE<- ifelse(BRFSS11$STATE==1,"Alabama",
                      ifelse(BRFSS11$STATE==2,"Alaska",
                             ifelse(BRFSS11$STATE==4,"Arizona",
                                    ifelse(BRFSS11$STATE==5,"Arkansas",
                                           ifelse(BRFSS11$STATE==6,"California",
                                                  ifelse(BRFSS11$STATE==8,"Colorado",
                                                         ifelse(BRFSS11$STATE==9,"Connecticut",
                                                                ifelse(BRFSS11$STATE==10,"Delaware",
                                                                       ifelse(BRFSS11$STATE==11,"District of Columbia",
                                                                              ifelse(BRFSS11$STATE==12,"Florida",
                                                                                     ifelse(BRFSS11$STATE==13,"Georgia",
                                                                                            ifelse(BRFSS11$STATE==15,"Hawaii",
                                                                                                   ifelse(BRFSS11$STATE==16,"Idaho",
                                                                                                          ifelse(BRFSS11$STATE==17,"Illinois",
                                                                                                                 ifelse(BRFSS11$STATE==18,"Indiana",
                                                                                                                        ifelse(BRFSS11$STATE==19,"Iowa",
                                                                                                                               ifelse(BRFSS11$STATE==20,"Kansas",
                                                                                                                                      ifelse(BRFSS11$STATE==21,"Kentucky",
                                                                                                                                             ifelse(BRFSS11$STATE==22,"Louisiana",
                                                                                                                                                    ifelse(BRFSS11$STATE==23,"Maine",
                                                                                                                                                           ifelse(BRFSS11$STATE==24,"Maryland",
                                                                                                                                                                  ifelse(BRFSS11$STATE==25,"Massachusetts",
                                                                                                                                                                         ifelse(BRFSS11$STATE==26,"Michigan",
                                                                                                                                                                                ifelse(BRFSS11$STATE==27,"Minnesota",
                                                                                                                                                                                       ifelse(BRFSS11$STATE==28,"Mississippi",
                                                                                                                                                                                              ifelse(BRFSS11$STATE==29,"Missouri",
                                                                                                                                                                                                     ifelse(BRFSS11$STATE==30,"Montana",
                                                                                                                                                                                                            ifelse(BRFSS11$STATE==31,"Nebraska",
                                                                                                                                                                                                                   ifelse(BRFSS11$STATE==32,"Nevada",
                                                                                                                                                                                                                          ifelse(BRFSS11$STATE==33,"New Hampshire",
                                                                                                                                                                                                                                 ifelse(BRFSS11$STATE==34,"New Jersey",
                                                                                                                                                                                                                                        ifelse(BRFSS11$STATE==35,"New Mexico",
                                                                                                                                                                                                                                               ifelse(BRFSS11$STATE==36,"New York",
                                                                                                                                                                                                                                                      ifelse(BRFSS11$STATE==37,"North Carolina",
                                                                                                                                                                                                                                                             ifelse(BRFSS11$STATE==38,"North Dakota",
                                                                                                                                                                                                                                                                    ifelse(BRFSS11$STATE==39,"Ohio",
                                                                                                                                                                                                                                                                           ifelse(BRFSS11$STATE==40,"Oklahoma",
                                                                                                                                                                                                                                                                                  ifelse(BRFSS11$STATE==41,"Oregon ",""
                                                                                                                                                                                                                                                                                  ))))))))))))))))))))))))))))))))))))))

#STRWT
BRFSS11$STRWT<- NULL

#STSTR
BRFSS11$STSTR<- NULL

#TOTINDA
BRFSS11$TOTINDA<- ifelse(BRFSS11$TOTINDA==1,"Yes",
                        ifelse(BRFSS11$TOTINDA==2,"No",""))

#WT2RAKE
BRFSS11$WT2RAKE<- NULL

#ADDEPEV2
BRFSS11$ADDEPEV2<- ifelse(BRFSS11$ADDEPEV2==1,"Yes",
                         ifelse(BRFSS11$ADDEPEV2==2,"No",""))

#ALCDAY5 - Convert days per week to approx days per month considering 30 days in a month.
BRFSS11$ALCDAY5<- ifelse(BRFSS11$ALCDAY5 %in% c(100:199), round((BRFSS11$ALCDAY5-100)*4.29,digits=0),
                        ifelse(BRFSS11$ALCDAY5 %in% c(200:299),BRFSS11$ALCDAY5-200,
                               ifelse(BRFSS11$ALCDAY5 ==888,0,"")))

#ASACTLIM
BRFSS11$ASACTLIM<- ifelse(BRFSS11$ASACTLIM<=365,BRFSS11$ASACTLIM,
                         ifelse(BRFSS11$ASACTLIM==888,0,""))

#ASATTACK
BRFSS11$ASATTACK<- ifelse(BRFSS11$ASATTACK==1,"Yes",
                         ifelse(BRFSS11$ASATTACK==2,"No",""))

#ASDRVIST
BRFSS11$ASDRVIST<- ifelse(BRFSS11$ASDRVIST<=87,BRFSS11$ASDRVIST,
                         ifelse(BRFSS11$ASDRVIST==88,0,""))

#ASERVIST
BRFSS11$ASERVIST<- ifelse(BRFSS11$ASERVIST<=87,BRFSS11$ASERVIST,
                         ifelse(BRFSS11$ASERVIST==88,0,""))

#ASINHALR
BRFSS11$ASINHALR<- ifelse(BRFSS11$ASINHALR==1,"1-4",
                         ifelse(BRFSS11$ASINHALR==2,"5-14",
                                ifelse(BRFSS11$ASINHALR==3,"15-29",
                                       ifelse(BRFSS11$ASINHALR==4,"30-59",
                                              ifelse(BRFSS11$ASINHALR==5,"60-99",
                                                     ifelse(BRFSS11$ASINHALR==6,">=100",
                                                            ifelse(BRFSS11$ASINHALR==8,"0","")))))))

#ASTHMA3
BRFSS11$ASTHMA3<- ifelse(BRFSS11$ASTHMA3==1,"Yes",
                        ifelse(BRFSS11$ASTHMA3==2,"No",""))

#ASTHMAGE
BRFSS11$ASTHMAGE<- ifelse(BRFSS11$ASTHMAGE==97,"<=10",
                         ifelse(BRFSS11$ASTHMAGE<97,BRFSS11$ASTHMAGE,""))

#ASTHMED3
BRFSS11$ASTHMED3<- ifelse(BRFSS11$ASTHMED3==1,"1-14",
                         ifelse(BRFSS11$ASTHMED3==2,"15-24",
                                ifelse(BRFSS11$ASTHMED3==3,"25-30",
                                       ifelse(BRFSS11$ASTHMED3==8,"0",""))))

#CADULT
BRFSS11$CADULT<- ifelse(BRFSS11$CADULT==1,"Yes/Male",
                       ifelse(BRFSS11$CADULT==2,"Yes/Female",""))

#CASTHDX2
BRFSS11$CASTHDX2<- ifelse(BRFSS11$CASTHDX2==1,"Yes",
                         ifelse(BRFSS11$CASTHDX2==2,"No",""))

#CASTHNO2
BRFSS11$CASTHNO2<- ifelse(BRFSS11$CASTHNO2==1,"Yes",
                         ifelse(BRFSS11$CASTHNO2==2,"No",""))

#CELLFON2
BRFSS11$CELLFON2<- ifelse(BRFSS11$CELLFON2==1,"Yes","")

#CHCKIDNY
BRFSS11$CHCKIDNY<- ifelse(BRFSS11$CHCKIDNY==1,"Yes",
                         ifelse(BRFSS11$CHCKIDNY==2,"No",""))

#CHCOCNCR
BRFSS11$CHCOCNCR<- ifelse(BRFSS11$CHCOCNCR==1,"Yes",
                         ifelse(BRFSS11$CHCOCNCR==2,"No",""))

#CHCSCNCR
BRFSS11$CHCSCNCR<- ifelse(BRFSS11$CHCSCNCR==1,"Yes",
                         ifelse(BRFSS11$CHCSCNCR==2,"No",""))

#CHECKUP1
BRFSS11$CHECKUP1<- ifelse(BRFSS11$CHECKUP1==1,"within past year",
                         ifelse(BRFSS11$CHECKUP1==2,"within past 2 years",
                                ifelse(BRFSS11$CHECKUP1==3,"within past 5 years",
                                       ifelse(BRFSS11$CHECKUP1==4,"5 or more years before",
                                              ifelse(BRFSS11$CHECKUP1==8,"never","")))))

#CHILDREN
BRFSS11$CHILDREN<- ifelse(BRFSS11$CHILDREN<=87,BRFSS11$CHILDREN,
                         ifelse(BRFSS11$CHILDREN==88,"0",""))

#CHKHEMO3
BRFSS11$CHKHEMO3<- ifelse(BRFSS11$CHKHEMO3<=76,BRFSS11$CHKHEMO3,
       ifelse(BRFSS11$CHKHEMO3==88,"0",
              ifelse(BRFSS11$CHKHEMO3==98,"never heard of the test","")))

#CPDEMO1
BRFSS11$CPDEMO1<- ifelse(BRFSS11$CPDEMO1==1,"Yes",
                        ifelse(BRFSS11$CPDEMO1==2,"No",""))

#CSTATE
BRFSS11$CSTATE<- ifelse(BRFSS11$CSTATE==1,"Yes",
                       ifelse(BRFSS11$CSTATE==2,"No",""))

#CTELENUM
BRFSS11$CTELENUM<- NULL

#CTELNUM1
BRFSS11$CTELNUM1<- NULL

#CVDCRHD4
BRFSS11$CVDCRHD4<- ifelse(BRFSS11$CVDCRHD4==1,"Yes",
                         ifelse(BRFSS11$CVDCRHD4==2,"No",""))

#CVDINFR4
BRFSS11$CVDINFR4<- ifelse(BRFSS11$CVDINFR4==1,"Yes",
                         ifelse(BRFSS11$CVDINFR4==2,"No",""))

#CVDSTRK3
BRFSS11$CVDSTRK3<- ifelse(BRFSS11$CVDSTRK3==1,"Yes",
                         ifelse(BRFSS11$CVDSTRK3==2,"No",""))

#DIABAGE2
BRFSS11$DIABAGE2<- ifelse(BRFSS11$DIABAGE2<=97,BRFSS11$DIABAGE2,"")

#DIABEDU
BRFSS11$DIABEDU<- ifelse(BRFSS11$DIABEDU==1,"Yes",
                        ifelse(BRFSS11$DIABEDU==2,"No",""))

#DIABETE3
BRFSS11$DIABETE3<- ifelse(BRFSS11$DIABETE3==1,"Yes",
                         ifelse(BRFSS11$DIABETE3==2,"Yes/female during pregnancy",
                                ifelse(BRFSS11$DIABETE3==3,"No",
                                       ifelse(BRFSS11$DIABETE3==2,"Borderline diabetes/No current diabetes",""))))

#DIABEYE
BRFSS11$DIABEYE<- ifelse(BRFSS11$DIABEYE==1,"Yes",
                        ifelse(BRFSS11$DIABEYE==2,"No",""))

#DISPCODE
BRFSS11$DISPCODE<- NULL

#DOCTDIAB
BRFSS11$DOCTDIAB<- ifelse(BRFSS11$DOCTDIAB==88,0,
                         ifelse(BRFSS11$DOCTDIAB<=76,BRFSS11$DOCTDIAB,""))

#DRNK3GE5
BRFSS11$DRNK3GE5<- ifelse(BRFSS11$DRNK3GE5==88,0,
                         ifelse(BRFSS11$DRNK3GE5<=76,BRFSS11$DRNK3GE5,""))

#DRNKANY5
BRFSS11$DRNKANY5<- ifelse(BRFSS11$DRNKANY5==1,"Yes",
                         ifelse(BRFSS11$DRNKANY5==2,"No",""))

#DROCDY3_
BRFSS11$DROCDY3_<- NULL

#EDUCA
BRFSS11$EDUCA<-ifelse(BRFSS11$EDUCA==1,"never attended school",
                     ifelse(BRFSS11$EDUCA==2,"1-8 elementary",
                            ifelse(BRFSS11$EDUCA==3,"9-11 high school",
                                   ifelse(BRFSS11$EDUCA==4,"grade 12 or higher",
                                          ifelse(BRFSS11$EDUCA==5,"college year 1-3",
                                                 ifelse(BRFSS11$EDUCA==6,"college year 4 or graduate",""))))))

#EMTSUPRT
BRFSS11$EMTSUPRT<- ifelse(BRFSS11$EMTSUPRT==1,"always",
                         ifelse(BRFSS11$EMTSUPRT==2,"usually",
                                ifelse(BRFSS11$EMTSUPRT==3,"sometimes",
                                       ifelse(BRFSS11$EMTSUPRT==4,"rarely",
                                              ifelse(BRFSS11$EMTSUPRT==5,"never","")))))

#EXERANY2
BRFSS11$EXERANY2<- ifelse(BRFSS11$EXERANY2==1,"Yes",
                         ifelse(BRFSS11$EXERANY2==2,"No",""))

#EYEEXAM
BRFSS11$EYEEXAM<- ifelse(BRFSS11$EYEEXAM==1,"within past one month",
                        ifelse(BRFSS11$EYEEXAM==2,"within past year",
                               ifelse(BRFSS11$EYEEXAM==3,"within past 2 years",
                                      ifelse(BRFSS11$EYEEXAM==4,"2 or more years",
                                             ifelse(BRFSS11$EYEEXAM==8,"never","")))))

#FEETCHK
BRFSS11$FEETCHK<- ifelse(BRFSS11$FEETCHK==88,0,
                        ifelse(BRFSS11$FEETCHK<=76,BRFSS11$DOCTDIAB,""))

#FEETCHK2
BRFSS11$FEETCHK2<- ifelse(BRFSS11$FEETCHK2 %in% c(100:199), round((BRFSS11$FEETCHK2-100)*365,digits=0),
                         ifelse(BRFSS11$FEETCHK2 %in% c(200:299),round((BRFSS11$FEETCHK2-200)*(365/7),digits=0),
                                ifelse(BRFSS11$FEETCHK2 %in% c(300:399),round((BRFSS11$FEETCHK2-300)*12,digits=0),
                                       ifelse(BRFSS11$FEETCHK2 %in% c(400:499),BRFSS11$FEETCHK2,
                                              ifelse(BRFSS11$FEETCHK2 ==888,0,"")))))

#AGE_G
BRFSS11$AGE_G<- ifelse(BRFSS11$AGE_G ==1, "18-24",
                      ifelse(BRFSS11$AGE_G ==2,"25-34",
                             ifelse(BRFSS11$AGE_G ==3,"35-44",
                                    ifelse(BRFSS11$AGE_G ==4,"45-54",
                                           ifelse(BRFSS11$AGE_G ==5,"55-64",">=65")))))

#AGEG5YR
BRFSS11$AGEG5YR<- ifelse(BRFSS11$AGEG5YR ==1, "18-24",
                        ifelse(BRFSS11$AGEG5YR ==2,"25-29",
                               ifelse(BRFSS11$AGEG5YR ==3,"30-34",
                                      ifelse(BRFSS11$AGEG5YR ==4,"35-39",
                                             ifelse(BRFSS11$AGEG5YR ==5,"40-44",
                                                    ifelse(BRFSS11$AGEG5YR ==6,"44-49",
                                                           ifelse(BRFSS11$AGEG5YR ==7,"50-54",
                                                                  ifelse(BRFSS11$AGEG5YR ==8,"55-59",
                                                                         ifelse(BRFSS11$AGEG5YR ==9,"60-64",
                                                                                ifelse(BRFSS11$AGEG5YR ==10,"65-69",
                                                                                       ifelse(BRFSS11$AGEG5YR ==11,"70-74",
                                                                                              ifelse(BRFSS11$AGEG5YR ==12,"75-79",
                                                                                                     ifelse(BRFSS11$AGEG5YR ==13,">=80","")))))))))))))

#AIDTST3
BRFSS11$AIDTST3<- ifelse(BRFSS11$AIDTST3==1,"Yes",
                        ifelse(BRFSS11$AIDTST3==2,"No",""))

#BMI5CAT
BRFSS11$BMI5CAT<- ifelse(BRFSS11$BMI5CAT==1,"Underweight (BMI<18.50)",
                        ifelse(BRFSS11$BMI5CAT==2,"Normal Weight (18.50<=BMI< 25.00)",
                               ifelse(BRFSS11$BMI5CAT==3,"Overweight (25.00<=BMI<30.00)",
                                      ifelse(BRFSS11$BMI5CAT==4,"Obese (30.00<=BMI<99.99)",""))))

#ASNOSLEP
BRFSS11$ASNOSLEP<- ifelse(BRFSS11$ASNOSLEP ==1, "1-2",
                         ifelse(BRFSS11$ASNOSLEP ==2,"3-4",
                                ifelse(BRFSS11$ASNOSLEP ==3,"5",
                                       ifelse(BRFSS11$ASNOSLEP ==4,"6-10",
                                              ifelse(BRFSS11$ASNOSLEP ==5,">=10",
                                                     ifelse(BRFSS11$ASNOSLEP ==8,"0",""))))))

#ASRCHKUP
BRFSS11$ASRCHKUP<- ifelse(BRFSS11$ASRCHKUP==88,0,
                         ifelse(BRFSS11$ASRCHKUP<=86,BRFSS11$ASRCHKUP,""))

#ASTHNOW
BRFSS11$ASTHNOW<- ifelse(BRFSS11$ASTHNOW==1,"Yes",
                        ifelse(BRFSS11$ASTHNOW==2,"No",""))

#ASYMPTOM
BRFSS11$ASYMPTOM<- ifelse(BRFSS11$ASYMPTOM ==1, "less than 1 in a week",
                         ifelse(BRFSS11$ASYMPTOM ==2,"1-2/week",
                                ifelse(BRFSS11$ASYMPTOM ==3,"3-6/week",
                                       ifelse(BRFSS11$ASYMPTOM ==4,"everyday but not all the time",
                                              ifelse(BRFSS11$ASYMPTOM ==5,"everyday all the time",
                                                     ifelse(BRFSS11$ASYMPTOM ==8,"never",""))))))

#AVEDRNK2
BRFSS11$AVEDRNK2<- ifelse(BRFSS11$AVEDRNK2<=76,BRFSS11$AVEDRNK2,"")

#BLDSTOOL
BRFSS11$BLDSTOOL<- ifelse(BRFSS11$BLDSTOOL==1,"Yes",
                         ifelse(BRFSS11$BLDSTOOL==2,"No",""))

#BLDSUGAR
BRFSS11$BLDSUGAR<- ifelse(BRFSS11$BLDSUGAR %in% c(100:199), round((BRFSS11$BLDSUGAR-100)*365,digits=0),
                         ifelse(BRFSS11$BLDSUGAR %in% c(200:299),round((BRFSS11$BLDSUGAR-200)*(365/7),digits=0),
                                ifelse(BRFSS11$BLDSUGAR %in% c(300:399),round((BRFSS11$BLDSUGAR-300)*12,digits=0),
                                       ifelse(BRFSS11$BLDSUGAR %in% c(400:499),BRFSS11$BLDSUGAR,
                                              ifelse(BRFSS11$BLDSUGAR ==888,0,"")))))



#FLSHTMY2
BRFSS11$FLSHTMY2<-ifelse(BRFSS11$FLSHTMY2==777777,"never",
                        ifelse(BRFSS11$FLSHTMY2<999999,paste(substr(BRFSS11$FLSHTMY2, 1, ifelse(nchar(BRFSS11$FLSHTMY2)==6,2,1)),"-",substr(BRFSS11$FLSHTMY2, ifelse(nchar(BRFSS11$FLSHTMY2)==6,3,2), nchar(BRFSS11$FLSHTMY2))),""))

#GENHLTH
BRFSS11$GENHLTH<-ifelse(BRFSS11$GENHLTH==1,"Excellent",
                       
                       ifelse(BRFSS11$GENHLTH==2,"Very Good",
                              
                              ifelse(BRFSS11$GENHLTH==3,"Good",
                                     ifelse(BRFSS11$GENHLTH==4,"Fair",
                                            ifelse(BRFSS11$GENHLTH==5,"poor","")))))


#HADHYST2
BRFSS11$HADHYST2<-ifelse(BRFSS11$HADHYST2==1,"Yes",
                        ifelse(BRFSS11$HADHYST2==2,"No",""))

#HADMAM
BRFSS11$HADMAM<-ifelse(BRFSS11$HADMAM==1,"Yes",
                      ifelse(BRFSS11$HADMAM==2,"No",""))

#HADPAP2
BRFSS11$HADPAP2<-ifelse(BRFSS11$HADPAP2==1,"Yes",
                       ifelse(BRFSS11$HADPAP2==2,"No",""))


#HADSGCO1
BRFSS11$HADSGCO1<-ifelse(BRFSS11$HADSGCO1==1,"Sigmoidal",
                        ifelse(BRFSS11$HADSGCO1==2,"Colonoscopy",""))

#HADSIGM3
BRFSS11$HADSIGM3<-ifelse(BRFSS11$HADSIGM3==1,"Yes",
                        ifelse(BRFSS11$HADSIGM3==2,"No",""))

#HAVARTH3
BRFSS11$HAVARTH3<-ifelse(BRFSS11$HAVARTH3==1,"Yes",
                        ifelse(BRFSS11$HAVARTH3==2,"No",""))

#HEIGHT3
BRFSS11$HEIGHT3<-ifelse(BRFSS11$HEIGHT3<=800,((as.numeric(substr(BRFSS11$HEIGHT3,1,1))*12) +  as.numeric(substr(BRFSS11$HEIGHT3,2,3)))*2.54,
                       ifelse(BRFSS11$HEIGHT3 %in% c(9000:9998),substr(BRFSS11$HEIGHT3,2,4),""))

#HIVTST6
BRFSS11$HIVTST6<-ifelse(BRFSS11$HIVTST6==1,"Yes",
                       ifelse(BRFSS11$HIVTST6==2,"No",""))

#HIVTSTD3
BRFSS11$HIVTSTD3-ifelse(BRFSS11$HIVTSTD3<777777,paste(substr(BRFSS11$HIVTSTD3, 1, ifelse(nchar(BRFSS11$HIVTSTD3)==6,2,1)),"-",substr(BRFSS11$HIVTSTD3, ifelse(nchar(BRFSS11$HIVTSTD3)==6,3,2), nchar(BRFSS11$HIVTSTD3))),"")

#HLTHPLN1
BRFSS11$HLTHPLN1<- ifelse(BRFSS11$HLTHPLN1==1,"Yes",
                         ifelse(BRFSS11$HLTHPLN1==2,"No",""))


#HOWLONG
BRFSS11$HOWLONG<- ifelse(BRFSS11$HOWLONG==1,"within 1 year",
                        ifelse(BRFSS11$HOWLONG==2,"1-2 years",
                               ifelse(BRFSS11$HOWLONG==2,"2-3 years",
                                      ifelse(BRFSS11$HOWLONG==2,"3-5 years",
                                             ifelse(BRFSS11$HOWLONG==2,"before 5 years","")))))

#HPVADSHT
BRFSS11$HPVADSHT<- ifelse(BRFSS11$HPVADSHT==3,"all shots taken",
                         ifelse(BRFSS11$HPVADSHT<=2,paste(BRFSS11$HPVADSHT,"shots taken"),""))

#HPVADVC2
BRFSS11$HPVADVC2<- ifelse(BRFSS11$HPVADVC2==1,"Yes",
                         ifelse(BRFSS11$HPVADVC2==2,"No",""))

#HTIN4
BRFSS11$HTIN4<- NULL

#HTM4
BRFSS11$HTM4<- NULL

#IDATE
BRFSS11$IDATE<- NULL

#IMFVPLAC
BRFSS11$IMFVPLAC<-ifelse(BRFSS11$IMFVPLAC==1,"Doctor's office/Health Management Organization",
                        
                        ifelse(BRFSS11$IMFVPLAC==2,"A health department",
                               
                               ifelse(BRFSS11$IMFVPLAC==3,"Another type of clinic or health center",
                                      ifelse(BRFSS11$IMFVPLAC==4,"A senior,recreation or community center",
                                             ifelse(BRFSS11$IMFVPLAC==5,"A store",
                                                    ifelse(BRFSS11$IMFVPLAC==6,"A hospital(e.g. in patient)",
                                                           ifelse(BRFSS11$IMFVPLAC==7,"An emergency room",
                                                                  ifelse(BRFSS11$IMFVPLAC==8,"Workplace",
                                                                         ifelse(BRFSS11$IMFVPLAC==9,"Some other kind of place",
                                                                                ifelse(BRFSS11$IMFVPLAC==10,"Received vaccination in Canada/Mexico",
                                                                                       ifelse(BRFSS11$IMFVPLAC==11,"A school","")))))))))))






#INCOME2
BRFSS11$INCOME2<-ifelse(BRFSS11$INCOME2==1,"Less than 10,000",
                       ifelse(BRFSS11$INCOME2==2,"Less than 15,000",
                              ifelse(BRFSS11$INCOME2==3,"Less than 20,000",
                                     ifelse(BRFSS11$INCOME2==4,"Less than 25,000",
                                            ifelse(BRFSS11$INCOME2==5,"Less than 35,000",
                                                   ifelse(BRFSS11$INCOME2==6,"Less than 50,000",
                                                          ifelse(BRFSS11$INCOME2==7,"Less than 75,000",
                                                                 ifelse(BRFSS11$INCOME2==8,"75,000 or more",""))))))))


#INSULIN
BRFSS11$INSULIN<-ifelse(BRFSS11$INSULIN==1,"Yes",
                       ifelse(BRFSS11$INSULIN==2,"No",""))


#IYEAR
BRFSS11$IYEAR<-NULL


#LANDLINE
BRFSS11$LANDLINE<-ifelse(BRFSS11$LANDLINE==1,"Yes",
                        ifelse(BRFSS11$LANDLINE==2," No",""))

#LASTPAP2
BRFSS11$LASTPAP2<-ifelse(BRFSS11$LASTPAP2==1,"Within 1 year",
                        ifelse(BRFSS11$LASTPAP2==2,"1 -2 years",
                               ifelse(BRFSS11$LASTPAP2==3,"2-3 years",
                                      ifelse(BRFSS11$LASTPAP2==4,"3-5 years",
                                             ifelse(BRFSS11$LASTPAP2==5,"before 5 years","")))))


#LASTSIG3
BRFSS11$LASTSIG3<-ifelse(BRFSS11$LASTSIG3==1,"Within 1 year",
                        ifelse(BRFSS11$LASTSIG3==2,"1-2 years",
                               ifelse(BRFSS11$LASTSIG3==3,"2-3 years",
                                      ifelse(BRFSS11$LASTSIG3==4,"3-5 years",
                                             ifelse(BRFSS11$LASTSIG3==5,"5-10 years",
                                                    ifelse(BRFSS11$LASTSIG3==6,"before 10 years",""))))))

#LASTSMK2
BRFSS11$LASTSMK2<-ifelse(BRFSS11$LASTSMK2==1,"Everyday",
                        ifelse(BRFSS11$LASTSMK2==2,"Somedays",
                               ifelse(BRFSS11$LASTSMK2==3,"Not at all","")))


#LENGEXAM
BRFSS11$LENGEXAM<-ifelse(BRFSS11$LENGEXAM==1,"Within 1 year",
                        ifelse(BRFSS11$LENGEXAM==2,"1 -2 years",
                               ifelse(BRFSS11$LENGEXAM==3,"2-3 years",
                                      ifelse(BRFSS11$LENGEXAM==4,"3-5 years",
                                             ifelse(BRFSS11$LENGEXAM==5,"before 5 years","")))))


#LSATISFY
BRFSS11$LSATISFY<-ifelse(BRFSS11$LSATISFY==1,"Very satisfied",
                        ifelse(BRFSS11$LSATISFY==2,"satisfied",
                               ifelse(BRFSS11$LSATISFY==3,"Dissatisfied",
                                      ifelse(BRFSS11$LSATISFY==4,"Very dissatisfied",""))))


#LSTBLDS3
BRFSS11$LSTBLDS3<-ifelse(BRFSS11$LSTBLDS3==1,"Within 1 year",
                        ifelse(BRFSS11$LSTBLDS3==2,"1 -2 years",
                               ifelse(BRFSS11$LSTBLDS3==3,"2-3 years",
                                      ifelse(BRFSS11$LSTBLDS3==4,"3-5 years",
                                             ifelse(BRFSS11$LSTBLDS3==5,"before 5 years","")))))


#MARITAL
BRFSS11$MARITAL<-ifelse(BRFSS11$MARITAL==1,"Married",
                       ifelse(BRFSS11$MARITAL==2,"Divorced",
                              ifelse(BRFSS11$MARITAL==3,"Widowed",
                                     ifelse(BRFSS11$MARITAL==4,"Separated",
                                            ifelse(BRFSS11$MARITAL==5,"Never married",
                                                   ifelse(BRFSS11$MARITAL==6,"A member of an unmarried couple",""))))))



#MAXDRNKS
BRFSS11$MAXDRNKS<-ifelse(BRFSS11$MAXDRNKS<=76,BRFSS11$MAXDRNKS,"")


#MEDCOST
BRFSS11$MEDCOST<-ifelse(BRFSS11$MEDCOST==1,"Yes",
                       ifelse(BRFSS11$MEDCOST==2,"No",""))

#MENTHLTH
BRFSS11$MENTHLTH<-ifelse(BRFSS11$MENTHLTH<=30,BRFSS11$MENTHLTH,
                        ifelse(BRFSS11$MENTHLTH==88,0,""))


#MSCODE
BRFSS11$MSCODE<-ifelse(BRFSS11$MSCODE==1,"In city center",
                      ifelse(BRFSS11$MSCODE==2,"outside city center but inside county",
                             ifelse(BRFSS11$MSCODE==3,"Inside the suburban county",
                                    ifelse(BRFSS11$MSCODE==5,"Not in an MSA",""))))


#NUMADULT
BRFSS11$NUMADULT<-ifelse(BRFSS11$NUMADULT<=99,BRFSS11$NUMADULT,"")


#NUMHHOL2
BRFSS11$NUMHHOL2<-ifelse(BRFSS11$NUMADULT==1,"Yes",
                        ifelse(BRFSS11$NUMADULT==2,"No",""))

#NUMMEN
BRFSS11$NUMMEN<-ifelse(BRFSS11$NUMMEN<=99,BRFSS11$NUMMEN,"")


#NUMPHON2
BRFSS11$NUMPHON2<-NULL


#NUMWOMEN
BRFSS11$NUMWOMEN<-ifelse(BRFSS11$NUMWOMEN<=99,BRFSS11$NUMWOMEN,"")


#PAINACT2
BRFSS11$PAINACT2<-NULL


#PDIABTST
BRFSS11$PDIABTST<-ifelse(BRFSS11$PDIABTST==1,"Yes",
                        ifelse(BRFSS11$PDIABTST==2,"No",""))


#PERSDOC2
BRFSS11$PERSDOC2<-ifelse(BRFSS11$PERSDOC2==1,"YEes, only 1",
                        ifelse(BRFSS11$PERSDOC2==2,"More than 1",
                               ifelse(BRFSS11$PERSDOC2==3,"no","")))

#PHYSHLTH
BRFSS11$PHYSHLTH<-ifelse(BRFSS11$PHYSHLTH<=30,BRFSS11$PHYSHLTH,
                        ifelse(BRFSS11$PHYSHLTH==88,0,""))

#PNEUVAC3
BRFSS11$PNEUVAC3<-ifelse(BRFSS11$PNEUVAC3==1,"Yes",
                        ifelse(BRFSS11$PNEUVAC3==2,"No",""))


#POORHLTH
BRFSS11$POORHLTH<-ifelse(BRFSS11$POORHLTH<=30,BRFSS11$POORHLTH,
                        ifelse(BRFSS11$POORHLTH==88,0,""))


#PREDIAB1
BRFSS11$PREDIAB1<-ifelse(BRFSS11$PREDIAB1==1,"Yes",
                        ifelse(BRFSS11$PREDIAB1==2,"Yes,during pregnency",
                               ifelse(BRFSS11$PREDIAB1==3,"No","")))


#PREGNANT
BRFSS11$PREGNANT<-ifelse(BRFSS11$PREGNANT==1,"Yes",
                        ifelse(BRFSS11$PREGNANT==2,"No",""))


#PROFEXAM
BRFSS11$PROFEXAM<-ifelse(BRFSS11$PROFEXAM==1,"Yes",
                        ifelse(BRFSS11$PROFEXAM==2,"No",""))

#PSATEST1
BRFSS11$PSATEST1<-ifelse(BRFSS11$PSATEST1==1,"Yes",
                        ifelse(BRFSS11$PSATEST1==2,"No",""))


#PSATIME
BRFSS11$PSATIME<-ifelse(BRFSS11$PSATIME==1,"Within 1 year",
                       ifelse(BRFSS11$PSATIME==2,"1 -2 years",
                              ifelse(BRFSS11$PSATIME==3,"2-3 years",
                                     ifelse(BRFSS11$PSATIME==4,"3-5 years",
                                            ifelse(BRFSS11$PSATIME==5,"before 5 years","")))))


#PVTRESD2
BRFSS11$PVTRESD2<-ifelse(BRFSS11$PVTRESD2==1,"Yes",
                        ifelse(BRFSS11$PVTRESD2==2,"No",""))


#QLACTLM2
BRFSS11$QLACTLM2<-ifelse(BRFSS11$QLACTLM2==1,"Yes",
                        ifelse(BRFSS11$QLACTLM2==2,"No",""))


#QLHLTH2
BRFSS11$QLHLTH2<-NULL


#QLMENTL2
BRFSS11$QLMENTL2<-NULL


#QLSTRES2
BRFSS11$QLSTRES2<-NULL


#QSTLANG
BRFSS11$QSTLANG<-ifelse(BRFSS11$QSTLANG==1,"English",
                       ifelse(BRFSS11$QSTLANG==2,"Spanish",
                              ifelse(BRFSS11$QSTLANG>=3,"other","")))


#RCSGENDR
BRFSS11$RCSGENDR<-ifelse(BRFSS11$RCSGENDR==1,"Boy",
                        ifelse(BRFSS11$RCSGENDR==2,"Girl",""))


#RCSRLTN2
BRFSS11$RCSRLTN2<-ifelse(BRFSS11$RCSRLTN2==1,"Parent",
                        ifelse(BRFSS11$RCSRLTN2==2,"Grandparent",
                               ifelse(BRFSS11$RCSRLTN2==3,"Foster Parent or Guardian",
                                      ifelse(BRFSS11$RCSRLTN2==4,"Sibling",
                                             ifelse(BRFSS11$RCSRLTN2==5,"Other relative",
                                                    ifelse(BRFSS11$RCSRLTN2==6,"Not related in any way",
                                                           ifelse(BRFSS11$RCSRLTN2==7,"Don't know not sure","")))))))



#RENTHOM1
BRFSS11$RENTHOM1<-ifelse(BRFSS11$RENTHOM1==1,"Parent",
                        ifelse(BRFSS11$RENTHOM1==2,"Own",
                               ifelse(BRFSS11$RENTHOM1==3,"Rent",
                                      ifelse(BRFSS11$RENTHOM1==4,"Other Arrangement",""))))


#SCNTLPAD
BRFSS11$SCNTLPAD<-ifelse(BRFSS11$SCNTLPAD==1,"By Salary",
                        ifelse(BRFSS11$SCNTLPAD==2,"Paid by hour",
                               ifelse(BRFSS11$SCNTLPAD==3,"Paid by job/task(commission)",
                                      ifelse(BRFSS11$SCNTLPAD==4,"Paid some other way",""))))



#SCNTLWK1
BRFSS11$SCNTLWK1<- ifelse(BRFSS11$SCNTLWK1==98,0,
                         ifelse(BRFSS11$SCNTLWK1<=96,BRFSS11$SCNTLWK1,""))


#SCNTPAID
BRFSS11$SCNTPAID<-NULL


#SCNTWRK1
BRFSS11$SCNTWRK1<-NULL


#SEATBELT
BRFSS11$SEATBELT<-ifelse(BRFSS11$SEATBELT==1,"always",
                        ifelse(BRFSS11$SEATBELT==2,"nearly always",
                               ifelse(BRFSS11$SEATBELT==3,"sometimes",
                                      ifelse(BRFSS11$SEATBELT==4,"seldom",
                                             ifelse(BRFSS11$SEATBELT==5,"never",
                                                    ifelse(BRFSS11$SEATBELT==8,"Never drive or ride a car",""))))))


#SEQNO
BRFSS11$SEQNO<-NULL


#SEX
BRFSS11$SEX<-ifelse(BRFSS11$SEX==1,"Male",
                   ifelse(BRFSS11$SEX==2,"Female",""))



#SMOKDAY2
BRFSS11$SMOKDAY2<-ifelse(BRFSS11$SMOKDAY2==1,"Everyday",
                        ifelse(BRFSS11$SMOKDAY2==2,"Somedays",
                               ifelse(BRFSS11$SMOKDAY2==3,"Not at all","")))



#SMOKE100
BRFSS11$SMOKE100<-ifelse(BRFSS11$SMOKE100==1,"Yes",
                        ifelse(BRFSS11$SMOKE100==2,"No",""))



#STOPSMK2
BRFSS11$STOPSMK2<-ifelse(BRFSS11$STOPSMK2==1,"Yes",
                        ifelse(BRFSS11$STOPSMK2==2,"No",""))


#USEEQUIP
BRFSS11$USEEQUIP<-ifelse(BRFSS11$USEEQUIP==1,"Yes",
                        ifelse(BRFSS11$USEEQUIP==2,"No",""))


#USENOW3
BRFSS11$USENOW3<-ifelse(BRFSS11$USENOW3==1,"Everyday",
                       ifelse(BRFSS11$USENOW3==2,"Somedays",
                              ifelse(BRFSS11$USENOW3==3,"Not at all","")))



#VETERAN3
BRFSS11$VETERAN3<-ifelse(BRFSS11$VETERAN3==1,"Yes",
                        ifelse(BRFSS11$VETERAN3==2,"No",""))


#WEIGHT2
BRFSS11$WEIGHT2<-ifelse(BRFSS11$WEIGHT2<=1000,round((as.numeric(BRFSS11$WEIGHT2)*0.453592),digits=0),
                       ifelse(BRFSS11$WEIGHT2 %in% c(9000:9998),BRFSS11$WEIGHT2-9000,""))

#WTKG3
BRFSS11$WTKG3<-NULL



####################################################################################

BRFSS11$HAVARTH3

BRFSS11_ARTHRITIS<- BRFSS11[BRFSS11$HAVARTH3!="",]

nrow(BRFSS11_ARTHRITIS)

nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$AGEG5YR=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$CHLDCNT=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$EDUCAG=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$HCVU651=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$INCOMG=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$PNEUMO2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$RFBING5=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$RFBMI5=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$RFHLTH=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$RFSMOK3=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$SMOKER3=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$STATE=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$TOTINDA=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$ADDEPEV2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$ALCDAY5=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$AVEDRNK2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$CADULT=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$CELLFON2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$CHCKIDNY=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$CPDEMO1=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$DISPCODE=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$EDUCA=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$EMTSUPRT=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$EXERANY2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$GENHLTH=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$HAVARTH3=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$HEIGHT3=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$HLTHPLN1=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$INCOME2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$LSATISFY=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$MARITAL=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$MAXDRNKS=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$MEDCOST=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$MSCODE=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$NUMADULT=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$NUMHHOL2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$NUMPHON2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$PERSDOC2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$PNEUVAC3=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$POORHLTH=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$PREGNANT=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$PROFEXAM=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$PVTRESD2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$QLACTLM2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$QSTLANG=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$RCSGENDR=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$RCSRLTN2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$RENTHOM1=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$SCNTLWK1=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$SEX=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$SMOKDAY2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$STOPSMK2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$USEEQUIP=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$USENOW3=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$VETERAN3=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$WEIGHT2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$ASTHMS1=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$BMI5=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$ASACTLIM=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$ASATTACK=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$ASDRVIST=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$ASINHALR=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$BLDSUGAR=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$CHCOCNCR=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$CHCSCNCR=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$CHECKUP1=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$CHILDREN=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$CHKHEMO3=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$CVDCRHD4=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$CVDINFR4=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$CVDSTRK3=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$DIABETE3=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$FLSHTMY2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$HIVTST6=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$INSULIN=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$LASTSMK2=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$PDIABTST=="",])
nrow(BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$PREDIAB1=="",])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$AGEG5YR),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$CHLDCNT),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$EDUCAG),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$HCVU651),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$INCOMG),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$PNEUMO2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$RFBING5),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$RFBMI5),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$RFHLTH),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$RFSMOK3),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$SMOKER3),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$STATE),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$TOTINDA),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$ADDEPEV2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$ALCDAY5),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$AVEDRNK2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$CADULT),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$CELLFON2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$CHCKIDNY),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$CPDEMO1),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$DISPCODE),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$EDUCA),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$EMTSUPRT),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$EXERANY2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$GENHLTH),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$HAVARTH3),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$HEIGHT3),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$HLTHPLN1),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$INCOME2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$LSATISFY),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$MARITAL),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$MAXDRNKS),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$MEDCOST),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$MSCODE),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$NUMADULT),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$NUMHHOL2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$NUMPHON2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$PERSDOC2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$PNEUVAC3),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$POORHLTH),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$PREGNANT),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$PROFEXAM),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$PVTRESD2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$QLACTLM2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$QSTLANG),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$RCSGENDR),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$RCSRLTN2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$RENTHOM1),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$SCNTLWK1),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$SEX),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$SMOKDAY2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$STOPSMK2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$USEEQUIP),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$USENOW3),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$VETERAN3),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$WEIGHT2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$ASTHMS1),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$BMI5),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$ASACTLIM),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$ASATTACK),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$ASDRVIST),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$ASINHALR),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$BLDSUGAR),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$CHCOCNCR),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$CHCSCNCR),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$CHECKUP1),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$CHILDREN),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$CHKHEMO3),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$CVDCRHD4),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$CVDINFR4),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$CVDSTRK3),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$DIABETE3),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$FLSHTMY2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$HIVTST6),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$INSULIN),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$LASTSMK2),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$PDIABTST),])
nrow(BRFSS11_ARTHRITIS[is.na(BRFSS11_ARTHRITIS$PREDIAB1),])


#########################################################################################


x11<-BRFSS11_ARTHRITIS[BRFSS11_ARTHRITIS$AGEG5YR!="",]
x11<-x11[x11$CHLDCNT!="",]
x11<-x11[x11$EDUCAG!="",]
x11<-x11[x11$RFBING5!="",]
x11<-x11[x11$RFHLTH!="",]
x11<-x11[x11$SMOKER3!="",]
x11<-x11[x11$TOTINDA!="",]
x11<-x11[x11$ADDEPEV2!="",]
x11<-x11[x11$ALCDAY5!="",]
x11<-x11[x11$CHCKIDNY!="",]
x11<-x11[x11$EDUCA!="",]
x11<-x11[x11$EXERANY2!="",]
x11<-x11[x11$GENHLTH!="",]
x11<-x11[x11$HAVARTH3!="",]
x11<-x11[x11$HEIGHT3!="",]
x11<-x11[x11$HLTHPLN1!="",]
x11<-x11[x11$MARITAL!="",]
x11<-x11[x11$MEDCOST!="",]
x11<-x11[x11$PERSDOC2!="",]
x11<-x11[x11$PNEUVAC3!="",]
x11<-x11[x11$PHYSHLTH!="",]
x11<-x11[x11$QLACTLM2!="",]
x11<-x11[x11$QSTLANG!="",]
x11<-x11[x11$SEX!="",]
x11<-x11[x11$VETERAN3!="",]
x11<-x11[x11$WEIGHT2!="",]
x11<-x11[x11$ASTHMS1!="",]
x11<-x11[x11$BMI5!="",]
x11<-x11[x11$CHCOCNCR!="",]
x11<-x11[x11$CVDSTRK3!="",]
x11<-x11[x11$DIABETE3!="",]
x11<-x11[x11$HCVU651!="",]
x11<-x11[x11$INCOME2!="",]
x11<-x11[x11$STATE!="",]
x11<-x11[x11$USEEQUIP!="",]
x11<-x11[x11$USENOW3!="",]
x11<-x11[x11$CHECKUP1!="",]
x11<-x11[x11$CHILDREN!="",]
x11<-x11[x15$CASTHDX2!="",]


a11<-x11[, c(	"AGEG5YR",	"CHLDCNT",	"EDUCAG",	"RFBING5",	"RFHLTH",	"SMOKER3",	"TOTINDA",	"ADDEPEV2",	"ALCDAY5",	"CHCKIDNY",	"EDUCA",	"EXERANY2",	"GENHLTH",	"HAVARTH3",	"HEIGHT3",	"HLTHPLN1",	"MARITAL",	"MEDCOST",	"PERSDOC2",	"PNEUVAC3",	"PHYSHLTH",	"QLACTLM2",	"QSTLANG",	"SEX",	"VETERAN3",	"WEIGHT2",	"ASTHMS1",	"BMI5",	"CHCOCNCR",	"CVDSTRK3",	"DIABETE3",	"HCVU651",	"INCOME2",	"STATE",	"USEEQUIP",	"USENOW3",	"CHECKUP1",	"CHILDREN","CASTHDX2")]
dim(a11)
x11<-a11
BRFSS11_ARTHRITIS<-x11

#########################################################################################

BRFSS12<-BRFSS_2012

#AGE65YR
BRFSS12$AGE65YR<- ifelse(BRFSS12$AGE65YR==1,"Yes",
                         ifelse(BRFSS12$AGE65YR==2,"No",""))

#ASTHMS1
BRFSS12$ASTHMS1<- ifelse(BRFSS12$ASTHMS1==1,"current",
                        ifelse(BRFSS12$ASTHMS1==2,"former",
                               ifelse(BRFSS12$ASTHMS1==3,"never","")))

#BMI5
BRFSS12$BMI5<- BRFSS12$BMI5/100

#CASTHM1
BRFSS12$CASTHM1<- ifelse(BRFSS12$CASTHM1==1,"No",
                        ifelse(BRFSS12$CASTHM1==2,"Yes",""))

#CHLDCNT
BRFSS12$CHLDCNT<- ifelse(BRFSS12$CHLDCNT==1,"no child",
                        ifelse(BRFSS12$CHLDCNT==2,"1",
                               ifelse(BRFSS12$CHLDCNT==3,"2",
                                      ifelse(BRFSS12$CHLDCNT==4,"3",
                                             ifelse(BRFSS12$CHLDCNT==5,"4",
                                                    ifelse(BRFSS12$CHLDCNT==6,"5",""))))))

#CLLCPWT
BRFSS12$CLLCPWT<- NULL

#DRDXAR1
BRFSS12$DRDXAR1<- ifelse(BRFSS12$DRDXAR1==1,"Yes",
                        ifelse(BRFSS12$DRDXAR1==2,"No",""))

#EDUCAG
BRFSS12$EDUCAG<- ifelse(BRFSS12$EDUCAG==1,"no High School",
                       ifelse(BRFSS12$EDUCAG==2,"High School Grad",
                              ifelse(BRFSS12$EDUCAG==3,"Attended College","College Grad")))

#HCVU651
BRFSS12$HCVU651<- ifelse(BRFSS12$HCVU651==1,"Yes",
                        ifelse(BRFSS12$HCVU651==2,"No",""))

#INCOMG
BRFSS12$INCOMG<- ifelse(BRFSS12$INCOMG==1,"less than 15000",
                       ifelse(BRFSS12$INCOMG==2,"15-25",
                              ifelse(BRFSS12$INCOMG==3,"25-35",
                                     ifelse(BRFSS12$INCOMG==4,"35-50",
                                            ifelse(BRFSS12$INCOMG==5,"greater than 50000","")))))

#LLCPWT
BRFSS12$LLCPWT<- NULL

#LTASTH1
BRFSS12$LTASTH1<- ifelse(BRFSS12$LTASTH1==1,"No",
                       ifelse(BRFSS12$LTASTH1==2,"Yes",""))

#PNEUMO2
BRFSS12$PNEUMO2<- ifelse(BRFSS12$PNEUMO2==1,"Yes",
                        ifelse(BRFSS12$PNEUMO2==2,"No",""))

#PSU
BRFSS12$PSU<- NULL

#PNEUMO2
BRFSS12$PNEUMO2<- NULL

#RFBING5
BRFSS12$RFBING5<- ifelse(BRFSS12$RFBING5==1,"No",
                        ifelse(BRFSS12$RFBING5==2,"Yes",""))

#RFBMI5
BRFSS12$RFBMI5<- ifelse(BRFSS12$RFBMI5==1,"BMI =<25",
                       ifelse(BRFSS12$RFBMI5==2,"BMI >25",""))

#RFHLTH
BRFSS12$RFHLTH<- ifelse(BRFSS12$RFHLTH==1,"good or better health",
                       ifelse(BRFSS12$RFHLTH==2,"fair or poor helath",""))

#RFSEAT2
BRFSS12$RFSEAT2<- ifelse(BRFSS12$RFSEAT2==1,"always wear seat belt",
                        ifelse(BRFSS12$RFSEAT2==2,"seldom or never",""))

#RFSEAT3
BRFSS12$RFSEAT3<- NULL

#RFSMOK3
BRFSS12$RFSMOK3<- ifelse(BRFSS12$RFSMOK3==1,"No",
                        ifelse(BRFSS12$RFSMOK3==2,"Yes",""))

#SMOKER3
BRFSS12$SMOKER3<- ifelse(BRFSS12$SMOKER3==1,"current/smokes everyday",
                                        ifelse(BRFSS12$SMOKER3==2,"current/smokes somedays",
                                               ifelse(BRFSS12$SMOKER3==3,"former",
                                                      ifelse(BRFSS12$SMOKER3==4,"never",""))))

#STATE
BRFSS12$STATE<- ifelse(BRFSS12$STATE==1,"Alabama",
                      ifelse(BRFSS12$STATE==2,"Alaska",
                             ifelse(BRFSS12$STATE==4,"Arizona",
                                    ifelse(BRFSS12$STATE==5,"Arkansas",
                                           ifelse(BRFSS12$STATE==6,"California",
                                                  ifelse(BRFSS12$STATE==8,"Colorado",
                                                         ifelse(BRFSS12$STATE==9,"Connecticut",
                                                                ifelse(BRFSS12$STATE==10,"Delaware",
                                                                       ifelse(BRFSS12$STATE==11,"District of Columbia",
                                                                              ifelse(BRFSS12$STATE==12,"Florida",
                                                                                     ifelse(BRFSS12$STATE==13,"Georgia",
                                                                                            ifelse(BRFSS12$STATE==15,"Hawaii",
                                                                                                   ifelse(BRFSS12$STATE==16,"Idaho",
                                                                                                          ifelse(BRFSS12$STATE==17,"Illinois",
                                                                                                                 ifelse(BRFSS12$STATE==18,"Indiana",
                                                                                                                        ifelse(BRFSS12$STATE==19,"Iowa",
                                                                                                                               ifelse(BRFSS12$STATE==20,"Kansas",
                                                                                                                                      ifelse(BRFSS12$STATE==21,"Kentucky",
                                                                                                                                             ifelse(BRFSS12$STATE==22,"Louisiana",
                                                                                                                                                    ifelse(BRFSS12$STATE==23,"Maine",
                                                                                                                                                           ifelse(BRFSS12$STATE==24,"Maryland",
                                                                                                                                                                  ifelse(BRFSS12$STATE==25,"Massachusetts",
                                                                                                                                                                         ifelse(BRFSS12$STATE==26,"Michigan",
                                                                                                                                                                                ifelse(BRFSS12$STATE==27,"Minnesota",
                                                                                                                                                                                       ifelse(BRFSS12$STATE==28,"Mississippi",
                                                                                                                                                                                              ifelse(BRFSS12$STATE==29,"Missouri",
                                                                                                                                                                                                     ifelse(BRFSS12$STATE==30,"Montana",
                                                                                                                                                                                                            ifelse(BRFSS12$STATE==31,"Nebraska",
                                                                                                                                                                                                                   ifelse(BRFSS12$STATE==32,"Nevada",
                                                                                                                                                                                                                          ifelse(BRFSS12$STATE==33,"New Hampshire",
                                                                                                                                                                                                                                 ifelse(BRFSS12$STATE==34,"New Jersey",
                                                                                                                                                                                                                                        ifelse(BRFSS12$STATE==35,"New Mexico",
                                                                                                                                                                                                                                               ifelse(BRFSS12$STATE==36,"New York",
                                                                                                                                                                                                                                                      ifelse(BRFSS12$STATE==37,"North Carolina",
                                                                                                                                                                                                                                                             ifelse(BRFSS12$STATE==38,"North Dakota",
                                                                                                                                                                                                                                                                    ifelse(BRFSS12$STATE==39,"Ohio",
                                                                                                                                                                                                                                                                           ifelse(BRFSS12$STATE==40,"Oklahoma",
                                                                                                                                                                                                                                                                                  ifelse(BRFSS12$STATE==41,"Oregon ",""
                                                                                                                                                                                                                                                                                  ))))))))))))))))))))))))))))))))))))))

#STRWT
BRFSS12$STRWT<- NULL

#STSTR
BRFSS12$STSTR<- NULL

#TOTINDA
BRFSS12$TOTINDA<- ifelse(BRFSS12$TOTINDA==1,"Yes",
                        ifelse(BRFSS12$TOTINDA==2,"No",""))

#WT2RAKE
BRFSS12$WT2RAKE<- NULL

#ADDEPEV2
BRFSS12$ADDEPEV2<- ifelse(BRFSS12$ADDEPEV2==1,"Yes",
                         ifelse(BRFSS12$ADDEPEV2==2,"No",""))

#ALCDAY5 - Convert days per week to approx days per month considering 30 days in a month.
BRFSS12$ALCDAY5<- ifelse(BRFSS12$ALCDAY5 %in% c(100:199), round((BRFSS12$ALCDAY5-100)*4.29,digits=0),
                        ifelse(BRFSS12$ALCDAY5 %in% c(200:299),BRFSS12$ALCDAY5-200,
                               ifelse(BRFSS12$ALCDAY5 ==888,0,"")))

#ASACTLIM
BRFSS12$ASACTLIM<- ifelse(BRFSS12$ASACTLIM<=365,BRFSS12$ASACTLIM,
                         ifelse(BRFSS12$ASACTLIM==888,0,""))

#ASATTACK
BRFSS12$ASATTACK<- ifelse(BRFSS12$ASATTACK==1,"Yes",
                         ifelse(BRFSS12$ASATTACK==2,"No",""))

#ASDRVIST
BRFSS12$ASDRVIST<- ifelse(BRFSS12$ASDRVIST<=87,BRFSS12$ASDRVIST,
                         ifelse(BRFSS12$ASDRVIST==88,0,""))

#ASERVIST
BRFSS12$ASERVIST<- ifelse(BRFSS12$ASERVIST<=87,BRFSS12$ASERVIST,
                         ifelse(BRFSS12$ASERVIST==88,0,""))

#ASINHALR
BRFSS12$ASINHALR<- ifelse(BRFSS12$ASINHALR==1,"1-4",
                         ifelse(BRFSS12$ASINHALR==2,"5-14",
                                ifelse(BRFSS12$ASINHALR==3,"15-29",
                                       ifelse(BRFSS12$ASINHALR==4,"30-59",
                                              ifelse(BRFSS12$ASINHALR==5,"60-99",
                                                     ifelse(BRFSS12$ASINHALR==6,">=100",
                                                            ifelse(BRFSS12$ASINHALR==8,"0","")))))))

#ASTHMA3
BRFSS12$ASTHMA3<- ifelse(BRFSS12$ASTHMA3==1,"Yes",
                        ifelse(BRFSS12$ASTHMA3==2,"No",""))

#ASTHMAGE
BRFSS12$ASTHMAGE<- ifelse(BRFSS12$ASTHMAGE==97,"<=10",
                         ifelse(BRFSS12$ASTHMAGE<97,BRFSS12$ASTHMAGE,""))

#ASTHMED3
BRFSS12$ASTHMED3<- ifelse(BRFSS12$ASTHMED3==1,"1-14",
                         ifelse(BRFSS12$ASTHMED3==2,"15-24",
                                ifelse(BRFSS12$ASTHMED3==3,"25-30",
                                       ifelse(BRFSS12$ASTHMED3==8,"0",""))))

#CADULT
BRFSS12$CADULT<- ifelse(BRFSS12$CADULT==1,"Yes/Male",
                       ifelse(BRFSS12$CADULT==2,"Yes/Female",""))

#CASTHDX2
BRFSS12$CASTHDX2<- ifelse(BRFSS12$CASTHDX2==1,"Yes",
                         ifelse(BRFSS12$CASTHDX2==2,"No",""))

#CASTHNO2
BRFSS12$CASTHNO2<- ifelse(BRFSS12$CASTHNO2==1,"Yes",
                         ifelse(BRFSS12$CASTHNO2==2,"No",""))

#CELLFON2
BRFSS12$CELLFON2<- ifelse(BRFSS12$CELLFON2==1,"Yes","")

#CHCKIDNY
BRFSS12$CHCKIDNY<- ifelse(BRFSS12$CHCKIDNY==1,"Yes",
                         ifelse(BRFSS12$CHCKIDNY==2,"No",""))

#CHCOCNCR
BRFSS12$CHCOCNCR<- ifelse(BRFSS12$CHCOCNCR==1,"Yes",
                         ifelse(BRFSS12$CHCOCNCR==2,"No",""))

#CHCSCNCR
BRFSS12$CHCSCNCR<- ifelse(BRFSS12$CHCSCNCR==1,"Yes",
                         ifelse(BRFSS12$CHCSCNCR==2,"No",""))

#CHECKUP1
BRFSS12$CHECKUP1<- ifelse(BRFSS12$CHECKUP1==1,"within past year",
                         ifelse(BRFSS12$CHECKUP1==2,"within past 2 years",
                                ifelse(BRFSS12$CHECKUP1==3,"within past 5 years",
                                       ifelse(BRFSS12$CHECKUP1==4,"5 or more years before",
                                              ifelse(BRFSS12$CHECKUP1==8,"never","")))))

#CHILDREN
BRFSS12$CHILDREN<- ifelse(BRFSS12$CHILDREN<=87,BRFSS12$CHILDREN,
                         ifelse(BRFSS12$CHILDREN==88,"0",""))

#CHKHEMO3
BRFSS12$CHKHEMO3<- ifelse(BRFSS12$CHKHEMO3<=76,BRFSS12$CHKHEMO3,
       ifelse(BRFSS12$CHKHEMO3==88,"0",
              ifelse(BRFSS12$CHKHEMO3==98,"never heard of the test","")))

#CPDEMO1
BRFSS12$CPDEMO1<- ifelse(BRFSS12$CPDEMO1==1,"Yes",
                        ifelse(BRFSS12$CPDEMO1==2,"No",""))

#CSTATE
BRFSS12$CSTATE<- ifelse(BRFSS12$CSTATE==1,"Yes",
                       ifelse(BRFSS12$CSTATE==2,"No",""))

#CTELENUM
BRFSS12$CTELENUM<- NULL

#CTELNUM1
BRFSS12$CTELNUM1<- NULL

#CVDCRHD4
BRFSS12$CVDCRHD4<- ifelse(BRFSS12$CVDCRHD4==1,"Yes",
                         ifelse(BRFSS12$CVDCRHD4==2,"No",""))

#CVDINFR4
BRFSS12$CVDINFR4<- ifelse(BRFSS12$CVDINFR4==1,"Yes",
                         ifelse(BRFSS12$CVDINFR4==2,"No",""))

#CVDSTRK3
BRFSS12$CVDSTRK3<- ifelse(BRFSS12$CVDSTRK3==1,"Yes",
                         ifelse(BRFSS12$CVDSTRK3==2,"No",""))

#DIABAGE2
BRFSS12$DIABAGE2<- ifelse(BRFSS12$DIABAGE2<=97,BRFSS12$DIABAGE2,"")

#DIABEDU
BRFSS12$DIABEDU<- ifelse(BRFSS12$DIABEDU==1,"Yes",
                        ifelse(BRFSS12$DIABEDU==2,"No",""))

#DIABETE3
BRFSS12$DIABETE3<- ifelse(BRFSS12$DIABETE3==1,"Yes",
                         ifelse(BRFSS12$DIABETE3==2,"Yes/female during pregnancy",
                                ifelse(BRFSS12$DIABETE3==3,"No",
                                       ifelse(BRFSS12$DIABETE3==2,"Borderline diabetes/No current diabetes",""))))

#DIABEYE
BRFSS12$DIABEYE<- ifelse(BRFSS12$DIABEYE==1,"Yes",
                        ifelse(BRFSS12$DIABEYE==2,"No",""))

#DISPCODE
BRFSS12$DISPCODE<- NULL

#DOCTDIAB
BRFSS12$DOCTDIAB<- ifelse(BRFSS12$DOCTDIAB==88,0,
                         ifelse(BRFSS12$DOCTDIAB<=76,BRFSS12$DOCTDIAB,""))

#DRNK3GE5
BRFSS12$DRNK3GE5<- ifelse(BRFSS12$DRNK3GE5==88,0,
                         ifelse(BRFSS12$DRNK3GE5<=76,BRFSS12$DRNK3GE5,""))

#DRNKANY5
BRFSS12$DRNKANY5<- ifelse(BRFSS12$DRNKANY5==1,"Yes",
                         ifelse(BRFSS12$DRNKANY5==2,"No",""))

#DROCDY3_
BRFSS12$DROCDY3_<- NULL

#EDUCA
BRFSS12$EDUCA<-ifelse(BRFSS12$EDUCA==1,"never attended school",
                     ifelse(BRFSS12$EDUCA==2,"1-8 elementary",
                            ifelse(BRFSS12$EDUCA==3,"9-11 high school",
                                   ifelse(BRFSS12$EDUCA==4,"grade 12 or higher",
                                          ifelse(BRFSS12$EDUCA==5,"college year 1-3",
                                                 ifelse(BRFSS12$EDUCA==6,"college year 4 or graduate",""))))))

#EMTSUPRT
BRFSS12$EMTSUPRT<- ifelse(BRFSS12$EMTSUPRT==1,"always",
                         ifelse(BRFSS12$EMTSUPRT==2,"usually",
                                ifelse(BRFSS12$EMTSUPRT==3,"sometimes",
                                       ifelse(BRFSS12$EMTSUPRT==4,"rarely",
                                              ifelse(BRFSS12$EMTSUPRT==5,"never","")))))

#EXERANY2
BRFSS12$EXERANY2<- ifelse(BRFSS12$EXERANY2==1,"Yes",
                         ifelse(BRFSS12$EXERANY2==2,"No",""))

#EYEEXAM
BRFSS12$EYEEXAM<- ifelse(BRFSS12$EYEEXAM==1,"within past one month",
                        ifelse(BRFSS12$EYEEXAM==2,"within past year",
                               ifelse(BRFSS12$EYEEXAM==3,"within past 2 years",
                                      ifelse(BRFSS12$EYEEXAM==4,"2 or more years",
                                             ifelse(BRFSS12$EYEEXAM==8,"never","")))))

#FEETCHK
BRFSS12$FEETCHK<- ifelse(BRFSS12$FEETCHK==88,0,
                        ifelse(BRFSS12$FEETCHK<=76,BRFSS12$DOCTDIAB,""))

#FEETCHK2
BRFSS12$FEETCHK2<- ifelse(BRFSS12$FEETCHK2 %in% c(100:199), round((BRFSS12$FEETCHK2-100)*365,digits=0),
                         ifelse(BRFSS12$FEETCHK2 %in% c(200:299),round((BRFSS12$FEETCHK2-200)*(365/7),digits=0),
                                ifelse(BRFSS12$FEETCHK2 %in% c(300:399),round((BRFSS12$FEETCHK2-300)*12,digits=0),
                                       ifelse(BRFSS12$FEETCHK2 %in% c(400:499),BRFSS12$FEETCHK2,
                                              ifelse(BRFSS12$FEETCHK2 ==888,0,"")))))

#AGE_G
BRFSS12$AGE_G<- ifelse(BRFSS12$AGE_G ==1, "18-24",
                      ifelse(BRFSS12$AGE_G ==2,"25-34",
                             ifelse(BRFSS12$AGE_G ==3,"35-44",
                                    ifelse(BRFSS12$AGE_G ==4,"45-54",
                                           ifelse(BRFSS12$AGE_G ==5,"55-64",">=65")))))

#AGEG5YR
BRFSS12$AGEG5YR<- ifelse(BRFSS12$AGEG5YR ==1, "18-24",
                        ifelse(BRFSS12$AGEG5YR ==2,"25-29",
                               ifelse(BRFSS12$AGEG5YR ==3,"30-34",
                                      ifelse(BRFSS12$AGEG5YR ==4,"35-39",
                                             ifelse(BRFSS12$AGEG5YR ==5,"40-44",
                                                    ifelse(BRFSS12$AGEG5YR ==6,"44-49",
                                                           ifelse(BRFSS12$AGEG5YR ==7,"50-54",
                                                                  ifelse(BRFSS12$AGEG5YR ==8,"55-59",
                                                                         ifelse(BRFSS12$AGEG5YR ==9,"60-64",
                                                                                ifelse(BRFSS12$AGEG5YR ==10,"65-69",
                                                                                       ifelse(BRFSS12$AGEG5YR ==11,"70-74",
                                                                                              ifelse(BRFSS12$AGEG5YR ==12,"75-79",
                                                                                                     ifelse(BRFSS12$AGEG5YR ==13,">=80","")))))))))))))

#AIDTST3
BRFSS12$AIDTST3<- ifelse(BRFSS12$AIDTST3==1,"Yes",
                        ifelse(BRFSS12$AIDTST3==2,"No",""))

#BMI5CAT
BRFSS12$BMI5CAT<- ifelse(BRFSS12$BMI5CAT==1,"Underweight (BMI<18.50)",
                        ifelse(BRFSS12$BMI5CAT==2,"Normal Weight (18.50<=BMI< 25.00)",
                               ifelse(BRFSS12$BMI5CAT==3,"Overweight (25.00<=BMI<30.00)",
                                      ifelse(BRFSS12$BMI5CAT==4,"Obese (30.00<=BMI<99.99)",""))))

#ASNOSLEP
BRFSS12$ASNOSLEP<- ifelse(BRFSS12$ASNOSLEP ==1, "1-2",
                         ifelse(BRFSS12$ASNOSLEP ==2,"3-4",
                                ifelse(BRFSS12$ASNOSLEP ==3,"5",
                                       ifelse(BRFSS12$ASNOSLEP ==4,"6-10",
                                              ifelse(BRFSS12$ASNOSLEP ==5,">=10",
                                                     ifelse(BRFSS12$ASNOSLEP ==8,"0",""))))))

#ASRCHKUP
BRFSS12$ASRCHKUP<- ifelse(BRFSS12$ASRCHKUP==88,0,
                         ifelse(BRFSS12$ASRCHKUP<=86,BRFSS12$ASRCHKUP,""))

#ASTHNOW
BRFSS12$ASTHNOW<- ifelse(BRFSS12$ASTHNOW==1,"Yes",
                        ifelse(BRFSS12$ASTHNOW==2,"No",""))

#ASYMPTOM
BRFSS12$ASYMPTOM<- ifelse(BRFSS12$ASYMPTOM ==1, "less than 1 in a week",
                         ifelse(BRFSS12$ASYMPTOM ==2,"1-2/week",
                                ifelse(BRFSS12$ASYMPTOM ==3,"3-6/week",
                                       ifelse(BRFSS12$ASYMPTOM ==4,"everyday but not all the time",
                                              ifelse(BRFSS12$ASYMPTOM ==5,"everyday all the time",
                                                     ifelse(BRFSS12$ASYMPTOM ==8,"never",""))))))

#AVEDRNK2
BRFSS12$AVEDRNK2<- ifelse(BRFSS12$AVEDRNK2<=76,BRFSS12$AVEDRNK2,"")

#BLDSTOOL
BRFSS12$BLDSTOOL<- ifelse(BRFSS12$BLDSTOOL==1,"Yes",
                         ifelse(BRFSS12$BLDSTOOL==2,"No",""))

#BLDSUGAR
BRFSS12$BLDSUGAR<- ifelse(BRFSS12$BLDSUGAR %in% c(100:199), round((BRFSS12$BLDSUGAR-100)*365,digits=0),
                         ifelse(BRFSS12$BLDSUGAR %in% c(200:299),round((BRFSS12$BLDSUGAR-200)*(365/7),digits=0),
                                ifelse(BRFSS12$BLDSUGAR %in% c(300:399),round((BRFSS12$BLDSUGAR-300)*12,digits=0),
                                       ifelse(BRFSS12$BLDSUGAR %in% c(400:499),BRFSS12$BLDSUGAR,
                                              ifelse(BRFSS12$BLDSUGAR ==888,0,"")))))



#FLSHTMY2
BRFSS12$FLSHTMY2<-ifelse(BRFSS12$FLSHTMY2==777777,"never",
                        ifelse(BRFSS12$FLSHTMY2<999999,paste(substr(BRFSS12$FLSHTMY2, 1, ifelse(nchar(BRFSS12$FLSHTMY2)==6,2,1)),"-",substr(BRFSS12$FLSHTMY2, ifelse(nchar(BRFSS12$FLSHTMY2)==6,3,2), nchar(BRFSS12$FLSHTMY2))),""))

#GENHLTH
BRFSS12$GENHLTH<-ifelse(BRFSS12$GENHLTH==1,"Excellent",
                       
                       ifelse(BRFSS12$GENHLTH==2,"Very Good",
                              
                              ifelse(BRFSS12$GENHLTH==3,"Good",
                                     ifelse(BRFSS12$GENHLTH==4,"Fair",
                                            ifelse(BRFSS12$GENHLTH==5,"poor","")))))


#HADHYST2
BRFSS12$HADHYST2<-ifelse(BRFSS12$HADHYST2==1,"Yes",
                        ifelse(BRFSS12$HADHYST2==2,"No",""))

#HADMAM
BRFSS12$HADMAM<-ifelse(BRFSS12$HADMAM==1,"Yes",
                      ifelse(BRFSS12$HADMAM==2,"No",""))

#HADPAP2
BRFSS12$HADPAP2<-ifelse(BRFSS12$HADPAP2==1,"Yes",
                       ifelse(BRFSS12$HADPAP2==2,"No",""))


#HADSGCO1
BRFSS12$HADSGCO1<-ifelse(BRFSS12$HADSGCO1==1,"Sigmoidal",
                        ifelse(BRFSS12$HADSGCO1==2,"Colonoscopy",""))

#HADSIGM3
BRFSS12$HADSIGM3<-ifelse(BRFSS12$HADSIGM3==1,"Yes",
                        ifelse(BRFSS12$HADSIGM3==2,"No",""))

#HAVARTH3
BRFSS12$HAVARTH3<-ifelse(BRFSS12$HAVARTH3==1,"Yes",
                        ifelse(BRFSS12$HAVARTH3==2,"No",""))

#HEIGHT3
BRFSS12$HEIGHT3<-ifelse(BRFSS12$HEIGHT3<=800,((as.numeric(substr(BRFSS12$HEIGHT3,1,1))*12) +  as.numeric(substr(BRFSS12$HEIGHT3,2,3)))*2.54,
                       ifelse(BRFSS12$HEIGHT3 %in% c(9000:9998),substr(BRFSS12$HEIGHT3,2,4),""))

#HIVTST6
BRFSS12$HIVTST6<-ifelse(BRFSS12$HIVTST6==1,"Yes",
                       ifelse(BRFSS12$HIVTST6==2,"No",""))

#HIVTSTD3
BRFSS12$HIVTSTD3-ifelse(BRFSS12$HIVTSTD3<777777,paste(substr(BRFSS12$HIVTSTD3, 1, ifelse(nchar(BRFSS12$HIVTSTD3)==6,2,1)),"-",substr(BRFSS12$HIVTSTD3, ifelse(nchar(BRFSS12$HIVTSTD3)==6,3,2), nchar(BRFSS12$HIVTSTD3))),"")

#HLTHPLN1
BRFSS12$HLTHPLN1<- ifelse(BRFSS12$HLTHPLN1==1,"Yes",
                         ifelse(BRFSS12$HLTHPLN1==2,"No",""))


#HOWLONG
BRFSS12$HOWLONG<- ifelse(BRFSS12$HOWLONG==1,"within 1 year",
                        ifelse(BRFSS12$HOWLONG==2,"1-2 years",
                               ifelse(BRFSS12$HOWLONG==2,"2-3 years",
                                      ifelse(BRFSS12$HOWLONG==2,"3-5 years",
                                             ifelse(BRFSS12$HOWLONG==2,"before 5 years","")))))

#HPVADSHT
BRFSS12$HPVADSHT<- ifelse(BRFSS12$HPVADSHT==3,"all shots taken",
                         ifelse(BRFSS12$HPVADSHT<=2,paste(BRFSS12$HPVADSHT,"shots taken"),""))

#HPVADVC2
BRFSS12$HPVADVC2<- ifelse(BRFSS12$HPVADVC2==1,"Yes",
                         ifelse(BRFSS12$HPVADVC2==2,"No",""))

#HTIN4
BRFSS12$HTIN4<- NULL

#HTM4
BRFSS12$HTM4<- NULL

#IDATE
BRFSS12$IDATE<- NULL

#IMFVPLAC
BRFSS12$IMFVPLAC<-ifelse(BRFSS12$IMFVPLAC==1,"Doctor's office/Health Management Organization",
                        
                        ifelse(BRFSS12$IMFVPLAC==2,"A health department",
                               
                               ifelse(BRFSS12$IMFVPLAC==3,"Another type of clinic or health center",
                                      ifelse(BRFSS12$IMFVPLAC==4,"A senior,recreation or community center",
                                             ifelse(BRFSS12$IMFVPLAC==5,"A store",
                                                    ifelse(BRFSS12$IMFVPLAC==6,"A hospital(e.g. in patient)",
                                                           ifelse(BRFSS12$IMFVPLAC==7,"An emergency room",
                                                                  ifelse(BRFSS12$IMFVPLAC==8,"Workplace",
                                                                         ifelse(BRFSS12$IMFVPLAC==9,"Some other kind of place",
                                                                                ifelse(BRFSS12$IMFVPLAC==10,"Received vaccination in Canada/Mexico",
                                                                                       ifelse(BRFSS12$IMFVPLAC==11,"A school","")))))))))))






#INCOME2
BRFSS12$INCOME2<-ifelse(BRFSS12$INCOME2==1,"Less than 10,000",
                       ifelse(BRFSS12$INCOME2==2,"Less than 15,000",
                              ifelse(BRFSS12$INCOME2==3,"Less than 20,000",
                                     ifelse(BRFSS12$INCOME2==4,"Less than 25,000",
                                            ifelse(BRFSS12$INCOME2==5,"Less than 35,000",
                                                   ifelse(BRFSS12$INCOME2==6,"Less than 50,000",
                                                          ifelse(BRFSS12$INCOME2==7,"Less than 75,000",
                                                                 ifelse(BRFSS12$INCOME2==8,"75,000 or more",""))))))))


#INSULIN
BRFSS12$INSULIN<-ifelse(BRFSS12$INSULIN==1,"Yes",
                       ifelse(BRFSS12$INSULIN==2,"No",""))


#IYEAR
BRFSS12$IYEAR<-NULL


#LANDLINE
BRFSS12$LANDLINE<-ifelse(BRFSS12$LANDLINE==1,"Yes",
                        ifelse(BRFSS12$LANDLINE==2," No",""))

#LASTPAP2
BRFSS12$LASTPAP2<-ifelse(BRFSS12$LASTPAP2==1,"Within 1 year",
                        ifelse(BRFSS12$LASTPAP2==2,"1 -2 years",
                               ifelse(BRFSS12$LASTPAP2==3,"2-3 years",
                                      ifelse(BRFSS12$LASTPAP2==4,"3-5 years",
                                             ifelse(BRFSS12$LASTPAP2==5,"before 5 years","")))))


#LASTSIG3
BRFSS12$LASTSIG3<-ifelse(BRFSS12$LASTSIG3==1,"Within 1 year",
                        ifelse(BRFSS12$LASTSIG3==2,"1-2 years",
                               ifelse(BRFSS12$LASTSIG3==3,"2-3 years",
                                      ifelse(BRFSS12$LASTSIG3==4,"3-5 years",
                                             ifelse(BRFSS12$LASTSIG3==5,"5-10 years",
                                                    ifelse(BRFSS12$LASTSIG3==6,"before 10 years",""))))))

#LASTSMK2
BRFSS12$LASTSMK2<-ifelse(BRFSS12$LASTSMK2==1,"Everyday",
                        ifelse(BRFSS12$LASTSMK2==2,"Somedays",
                               ifelse(BRFSS12$LASTSMK2==3,"Not at all","")))


#LENGEXAM
BRFSS12$LENGEXAM<-ifelse(BRFSS12$LENGEXAM==1,"Within 1 year",
                        ifelse(BRFSS12$LENGEXAM==2,"1 -2 years",
                               ifelse(BRFSS12$LENGEXAM==3,"2-3 years",
                                      ifelse(BRFSS12$LENGEXAM==4,"3-5 years",
                                             ifelse(BRFSS12$LENGEXAM==5,"before 5 years","")))))


#LSATISFY
BRFSS12$LSATISFY<-ifelse(BRFSS12$LSATISFY==1,"Very satisfied",
                        ifelse(BRFSS12$LSATISFY==2,"satisfied",
                               ifelse(BRFSS12$LSATISFY==3,"Dissatisfied",
                                      ifelse(BRFSS12$LSATISFY==4,"Very dissatisfied",""))))


#LSTBLDS3
BRFSS12$LSTBLDS3<-ifelse(BRFSS12$LSTBLDS3==1,"Within 1 year",
                        ifelse(BRFSS12$LSTBLDS3==2,"1 -2 years",
                               ifelse(BRFSS12$LSTBLDS3==3,"2-3 years",
                                      ifelse(BRFSS12$LSTBLDS3==4,"3-5 years",
                                             ifelse(BRFSS12$LSTBLDS3==5,"before 5 years","")))))


#MARITAL
BRFSS12$MARITAL<-ifelse(BRFSS12$MARITAL==1,"Married",
                       ifelse(BRFSS12$MARITAL==2,"Divorced",
                              ifelse(BRFSS12$MARITAL==3,"Widowed",
                                     ifelse(BRFSS12$MARITAL==4,"Separated",
                                            ifelse(BRFSS12$MARITAL==5,"Never married",
                                                   ifelse(BRFSS12$MARITAL==6,"A member of an unmarried couple",""))))))



#MAXDRNKS
BRFSS12$MAXDRNKS<-ifelse(BRFSS12$MAXDRNKS<=76,BRFSS12$MAXDRNKS,"")


#MEDCOST
BRFSS12$MEDCOST<-ifelse(BRFSS12$MEDCOST==1,"Yes",
                       ifelse(BRFSS12$MEDCOST==2,"No",""))

#MENTHLTH
BRFSS12$MENTHLTH<-ifelse(BRFSS12$MENTHLTH<=30,BRFSS12$MENTHLTH,
                        ifelse(BRFSS12$MENTHLTH==88,0,""))


#MSCODE
BRFSS12$MSCODE<-ifelse(BRFSS12$MSCODE==1,"In city center",
                      ifelse(BRFSS12$MSCODE==2,"outside city center but inside county",
                             ifelse(BRFSS12$MSCODE==3,"Inside the suburban county",
                                    ifelse(BRFSS12$MSCODE==5,"Not in an MSA",""))))


#NUMADULT
BRFSS12$NUMADULT<-ifelse(BRFSS12$NUMADULT<=99,BRFSS12$NUMADULT,"")


#NUMHHOL2
BRFSS12$NUMHHOL2<-ifelse(BRFSS12$NUMADULT==1,"Yes",
                        ifelse(BRFSS12$NUMADULT==2,"No",""))

#NUMMEN
BRFSS12$NUMMEN<-ifelse(BRFSS12$NUMMEN<=99,BRFSS12$NUMMEN,"")


#NUMPHON2
BRFSS12$NUMPHON2<-NULL


#NUMWOMEN
BRFSS12$NUMWOMEN<-ifelse(BRFSS12$NUMWOMEN<=99,BRFSS12$NUMWOMEN,"")


#PAINACT2
BRFSS12$PAINACT2<-NULL


#PDIABTST
BRFSS12$PDIABTST<-ifelse(BRFSS12$PDIABTST==1,"Yes",
                        ifelse(BRFSS12$PDIABTST==2,"No",""))


#PERSDOC2
BRFSS12$PERSDOC2<-ifelse(BRFSS12$PERSDOC2==1,"YEes, only 1",
                        ifelse(BRFSS12$PERSDOC2==2,"More than 1",
                               ifelse(BRFSS12$PERSDOC2==3,"no","")))

#PHYSHLTH
BRFSS12$PHYSHLTH<-ifelse(BRFSS12$PHYSHLTH<=30,BRFSS12$PHYSHLTH,
                        ifelse(BRFSS12$PHYSHLTH==88,0,""))

#PNEUVAC3
BRFSS12$PNEUVAC3<-ifelse(BRFSS12$PNEUVAC3==1,"Yes",
                        ifelse(BRFSS12$PNEUVAC3==2,"No",""))


#POORHLTH
BRFSS12$POORHLTH<-ifelse(BRFSS12$POORHLTH<=30,BRFSS12$POORHLTH,
                        ifelse(BRFSS12$POORHLTH==88,0,""))


#PREDIAB1
BRFSS12$PREDIAB1<-ifelse(BRFSS12$PREDIAB1==1,"Yes",
                        ifelse(BRFSS12$PREDIAB1==2,"Yes,during pregnency",
                               ifelse(BRFSS12$PREDIAB1==3,"No","")))


#PREGNANT
BRFSS12$PREGNANT<-ifelse(BRFSS12$PREGNANT==1,"Yes",
                        ifelse(BRFSS12$PREGNANT==2,"No",""))


#PROFEXAM
BRFSS12$PROFEXAM<-ifelse(BRFSS12$PROFEXAM==1,"Yes",
                        ifelse(BRFSS12$PROFEXAM==2,"No",""))

#PSATEST1
BRFSS12$PSATEST1<-ifelse(BRFSS12$PSATEST1==1,"Yes",
                        ifelse(BRFSS12$PSATEST1==2,"No",""))


#PSATIME
BRFSS12$PSATIME<-ifelse(BRFSS12$PSATIME==1,"Within 1 year",
                       ifelse(BRFSS12$PSATIME==2,"1 -2 years",
                              ifelse(BRFSS12$PSATIME==3,"2-3 years",
                                     ifelse(BRFSS12$PSATIME==4,"3-5 years",
                                            ifelse(BRFSS12$PSATIME==5,"before 5 years","")))))


#PVTRESD2
BRFSS12$PVTRESD2<-ifelse(BRFSS12$PVTRESD2==1,"Yes",
                        ifelse(BRFSS12$PVTRESD2==2,"No",""))


#QLACTLM2
BRFSS12$QLACTLM2<-ifelse(BRFSS12$QLACTLM2==1,"Yes",
                        ifelse(BRFSS12$QLACTLM2==2,"No",""))


#QLHLTH2
BRFSS12$QLHLTH2<-NULL


#QLMENTL2
BRFSS12$QLMENTL2<-NULL


#QLSTRES2
BRFSS12$QLSTRES2<-NULL


#QSTLANG
BRFSS12$QSTLANG<-ifelse(BRFSS12$QSTLANG==1,"English",
                       ifelse(BRFSS12$QSTLANG==2,"Spanish",
                              ifelse(BRFSS12$QSTLANG>=3,"other","")))


#RCSGENDR
BRFSS12$RCSGENDR<-ifelse(BRFSS12$RCSGENDR==1,"Boy",
                        ifelse(BRFSS12$RCSGENDR==2,"Girl",""))


#RCSRLTN2
BRFSS12$RCSRLTN2<-ifelse(BRFSS12$RCSRLTN2==1,"Parent",
                        ifelse(BRFSS12$RCSRLTN2==2,"Grandparent",
                               ifelse(BRFSS12$RCSRLTN2==3,"Foster Parent or Guardian",
                                      ifelse(BRFSS12$RCSRLTN2==4,"Sibling",
                                             ifelse(BRFSS12$RCSRLTN2==5,"Other relative",
                                                    ifelse(BRFSS12$RCSRLTN2==6,"Not related in any way",
                                                           ifelse(BRFSS12$RCSRLTN2==7,"Don't know not sure","")))))))



#RENTHOM1
BRFSS12$RENTHOM1<-ifelse(BRFSS12$RENTHOM1==1,"Parent",
                        ifelse(BRFSS12$RENTHOM1==2,"Own",
                               ifelse(BRFSS12$RENTHOM1==3,"Rent",
                                      ifelse(BRFSS12$RENTHOM1==4,"Other Arrangement",""))))


#SCNTLPAD
BRFSS12$SCNTLPAD<-ifelse(BRFSS12$SCNTLPAD==1,"By Salary",
                        ifelse(BRFSS12$SCNTLPAD==2,"Paid by hour",
                               ifelse(BRFSS12$SCNTLPAD==3,"Paid by job/task(commission)",
                                      ifelse(BRFSS12$SCNTLPAD==4,"Paid some other way",""))))



#SCNTLWK1
BRFSS12$SCNTLWK1<- ifelse(BRFSS12$SCNTLWK1==98,0,
                         ifelse(BRFSS12$SCNTLWK1<=96,BRFSS12$SCNTLWK1,""))


#SCNTPAID
BRFSS12$SCNTPAID<-NULL


#SCNTWRK1
BRFSS12$SCNTWRK1<-NULL


#SEATBELT
BRFSS12$SEATBELT<-ifelse(BRFSS12$SEATBELT==1,"always",
                        ifelse(BRFSS12$SEATBELT==2,"nearly always",
                               ifelse(BRFSS12$SEATBELT==3,"sometimes",
                                      ifelse(BRFSS12$SEATBELT==4,"seldom",
                                             ifelse(BRFSS12$SEATBELT==5,"never",
                                                    ifelse(BRFSS12$SEATBELT==8,"Never drive or ride a car",""))))))


#SEQNO
BRFSS12$SEQNO<-NULL


#SEX
BRFSS12$SEX<-ifelse(BRFSS12$SEX==1,"Male",
                   ifelse(BRFSS12$SEX==2,"Female",""))



#SMOKDAY2
BRFSS12$SMOKDAY2<-ifelse(BRFSS12$SMOKDAY2==1,"Everyday",
                        ifelse(BRFSS12$SMOKDAY2==2,"Somedays",
                               ifelse(BRFSS12$SMOKDAY2==3,"Not at all","")))



#SMOKE100
BRFSS12$SMOKE100<-ifelse(BRFSS12$SMOKE100==1,"Yes",
                        ifelse(BRFSS12$SMOKE100==2,"No",""))



#STOPSMK2
BRFSS12$STOPSMK2<-ifelse(BRFSS12$STOPSMK2==1,"Yes",
                        ifelse(BRFSS12$STOPSMK2==2,"No",""))


#USEEQUIP
BRFSS12$USEEQUIP<-ifelse(BRFSS12$USEEQUIP==1,"Yes",
                        ifelse(BRFSS12$USEEQUIP==2,"No",""))


#USENOW3
BRFSS12$USENOW3<-ifelse(BRFSS12$USENOW3==1,"Everyday",
                       ifelse(BRFSS12$USENOW3==2,"Somedays",
                              ifelse(BRFSS12$USENOW3==3,"Not at all","")))



#VETERAN3
BRFSS12$VETERAN3<-ifelse(BRFSS12$VETERAN3==1,"Yes",
                        ifelse(BRFSS12$VETERAN3==2,"No",""))


#WEIGHT2
BRFSS12$WEIGHT2<-ifelse(BRFSS12$WEIGHT2<=1000,round((as.numeric(BRFSS12$WEIGHT2)*0.453592),digits=0),
                       ifelse(BRFSS12$WEIGHT2 %in% c(9000:9998),BRFSS12$WEIGHT2-9000,""))

#WTKG3
BRFSS12$WTKG3<-NULL


###################################################################################

BRFSS12$HAVARTH3

BRFSS12_ARTHRITIS<- BRFSS12[BRFSS12$HAVARTH3!="",]

nrow(BRFSS12_ARTHRITIS)

nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$AGEG5YR=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$CHLDCNT=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$EDUCAG=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$HCVU651=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$INCOMG=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$PNEUMO2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$RFBING5=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$RFBMI5=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$RFHLTH=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$RFSMOK3=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$SMOKER3=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$STATE=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$TOTINDA=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$ADDEPEV2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$ALCDAY5=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$AVEDRNK2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$CADULT=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$CELLFON2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$CHCKIDNY=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$CPDEMO1=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$DISPCODE=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$EDUCA=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$EMTSUPRT=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$EXERANY2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$GENHLTH=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$HAVARTH3=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$HEIGHT3=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$HLTHPLN1=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$INCOME2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$LSATISFY=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$MARITAL=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$MAXDRNKS=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$MEDCOST=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$MSCODE=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$NUMADULT=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$NUMHHOL2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$NUMPHON2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$PERSDOC2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$PNEUVAC3=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$POORHLTH=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$PREGNANT=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$PROFEXAM=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$PVTRESD2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$QLACTLM2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$QSTLANG=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$RCSGENDR=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$RCSRLTN2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$RENTHOM1=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$SCNTLWK1=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$SEX=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$SMOKDAY2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$STOPSMK2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$USEEQUIP=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$USENOW3=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$VETERAN3=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$WEIGHT2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$ASTHMS1=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$BMI5=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$ASACTLIM=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$ASATTACK=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$ASDRVIST=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$ASINHALR=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$BLDSUGAR=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$CHCOCNCR=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$CHCSCNCR=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$CHECKUP1=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$CHILDREN=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$CHKHEMO3=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$CVDCRHD4=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$CVDINFR4=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$CVDSTRK3=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$DIABETE3=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$FLSHTMY2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$HIVTST6=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$INSULIN=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$LASTSMK2=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$PDIABTST=="",])
nrow(BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$PREDIAB1=="",])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$AGEG5YR),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$CHLDCNT),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$EDUCAG),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$HCVU651),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$INCOMG),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$PNEUMO2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$RFBING5),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$RFBMI5),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$RFHLTH),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$RFSMOK3),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$SMOKER3),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$STATE),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$TOTINDA),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$ADDEPEV2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$ALCDAY5),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$AVEDRNK2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$CADULT),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$CELLFON2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$CHCKIDNY),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$CPDEMO1),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$DISPCODE),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$EDUCA),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$EMTSUPRT),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$EXERANY2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$GENHLTH),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$HAVARTH3),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$HEIGHT3),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$HLTHPLN1),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$INCOME2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$LSATISFY),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$MARITAL),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$MAXDRNKS),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$MEDCOST),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$MSCODE),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$NUMADULT),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$NUMHHOL2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$NUMPHON2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$PERSDOC2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$PNEUVAC3),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$POORHLTH),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$PREGNANT),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$PROFEXAM),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$PVTRESD2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$QLACTLM2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$QSTLANG),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$RCSGENDR),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$RCSRLTN2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$RENTHOM1),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$SCNTLWK1),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$SEX),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$SMOKDAY2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$STOPSMK2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$USEEQUIP),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$USENOW3),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$VETERAN3),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$WEIGHT2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$ASTHMS1),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$BMI5),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$ASACTLIM),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$ASATTACK),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$ASDRVIST),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$ASINHALR),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$BLDSUGAR),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$CHCOCNCR),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$CHCSCNCR),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$CHECKUP1),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$CHILDREN),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$CHKHEMO3),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$CVDCRHD4),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$CVDINFR4),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$CVDSTRK3),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$DIABETE3),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$FLSHTMY2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$HIVTST6),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$INSULIN),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$LASTSMK2),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$PDIABTST),])
nrow(BRFSS12_ARTHRITIS[is.na(BRFSS12_ARTHRITIS$PREDIAB1),])


#########################################################################################


x12<-BRFSS12_ARTHRITIS[BRFSS12_ARTHRITIS$AGEG5YR!="",]
x12<-x12[x12$CHLDCNT!="",]
x12<-x12[x12$EDUCAG!="",]
x12<-x12[x12$RFBING5!="",]
x12<-x12[x12$RFHLTH!="",]
x12<-x12[x12$SMOKER3!="",]
x12<-x12[x12$TOTINDA!="",]
x12<-x12[x12$ADDEPEV2!="",]
x12<-x12[x12$ALCDAY5!="",]
x12<-x12[x12$CHCKIDNY!="",]
x12<-x12[x12$EDUCA!="",]
x12<-x12[x12$EXERANY2!="",]
x12<-x12[x12$GENHLTH!="",]
x12<-x12[x12$HAVARTH3!="",]
x12<-x12[x12$HEIGHT3!="",]
x12<-x12[x12$HLTHPLN1!="",]
x12<-x12[x12$MARITAL!="",]
x12<-x12[x12$MEDCOST!="",]
x12<-x12[x12$PERSDOC2!="",]
x12<-x12[x12$PNEUVAC3!="",]
x12<-x12[x12$PHYSHLTH!="",]
x12<-x12[x12$QLACTLM2!="",]
x12<-x12[x12$QSTLANG!="",]
x12<-x12[x12$SEX!="",]
x12<-x12[x12$VETERAN3!="",]
x12<-x12[x12$WEIGHT2!="",]
x12<-x12[x12$ASTHMS1!="",]
x12<-x12[x12$BMI5!="",]
x12<-x12[x12$CHCOCNCR!="",]
x12<-x12[x12$CVDSTRK3!="",]
x12<-x12[x12$DIABETE3!="",]
x12<-x12[x12$HCVU651!="",]
x12<-x12[x12$INCOME2!="",]
x12<-x12[x12$STATE!="",]
x12<-x12[x12$USEEQUIP!="",]
x12<-x12[x12$USENOW3!="",]
x12<-x12[x12$CHECKUP1!="",]
x12<-x12[x12$CHILDREN!="",]
x15<-x15[x15$CASTHDX2!="",]



a12<-x12[, c(	"AGEG5YR",	"CHLDCNT",	"EDUCAG",	"RFBING5",	"RFHLTH",	"SMOKER3",	"TOTINDA",	"ADDEPEV2",	"ALCDAY5",	"CHCKIDNY",	"EDUCA",	"EXERANY2",	"GENHLTH",	"HAVARTH3",	"HEIGHT3",	"HLTHPLN1",	"MARITAL",	"MEDCOST",	"PERSDOC2",	"PNEUVAC3",	"PHYSHLTH",	"QLACTLM2",	"QSTLANG",	"SEX",	"VETERAN3",	"WEIGHT2",	"ASTHMS1",	"BMI5",	"CHCOCNCR",	"CVDSTRK3",	"DIABETE3",	"HCVU651",	"INCOME2",	"STATE",	"USEEQUIP",	"USENOW3",	"CHECKUP1",	"CHILDREN","CASTHDX2")]
dim(a12)
x12<-a12
BRFSS12_ARTHRITIS<-x12

####################################################################################################

BRFSS13<-BRFSS_2013

#AGE65YR
BRFSS13$AGE65YR<- ifelse(BRFSS13$AGE65YR==1,"Yes",
                         ifelse(BRFSS13$AGE65YR==2,"No",""))

#ASTHMS1
BRFSS13$ASTHMS1<- ifelse(BRFSS13$ASTHMS1==1,"current",
                        ifelse(BRFSS13$ASTHMS1==2,"former",
                               ifelse(BRFSS13$ASTHMS1==3,"never","")))

#BMI5
BRFSS13$BMI5<- BRFSS13$BMI5/100

#CASTHM1
BRFSS13$CASTHM1<- ifelse(BRFSS13$CASTHM1==1,"No",
                        ifelse(BRFSS13$CASTHM1==2,"Yes",""))

#CHLDCNT
BRFSS13$CHLDCNT<- ifelse(BRFSS13$CHLDCNT==1,"no child",
                        ifelse(BRFSS13$CHLDCNT==2,"1",
                               ifelse(BRFSS13$CHLDCNT==3,"2",
                                      ifelse(BRFSS13$CHLDCNT==4,"3",
                                             ifelse(BRFSS13$CHLDCNT==5,"4",
                                                    ifelse(BRFSS13$CHLDCNT==6,"5",""))))))

#CLLCPWT
BRFSS13$CLLCPWT<- NULL

#DRDXAR1
BRFSS13$DRDXAR1<- ifelse(BRFSS13$DRDXAR1==1,"Yes",
                        ifelse(BRFSS13$DRDXAR1==2,"No",""))

#EDUCAG
BRFSS13$EDUCAG<- ifelse(BRFSS13$EDUCAG==1,"no High School",
                       ifelse(BRFSS13$EDUCAG==2,"High School Grad",
                              ifelse(BRFSS13$EDUCAG==3,"Attended College","College Grad")))

#HCVU651
BRFSS13$HCVU651<- ifelse(BRFSS13$HCVU651==1,"Yes",
                        ifelse(BRFSS13$HCVU651==2,"No",""))

#INCOMG
BRFSS13$INCOMG<- ifelse(BRFSS13$INCOMG==1,"less than 15000",
                       ifelse(BRFSS13$INCOMG==2,"15-25",
                              ifelse(BRFSS13$INCOMG==3,"25-35",
                                     ifelse(BRFSS13$INCOMG==4,"35-50",
                                            ifelse(BRFSS13$INCOMG==5,"greater than 50000","")))))

#LLCPWT
BRFSS13$LLCPWT<- NULL

#LTASTH1
BRFSS13$LTASTH1<- ifelse(BRFSS13$LTASTH1==1,"No",
                       ifelse(BRFSS13$LTASTH1==2,"Yes",""))

#PNEUMO2
BRFSS13$PNEUMO2<- ifelse(BRFSS13$PNEUMO2==1,"Yes",
                        ifelse(BRFSS13$PNEUMO2==2,"No",""))

#PSU
BRFSS13$PSU<- NULL

#PNEUMO2
BRFSS13$PNEUMO2<- NULL

#RFBING5
BRFSS13$RFBING5<- ifelse(BRFSS13$RFBING5==1,"No",
                        ifelse(BRFSS13$RFBING5==2,"Yes",""))

#RFBMI5
BRFSS13$RFBMI5<- ifelse(BRFSS13$RFBMI5==1,"BMI =<25",
                       ifelse(BRFSS13$RFBMI5==2,"BMI >25",""))

#RFHLTH
BRFSS13$RFHLTH<- ifelse(BRFSS13$RFHLTH==1,"good or better health",
                       ifelse(BRFSS13$RFHLTH==2,"fair or poor helath",""))

#RFSEAT2
BRFSS13$RFSEAT2<- ifelse(BRFSS13$RFSEAT2==1,"always wear seat belt",
                        ifelse(BRFSS13$RFSEAT2==2,"seldom or never",""))

#RFSEAT3
BRFSS13$RFSEAT3<- NULL

#RFSMOK3
BRFSS13$RFSMOK3<- ifelse(BRFSS13$RFSMOK3==1,"No",
                        ifelse(BRFSS13$RFSMOK3==2,"Yes",""))

#SMOKER3
BRFSS13$SMOKER3<- ifelse(BRFSS13$SMOKER3==1,"current/smokes everyday",
                                        ifelse(BRFSS13$SMOKER3==2,"current/smokes somedays",
                                               ifelse(BRFSS13$SMOKER3==3,"former",
                                                      ifelse(BRFSS13$SMOKER3==4,"never",""))))

#STATE
BRFSS13$STATE<- ifelse(BRFSS13$STATE==1,"Alabama",
                      ifelse(BRFSS13$STATE==2,"Alaska",
                             ifelse(BRFSS13$STATE==4,"Arizona",
                                    ifelse(BRFSS13$STATE==5,"Arkansas",
                                           ifelse(BRFSS13$STATE==6,"California",
                                                  ifelse(BRFSS13$STATE==8,"Colorado",
                                                         ifelse(BRFSS13$STATE==9,"Connecticut",
                                                                ifelse(BRFSS13$STATE==10,"Delaware",
                                                                       ifelse(BRFSS13$STATE==11,"District of Columbia",
                                                                              ifelse(BRFSS13$STATE==12,"Florida",
                                                                                     ifelse(BRFSS13$STATE==13,"Georgia",
                                                                                            ifelse(BRFSS13$STATE==15,"Hawaii",
                                                                                                   ifelse(BRFSS13$STATE==16,"Idaho",
                                                                                                          ifelse(BRFSS13$STATE==17,"Illinois",
                                                                                                                 ifelse(BRFSS13$STATE==18,"Indiana",
                                                                                                                        ifelse(BRFSS13$STATE==19,"Iowa",
                                                                                                                               ifelse(BRFSS13$STATE==20,"Kansas",
                                                                                                                                      ifelse(BRFSS13$STATE==21,"Kentucky",
                                                                                                                                             ifelse(BRFSS13$STATE==22,"Louisiana",
                                                                                                                                                    ifelse(BRFSS13$STATE==23,"Maine",
                                                                                                                                                           ifelse(BRFSS13$STATE==24,"Maryland",
                                                                                                                                                                  ifelse(BRFSS13$STATE==25,"Massachusetts",
                                                                                                                                                                         ifelse(BRFSS13$STATE==26,"Michigan",
                                                                                                                                                                                ifelse(BRFSS13$STATE==27,"Minnesota",
                                                                                                                                                                                       ifelse(BRFSS13$STATE==28,"Mississippi",
                                                                                                                                                                                              ifelse(BRFSS13$STATE==29,"Missouri",
                                                                                                                                                                                                     ifelse(BRFSS13$STATE==30,"Montana",
                                                                                                                                                                                                            ifelse(BRFSS13$STATE==31,"Nebraska",
                                                                                                                                                                                                                   ifelse(BRFSS13$STATE==32,"Nevada",
                                                                                                                                                                                                                          ifelse(BRFSS13$STATE==33,"New Hampshire",
                                                                                                                                                                                                                                 ifelse(BRFSS13$STATE==34,"New Jersey",
                                                                                                                                                                                                                                        ifelse(BRFSS13$STATE==35,"New Mexico",
                                                                                                                                                                                                                                               ifelse(BRFSS13$STATE==36,"New York",
                                                                                                                                                                                                                                                      ifelse(BRFSS13$STATE==37,"North Carolina",
                                                                                                                                                                                                                                                             ifelse(BRFSS13$STATE==38,"North Dakota",
                                                                                                                                                                                                                                                                    ifelse(BRFSS13$STATE==39,"Ohio",
                                                                                                                                                                                                                                                                           ifelse(BRFSS13$STATE==40,"Oklahoma",
                                                                                                                                                                                                                                                                                  ifelse(BRFSS13$STATE==41,"Oregon ",""
                                                                                                                                                                                                                                                                                  ))))))))))))))))))))))))))))))))))))))

#STRWT
BRFSS13$STRWT<- NULL

#STSTR
BRFSS13$STSTR<- NULL

#TOTINDA
BRFSS13$TOTINDA<- ifelse(BRFSS13$TOTINDA==1,"Yes",
                        ifelse(BRFSS13$TOTINDA==2,"No",""))

#WT2RAKE
BRFSS13$WT2RAKE<- NULL

#ADDEPEV2
BRFSS13$ADDEPEV2<- ifelse(BRFSS13$ADDEPEV2==1,"Yes",
                         ifelse(BRFSS13$ADDEPEV2==2,"No",""))

#ALCDAY5 - Convert days per week to approx days per month considering 30 days in a month.
BRFSS13$ALCDAY5<- ifelse(BRFSS13$ALCDAY5 %in% c(100:199), round((BRFSS13$ALCDAY5-100)*4.29,digits=0),
                        ifelse(BRFSS13$ALCDAY5 %in% c(200:299),BRFSS13$ALCDAY5-200,
                               ifelse(BRFSS13$ALCDAY5 ==888,0,"")))

#ASACTLIM
BRFSS13$ASACTLIM<- ifelse(BRFSS13$ASACTLIM<=365,BRFSS13$ASACTLIM,
                         ifelse(BRFSS13$ASACTLIM==888,0,""))

#ASATTACK
BRFSS13$ASATTACK<- ifelse(BRFSS13$ASATTACK==1,"Yes",
                         ifelse(BRFSS13$ASATTACK==2,"No",""))

#ASDRVIST
BRFSS13$ASDRVIST<- ifelse(BRFSS13$ASDRVIST<=87,BRFSS13$ASDRVIST,
                         ifelse(BRFSS13$ASDRVIST==88,0,""))

#ASERVIST
BRFSS13$ASERVIST<- ifelse(BRFSS13$ASERVIST<=87,BRFSS13$ASERVIST,
                         ifelse(BRFSS13$ASERVIST==88,0,""))

#ASINHALR
BRFSS13$ASINHALR<- ifelse(BRFSS13$ASINHALR==1,"1-4",
                         ifelse(BRFSS13$ASINHALR==2,"5-14",
                                ifelse(BRFSS13$ASINHALR==3,"15-29",
                                       ifelse(BRFSS13$ASINHALR==4,"30-59",
                                              ifelse(BRFSS13$ASINHALR==5,"60-99",
                                                     ifelse(BRFSS13$ASINHALR==6,">=100",
                                                            ifelse(BRFSS13$ASINHALR==8,"0","")))))))

#ASTHMA3
BRFSS13$ASTHMA3<- ifelse(BRFSS13$ASTHMA3==1,"Yes",
                        ifelse(BRFSS13$ASTHMA3==2,"No",""))

#ASTHMAGE
BRFSS13$ASTHMAGE<- ifelse(BRFSS13$ASTHMAGE==97,"<=10",
                         ifelse(BRFSS13$ASTHMAGE<97,BRFSS13$ASTHMAGE,""))

#ASTHMED3
BRFSS13$ASTHMED3<- ifelse(BRFSS13$ASTHMED3==1,"1-14",
                         ifelse(BRFSS13$ASTHMED3==2,"15-24",
                                ifelse(BRFSS13$ASTHMED3==3,"25-30",
                                       ifelse(BRFSS13$ASTHMED3==8,"0",""))))

#CADULT
BRFSS13$CADULT<- ifelse(BRFSS13$CADULT==1,"Yes/Male",
                       ifelse(BRFSS13$CADULT==2,"Yes/Female",""))

#CASTHDX2
BRFSS13$CASTHDX2<- ifelse(BRFSS13$CASTHDX2==1,"Yes",
                         ifelse(BRFSS13$CASTHDX2==2,"No",""))

#CASTHNO2
BRFSS13$CASTHNO2<- ifelse(BRFSS13$CASTHNO2==1,"Yes",
                         ifelse(BRFSS13$CASTHNO2==2,"No",""))

#CELLFON2
BRFSS13$CELLFON2<- ifelse(BRFSS13$CELLFON2==1,"Yes","")

#CHCKIDNY
BRFSS13$CHCKIDNY<- ifelse(BRFSS13$CHCKIDNY==1,"Yes",
                         ifelse(BRFSS13$CHCKIDNY==2,"No",""))

#CHCOCNCR
BRFSS13$CHCOCNCR<- ifelse(BRFSS13$CHCOCNCR==1,"Yes",
                         ifelse(BRFSS13$CHCOCNCR==2,"No",""))

#CHCSCNCR
BRFSS13$CHCSCNCR<- ifelse(BRFSS13$CHCSCNCR==1,"Yes",
                         ifelse(BRFSS13$CHCSCNCR==2,"No",""))

#CHECKUP1
BRFSS13$CHECKUP1<- ifelse(BRFSS13$CHECKUP1==1,"within past year",
                         ifelse(BRFSS13$CHECKUP1==2,"within past 2 years",
                                ifelse(BRFSS13$CHECKUP1==3,"within past 5 years",
                                       ifelse(BRFSS13$CHECKUP1==4,"5 or more years before",
                                              ifelse(BRFSS13$CHECKUP1==8,"never","")))))

#CHILDREN
BRFSS13$CHILDREN<- ifelse(BRFSS13$CHILDREN<=87,BRFSS13$CHILDREN,
                         ifelse(BRFSS13$CHILDREN==88,"0",""))

#CHKHEMO3
BRFSS13$CHKHEMO3<- ifelse(BRFSS13$CHKHEMO3<=76,BRFSS13$CHKHEMO3,
       ifelse(BRFSS13$CHKHEMO3==88,"0",
              ifelse(BRFSS13$CHKHEMO3==98,"never heard of the test","")))

#CPDEMO1
BRFSS13$CPDEMO1<- ifelse(BRFSS13$CPDEMO1==1,"Yes",
                        ifelse(BRFSS13$CPDEMO1==2,"No",""))

#CSTATE
BRFSS13$CSTATE<- ifelse(BRFSS13$CSTATE==1,"Yes",
                       ifelse(BRFSS13$CSTATE==2,"No",""))

#CTELENUM
BRFSS13$CTELENUM<- NULL

#CTELNUM1
BRFSS13$CTELNUM1<- NULL

#CVDCRHD4
BRFSS13$CVDCRHD4<- ifelse(BRFSS13$CVDCRHD4==1,"Yes",
                         ifelse(BRFSS13$CVDCRHD4==2,"No",""))

#CVDINFR4
BRFSS13$CVDINFR4<- ifelse(BRFSS13$CVDINFR4==1,"Yes",
                         ifelse(BRFSS13$CVDINFR4==2,"No",""))

#CVDSTRK3
BRFSS13$CVDSTRK3<- ifelse(BRFSS13$CVDSTRK3==1,"Yes",
                         ifelse(BRFSS13$CVDSTRK3==2,"No",""))

#DIABAGE2
BRFSS13$DIABAGE2<- ifelse(BRFSS13$DIABAGE2<=97,BRFSS13$DIABAGE2,"")

#DIABEDU
BRFSS13$DIABEDU<- ifelse(BRFSS13$DIABEDU==1,"Yes",
                        ifelse(BRFSS13$DIABEDU==2,"No",""))

#DIABETE3
BRFSS13$DIABETE3<- ifelse(BRFSS13$DIABETE3==1,"Yes",
                         ifelse(BRFSS13$DIABETE3==2,"Yes/female during pregnancy",
                                ifelse(BRFSS13$DIABETE3==3,"No",
                                       ifelse(BRFSS13$DIABETE3==2,"Borderline diabetes/No current diabetes",""))))

#DIABEYE
BRFSS13$DIABEYE<- ifelse(BRFSS13$DIABEYE==1,"Yes",
                        ifelse(BRFSS13$DIABEYE==2,"No",""))

#DISPCODE
BRFSS13$DISPCODE<- NULL

#DOCTDIAB
BRFSS13$DOCTDIAB<- ifelse(BRFSS13$DOCTDIAB==88,0,
                         ifelse(BRFSS13$DOCTDIAB<=76,BRFSS13$DOCTDIAB,""))

#DRNK3GE5
BRFSS13$DRNK3GE5<- ifelse(BRFSS13$DRNK3GE5==88,0,
                         ifelse(BRFSS13$DRNK3GE5<=76,BRFSS13$DRNK3GE5,""))

#DRNKANY5
BRFSS13$DRNKANY5<- ifelse(BRFSS13$DRNKANY5==1,"Yes",
                         ifelse(BRFSS13$DRNKANY5==2,"No",""))

#DROCDY3_
BRFSS13$DROCDY3_<- NULL

#EDUCA
BRFSS13$EDUCA<-ifelse(BRFSS13$EDUCA==1,"never attended school",
                     ifelse(BRFSS13$EDUCA==2,"1-8 elementary",
                            ifelse(BRFSS13$EDUCA==3,"9-11 high school",
                                   ifelse(BRFSS13$EDUCA==4,"grade 12 or higher",
                                          ifelse(BRFSS13$EDUCA==5,"college year 1-3",
                                                 ifelse(BRFSS13$EDUCA==6,"college year 4 or graduate",""))))))

#EMTSUPRT
BRFSS13$EMTSUPRT<- ifelse(BRFSS13$EMTSUPRT==1,"always",
                         ifelse(BRFSS13$EMTSUPRT==2,"usually",
                                ifelse(BRFSS13$EMTSUPRT==3,"sometimes",
                                       ifelse(BRFSS13$EMTSUPRT==4,"rarely",
                                              ifelse(BRFSS13$EMTSUPRT==5,"never","")))))

#EXERANY2
BRFSS13$EXERANY2<- ifelse(BRFSS13$EXERANY2==1,"Yes",
                         ifelse(BRFSS13$EXERANY2==2,"No",""))

#EYEEXAM
BRFSS13$EYEEXAM<- ifelse(BRFSS13$EYEEXAM==1,"within past one month",
                        ifelse(BRFSS13$EYEEXAM==2,"within past year",
                               ifelse(BRFSS13$EYEEXAM==3,"within past 2 years",
                                      ifelse(BRFSS13$EYEEXAM==4,"2 or more years",
                                             ifelse(BRFSS13$EYEEXAM==8,"never","")))))

#FEETCHK
BRFSS13$FEETCHK<- ifelse(BRFSS13$FEETCHK==88,0,
                        ifelse(BRFSS13$FEETCHK<=76,BRFSS13$DOCTDIAB,""))

#FEETCHK2
BRFSS13$FEETCHK2<- ifelse(BRFSS13$FEETCHK2 %in% c(100:199), round((BRFSS13$FEETCHK2-100)*365,digits=0),
                         ifelse(BRFSS13$FEETCHK2 %in% c(200:299),round((BRFSS13$FEETCHK2-200)*(365/7),digits=0),
                                ifelse(BRFSS13$FEETCHK2 %in% c(300:399),round((BRFSS13$FEETCHK2-300)*12,digits=0),
                                       ifelse(BRFSS13$FEETCHK2 %in% c(400:499),BRFSS13$FEETCHK2,
                                              ifelse(BRFSS13$FEETCHK2 ==888,0,"")))))

#AGE_G
BRFSS13$AGE_G<- ifelse(BRFSS13$AGE_G ==1, "18-24",
                      ifelse(BRFSS13$AGE_G ==2,"25-34",
                             ifelse(BRFSS13$AGE_G ==3,"35-44",
                                    ifelse(BRFSS13$AGE_G ==4,"45-54",
                                           ifelse(BRFSS13$AGE_G ==5,"55-64",">=65")))))

#AGEG5YR
BRFSS13$AGEG5YR<- ifelse(BRFSS13$AGEG5YR ==1, "18-24",
                        ifelse(BRFSS13$AGEG5YR ==2,"25-29",
                               ifelse(BRFSS13$AGEG5YR ==3,"30-34",
                                      ifelse(BRFSS13$AGEG5YR ==4,"35-39",
                                             ifelse(BRFSS13$AGEG5YR ==5,"40-44",
                                                    ifelse(BRFSS13$AGEG5YR ==6,"44-49",
                                                           ifelse(BRFSS13$AGEG5YR ==7,"50-54",
                                                                  ifelse(BRFSS13$AGEG5YR ==8,"55-59",
                                                                         ifelse(BRFSS13$AGEG5YR ==9,"60-64",
                                                                                ifelse(BRFSS13$AGEG5YR ==10,"65-69",
                                                                                       ifelse(BRFSS13$AGEG5YR ==11,"70-74",
                                                                                              ifelse(BRFSS13$AGEG5YR ==12,"75-79",
                                                                                                     ifelse(BRFSS13$AGEG5YR ==13,">=80","")))))))))))))

#AIDTST3
BRFSS13$AIDTST3<- ifelse(BRFSS13$AIDTST3==1,"Yes",
                        ifelse(BRFSS13$AIDTST3==2,"No",""))

#BMI5CAT
BRFSS13$BMI5CAT<- ifelse(BRFSS13$BMI5CAT==1,"Underweight (BMI<18.50)",
                        ifelse(BRFSS13$BMI5CAT==2,"Normal Weight (18.50<=BMI< 25.00)",
                               ifelse(BRFSS13$BMI5CAT==3,"Overweight (25.00<=BMI<30.00)",
                                      ifelse(BRFSS13$BMI5CAT==4,"Obese (30.00<=BMI<99.99)",""))))

#ASNOSLEP
BRFSS13$ASNOSLEP<- ifelse(BRFSS13$ASNOSLEP ==1, "1-2",
                         ifelse(BRFSS13$ASNOSLEP ==2,"3-4",
                                ifelse(BRFSS13$ASNOSLEP ==3,"5",
                                       ifelse(BRFSS13$ASNOSLEP ==4,"6-10",
                                              ifelse(BRFSS13$ASNOSLEP ==5,">=10",
                                                     ifelse(BRFSS13$ASNOSLEP ==8,"0",""))))))

#ASRCHKUP
BRFSS13$ASRCHKUP<- ifelse(BRFSS13$ASRCHKUP==88,0,
                         ifelse(BRFSS13$ASRCHKUP<=86,BRFSS13$ASRCHKUP,""))

#ASTHNOW
BRFSS13$ASTHNOW<- ifelse(BRFSS13$ASTHNOW==1,"Yes",
                        ifelse(BRFSS13$ASTHNOW==2,"No",""))

#ASYMPTOM
BRFSS13$ASYMPTOM<- ifelse(BRFSS13$ASYMPTOM ==1, "less than 1 in a week",
                         ifelse(BRFSS13$ASYMPTOM ==2,"1-2/week",
                                ifelse(BRFSS13$ASYMPTOM ==3,"3-6/week",
                                       ifelse(BRFSS13$ASYMPTOM ==4,"everyday but not all the time",
                                              ifelse(BRFSS13$ASYMPTOM ==5,"everyday all the time",
                                                     ifelse(BRFSS13$ASYMPTOM ==8,"never",""))))))

#AVEDRNK2
BRFSS13$AVEDRNK2<- ifelse(BRFSS13$AVEDRNK2<=76,BRFSS13$AVEDRNK2,"")

#BLDSTOOL
BRFSS13$BLDSTOOL<- ifelse(BRFSS13$BLDSTOOL==1,"Yes",
                         ifelse(BRFSS13$BLDSTOOL==2,"No",""))

#BLDSUGAR
BRFSS13$BLDSUGAR<- ifelse(BRFSS13$BLDSUGAR %in% c(100:199), round((BRFSS13$BLDSUGAR-100)*365,digits=0),
                         ifelse(BRFSS13$BLDSUGAR %in% c(200:299),round((BRFSS13$BLDSUGAR-200)*(365/7),digits=0),
                                ifelse(BRFSS13$BLDSUGAR %in% c(300:399),round((BRFSS13$BLDSUGAR-300)*12,digits=0),
                                       ifelse(BRFSS13$BLDSUGAR %in% c(400:499),BRFSS13$BLDSUGAR,
                                              ifelse(BRFSS13$BLDSUGAR ==888,0,"")))))



#FLSHTMY2
BRFSS13$FLSHTMY2<-ifelse(BRFSS13$FLSHTMY2==777777,"never",
                        ifelse(BRFSS13$FLSHTMY2<999999,paste(substr(BRFSS13$FLSHTMY2, 1, ifelse(nchar(BRFSS13$FLSHTMY2)==6,2,1)),"-",substr(BRFSS13$FLSHTMY2, ifelse(nchar(BRFSS13$FLSHTMY2)==6,3,2), nchar(BRFSS13$FLSHTMY2))),""))

#GENHLTH
BRFSS13$GENHLTH<-ifelse(BRFSS13$GENHLTH==1,"Excellent",
                       
                       ifelse(BRFSS13$GENHLTH==2,"Very Good",
                              
                              ifelse(BRFSS13$GENHLTH==3,"Good",
                                     ifelse(BRFSS13$GENHLTH==4,"Fair",
                                            ifelse(BRFSS13$GENHLTH==5,"poor","")))))


#HADHYST2
BRFSS13$HADHYST2<-ifelse(BRFSS13$HADHYST2==1,"Yes",
                        ifelse(BRFSS13$HADHYST2==2,"No",""))

#HADMAM
BRFSS13$HADMAM<-ifelse(BRFSS13$HADMAM==1,"Yes",
                      ifelse(BRFSS13$HADMAM==2,"No",""))

#HADPAP2
BRFSS13$HADPAP2<-ifelse(BRFSS13$HADPAP2==1,"Yes",
                       ifelse(BRFSS13$HADPAP2==2,"No",""))


#HADSGCO1
BRFSS13$HADSGCO1<-ifelse(BRFSS13$HADSGCO1==1,"Sigmoidal",
                        ifelse(BRFSS13$HADSGCO1==2,"Colonoscopy",""))

#HADSIGM3
BRFSS13$HADSIGM3<-ifelse(BRFSS13$HADSIGM3==1,"Yes",
                        ifelse(BRFSS13$HADSIGM3==2,"No",""))

#HAVARTH3
BRFSS13$HAVARTH3<-ifelse(BRFSS13$HAVARTH3==1,"Yes",
                        ifelse(BRFSS13$HAVARTH3==2,"No",""))

#HEIGHT3
BRFSS13$HEIGHT3<-ifelse(BRFSS13$HEIGHT3<=800,((as.numeric(substr(BRFSS13$HEIGHT3,1,1))*12) +  as.numeric(substr(BRFSS13$HEIGHT3,2,3)))*2.54,
                       ifelse(BRFSS13$HEIGHT3 %in% c(9000:9998),substr(BRFSS13$HEIGHT3,2,4),""))

#HIVTST6
BRFSS13$HIVTST6<-ifelse(BRFSS13$HIVTST6==1,"Yes",
                       ifelse(BRFSS13$HIVTST6==2,"No",""))

#HIVTSTD3
BRFSS13$HIVTSTD3-ifelse(BRFSS13$HIVTSTD3<777777,paste(substr(BRFSS13$HIVTSTD3, 1, ifelse(nchar(BRFSS13$HIVTSTD3)==6,2,1)),"-",substr(BRFSS13$HIVTSTD3, ifelse(nchar(BRFSS13$HIVTSTD3)==6,3,2), nchar(BRFSS13$HIVTSTD3))),"")

#HLTHPLN1
BRFSS13$HLTHPLN1<- ifelse(BRFSS13$HLTHPLN1==1,"Yes",
                         ifelse(BRFSS13$HLTHPLN1==2,"No",""))


#HOWLONG
BRFSS13$HOWLONG<- ifelse(BRFSS13$HOWLONG==1,"within 1 year",
                        ifelse(BRFSS13$HOWLONG==2,"1-2 years",
                               ifelse(BRFSS13$HOWLONG==2,"2-3 years",
                                      ifelse(BRFSS13$HOWLONG==2,"3-5 years",
                                             ifelse(BRFSS13$HOWLONG==2,"before 5 years","")))))

#HPVADSHT
BRFSS13$HPVADSHT<- ifelse(BRFSS13$HPVADSHT==3,"all shots taken",
                         ifelse(BRFSS13$HPVADSHT<=2,paste(BRFSS13$HPVADSHT,"shots taken"),""))

#HPVADVC2
BRFSS13$HPVADVC2<- ifelse(BRFSS13$HPVADVC2==1,"Yes",
                         ifelse(BRFSS13$HPVADVC2==2,"No",""))

#HTIN4
BRFSS13$HTIN4<- NULL

#HTM4
BRFSS13$HTM4<- NULL

#IDATE
BRFSS13$IDATE<- NULL

#IMFVPLAC
BRFSS13$IMFVPLAC<-ifelse(BRFSS13$IMFVPLAC==1,"Doctor's office/Health Management Organization",
                        
                        ifelse(BRFSS13$IMFVPLAC==2,"A health department",
                               
                               ifelse(BRFSS13$IMFVPLAC==3,"Another type of clinic or health center",
                                      ifelse(BRFSS13$IMFVPLAC==4,"A senior,recreation or community center",
                                             ifelse(BRFSS13$IMFVPLAC==5,"A store",
                                                    ifelse(BRFSS13$IMFVPLAC==6,"A hospital(e.g. in patient)",
                                                           ifelse(BRFSS13$IMFVPLAC==7,"An emergency room",
                                                                  ifelse(BRFSS13$IMFVPLAC==8,"Workplace",
                                                                         ifelse(BRFSS13$IMFVPLAC==9,"Some other kind of place",
                                                                                ifelse(BRFSS13$IMFVPLAC==10,"Received vaccination in Canada/Mexico",
                                                                                       ifelse(BRFSS13$IMFVPLAC==11,"A school","")))))))))))






#INCOME2
BRFSS13$INCOME2<-ifelse(BRFSS13$INCOME2==1,"Less than 10,000",
                       ifelse(BRFSS13$INCOME2==2,"Less than 15,000",
                              ifelse(BRFSS13$INCOME2==3,"Less than 20,000",
                                     ifelse(BRFSS13$INCOME2==4,"Less than 25,000",
                                            ifelse(BRFSS13$INCOME2==5,"Less than 35,000",
                                                   ifelse(BRFSS13$INCOME2==6,"Less than 50,000",
                                                          ifelse(BRFSS13$INCOME2==7,"Less than 75,000",
                                                                 ifelse(BRFSS13$INCOME2==8,"75,000 or more",""))))))))


#INSULIN
BRFSS13$INSULIN<-ifelse(BRFSS13$INSULIN==1,"Yes",
                       ifelse(BRFSS13$INSULIN==2,"No",""))


#IYEAR
BRFSS13$IYEAR<-NULL


#LANDLINE
BRFSS13$LANDLINE<-ifelse(BRFSS13$LANDLINE==1,"Yes",
                        ifelse(BRFSS13$LANDLINE==2," No",""))

#LASTPAP2
BRFSS13$LASTPAP2<-ifelse(BRFSS13$LASTPAP2==1,"Within 1 year",
                        ifelse(BRFSS13$LASTPAP2==2,"1 -2 years",
                               ifelse(BRFSS13$LASTPAP2==3,"2-3 years",
                                      ifelse(BRFSS13$LASTPAP2==4,"3-5 years",
                                             ifelse(BRFSS13$LASTPAP2==5,"before 5 years","")))))


#LASTSIG3
BRFSS13$LASTSIG3<-ifelse(BRFSS13$LASTSIG3==1,"Within 1 year",
                        ifelse(BRFSS13$LASTSIG3==2,"1-2 years",
                               ifelse(BRFSS13$LASTSIG3==3,"2-3 years",
                                      ifelse(BRFSS13$LASTSIG3==4,"3-5 years",
                                             ifelse(BRFSS13$LASTSIG3==5,"5-10 years",
                                                    ifelse(BRFSS13$LASTSIG3==6,"before 10 years",""))))))

#LASTSMK2
BRFSS13$LASTSMK2<-ifelse(BRFSS13$LASTSMK2==1,"Everyday",
                        ifelse(BRFSS13$LASTSMK2==2,"Somedays",
                               ifelse(BRFSS13$LASTSMK2==3,"Not at all","")))


#LENGEXAM
BRFSS13$LENGEXAM<-ifelse(BRFSS13$LENGEXAM==1,"Within 1 year",
                        ifelse(BRFSS13$LENGEXAM==2,"1 -2 years",
                               ifelse(BRFSS13$LENGEXAM==3,"2-3 years",
                                      ifelse(BRFSS13$LENGEXAM==4,"3-5 years",
                                             ifelse(BRFSS13$LENGEXAM==5,"before 5 years","")))))


#LSATISFY
BRFSS13$LSATISFY<-ifelse(BRFSS13$LSATISFY==1,"Very satisfied",
                        ifelse(BRFSS13$LSATISFY==2,"satisfied",
                               ifelse(BRFSS13$LSATISFY==3,"Dissatisfied",
                                      ifelse(BRFSS13$LSATISFY==4,"Very dissatisfied",""))))


#LSTBLDS3
BRFSS13$LSTBLDS3<-ifelse(BRFSS13$LSTBLDS3==1,"Within 1 year",
                        ifelse(BRFSS13$LSTBLDS3==2,"1 -2 years",
                               ifelse(BRFSS13$LSTBLDS3==3,"2-3 years",
                                      ifelse(BRFSS13$LSTBLDS3==4,"3-5 years",
                                             ifelse(BRFSS13$LSTBLDS3==5,"before 5 years","")))))


#MARITAL
BRFSS13$MARITAL<-ifelse(BRFSS13$MARITAL==1,"Married",
                       ifelse(BRFSS13$MARITAL==2,"Divorced",
                              ifelse(BRFSS13$MARITAL==3,"Widowed",
                                     ifelse(BRFSS13$MARITAL==4,"Separated",
                                            ifelse(BRFSS13$MARITAL==5,"Never married",
                                                   ifelse(BRFSS13$MARITAL==6,"A member of an unmarried couple",""))))))



#MAXDRNKS
BRFSS13$MAXDRNKS<-ifelse(BRFSS13$MAXDRNKS<=76,BRFSS13$MAXDRNKS,"")


#MEDCOST
BRFSS13$MEDCOST<-ifelse(BRFSS13$MEDCOST==1,"Yes",
                       ifelse(BRFSS13$MEDCOST==2,"No",""))

#MENTHLTH
BRFSS13$MENTHLTH<-ifelse(BRFSS13$MENTHLTH<=30,BRFSS13$MENTHLTH,
                        ifelse(BRFSS13$MENTHLTH==88,0,""))


#MSCODE
BRFSS13$MSCODE<-ifelse(BRFSS13$MSCODE==1,"In city center",
                      ifelse(BRFSS13$MSCODE==2,"outside city center but inside county",
                             ifelse(BRFSS13$MSCODE==3,"Inside the suburban county",
                                    ifelse(BRFSS13$MSCODE==5,"Not in an MSA",""))))


#NUMADULT
BRFSS13$NUMADULT<-ifelse(BRFSS13$NUMADULT<=99,BRFSS13$NUMADULT,"")


#NUMHHOL2
BRFSS13$NUMHHOL2<-ifelse(BRFSS13$NUMADULT==1,"Yes",
                        ifelse(BRFSS13$NUMADULT==2,"No",""))

#NUMMEN
BRFSS13$NUMMEN<-ifelse(BRFSS13$NUMMEN<=99,BRFSS13$NUMMEN,"")


#NUMPHON2
BRFSS13$NUMPHON2<-NULL


#NUMWOMEN
BRFSS13$NUMWOMEN<-ifelse(BRFSS13$NUMWOMEN<=99,BRFSS13$NUMWOMEN,"")


#PAINACT2
BRFSS13$PAINACT2<-NULL


#PDIABTST
BRFSS13$PDIABTST<-ifelse(BRFSS13$PDIABTST==1,"Yes",
                        ifelse(BRFSS13$PDIABTST==2,"No",""))


#PERSDOC2
BRFSS13$PERSDOC2<-ifelse(BRFSS13$PERSDOC2==1,"YEes, only 1",
                        ifelse(BRFSS13$PERSDOC2==2,"More than 1",
                               ifelse(BRFSS13$PERSDOC2==3,"no","")))

#PHYSHLTH
BRFSS13$PHYSHLTH<-ifelse(BRFSS13$PHYSHLTH<=30,BRFSS13$PHYSHLTH,
                        ifelse(BRFSS13$PHYSHLTH==88,0,""))

#PNEUVAC3
BRFSS13$PNEUVAC3<-ifelse(BRFSS13$PNEUVAC3==1,"Yes",
                        ifelse(BRFSS13$PNEUVAC3==2,"No",""))


#POORHLTH
BRFSS13$POORHLTH<-ifelse(BRFSS13$POORHLTH<=30,BRFSS13$POORHLTH,
                        ifelse(BRFSS13$POORHLTH==88,0,""))


#PREDIAB1
BRFSS13$PREDIAB1<-ifelse(BRFSS13$PREDIAB1==1,"Yes",
                        ifelse(BRFSS13$PREDIAB1==2,"Yes,during pregnency",
                               ifelse(BRFSS13$PREDIAB1==3,"No","")))


#PREGNANT
BRFSS13$PREGNANT<-ifelse(BRFSS13$PREGNANT==1,"Yes",
                        ifelse(BRFSS13$PREGNANT==2,"No",""))


#PROFEXAM
BRFSS13$PROFEXAM<-ifelse(BRFSS13$PROFEXAM==1,"Yes",
                        ifelse(BRFSS13$PROFEXAM==2,"No",""))

#PSATEST1
BRFSS13$PSATEST1<-ifelse(BRFSS13$PSATEST1==1,"Yes",
                        ifelse(BRFSS13$PSATEST1==2,"No",""))


#PSATIME
BRFSS13$PSATIME<-ifelse(BRFSS13$PSATIME==1,"Within 1 year",
                       ifelse(BRFSS13$PSATIME==2,"1 -2 years",
                              ifelse(BRFSS13$PSATIME==3,"2-3 years",
                                     ifelse(BRFSS13$PSATIME==4,"3-5 years",
                                            ifelse(BRFSS13$PSATIME==5,"before 5 years","")))))


#PVTRESD2
BRFSS13$PVTRESD2<-ifelse(BRFSS13$PVTRESD2==1,"Yes",
                        ifelse(BRFSS13$PVTRESD2==2,"No",""))


#QLACTLM2
BRFSS13$QLACTLM2<-ifelse(BRFSS13$QLACTLM2==1,"Yes",
                        ifelse(BRFSS13$QLACTLM2==2,"No",""))


#QLHLTH2
BRFSS13$QLHLTH2<-NULL


#QLMENTL2
BRFSS13$QLMENTL2<-NULL


#QLSTRES2
BRFSS13$QLSTRES2<-NULL


#QSTLANG
BRFSS13$QSTLANG<-ifelse(BRFSS13$QSTLANG==1,"English",
                       ifelse(BRFSS13$QSTLANG==2,"Spanish",
                              ifelse(BRFSS13$QSTLANG>=3,"other","")))


#RCSGENDR
BRFSS13$RCSGENDR<-ifelse(BRFSS13$RCSGENDR==1,"Boy",
                        ifelse(BRFSS13$RCSGENDR==2,"Girl",""))


#RCSRLTN2
BRFSS13$RCSRLTN2<-ifelse(BRFSS13$RCSRLTN2==1,"Parent",
                        ifelse(BRFSS13$RCSRLTN2==2,"Grandparent",
                               ifelse(BRFSS13$RCSRLTN2==3,"Foster Parent or Guardian",
                                      ifelse(BRFSS13$RCSRLTN2==4,"Sibling",
                                             ifelse(BRFSS13$RCSRLTN2==5,"Other relative",
                                                    ifelse(BRFSS13$RCSRLTN2==6,"Not related in any way",
                                                           ifelse(BRFSS13$RCSRLTN2==7,"Don't know not sure","")))))))



#RENTHOM1
BRFSS13$RENTHOM1<-ifelse(BRFSS13$RENTHOM1==1,"Parent",
                        ifelse(BRFSS13$RENTHOM1==2,"Own",
                               ifelse(BRFSS13$RENTHOM1==3,"Rent",
                                      ifelse(BRFSS13$RENTHOM1==4,"Other Arrangement",""))))


#SCNTLPAD
BRFSS13$SCNTLPAD<-ifelse(BRFSS13$SCNTLPAD==1,"By Salary",
                        ifelse(BRFSS13$SCNTLPAD==2,"Paid by hour",
                               ifelse(BRFSS13$SCNTLPAD==3,"Paid by job/task(commission)",
                                      ifelse(BRFSS13$SCNTLPAD==4,"Paid some other way",""))))



#SCNTLWK1
BRFSS13$SCNTLWK1<- ifelse(BRFSS13$SCNTLWK1==98,0,
                         ifelse(BRFSS13$SCNTLWK1<=96,BRFSS13$SCNTLWK1,""))


#SCNTPAID
BRFSS13$SCNTPAID<-NULL


#SCNTWRK1
BRFSS13$SCNTWRK1<-NULL


#SEATBELT
BRFSS13$SEATBELT<-ifelse(BRFSS13$SEATBELT==1,"always",
                        ifelse(BRFSS13$SEATBELT==2,"nearly always",
                               ifelse(BRFSS13$SEATBELT==3,"sometimes",
                                      ifelse(BRFSS13$SEATBELT==4,"seldom",
                                             ifelse(BRFSS13$SEATBELT==5,"never",
                                                    ifelse(BRFSS13$SEATBELT==8,"Never drive or ride a car",""))))))


#SEQNO
BRFSS13$SEQNO<-NULL


#SEX
BRFSS13$SEX<-ifelse(BRFSS13$SEX==1,"Male",
                   ifelse(BRFSS13$SEX==2,"Female",""))



#SMOKDAY2
BRFSS13$SMOKDAY2<-ifelse(BRFSS13$SMOKDAY2==1,"Everyday",
                        ifelse(BRFSS13$SMOKDAY2==2,"Somedays",
                               ifelse(BRFSS13$SMOKDAY2==3,"Not at all","")))



#SMOKE100
BRFSS13$SMOKE100<-ifelse(BRFSS13$SMOKE100==1,"Yes",
                        ifelse(BRFSS13$SMOKE100==2,"No",""))



#STOPSMK2
BRFSS13$STOPSMK2<-ifelse(BRFSS13$STOPSMK2==1,"Yes",
                        ifelse(BRFSS13$STOPSMK2==2,"No",""))


#USEEQUIP
BRFSS13$USEEQUIP<-ifelse(BRFSS13$USEEQUIP==1,"Yes",
                        ifelse(BRFSS13$USEEQUIP==2,"No",""))


#USENOW3
BRFSS13$USENOW3<-ifelse(BRFSS13$USENOW3==1,"Everyday",
                       ifelse(BRFSS13$USENOW3==2,"Somedays",
                              ifelse(BRFSS13$USENOW3==3,"Not at all","")))



#VETERAN3
BRFSS13$VETERAN3<-ifelse(BRFSS13$VETERAN3==1,"Yes",
                        ifelse(BRFSS13$VETERAN3==2,"No",""))


#WEIGHT2
BRFSS13$WEIGHT2<-ifelse(BRFSS13$WEIGHT2<=1000,round((as.numeric(BRFSS13$WEIGHT2)*0.453592),digits=0),
                       ifelse(BRFSS13$WEIGHT2 %in% c(9000:9998),BRFSS13$WEIGHT2-9000,""))

#WTKG3
BRFSS13$WTKG3<-NULL


###########################################################################################################


BRFSS13$HAVARTH3

BRFSS13_ARTHRITIS<- BRFSS13[BRFSS13$HAVARTH3!="",]

nrow(BRFSS13_ARTHRITIS)

nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$AGEG5YR=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$CHLDCNT=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$EDUCAG=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$HCVU651=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$INCOMG=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$PNEUMO2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$RFBING5=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$RFBMI5=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$RFHLTH=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$RFSMOK3=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$SMOKER3=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$STATE=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$TOTINDA=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$ADDEPEV2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$ALCDAY5=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$AVEDRNK2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$CADULT=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$CELLFON2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$CHCKIDNY=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$CPDEMO1=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$DISPCODE=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$EDUCA=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$EMTSUPRT=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$EXERANY2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$GENHLTH=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$HAVARTH3=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$HEIGHT3=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$HLTHPLN1=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$INCOME2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$LSATISFY=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$MARITAL=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$MAXDRNKS=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$MEDCOST=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$MSCODE=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$NUMADULT=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$NUMHHOL2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$NUMPHON2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$PERSDOC2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$PNEUVAC3=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$POORHLTH=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$PREGNANT=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$PROFEXAM=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$PVTRESD2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$QLACTLM2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$QSTLANG=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$RCSGENDR=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$RCSRLTN2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$RENTHOM1=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$SCNTLWK1=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$SEX=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$SMOKDAY2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$STOPSMK2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$USEEQUIP=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$USENOW3=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$VETERAN3=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$WEIGHT2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$ASTHMS1=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$BMI5=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$ASACTLIM=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$ASATTACK=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$ASDRVIST=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$ASINHALR=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$BLDSUGAR=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$CHCOCNCR=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$CHCSCNCR=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$CHECKUP1=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$CHILDREN=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$CHKHEMO3=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$CVDCRHD4=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$CVDINFR4=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$CVDSTRK3=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$DIABETE3=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$FLSHTMY2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$HIVTST6=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$INSULIN=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$LASTSMK2=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$PDIABTST=="",])
nrow(BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$PREDIAB1=="",])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$AGEG5YR),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$CHLDCNT),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$EDUCAG),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$HCVU651),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$INCOMG),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$PNEUMO2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$RFBING5),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$RFBMI5),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$RFHLTH),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$RFSMOK3),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$SMOKER3),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$STATE),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$TOTINDA),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$ADDEPEV2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$ALCDAY5),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$AVEDRNK2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$CADULT),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$CELLFON2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$CHCKIDNY),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$CPDEMO1),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$DISPCODE),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$EDUCA),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$EMTSUPRT),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$EXERANY2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$GENHLTH),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$HAVARTH3),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$HEIGHT3),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$HLTHPLN1),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$INCOME2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$LSATISFY),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$MARITAL),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$MAXDRNKS),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$MEDCOST),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$MSCODE),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$NUMADULT),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$NUMHHOL2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$NUMPHON2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$PERSDOC2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$PNEUVAC3),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$POORHLTH),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$PREGNANT),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$PROFEXAM),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$PVTRESD2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$QLACTLM2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$QSTLANG),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$RCSGENDR),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$RCSRLTN2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$RENTHOM1),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$SCNTLWK1),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$SEX),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$SMOKDAY2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$STOPSMK2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$USEEQUIP),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$USENOW3),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$VETERAN3),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$WEIGHT2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$ASTHMS1),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$BMI5),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$ASACTLIM),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$ASATTACK),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$ASDRVIST),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$ASINHALR),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$BLDSUGAR),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$CHCOCNCR),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$CHCSCNCR),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$CHECKUP1),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$CHILDREN),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$CHKHEMO3),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$CVDCRHD4),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$CVDINFR4),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$CVDSTRK3),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$DIABETE3),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$FLSHTMY2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$HIVTST6),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$INSULIN),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$LASTSMK2),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$PDIABTST),])
nrow(BRFSS13_ARTHRITIS[is.na(BRFSS13_ARTHRITIS$PREDIAB1),])


#########################################################################################


x13<-BRFSS13_ARTHRITIS[BRFSS13_ARTHRITIS$AGEG5YR!="",]
x13<-x13[x13$CHLDCNT!="",]
x13<-x13[x13$EDUCAG!="",]
x13<-x13[x13$RFBING5!="",]
x13<-x13[x13$RFHLTH!="",]
x13<-x13[x13$SMOKER3!="",]
x13<-x13[x13$TOTINDA!="",]
x13<-x13[x13$ADDEPEV2!="",]
x13<-x13[x13$ALCDAY5!="",]
x13<-x13[x13$CHCKIDNY!="",]
x13<-x13[x13$EDUCA!="",]
x13<-x13[x13$EXERANY2!="",]
x13<-x13[x13$GENHLTH!="",]
x13<-x13[x13$HAVARTH3!="",]
x13<-x13[x13$HEIGHT3!="",]
x13<-x13[x13$HLTHPLN1!="",]
x13<-x13[x13$MARITAL!="",]
x13<-x13[x13$MEDCOST!="",]
x13<-x13[x13$PERSDOC2!="",]
x13<-x13[x13$PNEUVAC3!="",]
x13<-x13[x13$PHYSHLTH!="",]
x13<-x13[x13$QLACTLM2!="",]
x13<-x13[x13$QSTLANG!="",]
x13<-x13[x13$SEX!="",]
x13<-x13[x13$VETERAN3!="",]
x13<-x13[x13$WEIGHT2!="",]
x13<-x13[x13$ASTHMS1!="",]
x13<-x13[x13$BMI5!="",]
x13<-x13[x13$CHCOCNCR!="",]
x13<-x13[x13$CVDSTRK3!="",]
x13<-x13[x13$DIABETE3!="",]
x13<-x13[x13$HCVU651!="",]
x13<-x13[x13$INCOME2!="",]
x13<-x13[x13$STATE!="",]
x13<-x13[x13$USEEQUIP!="",]
x13<-x13[x13$USENOW3!="",]
x13<-x13[x13$CHECKUP1!="",]
x13<-x13[x13$CHILDREN!="",]
x15<-x15[x15$CASTHDX2!="",]



a13<-x13[, c(	"AGEG5YR",	"CHLDCNT",	"EDUCAG",	"RFBING5",	"RFHLTH",	"SMOKER3",	"TOTINDA",	"ADDEPEV2",	"ALCDAY5",	"CHCKIDNY",	"EDUCA",	"EXERANY2",	"GENHLTH",	"HAVARTH3",	"HEIGHT3",	"HLTHPLN1",	"MARITAL",	"MEDCOST",	"PERSDOC2",	"PNEUVAC3",	"PHYSHLTH",	"QLACTLM2",	"QSTLANG",	"SEX",	"VETERAN3",	"WEIGHT2",	"ASTHMS1",	"BMI5",	"CHCOCNCR",	"CVDSTRK3",	"DIABETE3",	"HCVU651",	"INCOME2",	"STATE",	"USEEQUIP",	"USENOW3",	"CHECKUP1",	"CHILDREN","CASTHDX2")]
dim(a13)
x13<-a13
BRFSS13_ARTHRITIS<-x13


########################################################################################################

BRFSS14<-BRFSS_2014

#AGE65YR
BRFSS14$AGE65YR<- ifelse(BRFSS14$AGE65YR==1,"Yes",
                         ifelse(BRFSS14$AGE65YR==2,"No",""))

#ASTHMS1
BRFSS14$ASTHMS1<- ifelse(BRFSS14$ASTHMS1==1,"current",
                        ifelse(BRFSS14$ASTHMS1==2,"former",
                               ifelse(BRFSS14$ASTHMS1==3,"never","")))

#BMI5
BRFSS14$BMI5<- BRFSS14$BMI5/100

#CASTHM1
BRFSS14$CASTHM1<- ifelse(BRFSS14$CASTHM1==1,"No",
                        ifelse(BRFSS14$CASTHM1==2,"Yes",""))

#CHLDCNT
BRFSS14$CHLDCNT<- ifelse(BRFSS14$CHLDCNT==1,"no child",
                        ifelse(BRFSS14$CHLDCNT==2,"1",
                               ifelse(BRFSS14$CHLDCNT==3,"2",
                                      ifelse(BRFSS14$CHLDCNT==4,"3",
                                             ifelse(BRFSS14$CHLDCNT==5,"4",
                                                    ifelse(BRFSS14$CHLDCNT==6,"5",""))))))

#CLLCPWT
BRFSS14$CLLCPWT<- NULL

#DRDXAR1
BRFSS14$DRDXAR1<- ifelse(BRFSS14$DRDXAR1==1,"Yes",
                        ifelse(BRFSS14$DRDXAR1==2,"No",""))

#EDUCAG
BRFSS14$EDUCAG<- ifelse(BRFSS14$EDUCAG==1,"no High School",
                       ifelse(BRFSS14$EDUCAG==2,"High School Grad",
                              ifelse(BRFSS14$EDUCAG==3,"Attended College","College Grad")))

#HCVU651
BRFSS14$HCVU651<- ifelse(BRFSS14$HCVU651==1,"Yes",
                        ifelse(BRFSS14$HCVU651==2,"No",""))

#INCOMG
BRFSS14$INCOMG<- ifelse(BRFSS14$INCOMG==1,"less than 15000",
                       ifelse(BRFSS14$INCOMG==2,"15-25",
                              ifelse(BRFSS14$INCOMG==3,"25-35",
                                     ifelse(BRFSS14$INCOMG==4,"35-50",
                                            ifelse(BRFSS14$INCOMG==5,"greater than 50000","")))))

#LLCPWT
BRFSS14$LLCPWT<- NULL

#LTASTH1
BRFSS14$LTASTH1<- ifelse(BRFSS14$LTASTH1==1,"No",
                       ifelse(BRFSS14$LTASTH1==2,"Yes",""))

#PNEUMO2
BRFSS14$PNEUMO2<- ifelse(BRFSS14$PNEUMO2==1,"Yes",
                        ifelse(BRFSS14$PNEUMO2==2,"No",""))

#PSU
BRFSS14$PSU<- NULL

#PNEUMO2
BRFSS14$PNEUMO2<- NULL

#RFBING5
BRFSS14$RFBING5<- ifelse(BRFSS14$RFBING5==1,"No",
                        ifelse(BRFSS14$RFBING5==2,"Yes",""))

#RFBMI5
BRFSS14$RFBMI5<- ifelse(BRFSS14$RFBMI5==1,"BMI =<25",
                       ifelse(BRFSS14$RFBMI5==2,"BMI >25",""))

#RFHLTH
BRFSS14$RFHLTH<- ifelse(BRFSS14$RFHLTH==1,"good or better health",
                       ifelse(BRFSS14$RFHLTH==2,"fair or poor helath",""))

#RFSEAT2
BRFSS14$RFSEAT2<- ifelse(BRFSS14$RFSEAT2==1,"always wear seat belt",
                        ifelse(BRFSS14$RFSEAT2==2,"seldom or never",""))

#RFSEAT3
BRFSS14$RFSEAT3<- NULL

#RFSMOK3
BRFSS14$RFSMOK3<- ifelse(BRFSS14$RFSMOK3==1,"No",
                        ifelse(BRFSS14$RFSMOK3==2,"Yes",""))

#SMOKER3
BRFSS14$SMOKER3<- ifelse(BRFSS14$SMOKER3==1,"current/smokes everyday",
                                        ifelse(BRFSS14$SMOKER3==2,"current/smokes somedays",
                                               ifelse(BRFSS14$SMOKER3==3,"former",
                                                      ifelse(BRFSS14$SMOKER3==4,"never",""))))

#STATE
BRFSS14$STATE<- ifelse(BRFSS14$STATE==1,"Alabama",
                      ifelse(BRFSS14$STATE==2,"Alaska",
                             ifelse(BRFSS14$STATE==4,"Arizona",
                                    ifelse(BRFSS14$STATE==5,"Arkansas",
                                           ifelse(BRFSS14$STATE==6,"California",
                                                  ifelse(BRFSS14$STATE==8,"Colorado",
                                                         ifelse(BRFSS14$STATE==9,"Connecticut",
                                                                ifelse(BRFSS14$STATE==10,"Delaware",
                                                                       ifelse(BRFSS14$STATE==11,"District of Columbia",
                                                                              ifelse(BRFSS14$STATE==12,"Florida",
                                                                                     ifelse(BRFSS14$STATE==13,"Georgia",
                                                                                            ifelse(BRFSS14$STATE==15,"Hawaii",
                                                                                                   ifelse(BRFSS14$STATE==16,"Idaho",
                                                                                                          ifelse(BRFSS14$STATE==17,"Illinois",
                                                                                                                 ifelse(BRFSS14$STATE==18,"Indiana",
                                                                                                                        ifelse(BRFSS14$STATE==19,"Iowa",
                                                                                                                               ifelse(BRFSS14$STATE==20,"Kansas",
                                                                                                                                      ifelse(BRFSS14$STATE==21,"Kentucky",
                                                                                                                                             ifelse(BRFSS14$STATE==22,"Louisiana",
                                                                                                                                                    ifelse(BRFSS14$STATE==23,"Maine",
                                                                                                                                                           ifelse(BRFSS14$STATE==24,"Maryland",
                                                                                                                                                                  ifelse(BRFSS14$STATE==25,"Massachusetts",
                                                                                                                                                                         ifelse(BRFSS14$STATE==26,"Michigan",
                                                                                                                                                                                ifelse(BRFSS14$STATE==27,"Minnesota",
                                                                                                                                                                                       ifelse(BRFSS14$STATE==28,"Mississippi",
                                                                                                                                                                                              ifelse(BRFSS14$STATE==29,"Missouri",
                                                                                                                                                                                                     ifelse(BRFSS14$STATE==30,"Montana",
                                                                                                                                                                                                            ifelse(BRFSS14$STATE==31,"Nebraska",
                                                                                                                                                                                                                   ifelse(BRFSS14$STATE==32,"Nevada",
                                                                                                                                                                                                                          ifelse(BRFSS14$STATE==33,"New Hampshire",
                                                                                                                                                                                                                                 ifelse(BRFSS14$STATE==34,"New Jersey",
                                                                                                                                                                                                                                        ifelse(BRFSS14$STATE==35,"New Mexico",
                                                                                                                                                                                                                                               ifelse(BRFSS14$STATE==36,"New York",
                                                                                                                                                                                                                                                      ifelse(BRFSS14$STATE==37,"North Carolina",
                                                                                                                                                                                                                                                             ifelse(BRFSS14$STATE==38,"North Dakota",
                                                                                                                                                                                                                                                                    ifelse(BRFSS14$STATE==39,"Ohio",
                                                                                                                                                                                                                                                                           ifelse(BRFSS14$STATE==40,"Oklahoma",
                                                                                                                                                                                                                                                                                  ifelse(BRFSS14$STATE==41,"Oregon ",""
                                                                                                                                                                                                                                                                                  ))))))))))))))))))))))))))))))))))))))

#STRWT
BRFSS14$STRWT<- NULL

#STSTR
BRFSS14$STSTR<- NULL

#TOTINDA
BRFSS14$TOTINDA<- ifelse(BRFSS14$TOTINDA==1,"Yes",
                        ifelse(BRFSS14$TOTINDA==2,"No",""))

#WT2RAKE
BRFSS14$WT2RAKE<- NULL

#ADDEPEV2
BRFSS14$ADDEPEV2<- ifelse(BRFSS14$ADDEPEV2==1,"Yes",
                         ifelse(BRFSS14$ADDEPEV2==2,"No",""))

#ALCDAY5 - Convert days per week to approx days per month considering 30 days in a month.
BRFSS14$ALCDAY5<- ifelse(BRFSS14$ALCDAY5 %in% c(100:199), round((BRFSS14$ALCDAY5-100)*4.29,digits=0),
                        ifelse(BRFSS14$ALCDAY5 %in% c(200:299),BRFSS14$ALCDAY5-200,
                               ifelse(BRFSS14$ALCDAY5 ==888,0,"")))

#ASACTLIM
BRFSS14$ASACTLIM<- ifelse(BRFSS14$ASACTLIM<=365,BRFSS14$ASACTLIM,
                         ifelse(BRFSS14$ASACTLIM==888,0,""))

#ASATTACK
BRFSS14$ASATTACK<- ifelse(BRFSS14$ASATTACK==1,"Yes",
                         ifelse(BRFSS14$ASATTACK==2,"No",""))

#ASDRVIST
BRFSS14$ASDRVIST<- ifelse(BRFSS14$ASDRVIST<=87,BRFSS14$ASDRVIST,
                         ifelse(BRFSS14$ASDRVIST==88,0,""))

#ASERVIST
BRFSS14$ASERVIST<- ifelse(BRFSS14$ASERVIST<=87,BRFSS14$ASERVIST,
                         ifelse(BRFSS14$ASERVIST==88,0,""))

#ASINHALR
BRFSS14$ASINHALR<- ifelse(BRFSS14$ASINHALR==1,"1-4",
                         ifelse(BRFSS14$ASINHALR==2,"5-14",
                                ifelse(BRFSS14$ASINHALR==3,"15-29",
                                       ifelse(BRFSS14$ASINHALR==4,"30-59",
                                              ifelse(BRFSS14$ASINHALR==5,"60-99",
                                                     ifelse(BRFSS14$ASINHALR==6,">=100",
                                                            ifelse(BRFSS14$ASINHALR==8,"0","")))))))

#ASTHMA3
BRFSS14$ASTHMA3<- ifelse(BRFSS14$ASTHMA3==1,"Yes",
                        ifelse(BRFSS14$ASTHMA3==2,"No",""))

#ASTHMAGE
BRFSS14$ASTHMAGE<- ifelse(BRFSS14$ASTHMAGE==97,"<=10",
                         ifelse(BRFSS14$ASTHMAGE<97,BRFSS14$ASTHMAGE,""))

#ASTHMED3
BRFSS14$ASTHMED3<- ifelse(BRFSS14$ASTHMED3==1,"1-14",
                         ifelse(BRFSS14$ASTHMED3==2,"15-24",
                                ifelse(BRFSS14$ASTHMED3==3,"25-30",
                                       ifelse(BRFSS14$ASTHMED3==8,"0",""))))

#CADULT
BRFSS14$CADULT<- ifelse(BRFSS14$CADULT==1,"Yes/Male",
                       ifelse(BRFSS14$CADULT==2,"Yes/Female",""))

#CASTHDX2
BRFSS14$CASTHDX2<- ifelse(BRFSS14$CASTHDX2==1,"Yes",
                         ifelse(BRFSS14$CASTHDX2==2,"No",""))

#CASTHNO2
BRFSS14$CASTHNO2<- ifelse(BRFSS14$CASTHNO2==1,"Yes",
                         ifelse(BRFSS14$CASTHNO2==2,"No",""))

#CELLFON2
BRFSS14$CELLFON2<- ifelse(BRFSS14$CELLFON2==1,"Yes","")

#CHCKIDNY
BRFSS14$CHCKIDNY<- ifelse(BRFSS14$CHCKIDNY==1,"Yes",
                         ifelse(BRFSS14$CHCKIDNY==2,"No",""))

#CHCOCNCR
BRFSS14$CHCOCNCR<- ifelse(BRFSS14$CHCOCNCR==1,"Yes",
                         ifelse(BRFSS14$CHCOCNCR==2,"No",""))

#CHCSCNCR
BRFSS14$CHCSCNCR<- ifelse(BRFSS14$CHCSCNCR==1,"Yes",
                         ifelse(BRFSS14$CHCSCNCR==2,"No",""))

#CHECKUP1
BRFSS14$CHECKUP1<- ifelse(BRFSS14$CHECKUP1==1,"within past year",
                         ifelse(BRFSS14$CHECKUP1==2,"within past 2 years",
                                ifelse(BRFSS14$CHECKUP1==3,"within past 5 years",
                                       ifelse(BRFSS14$CHECKUP1==4,"5 or more years before",
                                              ifelse(BRFSS14$CHECKUP1==8,"never","")))))

#CHILDREN
BRFSS14$CHILDREN<- ifelse(BRFSS14$CHILDREN<=87,BRFSS14$CHILDREN,
                         ifelse(BRFSS14$CHILDREN==88,"0",""))

#CHKHEMO3
BRFSS14$CHKHEMO3<- ifelse(BRFSS14$CHKHEMO3<=76,BRFSS14$CHKHEMO3,
       ifelse(BRFSS14$CHKHEMO3==88,"0",
              ifelse(BRFSS14$CHKHEMO3==98,"never heard of the test","")))

#CPDEMO1
BRFSS14$CPDEMO1<- ifelse(BRFSS14$CPDEMO1==1,"Yes",
                        ifelse(BRFSS14$CPDEMO1==2,"No",""))

#CSTATE
BRFSS14$CSTATE<- ifelse(BRFSS14$CSTATE==1,"Yes",
                       ifelse(BRFSS14$CSTATE==2,"No",""))

#CTELENUM
BRFSS14$CTELENUM<- NULL

#CTELNUM1
BRFSS14$CTELNUM1<- NULL

#CVDCRHD4
BRFSS14$CVDCRHD4<- ifelse(BRFSS14$CVDCRHD4==1,"Yes",
                         ifelse(BRFSS14$CVDCRHD4==2,"No",""))

#CVDINFR4
BRFSS14$CVDINFR4<- ifelse(BRFSS14$CVDINFR4==1,"Yes",
                         ifelse(BRFSS14$CVDINFR4==2,"No",""))

#CVDSTRK3
BRFSS14$CVDSTRK3<- ifelse(BRFSS14$CVDSTRK3==1,"Yes",
                         ifelse(BRFSS14$CVDSTRK3==2,"No",""))

#DIABAGE2
BRFSS14$DIABAGE2<- ifelse(BRFSS14$DIABAGE2<=97,BRFSS14$DIABAGE2,"")

#DIABEDU
BRFSS14$DIABEDU<- ifelse(BRFSS14$DIABEDU==1,"Yes",
                        ifelse(BRFSS14$DIABEDU==2,"No",""))

#DIABETE3
BRFSS14$DIABETE3<- ifelse(BRFSS14$DIABETE3==1,"Yes",
                         ifelse(BRFSS14$DIABETE3==2,"Yes/female during pregnancy",
                                ifelse(BRFSS14$DIABETE3==3,"No",
                                       ifelse(BRFSS14$DIABETE3==2,"Borderline diabetes/No current diabetes",""))))

#DIABEYE
BRFSS14$DIABEYE<- ifelse(BRFSS14$DIABEYE==1,"Yes",
                        ifelse(BRFSS14$DIABEYE==2,"No",""))

#DISPCODE
BRFSS14$DISPCODE<- NULL

#DOCTDIAB
BRFSS14$DOCTDIAB<- ifelse(BRFSS14$DOCTDIAB==88,0,
                         ifelse(BRFSS14$DOCTDIAB<=76,BRFSS14$DOCTDIAB,""))

#DRNK3GE5
BRFSS14$DRNK3GE5<- ifelse(BRFSS14$DRNK3GE5==88,0,
                         ifelse(BRFSS14$DRNK3GE5<=76,BRFSS14$DRNK3GE5,""))

#DRNKANY5
BRFSS14$DRNKANY5<- ifelse(BRFSS14$DRNKANY5==1,"Yes",
                         ifelse(BRFSS14$DRNKANY5==2,"No",""))

#DROCDY3_
BRFSS14$DROCDY3_<- NULL

#EDUCA
BRFSS14$EDUCA<-ifelse(BRFSS14$EDUCA==1,"never attended school",
                     ifelse(BRFSS14$EDUCA==2,"1-8 elementary",
                            ifelse(BRFSS14$EDUCA==3,"9-11 high school",
                                   ifelse(BRFSS14$EDUCA==4,"grade 12 or higher",
                                          ifelse(BRFSS14$EDUCA==5,"college year 1-3",
                                                 ifelse(BRFSS14$EDUCA==6,"college year 4 or graduate",""))))))

#EMTSUPRT
BRFSS14$EMTSUPRT<- ifelse(BRFSS14$EMTSUPRT==1,"always",
                         ifelse(BRFSS14$EMTSUPRT==2,"usually",
                                ifelse(BRFSS14$EMTSUPRT==3,"sometimes",
                                       ifelse(BRFSS14$EMTSUPRT==4,"rarely",
                                              ifelse(BRFSS14$EMTSUPRT==5,"never","")))))

#EXERANY2
BRFSS14$EXERANY2<- ifelse(BRFSS14$EXERANY2==1,"Yes",
                         ifelse(BRFSS14$EXERANY2==2,"No",""))

#EYEEXAM
BRFSS14$EYEEXAM<- ifelse(BRFSS14$EYEEXAM==1,"within past one month",
                        ifelse(BRFSS14$EYEEXAM==2,"within past year",
                               ifelse(BRFSS14$EYEEXAM==3,"within past 2 years",
                                      ifelse(BRFSS14$EYEEXAM==4,"2 or more years",
                                             ifelse(BRFSS14$EYEEXAM==8,"never","")))))

#FEETCHK
BRFSS14$FEETCHK<- ifelse(BRFSS14$FEETCHK==88,0,
                        ifelse(BRFSS14$FEETCHK<=76,BRFSS14$DOCTDIAB,""))

#FEETCHK2
BRFSS14$FEETCHK2<- ifelse(BRFSS14$FEETCHK2 %in% c(100:199), round((BRFSS14$FEETCHK2-100)*365,digits=0),
                         ifelse(BRFSS14$FEETCHK2 %in% c(200:299),round((BRFSS14$FEETCHK2-200)*(365/7),digits=0),
                                ifelse(BRFSS14$FEETCHK2 %in% c(300:399),round((BRFSS14$FEETCHK2-300)*12,digits=0),
                                       ifelse(BRFSS14$FEETCHK2 %in% c(400:499),BRFSS14$FEETCHK2,
                                              ifelse(BRFSS14$FEETCHK2 ==888,0,"")))))

#AGE_G
BRFSS14$AGE_G<- ifelse(BRFSS14$AGE_G ==1, "18-24",
                      ifelse(BRFSS14$AGE_G ==2,"25-34",
                             ifelse(BRFSS14$AGE_G ==3,"35-44",
                                    ifelse(BRFSS14$AGE_G ==4,"45-54",
                                           ifelse(BRFSS14$AGE_G ==5,"55-64",">=65")))))

#AGEG5YR
BRFSS14$AGEG5YR<- ifelse(BRFSS14$AGEG5YR ==1, "18-24",
                        ifelse(BRFSS14$AGEG5YR ==2,"25-29",
                               ifelse(BRFSS14$AGEG5YR ==3,"30-34",
                                      ifelse(BRFSS14$AGEG5YR ==4,"35-39",
                                             ifelse(BRFSS14$AGEG5YR ==5,"40-44",
                                                    ifelse(BRFSS14$AGEG5YR ==6,"44-49",
                                                           ifelse(BRFSS14$AGEG5YR ==7,"50-54",
                                                                  ifelse(BRFSS14$AGEG5YR ==8,"55-59",
                                                                         ifelse(BRFSS14$AGEG5YR ==9,"60-64",
                                                                                ifelse(BRFSS14$AGEG5YR ==10,"65-69",
                                                                                       ifelse(BRFSS14$AGEG5YR ==11,"70-74",
                                                                                              ifelse(BRFSS14$AGEG5YR ==12,"75-79",
                                                                                                     ifelse(BRFSS14$AGEG5YR ==13,">=80","")))))))))))))

#AIDTST3
BRFSS14$AIDTST3<- ifelse(BRFSS14$AIDTST3==1,"Yes",
                        ifelse(BRFSS14$AIDTST3==2,"No",""))

#BMI5CAT
BRFSS14$BMI5CAT<- ifelse(BRFSS14$BMI5CAT==1,"Underweight (BMI<18.50)",
                        ifelse(BRFSS14$BMI5CAT==2,"Normal Weight (18.50<=BMI< 25.00)",
                               ifelse(BRFSS14$BMI5CAT==3,"Overweight (25.00<=BMI<30.00)",
                                      ifelse(BRFSS14$BMI5CAT==4,"Obese (30.00<=BMI<99.99)",""))))

#ASNOSLEP
BRFSS14$ASNOSLEP<- ifelse(BRFSS14$ASNOSLEP ==1, "1-2",
                         ifelse(BRFSS14$ASNOSLEP ==2,"3-4",
                                ifelse(BRFSS14$ASNOSLEP ==3,"5",
                                       ifelse(BRFSS14$ASNOSLEP ==4,"6-10",
                                              ifelse(BRFSS14$ASNOSLEP ==5,">=10",
                                                     ifelse(BRFSS14$ASNOSLEP ==8,"0",""))))))

#ASRCHKUP
BRFSS14$ASRCHKUP<- ifelse(BRFSS14$ASRCHKUP==88,0,
                         ifelse(BRFSS14$ASRCHKUP<=86,BRFSS14$ASRCHKUP,""))

#ASTHNOW
BRFSS14$ASTHNOW<- ifelse(BRFSS14$ASTHNOW==1,"Yes",
                        ifelse(BRFSS14$ASTHNOW==2,"No",""))

#ASYMPTOM
BRFSS14$ASYMPTOM<- ifelse(BRFSS14$ASYMPTOM ==1, "less than 1 in a week",
                         ifelse(BRFSS14$ASYMPTOM ==2,"1-2/week",
                                ifelse(BRFSS14$ASYMPTOM ==3,"3-6/week",
                                       ifelse(BRFSS14$ASYMPTOM ==4,"everyday but not all the time",
                                              ifelse(BRFSS14$ASYMPTOM ==5,"everyday all the time",
                                                     ifelse(BRFSS14$ASYMPTOM ==8,"never",""))))))

#AVEDRNK2
BRFSS14$AVEDRNK2<- ifelse(BRFSS14$AVEDRNK2<=76,BRFSS14$AVEDRNK2,"")

#BLDSTOOL
BRFSS14$BLDSTOOL<- ifelse(BRFSS14$BLDSTOOL==1,"Yes",
                         ifelse(BRFSS14$BLDSTOOL==2,"No",""))

#BLDSUGAR
BRFSS14$BLDSUGAR<- ifelse(BRFSS14$BLDSUGAR %in% c(100:199), round((BRFSS14$BLDSUGAR-100)*365,digits=0),
                         ifelse(BRFSS14$BLDSUGAR %in% c(200:299),round((BRFSS14$BLDSUGAR-200)*(365/7),digits=0),
                                ifelse(BRFSS14$BLDSUGAR %in% c(300:399),round((BRFSS14$BLDSUGAR-300)*12,digits=0),
                                       ifelse(BRFSS14$BLDSUGAR %in% c(400:499),BRFSS14$BLDSUGAR,
                                              ifelse(BRFSS14$BLDSUGAR ==888,0,"")))))



#FLSHTMY2
BRFSS14$FLSHTMY2<-ifelse(BRFSS14$FLSHTMY2==777777,"never",
                        ifelse(BRFSS14$FLSHTMY2<999999,paste(substr(BRFSS14$FLSHTMY2, 1, ifelse(nchar(BRFSS14$FLSHTMY2)==6,2,1)),"-",substr(BRFSS14$FLSHTMY2, ifelse(nchar(BRFSS14$FLSHTMY2)==6,3,2), nchar(BRFSS14$FLSHTMY2))),""))

#GENHLTH
BRFSS14$GENHLTH<-ifelse(BRFSS14$GENHLTH==1,"Excellent",
                       
                       ifelse(BRFSS14$GENHLTH==2,"Very Good",
                              
                              ifelse(BRFSS14$GENHLTH==3,"Good",
                                     ifelse(BRFSS14$GENHLTH==4,"Fair",
                                            ifelse(BRFSS14$GENHLTH==5,"poor","")))))


#HADHYST2
BRFSS14$HADHYST2<-ifelse(BRFSS14$HADHYST2==1,"Yes",
                        ifelse(BRFSS14$HADHYST2==2,"No",""))

#HADMAM
BRFSS14$HADMAM<-ifelse(BRFSS14$HADMAM==1,"Yes",
                      ifelse(BRFSS14$HADMAM==2,"No",""))

#HADPAP2
BRFSS14$HADPAP2<-ifelse(BRFSS14$HADPAP2==1,"Yes",
                       ifelse(BRFSS14$HADPAP2==2,"No",""))


#HADSGCO1
BRFSS14$HADSGCO1<-ifelse(BRFSS14$HADSGCO1==1,"Sigmoidal",
                        ifelse(BRFSS14$HADSGCO1==2,"Colonoscopy",""))

#HADSIGM3
BRFSS14$HADSIGM3<-ifelse(BRFSS14$HADSIGM3==1,"Yes",
                        ifelse(BRFSS14$HADSIGM3==2,"No",""))

#HAVARTH3
BRFSS14$HAVARTH3<-ifelse(BRFSS14$HAVARTH3==1,"Yes",
                        ifelse(BRFSS14$HAVARTH3==2,"No",""))

#HEIGHT3
BRFSS14$HEIGHT3<-ifelse(BRFSS14$HEIGHT3<=800,((as.numeric(substr(BRFSS14$HEIGHT3,1,1))*12) +  as.numeric(substr(BRFSS14$HEIGHT3,2,3)))*2.54,
                       ifelse(BRFSS14$HEIGHT3 %in% c(9000:9998),substr(BRFSS14$HEIGHT3,2,4),""))

#HIVTST6
BRFSS14$HIVTST6<-ifelse(BRFSS14$HIVTST6==1,"Yes",
                       ifelse(BRFSS14$HIVTST6==2,"No",""))

#HIVTSTD3
BRFSS14$HIVTSTD3-ifelse(BRFSS14$HIVTSTD3<777777,paste(substr(BRFSS14$HIVTSTD3, 1, ifelse(nchar(BRFSS14$HIVTSTD3)==6,2,1)),"-",substr(BRFSS14$HIVTSTD3, ifelse(nchar(BRFSS14$HIVTSTD3)==6,3,2), nchar(BRFSS14$HIVTSTD3))),"")

#HLTHPLN1
BRFSS14$HLTHPLN1<- ifelse(BRFSS14$HLTHPLN1==1,"Yes",
                         ifelse(BRFSS14$HLTHPLN1==2,"No",""))


#HOWLONG
BRFSS14$HOWLONG<- ifelse(BRFSS14$HOWLONG==1,"within 1 year",
                        ifelse(BRFSS14$HOWLONG==2,"1-2 years",
                               ifelse(BRFSS14$HOWLONG==2,"2-3 years",
                                      ifelse(BRFSS14$HOWLONG==2,"3-5 years",
                                             ifelse(BRFSS14$HOWLONG==2,"before 5 years","")))))

#HPVADSHT
BRFSS14$HPVADSHT<- ifelse(BRFSS14$HPVADSHT==3,"all shots taken",
                         ifelse(BRFSS14$HPVADSHT<=2,paste(BRFSS14$HPVADSHT,"shots taken"),""))

#HPVADVC2
BRFSS14$HPVADVC2<- ifelse(BRFSS14$HPVADVC2==1,"Yes",
                         ifelse(BRFSS14$HPVADVC2==2,"No",""))

#HTIN4
BRFSS14$HTIN4<- NULL

#HTM4
BRFSS14$HTM4<- NULL

#IDATE
BRFSS14$IDATE<- NULL

#IMFVPLAC
BRFSS14$IMFVPLAC<-ifelse(BRFSS14$IMFVPLAC==1,"Doctor's office/Health Management Organization",
                        
                        ifelse(BRFSS14$IMFVPLAC==2,"A health department",
                               
                               ifelse(BRFSS14$IMFVPLAC==3,"Another type of clinic or health center",
                                      ifelse(BRFSS14$IMFVPLAC==4,"A senior,recreation or community center",
                                             ifelse(BRFSS14$IMFVPLAC==5,"A store",
                                                    ifelse(BRFSS14$IMFVPLAC==6,"A hospital(e.g. in patient)",
                                                           ifelse(BRFSS14$IMFVPLAC==7,"An emergency room",
                                                                  ifelse(BRFSS14$IMFVPLAC==8,"Workplace",
                                                                         ifelse(BRFSS14$IMFVPLAC==9,"Some other kind of place",
                                                                                ifelse(BRFSS14$IMFVPLAC==10,"Received vaccination in Canada/Mexico",
                                                                                       ifelse(BRFSS14$IMFVPLAC==11,"A school","")))))))))))






#INCOME2
BRFSS14$INCOME2<-ifelse(BRFSS14$INCOME2==1,"Less than 10,000",
                       ifelse(BRFSS14$INCOME2==2,"Less than 15,000",
                              ifelse(BRFSS14$INCOME2==3,"Less than 20,000",
                                     ifelse(BRFSS14$INCOME2==4,"Less than 25,000",
                                            ifelse(BRFSS14$INCOME2==5,"Less than 35,000",
                                                   ifelse(BRFSS14$INCOME2==6,"Less than 50,000",
                                                          ifelse(BRFSS14$INCOME2==7,"Less than 75,000",
                                                                 ifelse(BRFSS14$INCOME2==8,"75,000 or more",""))))))))


#INSULIN
BRFSS14$INSULIN<-ifelse(BRFSS14$INSULIN==1,"Yes",
                       ifelse(BRFSS14$INSULIN==2,"No",""))


#IYEAR
BRFSS14$IYEAR<-NULL


#LANDLINE
BRFSS14$LANDLINE<-ifelse(BRFSS14$LANDLINE==1,"Yes",
                        ifelse(BRFSS14$LANDLINE==2," No",""))

#LASTPAP2
BRFSS14$LASTPAP2<-ifelse(BRFSS14$LASTPAP2==1,"Within 1 year",
                        ifelse(BRFSS14$LASTPAP2==2,"1 -2 years",
                               ifelse(BRFSS14$LASTPAP2==3,"2-3 years",
                                      ifelse(BRFSS14$LASTPAP2==4,"3-5 years",
                                             ifelse(BRFSS14$LASTPAP2==5,"before 5 years","")))))


#LASTSIG3
BRFSS14$LASTSIG3<-ifelse(BRFSS14$LASTSIG3==1,"Within 1 year",
                        ifelse(BRFSS14$LASTSIG3==2,"1-2 years",
                               ifelse(BRFSS14$LASTSIG3==3,"2-3 years",
                                      ifelse(BRFSS14$LASTSIG3==4,"3-5 years",
                                             ifelse(BRFSS14$LASTSIG3==5,"5-10 years",
                                                    ifelse(BRFSS14$LASTSIG3==6,"before 10 years",""))))))

#LASTSMK2
BRFSS14$LASTSMK2<-ifelse(BRFSS14$LASTSMK2==1,"Everyday",
                        ifelse(BRFSS14$LASTSMK2==2,"Somedays",
                               ifelse(BRFSS14$LASTSMK2==3,"Not at all","")))


#LENGEXAM
BRFSS14$LENGEXAM<-ifelse(BRFSS14$LENGEXAM==1,"Within 1 year",
                        ifelse(BRFSS14$LENGEXAM==2,"1 -2 years",
                               ifelse(BRFSS14$LENGEXAM==3,"2-3 years",
                                      ifelse(BRFSS14$LENGEXAM==4,"3-5 years",
                                             ifelse(BRFSS14$LENGEXAM==5,"before 5 years","")))))


#LSATISFY
BRFSS14$LSATISFY<-ifelse(BRFSS14$LSATISFY==1,"Very satisfied",
                        ifelse(BRFSS14$LSATISFY==2,"satisfied",
                               ifelse(BRFSS14$LSATISFY==3,"Dissatisfied",
                                      ifelse(BRFSS14$LSATISFY==4,"Very dissatisfied",""))))


#LSTBLDS3
BRFSS14$LSTBLDS3<-ifelse(BRFSS14$LSTBLDS3==1,"Within 1 year",
                        ifelse(BRFSS14$LSTBLDS3==2,"1 -2 years",
                               ifelse(BRFSS14$LSTBLDS3==3,"2-3 years",
                                      ifelse(BRFSS14$LSTBLDS3==4,"3-5 years",
                                             ifelse(BRFSS14$LSTBLDS3==5,"before 5 years","")))))


#MARITAL
BRFSS14$MARITAL<-ifelse(BRFSS14$MARITAL==1,"Married",
                       ifelse(BRFSS14$MARITAL==2,"Divorced",
                              ifelse(BRFSS14$MARITAL==3,"Widowed",
                                     ifelse(BRFSS14$MARITAL==4,"Separated",
                                            ifelse(BRFSS14$MARITAL==5,"Never married",
                                                   ifelse(BRFSS14$MARITAL==6,"A member of an unmarried couple",""))))))



#MAXDRNKS
BRFSS14$MAXDRNKS<-ifelse(BRFSS14$MAXDRNKS<=76,BRFSS14$MAXDRNKS,"")


#MEDCOST
BRFSS14$MEDCOST<-ifelse(BRFSS14$MEDCOST==1,"Yes",
                       ifelse(BRFSS14$MEDCOST==2,"No",""))

#MENTHLTH
BRFSS14$MENTHLTH<-ifelse(BRFSS14$MENTHLTH<=30,BRFSS14$MENTHLTH,
                        ifelse(BRFSS14$MENTHLTH==88,0,""))


#MSCODE
BRFSS14$MSCODE<-ifelse(BRFSS14$MSCODE==1,"In city center",
                      ifelse(BRFSS14$MSCODE==2,"outside city center but inside county",
                             ifelse(BRFSS14$MSCODE==3,"Inside the suburban county",
                                    ifelse(BRFSS14$MSCODE==5,"Not in an MSA",""))))


#NUMADULT
BRFSS14$NUMADULT<-ifelse(BRFSS14$NUMADULT<=99,BRFSS14$NUMADULT,"")


#NUMHHOL2
BRFSS14$NUMHHOL2<-ifelse(BRFSS14$NUMADULT==1,"Yes",
                        ifelse(BRFSS14$NUMADULT==2,"No",""))

#NUMMEN
BRFSS14$NUMMEN<-ifelse(BRFSS14$NUMMEN<=99,BRFSS14$NUMMEN,"")


#NUMPHON2
BRFSS14$NUMPHON2<-NULL


#NUMWOMEN
BRFSS14$NUMWOMEN<-ifelse(BRFSS14$NUMWOMEN<=99,BRFSS14$NUMWOMEN,"")


#PAINACT2
BRFSS14$PAINACT2<-NULL


#PDIABTST
BRFSS14$PDIABTST<-ifelse(BRFSS14$PDIABTST==1,"Yes",
                        ifelse(BRFSS14$PDIABTST==2,"No",""))


#PERSDOC2
BRFSS14$PERSDOC2<-ifelse(BRFSS14$PERSDOC2==1,"YEes, only 1",
                        ifelse(BRFSS14$PERSDOC2==2,"More than 1",
                               ifelse(BRFSS14$PERSDOC2==3,"no","")))

#PHYSHLTH
BRFSS14$PHYSHLTH<-ifelse(BRFSS14$PHYSHLTH<=30,BRFSS14$PHYSHLTH,
                        ifelse(BRFSS14$PHYSHLTH==88,0,""))

#PNEUVAC3
BRFSS14$PNEUVAC3<-ifelse(BRFSS14$PNEUVAC3==1,"Yes",
                        ifelse(BRFSS14$PNEUVAC3==2,"No",""))


#POORHLTH
BRFSS14$POORHLTH<-ifelse(BRFSS14$POORHLTH<=30,BRFSS14$POORHLTH,
                        ifelse(BRFSS14$POORHLTH==88,0,""))


#PREDIAB1
BRFSS14$PREDIAB1<-ifelse(BRFSS14$PREDIAB1==1,"Yes",
                        ifelse(BRFSS14$PREDIAB1==2,"Yes,during pregnency",
                               ifelse(BRFSS14$PREDIAB1==3,"No","")))


#PREGNANT
BRFSS14$PREGNANT<-ifelse(BRFSS14$PREGNANT==1,"Yes",
                        ifelse(BRFSS14$PREGNANT==2,"No",""))


#PROFEXAM
BRFSS14$PROFEXAM<-ifelse(BRFSS14$PROFEXAM==1,"Yes",
                        ifelse(BRFSS14$PROFEXAM==2,"No",""))

#PSATEST1
BRFSS14$PSATEST1<-ifelse(BRFSS14$PSATEST1==1,"Yes",
                        ifelse(BRFSS14$PSATEST1==2,"No",""))


#PSATIME
BRFSS14$PSATIME<-ifelse(BRFSS14$PSATIME==1,"Within 1 year",
                       ifelse(BRFSS14$PSATIME==2,"1 -2 years",
                              ifelse(BRFSS14$PSATIME==3,"2-3 years",
                                     ifelse(BRFSS14$PSATIME==4,"3-5 years",
                                            ifelse(BRFSS14$PSATIME==5,"before 5 years","")))))


#PVTRESD2
BRFSS14$PVTRESD2<-ifelse(BRFSS14$PVTRESD2==1,"Yes",
                        ifelse(BRFSS14$PVTRESD2==2,"No",""))


#QLACTLM2
BRFSS14$QLACTLM2<-ifelse(BRFSS14$QLACTLM2==1,"Yes",
                        ifelse(BRFSS14$QLACTLM2==2,"No",""))


#QLHLTH2
BRFSS14$QLHLTH2<-NULL


#QLMENTL2
BRFSS14$QLMENTL2<-NULL


#QLSTRES2
BRFSS14$QLSTRES2<-NULL


#QSTLANG
BRFSS14$QSTLANG<-ifelse(BRFSS14$QSTLANG==1,"English",
                       ifelse(BRFSS14$QSTLANG==2,"Spanish",
                              ifelse(BRFSS14$QSTLANG>=3,"other","")))


#RCSGENDR
BRFSS14$RCSGENDR<-ifelse(BRFSS14$RCSGENDR==1,"Boy",
                        ifelse(BRFSS14$RCSGENDR==2,"Girl",""))


#RCSRLTN2
BRFSS14$RCSRLTN2<-ifelse(BRFSS14$RCSRLTN2==1,"Parent",
                        ifelse(BRFSS14$RCSRLTN2==2,"Grandparent",
                               ifelse(BRFSS14$RCSRLTN2==3,"Foster Parent or Guardian",
                                      ifelse(BRFSS14$RCSRLTN2==4,"Sibling",
                                             ifelse(BRFSS14$RCSRLTN2==5,"Other relative",
                                                    ifelse(BRFSS14$RCSRLTN2==6,"Not related in any way",
                                                           ifelse(BRFSS14$RCSRLTN2==7,"Don't know not sure","")))))))



#RENTHOM1
BRFSS14$RENTHOM1<-ifelse(BRFSS14$RENTHOM1==1,"Parent",
                        ifelse(BRFSS14$RENTHOM1==2,"Own",
                               ifelse(BRFSS14$RENTHOM1==3,"Rent",
                                      ifelse(BRFSS14$RENTHOM1==4,"Other Arrangement",""))))


#SCNTLPAD
BRFSS14$SCNTLPAD<-ifelse(BRFSS14$SCNTLPAD==1,"By Salary",
                        ifelse(BRFSS14$SCNTLPAD==2,"Paid by hour",
                               ifelse(BRFSS14$SCNTLPAD==3,"Paid by job/task(commission)",
                                      ifelse(BRFSS14$SCNTLPAD==4,"Paid some other way",""))))



#SCNTLWK1
BRFSS14$SCNTLWK1<- ifelse(BRFSS14$SCNTLWK1==98,0,
                         ifelse(BRFSS14$SCNTLWK1<=96,BRFSS14$SCNTLWK1,""))


#SCNTPAID
BRFSS14$SCNTPAID<-NULL


#SCNTWRK1
BRFSS14$SCNTWRK1<-NULL


#SEATBELT
BRFSS14$SEATBELT<-ifelse(BRFSS14$SEATBELT==1,"always",
                        ifelse(BRFSS14$SEATBELT==2,"nearly always",
                               ifelse(BRFSS14$SEATBELT==3,"sometimes",
                                      ifelse(BRFSS14$SEATBELT==4,"seldom",
                                             ifelse(BRFSS14$SEATBELT==5,"never",
                                                    ifelse(BRFSS14$SEATBELT==8,"Never drive or ride a car",""))))))


#SEQNO
BRFSS14$SEQNO<-NULL


#SEX
BRFSS14$SEX<-ifelse(BRFSS14$SEX==1,"Male",
                   ifelse(BRFSS14$SEX==2,"Female",""))



#SMOKDAY2
BRFSS14$SMOKDAY2<-ifelse(BRFSS14$SMOKDAY2==1,"Everyday",
                        ifelse(BRFSS14$SMOKDAY2==2,"Somedays",
                               ifelse(BRFSS14$SMOKDAY2==3,"Not at all","")))



#SMOKE100
BRFSS14$SMOKE100<-ifelse(BRFSS14$SMOKE100==1,"Yes",
                        ifelse(BRFSS14$SMOKE100==2,"No",""))



#STOPSMK2
BRFSS14$STOPSMK2<-ifelse(BRFSS14$STOPSMK2==1,"Yes",
                        ifelse(BRFSS14$STOPSMK2==2,"No",""))


#USEEQUIP
BRFSS14$USEEQUIP<-ifelse(BRFSS14$USEEQUIP==1,"Yes",
                        ifelse(BRFSS14$USEEQUIP==2,"No",""))


#USENOW3
BRFSS14$USENOW3<-ifelse(BRFSS14$USENOW3==1,"Everyday",
                       ifelse(BRFSS14$USENOW3==2,"Somedays",
                              ifelse(BRFSS14$USENOW3==3,"Not at all","")))



#VETERAN3
BRFSS14$VETERAN3<-ifelse(BRFSS14$VETERAN3==1,"Yes",
                        ifelse(BRFSS14$VETERAN3==2,"No",""))


#WEIGHT2
BRFSS14$WEIGHT2<-ifelse(BRFSS14$WEIGHT2<=1000,round((as.numeric(BRFSS14$WEIGHT2)*0.453592),digits=0),
                       ifelse(BRFSS14$WEIGHT2 %in% c(9000:9998),BRFSS14$WEIGHT2-9000,""))

#WTKG3
BRFSS14$WTKG3<-NULL


##########################################################################################

BRFSS14$HAVARTH3

BRFSS14_ARTHRITIS<- BRFSS14[BRFSS14$HAVARTH3!="",]

nrow(BRFSS14_ARTHRITIS)

nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$AGEG5YR=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$CHLDCNT=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$EDUCAG=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$HCVU651=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$INCOMG=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$PNEUMO2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$RFBING5=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$RFBMI5=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$RFHLTH=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$RFSMOK3=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$SMOKER3=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$STATE=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$TOTINDA=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$ADDEPEV2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$ALCDAY5=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$AVEDRNK2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$CADULT=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$CELLFON2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$CHCKIDNY=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$CPDEMO1=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$DISPCODE=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$EDUCA=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$EMTSUPRT=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$EXERANY2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$GENHLTH=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$HAVARTH3=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$HEIGHT3=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$HLTHPLN1=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$INCOME2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$LSATISFY=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$MARITAL=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$MAXDRNKS=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$MEDCOST=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$MSCODE=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$NUMADULT=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$NUMHHOL2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$NUMPHON2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$PERSDOC2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$PNEUVAC3=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$POORHLTH=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$PREGNANT=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$PROFEXAM=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$PVTRESD2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$QLACTLM2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$QSTLANG=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$RCSGENDR=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$RCSRLTN2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$RENTHOM1=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$SCNTLWK1=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$SEX=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$SMOKDAY2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$STOPSMK2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$USEEQUIP=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$USENOW3=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$VETERAN3=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$WEIGHT2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$ASTHMS1=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$BMI5=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$ASACTLIM=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$ASATTACK=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$ASDRVIST=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$ASINHALR=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$BLDSUGAR=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$CHCOCNCR=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$CHCSCNCR=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$CHECKUP1=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$CHILDREN=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$CHKHEMO3=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$CVDCRHD4=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$CVDINFR4=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$CVDSTRK3=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$DIABETE3=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$FLSHTMY2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$HIVTST6=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$INSULIN=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$LASTSMK2=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$PDIABTST=="",])
nrow(BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$PREDIAB1=="",])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$AGEG5YR),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$CHLDCNT),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$EDUCAG),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$HCVU651),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$INCOMG),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$PNEUMO2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$RFBING5),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$RFBMI5),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$RFHLTH),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$RFSMOK3),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$SMOKER3),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$STATE),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$TOTINDA),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$ADDEPEV2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$ALCDAY5),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$AVEDRNK2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$CADULT),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$CELLFON2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$CHCKIDNY),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$CPDEMO1),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$DISPCODE),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$EDUCA),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$EMTSUPRT),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$EXERANY2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$GENHLTH),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$HAVARTH3),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$HEIGHT3),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$HLTHPLN1),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$INCOME2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$LSATISFY),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$MARITAL),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$MAXDRNKS),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$MEDCOST),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$MSCODE),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$NUMADULT),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$NUMHHOL2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$NUMPHON2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$PERSDOC2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$PNEUVAC3),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$POORHLTH),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$PREGNANT),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$PROFEXAM),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$PVTRESD2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$QLACTLM2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$QSTLANG),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$RCSGENDR),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$RCSRLTN2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$RENTHOM1),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$SCNTLWK1),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$SEX),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$SMOKDAY2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$STOPSMK2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$USEEQUIP),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$USENOW3),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$VETERAN3),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$WEIGHT2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$ASTHMS1),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$BMI5),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$ASACTLIM),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$ASATTACK),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$ASDRVIST),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$ASINHALR),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$BLDSUGAR),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$CHCOCNCR),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$CHCSCNCR),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$CHECKUP1),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$CHILDREN),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$CHKHEMO3),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$CVDCRHD4),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$CVDINFR4),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$CVDSTRK3),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$DIABETE3),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$FLSHTMY2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$HIVTST6),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$INSULIN),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$LASTSMK2),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$PDIABTST),])
nrow(BRFSS14_ARTHRITIS[is.na(BRFSS14_ARTHRITIS$PREDIAB1),])


#########################################################################################


x14<-BRFSS14_ARTHRITIS[BRFSS14_ARTHRITIS$AGEG5YR!="",]
x14<-x14[x14$CHLDCNT!="",]
x14<-x14[x14$EDUCAG!="",]
x14<-x14[x14$RFBING5!="",]
x14<-x14[x14$RFHLTH!="",]
x14<-x14[x14$SMOKER3!="",]
x14<-x14[x14$TOTINDA!="",]
x14<-x14[x14$ADDEPEV2!="",]
x14<-x14[x14$ALCDAY5!="",]
x14<-x14[x14$CHCKIDNY!="",]
x14<-x14[x14$EDUCA!="",]
x14<-x14[x14$EXERANY2!="",]
x14<-x14[x14$GENHLTH!="",]
x14<-x14[x14$HAVARTH3!="",]
x14<-x14[x14$HEIGHT3!="",]
x14<-x14[x14$HLTHPLN1!="",]
x14<-x14[x14$MARITAL!="",]
x14<-x14[x14$MEDCOST!="",]
x14<-x14[x14$PERSDOC2!="",]
x14<-x14[x14$PNEUVAC3!="",]
x14<-x14[x14$PHYSHLTH!="",]
x14<-x14[x14$QLACTLM2!="",]
x14<-x14[x14$QSTLANG!="",]
x14<-x14[x14$SEX!="",]
x14<-x14[x14$VETERAN3!="",]
x14<-x14[x14$WEIGHT2!="",]
x14<-x14[x14$ASTHMS1!="",]
x14<-x14[x14$BMI5!="",]
x14<-x14[x14$CHCOCNCR!="",]
x14<-x14[x14$CVDSTRK3!="",]
x14<-x14[x14$DIABETE3!="",]
x14<-x14[x14$HCVU651!="",]
x14<-x14[x14$INCOME2!="",]
x14<-x14[x14$STATE!="",]
x14<-x14[x14$USEEQUIP!="",]
x14<-x14[x14$USENOW3!="",]
x14<-x14[x14$CHECKUP1!="",]
x14<-x14[x14$CHILDREN!="",]
x15<-x15[x15$CASTHDX2!="",]



a14<-x14[, c(	"AGEG5YR",	"CHLDCNT",	"EDUCAG",	"RFBING5",	"RFHLTH",	"SMOKER3",	"TOTINDA",	"ADDEPEV2",	"ALCDAY5",	"CHCKIDNY",	"EDUCA",	"EXERANY2",	"GENHLTH",	"HAVARTH3",	"HEIGHT3",	"HLTHPLN1",	"MARITAL",	"MEDCOST",	"PERSDOC2",	"PNEUVAC3",	"PHYSHLTH",	"QLACTLM2",	"QSTLANG",	"SEX",	"VETERAN3",	"WEIGHT2",	"ASTHMS1",	"BMI5",	"CHCOCNCR",	"CVDSTRK3",	"DIABETE3",	"HCVU651",	"INCOME2",	"STATE",	"USEEQUIP",	"USENOW3",	"CHECKUP1",	"CHILDREN","CASTHDX2")]
dim(a14)
x14<-a14
BRFSS14_ARTHRITIS<-x14


#############################################################################################

BRFSS15<-BRFSS_2015

#AGE65YR
BRFSS15$AGE65YR<- ifelse(BRFSS15$AGE65YR==1,"Yes",
                         ifelse(BRFSS15$AGE65YR==2,"No",""))

#ASTHMS1
BRFSS15$ASTHMS1<- ifelse(BRFSS15$ASTHMS1==1,"current",
                        ifelse(BRFSS15$ASTHMS1==2,"former",
                               ifelse(BRFSS15$ASTHMS1==3,"never","")))

#BMI5
BRFSS15$BMI5<- BRFSS15$BMI5/100

#CASTHM1
BRFSS15$CASTHM1<- ifelse(BRFSS15$CASTHM1==1,"No",
                        ifelse(BRFSS15$CASTHM1==2,"Yes",""))

#CHLDCNT
BRFSS15$CHLDCNT<- ifelse(BRFSS15$CHLDCNT==1,"no child",
                        ifelse(BRFSS15$CHLDCNT==2,"1",
                               ifelse(BRFSS15$CHLDCNT==3,"2",
                                      ifelse(BRFSS15$CHLDCNT==4,"3",
                                             ifelse(BRFSS15$CHLDCNT==5,"4",
                                                    ifelse(BRFSS15$CHLDCNT==6,"5",""))))))

#CLLCPWT
BRFSS15$CLLCPWT<- NULL

#DRDXAR1
BRFSS15$DRDXAR1<- ifelse(BRFSS15$DRDXAR1==1,"Yes",
                        ifelse(BRFSS15$DRDXAR1==2,"No",""))

#EDUCAG
BRFSS15$EDUCAG<- ifelse(BRFSS15$EDUCAG==1,"no High School",
                       ifelse(BRFSS15$EDUCAG==2,"High School Grad",
                              ifelse(BRFSS15$EDUCAG==3,"Attended College","College Grad")))

#HCVU651
BRFSS15$HCVU651<- ifelse(BRFSS15$HCVU651==1,"Yes",
                        ifelse(BRFSS15$HCVU651==2,"No",""))

#INCOMG
BRFSS15$INCOMG<- ifelse(BRFSS15$INCOMG==1,"less than 15000",
                       ifelse(BRFSS15$INCOMG==2,"15-25",
                              ifelse(BRFSS15$INCOMG==3,"25-35",
                                     ifelse(BRFSS15$INCOMG==4,"35-50",
                                            ifelse(BRFSS15$INCOMG==5,"greater than 50000","")))))

#LLCPWT
BRFSS15$LLCPWT<- NULL

#LTASTH1
BRFSS15$LTASTH1<- ifelse(BRFSS15$LTASTH1==1,"No",
                       ifelse(BRFSS15$LTASTH1==2,"Yes",""))

#PNEUMO2
BRFSS15$PNEUMO2<- ifelse(BRFSS15$PNEUMO2==1,"Yes",
                        ifelse(BRFSS15$PNEUMO2==2,"No",""))

#PSU
BRFSS15$PSU<- NULL

#PNEUMO2
BRFSS15$PNEUMO2<- NULL

#RFBING5
BRFSS15$RFBING5<- ifelse(BRFSS15$RFBING5==1,"No",
                        ifelse(BRFSS15$RFBING5==2,"Yes",""))

#RFBMI5
BRFSS15$RFBMI5<- ifelse(BRFSS15$RFBMI5==1,"BMI =<25",
                       ifelse(BRFSS15$RFBMI5==2,"BMI >25",""))

#RFHLTH
BRFSS15$RFHLTH<- ifelse(BRFSS15$RFHLTH==1,"good or better health",
                       ifelse(BRFSS15$RFHLTH==2,"fair or poor helath",""))

#RFSEAT2
BRFSS15$RFSEAT2<- ifelse(BRFSS15$RFSEAT2==1,"always wear seat belt",
                        ifelse(BRFSS15$RFSEAT2==2,"seldom or never",""))

#RFSEAT3
BRFSS15$RFSEAT3<- NULL

#RFSMOK3
BRFSS15$RFSMOK3<- ifelse(BRFSS15$RFSMOK3==1,"No",
                        ifelse(BRFSS15$RFSMOK3==2,"Yes",""))

#SMOKER3
BRFSS15$SMOKER3<- ifelse(BRFSS15$SMOKER3==1,"current/smokes everyday",
                                        ifelse(BRFSS15$SMOKER3==2,"current/smokes somedays",
                                               ifelse(BRFSS15$SMOKER3==3,"former",
                                                      ifelse(BRFSS15$SMOKER3==4,"never",""))))

#STATE
BRFSS15$STATE<- ifelse(BRFSS15$STATE==1,"Alabama",
                      ifelse(BRFSS15$STATE==2,"Alaska",
                             ifelse(BRFSS15$STATE==4,"Arizona",
                                    ifelse(BRFSS15$STATE==5,"Arkansas",
                                           ifelse(BRFSS15$STATE==6,"California",
                                                  ifelse(BRFSS15$STATE==8,"Colorado",
                                                         ifelse(BRFSS15$STATE==9,"Connecticut",
                                                                ifelse(BRFSS15$STATE==10,"Delaware",
                                                                       ifelse(BRFSS15$STATE==11,"District of Columbia",
                                                                              ifelse(BRFSS15$STATE==12,"Florida",
                                                                                     ifelse(BRFSS15$STATE==13,"Georgia",
                                                                                            ifelse(BRFSS15$STATE==15,"Hawaii",
                                                                                                   ifelse(BRFSS15$STATE==16,"Idaho",
                                                                                                          ifelse(BRFSS15$STATE==17,"Illinois",
                                                                                                                 ifelse(BRFSS15$STATE==18,"Indiana",
                                                                                                                        ifelse(BRFSS15$STATE==19,"Iowa",
                                                                                                                               ifelse(BRFSS15$STATE==20,"Kansas",
                                                                                                                                      ifelse(BRFSS15$STATE==21,"Kentucky",
                                                                                                                                             ifelse(BRFSS15$STATE==22,"Louisiana",
                                                                                                                                                    ifelse(BRFSS15$STATE==23,"Maine",
                                                                                                                                                           ifelse(BRFSS15$STATE==24,"Maryland",
                                                                                                                                                                  ifelse(BRFSS15$STATE==25,"Massachusetts",
                                                                                                                                                                         ifelse(BRFSS15$STATE==26,"Michigan",
                                                                                                                                                                                ifelse(BRFSS15$STATE==27,"Minnesota",
                                                                                                                                                                                       ifelse(BRFSS15$STATE==28,"Mississippi",
                                                                                                                                                                                              ifelse(BRFSS15$STATE==29,"Missouri",
                                                                                                                                                                                                     ifelse(BRFSS15$STATE==30,"Montana",
                                                                                                                                                                                                            ifelse(BRFSS15$STATE==31,"Nebraska",
                                                                                                                                                                                                                   ifelse(BRFSS15$STATE==32,"Nevada",
                                                                                                                                                                                                                          ifelse(BRFSS15$STATE==33,"New Hampshire",
                                                                                                                                                                                                                                 ifelse(BRFSS15$STATE==34,"New Jersey",
                                                                                                                                                                                                                                        ifelse(BRFSS15$STATE==35,"New Mexico",
                                                                                                                                                                                                                                               ifelse(BRFSS15$STATE==36,"New York",
                                                                                                                                                                                                                                                      ifelse(BRFSS15$STATE==37,"North Carolina",
                                                                                                                                                                                                                                                             ifelse(BRFSS15$STATE==38,"North Dakota",
                                                                                                                                                                                                                                                                    ifelse(BRFSS15$STATE==39,"Ohio",
                                                                                                                                                                                                                                                                           ifelse(BRFSS15$STATE==40,"Oklahoma",
                                                                                                                                                                                                                                                                                  ifelse(BRFSS15$STATE==41,"Oregon ",""
                                                                                                                                                                                                                                                                                  ))))))))))))))))))))))))))))))))))))))

#STRWT
BRFSS15$STRWT<- NULL

#STSTR
BRFSS15$STSTR<- NULL

#TOTINDA
BRFSS15$TOTINDA<- ifelse(BRFSS15$TOTINDA==1,"Yes",
                        ifelse(BRFSS15$TOTINDA==2,"No",""))

#WT2RAKE
BRFSS15$WT2RAKE<- NULL

#ADDEPEV2
BRFSS15$ADDEPEV2<- ifelse(BRFSS15$ADDEPEV2==1,"Yes",
                         ifelse(BRFSS15$ADDEPEV2==2,"No",""))

#ALCDAY5 - Convert days per week to approx days per month considering 30 days in a month.
BRFSS15$ALCDAY5<- ifelse(BRFSS15$ALCDAY5 %in% c(100:199), round((BRFSS15$ALCDAY5-100)*4.29,digits=0),
                        ifelse(BRFSS15$ALCDAY5 %in% c(200:299),BRFSS15$ALCDAY5-200,
                               ifelse(BRFSS15$ALCDAY5 ==888,0,"")))

#ASACTLIM
BRFSS15$ASACTLIM<- ifelse(BRFSS15$ASACTLIM<=365,BRFSS15$ASACTLIM,
                         ifelse(BRFSS15$ASACTLIM==888,0,""))

#ASATTACK
BRFSS15$ASATTACK<- ifelse(BRFSS15$ASATTACK==1,"Yes",
                         ifelse(BRFSS15$ASATTACK==2,"No",""))

#ASDRVIST
BRFSS15$ASDRVIST<- ifelse(BRFSS15$ASDRVIST<=87,BRFSS15$ASDRVIST,
                         ifelse(BRFSS15$ASDRVIST==88,0,""))

#ASERVIST
BRFSS15$ASERVIST<- ifelse(BRFSS15$ASERVIST<=87,BRFSS15$ASERVIST,
                         ifelse(BRFSS15$ASERVIST==88,0,""))

#ASINHALR
BRFSS15$ASINHALR<- ifelse(BRFSS15$ASINHALR==1,"1-4",
                         ifelse(BRFSS15$ASINHALR==2,"5-14",
                                ifelse(BRFSS15$ASINHALR==3,"15-29",
                                       ifelse(BRFSS15$ASINHALR==4,"30-59",
                                              ifelse(BRFSS15$ASINHALR==5,"60-99",
                                                     ifelse(BRFSS15$ASINHALR==6,">=100",
                                                            ifelse(BRFSS15$ASINHALR==8,"0","")))))))

#ASTHMA3
BRFSS15$ASTHMA3<- ifelse(BRFSS15$ASTHMA3==1,"Yes",
                        ifelse(BRFSS15$ASTHMA3==2,"No",""))

#ASTHMAGE
BRFSS15$ASTHMAGE<- ifelse(BRFSS15$ASTHMAGE==97,"<=10",
                         ifelse(BRFSS15$ASTHMAGE<97,BRFSS15$ASTHMAGE,""))

#ASTHMED3
BRFSS15$ASTHMED3<- ifelse(BRFSS15$ASTHMED3==1,"1-14",
                         ifelse(BRFSS15$ASTHMED3==2,"15-24",
                                ifelse(BRFSS15$ASTHMED3==3,"25-30",
                                       ifelse(BRFSS15$ASTHMED3==8,"0",""))))

#CADULT
BRFSS15$CADULT<- ifelse(BRFSS15$CADULT==1,"Yes/Male",
                       ifelse(BRFSS15$CADULT==2,"Yes/Female",""))

#CASTHDX2
BRFSS15$CASTHDX2<- ifelse(BRFSS15$CASTHDX2==1,"Yes",
                         ifelse(BRFSS15$CASTHDX2==2,"No",""))

#CASTHNO2
BRFSS15$CASTHNO2<- ifelse(BRFSS15$CASTHNO2==1,"Yes",
                         ifelse(BRFSS15$CASTHNO2==2,"No",""))

#CELLFON2
BRFSS15$CELLFON2<- ifelse(BRFSS15$CELLFON2==1,"Yes","")

#CHCKIDNY
BRFSS15$CHCKIDNY<- ifelse(BRFSS15$CHCKIDNY==1,"Yes",
                         ifelse(BRFSS15$CHCKIDNY==2,"No",""))

#CHCOCNCR
BRFSS15$CHCOCNCR<- ifelse(BRFSS15$CHCOCNCR==1,"Yes",
                         ifelse(BRFSS15$CHCOCNCR==2,"No",""))

#CHCSCNCR
BRFSS15$CHCSCNCR<- ifelse(BRFSS15$CHCSCNCR==1,"Yes",
                         ifelse(BRFSS15$CHCSCNCR==2,"No",""))

#CHECKUP1
BRFSS15$CHECKUP1<- ifelse(BRFSS15$CHECKUP1==1,"within past year",
                         ifelse(BRFSS15$CHECKUP1==2,"within past 2 years",
                                ifelse(BRFSS15$CHECKUP1==3,"within past 5 years",
                                       ifelse(BRFSS15$CHECKUP1==4,"5 or more years before",
                                              ifelse(BRFSS15$CHECKUP1==8,"never","")))))

#CHILDREN
BRFSS15$CHILDREN<- ifelse(BRFSS15$CHILDREN<=87,BRFSS15$CHILDREN,
                         ifelse(BRFSS15$CHILDREN==88,"0",""))

#CHKHEMO3
BRFSS15$CHKHEMO3<- ifelse(BRFSS15$CHKHEMO3<=76,BRFSS15$CHKHEMO3,
       ifelse(BRFSS15$CHKHEMO3==88,"0",
              ifelse(BRFSS15$CHKHEMO3==98,"never heard of the test","")))

#CPDEMO1
BRFSS15$CPDEMO1<- ifelse(BRFSS15$CPDEMO1==1,"Yes",
                        ifelse(BRFSS15$CPDEMO1==2,"No",""))

#CSTATE
BRFSS15$CSTATE<- ifelse(BRFSS15$CSTATE==1,"Yes",
                       ifelse(BRFSS15$CSTATE==2,"No",""))

#CTELENUM
BRFSS15$CTELENUM<- NULL

#CTELNUM1
BRFSS15$CTELNUM1<- NULL

#CVDCRHD4
BRFSS15$CVDCRHD4<- ifelse(BRFSS15$CVDCRHD4==1,"Yes",
                         ifelse(BRFSS15$CVDCRHD4==2,"No",""))

#CVDINFR4
BRFSS15$CVDINFR4<- ifelse(BRFSS15$CVDINFR4==1,"Yes",
                         ifelse(BRFSS15$CVDINFR4==2,"No",""))

#CVDSTRK3
BRFSS15$CVDSTRK3<- ifelse(BRFSS15$CVDSTRK3==1,"Yes",
                         ifelse(BRFSS15$CVDSTRK3==2,"No",""))

#DIABAGE2
BRFSS15$DIABAGE2<- ifelse(BRFSS15$DIABAGE2<=97,BRFSS15$DIABAGE2,"")

#DIABEDU
BRFSS15$DIABEDU<- ifelse(BRFSS15$DIABEDU==1,"Yes",
                        ifelse(BRFSS15$DIABEDU==2,"No",""))

#DIABETE3
BRFSS15$DIABETE3<- ifelse(BRFSS15$DIABETE3==1,"Yes",
                         ifelse(BRFSS15$DIABETE3==2,"Yes/female during pregnancy",
                                ifelse(BRFSS15$DIABETE3==3,"No",
                                       ifelse(BRFSS15$DIABETE3==2,"Borderline diabetes/No current diabetes",""))))

#DIABEYE
BRFSS15$DIABEYE<- ifelse(BRFSS15$DIABEYE==1,"Yes",
                        ifelse(BRFSS15$DIABEYE==2,"No",""))

#DISPCODE
BRFSS15$DISPCODE<- NULL

#DOCTDIAB
BRFSS15$DOCTDIAB<- ifelse(BRFSS15$DOCTDIAB==88,0,
                         ifelse(BRFSS15$DOCTDIAB<=76,BRFSS15$DOCTDIAB,""))

#DRNK3GE5
BRFSS15$DRNK3GE5<- ifelse(BRFSS15$DRNK3GE5==88,0,
                         ifelse(BRFSS15$DRNK3GE5<=76,BRFSS15$DRNK3GE5,""))

#DRNKANY5
BRFSS15$DRNKANY5<- ifelse(BRFSS15$DRNKANY5==1,"Yes",
                         ifelse(BRFSS15$DRNKANY5==2,"No",""))

#DROCDY3_
BRFSS15$DROCDY3_<- NULL

#EDUCA
BRFSS15$EDUCA<-ifelse(BRFSS15$EDUCA==1,"never attended school",
                     ifelse(BRFSS15$EDUCA==2,"1-8 elementary",
                            ifelse(BRFSS15$EDUCA==3,"9-11 high school",
                                   ifelse(BRFSS15$EDUCA==4,"grade 12 or higher",
                                          ifelse(BRFSS15$EDUCA==5,"college year 1-3",
                                                 ifelse(BRFSS15$EDUCA==6,"college year 4 or graduate",""))))))

#EMTSUPRT
BRFSS15$EMTSUPRT<- ifelse(BRFSS15$EMTSUPRT==1,"always",
                         ifelse(BRFSS15$EMTSUPRT==2,"usually",
                                ifelse(BRFSS15$EMTSUPRT==3,"sometimes",
                                       ifelse(BRFSS15$EMTSUPRT==4,"rarely",
                                              ifelse(BRFSS15$EMTSUPRT==5,"never","")))))

#EXERANY2
BRFSS15$EXERANY2<- ifelse(BRFSS15$EXERANY2==1,"Yes",
                         ifelse(BRFSS15$EXERANY2==2,"No",""))

#EYEEXAM
BRFSS15$EYEEXAM<- ifelse(BRFSS15$EYEEXAM==1,"within past one month",
                        ifelse(BRFSS15$EYEEXAM==2,"within past year",
                               ifelse(BRFSS15$EYEEXAM==3,"within past 2 years",
                                      ifelse(BRFSS15$EYEEXAM==4,"2 or more years",
                                             ifelse(BRFSS15$EYEEXAM==8,"never","")))))

#FEETCHK
BRFSS15$FEETCHK<- ifelse(BRFSS15$FEETCHK==88,0,
                        ifelse(BRFSS15$FEETCHK<=76,BRFSS15$DOCTDIAB,""))

#FEETCHK2
BRFSS15$FEETCHK2<- ifelse(BRFSS15$FEETCHK2 %in% c(100:199), round((BRFSS15$FEETCHK2-100)*365,digits=0),
                         ifelse(BRFSS15$FEETCHK2 %in% c(200:299),round((BRFSS15$FEETCHK2-200)*(365/7),digits=0),
                                ifelse(BRFSS15$FEETCHK2 %in% c(300:399),round((BRFSS15$FEETCHK2-300)*12,digits=0),
                                       ifelse(BRFSS15$FEETCHK2 %in% c(400:499),BRFSS15$FEETCHK2,
                                              ifelse(BRFSS15$FEETCHK2 ==888,0,"")))))

#AGE_G
BRFSS15$AGE_G<- ifelse(BRFSS15$AGE_G ==1, "18-24",
                      ifelse(BRFSS15$AGE_G ==2,"25-34",
                             ifelse(BRFSS15$AGE_G ==3,"35-44",
                                    ifelse(BRFSS15$AGE_G ==4,"45-54",
                                           ifelse(BRFSS15$AGE_G ==5,"55-64",">=65")))))

#AGEG5YR
BRFSS15$AGEG5YR<- ifelse(BRFSS15$AGEG5YR ==1, "18-24",
                        ifelse(BRFSS15$AGEG5YR ==2,"25-29",
                               ifelse(BRFSS15$AGEG5YR ==3,"30-34",
                                      ifelse(BRFSS15$AGEG5YR ==4,"35-39",
                                             ifelse(BRFSS15$AGEG5YR ==5,"40-44",
                                                    ifelse(BRFSS15$AGEG5YR ==6,"44-49",
                                                           ifelse(BRFSS15$AGEG5YR ==7,"50-54",
                                                                  ifelse(BRFSS15$AGEG5YR ==8,"55-59",
                                                                         ifelse(BRFSS15$AGEG5YR ==9,"60-64",
                                                                                ifelse(BRFSS15$AGEG5YR ==10,"65-69",
                                                                                       ifelse(BRFSS15$AGEG5YR ==11,"70-74",
                                                                                              ifelse(BRFSS15$AGEG5YR ==12,"75-79",
                                                                                                     ifelse(BRFSS15$AGEG5YR ==13,">=80","")))))))))))))

#AIDTST3
BRFSS15$AIDTST3<- ifelse(BRFSS15$AIDTST3==1,"Yes",
                        ifelse(BRFSS15$AIDTST3==2,"No",""))

#BMI5CAT
BRFSS15$BMI5CAT<- ifelse(BRFSS15$BMI5CAT==1,"Underweight (BMI<18.50)",
                        ifelse(BRFSS15$BMI5CAT==2,"Normal Weight (18.50<=BMI< 25.00)",
                               ifelse(BRFSS15$BMI5CAT==3,"Overweight (25.00<=BMI<30.00)",
                                      ifelse(BRFSS15$BMI5CAT==4,"Obese (30.00<=BMI<99.99)",""))))

#ASNOSLEP
BRFSS15$ASNOSLEP<- ifelse(BRFSS15$ASNOSLEP ==1, "1-2",
                         ifelse(BRFSS15$ASNOSLEP ==2,"3-4",
                                ifelse(BRFSS15$ASNOSLEP ==3,"5",
                                       ifelse(BRFSS15$ASNOSLEP ==4,"6-10",
                                              ifelse(BRFSS15$ASNOSLEP ==5,">=10",
                                                     ifelse(BRFSS15$ASNOSLEP ==8,"0",""))))))

#ASRCHKUP
BRFSS15$ASRCHKUP<- ifelse(BRFSS15$ASRCHKUP==88,0,
                         ifelse(BRFSS15$ASRCHKUP<=86,BRFSS15$ASRCHKUP,""))

#ASTHNOW
BRFSS15$ASTHNOW<- ifelse(BRFSS15$ASTHNOW==1,"Yes",
                        ifelse(BRFSS15$ASTHNOW==2,"No",""))

#ASYMPTOM
BRFSS15$ASYMPTOM<- ifelse(BRFSS15$ASYMPTOM ==1, "less than 1 in a week",
                         ifelse(BRFSS15$ASYMPTOM ==2,"1-2/week",
                                ifelse(BRFSS15$ASYMPTOM ==3,"3-6/week",
                                       ifelse(BRFSS15$ASYMPTOM ==4,"everyday but not all the time",
                                              ifelse(BRFSS15$ASYMPTOM ==5,"everyday all the time",
                                                     ifelse(BRFSS15$ASYMPTOM ==8,"never",""))))))

#AVEDRNK2
BRFSS15$AVEDRNK2<- ifelse(BRFSS15$AVEDRNK2<=76,BRFSS15$AVEDRNK2,"")

#BLDSTOOL
BRFSS15$BLDSTOOL<- ifelse(BRFSS15$BLDSTOOL==1,"Yes",
                         ifelse(BRFSS15$BLDSTOOL==2,"No",""))

#BLDSUGAR
BRFSS15$BLDSUGAR<- ifelse(BRFSS15$BLDSUGAR %in% c(100:199), round((BRFSS15$BLDSUGAR-100)*365,digits=0),
                         ifelse(BRFSS15$BLDSUGAR %in% c(200:299),round((BRFSS15$BLDSUGAR-200)*(365/7),digits=0),
                                ifelse(BRFSS15$BLDSUGAR %in% c(300:399),round((BRFSS15$BLDSUGAR-300)*12,digits=0),
                                       ifelse(BRFSS15$BLDSUGAR %in% c(400:499),BRFSS15$BLDSUGAR,
                                              ifelse(BRFSS15$BLDSUGAR ==888,0,"")))))



#FLSHTMY2
BRFSS15$FLSHTMY2<-ifelse(BRFSS15$FLSHTMY2==777777,"never",
                        ifelse(BRFSS15$FLSHTMY2<999999,paste(substr(BRFSS15$FLSHTMY2, 1, ifelse(nchar(BRFSS15$FLSHTMY2)==6,2,1)),"-",substr(BRFSS15$FLSHTMY2, ifelse(nchar(BRFSS15$FLSHTMY2)==6,3,2), nchar(BRFSS15$FLSHTMY2))),""))

#GENHLTH
BRFSS15$GENHLTH<-ifelse(BRFSS15$GENHLTH==1,"Excellent",
                       
                       ifelse(BRFSS15$GENHLTH==2,"Very Good",
                              
                              ifelse(BRFSS15$GENHLTH==3,"Good",
                                     ifelse(BRFSS15$GENHLTH==4,"Fair",
                                            ifelse(BRFSS15$GENHLTH==5,"poor","")))))


#HADHYST2
BRFSS15$HADHYST2<-ifelse(BRFSS15$HADHYST2==1,"Yes",
                        ifelse(BRFSS15$HADHYST2==2,"No",""))

#HADMAM
BRFSS15$HADMAM<-ifelse(BRFSS15$HADMAM==1,"Yes",
                      ifelse(BRFSS15$HADMAM==2,"No",""))

#HADPAP2
BRFSS15$HADPAP2<-ifelse(BRFSS15$HADPAP2==1,"Yes",
                       ifelse(BRFSS15$HADPAP2==2,"No",""))


#HADSGCO1
BRFSS15$HADSGCO1<-ifelse(BRFSS15$HADSGCO1==1,"Sigmoidal",
                        ifelse(BRFSS15$HADSGCO1==2,"Colonoscopy",""))

#HADSIGM3
BRFSS15$HADSIGM3<-ifelse(BRFSS15$HADSIGM3==1,"Yes",
                        ifelse(BRFSS15$HADSIGM3==2,"No",""))

#HAVARTH3
BRFSS15$HAVARTH3<-ifelse(BRFSS15$HAVARTH3==1,"Yes",
                        ifelse(BRFSS15$HAVARTH3==2,"No",""))

#HEIGHT3
BRFSS15$HEIGHT3<-ifelse(BRFSS15$HEIGHT3<=800,((as.numeric(substr(BRFSS15$HEIGHT3,1,1))*12) +  as.numeric(substr(BRFSS15$HEIGHT3,2,3)))*2.54,
                       ifelse(BRFSS15$HEIGHT3 %in% c(9000:9998),substr(BRFSS15$HEIGHT3,2,4),""))

#HIVTST6
BRFSS15$HIVTST6<-ifelse(BRFSS15$HIVTST6==1,"Yes",
                       ifelse(BRFSS15$HIVTST6==2,"No",""))

#HIVTSTD3
BRFSS15$HIVTSTD3-ifelse(BRFSS15$HIVTSTD3<777777,paste(substr(BRFSS15$HIVTSTD3, 1, ifelse(nchar(BRFSS15$HIVTSTD3)==6,2,1)),"-",substr(BRFSS15$HIVTSTD3, ifelse(nchar(BRFSS15$HIVTSTD3)==6,3,2), nchar(BRFSS15$HIVTSTD3))),"")

#HLTHPLN1
BRFSS15$HLTHPLN1<- ifelse(BRFSS15$HLTHPLN1==1,"Yes",
                         ifelse(BRFSS15$HLTHPLN1==2,"No",""))


#HOWLONG
BRFSS15$HOWLONG<- ifelse(BRFSS15$HOWLONG==1,"within 1 year",
                        ifelse(BRFSS15$HOWLONG==2,"1-2 years",
                               ifelse(BRFSS15$HOWLONG==2,"2-3 years",
                                      ifelse(BRFSS15$HOWLONG==2,"3-5 years",
                                             ifelse(BRFSS15$HOWLONG==2,"before 5 years","")))))

#HPVADSHT
BRFSS15$HPVADSHT<- ifelse(BRFSS15$HPVADSHT==3,"all shots taken",
                         ifelse(BRFSS15$HPVADSHT<=2,paste(BRFSS15$HPVADSHT,"shots taken"),""))

#HPVADVC2
BRFSS15$HPVADVC2<- ifelse(BRFSS15$HPVADVC2==1,"Yes",
                         ifelse(BRFSS15$HPVADVC2==2,"No",""))

#HTIN4
BRFSS15$HTIN4<- NULL

#HTM4
BRFSS15$HTM4<- NULL

#IDATE
BRFSS15$IDATE<- NULL

#IMFVPLAC
BRFSS15$IMFVPLAC<-ifelse(BRFSS15$IMFVPLAC==1,"Doctor's office/Health Management Organization",
                        
                        ifelse(BRFSS15$IMFVPLAC==2,"A health department",
                               
                               ifelse(BRFSS15$IMFVPLAC==3,"Another type of clinic or health center",
                                      ifelse(BRFSS15$IMFVPLAC==4,"A senior,recreation or community center",
                                             ifelse(BRFSS15$IMFVPLAC==5,"A store",
                                                    ifelse(BRFSS15$IMFVPLAC==6,"A hospital(e.g. in patient)",
                                                           ifelse(BRFSS15$IMFVPLAC==7,"An emergency room",
                                                                  ifelse(BRFSS15$IMFVPLAC==8,"Workplace",
                                                                         ifelse(BRFSS15$IMFVPLAC==9,"Some other kind of place",
                                                                                ifelse(BRFSS15$IMFVPLAC==10,"Received vaccination in Canada/Mexico",
                                                                                       ifelse(BRFSS15$IMFVPLAC==11,"A school","")))))))))))






#INCOME2
BRFSS15$INCOME2<-ifelse(BRFSS15$INCOME2==1,"Less than 10,000",
                       ifelse(BRFSS15$INCOME2==2,"Less than 15,000",
                              ifelse(BRFSS15$INCOME2==3,"Less than 20,000",
                                     ifelse(BRFSS15$INCOME2==4,"Less than 25,000",
                                            ifelse(BRFSS15$INCOME2==5,"Less than 35,000",
                                                   ifelse(BRFSS15$INCOME2==6,"Less than 50,000",
                                                          ifelse(BRFSS15$INCOME2==7,"Less than 75,000",
                                                                 ifelse(BRFSS15$INCOME2==8,"75,000 or more",""))))))))


#INSULIN
BRFSS15$INSULIN<-ifelse(BRFSS15$INSULIN==1,"Yes",
                       ifelse(BRFSS15$INSULIN==2,"No",""))


#IYEAR
BRFSS15$IYEAR<-NULL


#LANDLINE
BRFSS15$LANDLINE<-ifelse(BRFSS15$LANDLINE==1,"Yes",
                        ifelse(BRFSS15$LANDLINE==2," No",""))

#LASTPAP2
BRFSS15$LASTPAP2<-ifelse(BRFSS15$LASTPAP2==1,"Within 1 year",
                        ifelse(BRFSS15$LASTPAP2==2,"1 -2 years",
                               ifelse(BRFSS15$LASTPAP2==3,"2-3 years",
                                      ifelse(BRFSS15$LASTPAP2==4,"3-5 years",
                                             ifelse(BRFSS15$LASTPAP2==5,"before 5 years","")))))


#LASTSIG3
BRFSS15$LASTSIG3<-ifelse(BRFSS15$LASTSIG3==1,"Within 1 year",
                        ifelse(BRFSS15$LASTSIG3==2,"1-2 years",
                               ifelse(BRFSS15$LASTSIG3==3,"2-3 years",
                                      ifelse(BRFSS15$LASTSIG3==4,"3-5 years",
                                             ifelse(BRFSS15$LASTSIG3==5,"5-10 years",
                                                    ifelse(BRFSS15$LASTSIG3==6,"before 10 years",""))))))

#LASTSMK2
BRFSS15$LASTSMK2<-ifelse(BRFSS15$LASTSMK2==1,"Everyday",
                        ifelse(BRFSS15$LASTSMK2==2,"Somedays",
                               ifelse(BRFSS15$LASTSMK2==3,"Not at all","")))


#LENGEXAM
BRFSS15$LENGEXAM<-ifelse(BRFSS15$LENGEXAM==1,"Within 1 year",
                        ifelse(BRFSS15$LENGEXAM==2,"1 -2 years",
                               ifelse(BRFSS15$LENGEXAM==3,"2-3 years",
                                      ifelse(BRFSS15$LENGEXAM==4,"3-5 years",
                                             ifelse(BRFSS15$LENGEXAM==5,"before 5 years","")))))


#LSATISFY
BRFSS15$LSATISFY<-ifelse(BRFSS15$LSATISFY==1,"Very satisfied",
                        ifelse(BRFSS15$LSATISFY==2,"satisfied",
                               ifelse(BRFSS15$LSATISFY==3,"Dissatisfied",
                                      ifelse(BRFSS15$LSATISFY==4,"Very dissatisfied",""))))


#LSTBLDS3
BRFSS15$LSTBLDS3<-ifelse(BRFSS15$LSTBLDS3==1,"Within 1 year",
                        ifelse(BRFSS15$LSTBLDS3==2,"1 -2 years",
                               ifelse(BRFSS15$LSTBLDS3==3,"2-3 years",
                                      ifelse(BRFSS15$LSTBLDS3==4,"3-5 years",
                                             ifelse(BRFSS15$LSTBLDS3==5,"before 5 years","")))))


#MARITAL
BRFSS15$MARITAL<-ifelse(BRFSS15$MARITAL==1,"Married",
                       ifelse(BRFSS15$MARITAL==2,"Divorced",
                              ifelse(BRFSS15$MARITAL==3,"Widowed",
                                     ifelse(BRFSS15$MARITAL==4,"Separated",
                                            ifelse(BRFSS15$MARITAL==5,"Never married",
                                                   ifelse(BRFSS15$MARITAL==6,"A member of an unmarried couple",""))))))



#MAXDRNKS
BRFSS15$MAXDRNKS<-ifelse(BRFSS15$MAXDRNKS<=76,BRFSS15$MAXDRNKS,"")


#MEDCOST
BRFSS15$MEDCOST<-ifelse(BRFSS15$MEDCOST==1,"Yes",
                       ifelse(BRFSS15$MEDCOST==2,"No",""))

#MENTHLTH
BRFSS15$MENTHLTH<-ifelse(BRFSS15$MENTHLTH<=30,BRFSS15$MENTHLTH,
                        ifelse(BRFSS15$MENTHLTH==88,0,""))


#MSCODE
BRFSS15$MSCODE<-ifelse(BRFSS15$MSCODE==1,"In city center",
                      ifelse(BRFSS15$MSCODE==2,"outside city center but inside county",
                             ifelse(BRFSS15$MSCODE==3,"Inside the suburban county",
                                    ifelse(BRFSS15$MSCODE==5,"Not in an MSA",""))))


#NUMADULT
BRFSS15$NUMADULT<-ifelse(BRFSS15$NUMADULT<=99,BRFSS15$NUMADULT,"")


#NUMHHOL2
BRFSS15$NUMHHOL2<-ifelse(BRFSS15$NUMADULT==1,"Yes",
                        ifelse(BRFSS15$NUMADULT==2,"No",""))

#NUMMEN
BRFSS15$NUMMEN<-ifelse(BRFSS15$NUMMEN<=99,BRFSS15$NUMMEN,"")


#NUMPHON2
BRFSS15$NUMPHON2<-NULL


#NUMWOMEN
BRFSS15$NUMWOMEN<-ifelse(BRFSS15$NUMWOMEN<=99,BRFSS15$NUMWOMEN,"")


#PAINACT2
BRFSS15$PAINACT2<-NULL


#PDIABTST
BRFSS15$PDIABTST<-ifelse(BRFSS15$PDIABTST==1,"Yes",
                        ifelse(BRFSS15$PDIABTST==2,"No",""))


#PERSDOC2
BRFSS15$PERSDOC2<-ifelse(BRFSS15$PERSDOC2==1,"YEes, only 1",
                        ifelse(BRFSS15$PERSDOC2==2,"More than 1",
                               ifelse(BRFSS15$PERSDOC2==3,"no","")))

#PHYSHLTH
BRFSS15$PHYSHLTH<-ifelse(BRFSS15$PHYSHLTH<=30,BRFSS15$PHYSHLTH,
                        ifelse(BRFSS15$PHYSHLTH==88,0,""))

#PNEUVAC3
BRFSS15$PNEUVAC3<-ifelse(BRFSS15$PNEUVAC3==1,"Yes",
                        ifelse(BRFSS15$PNEUVAC3==2,"No",""))


#POORHLTH
BRFSS15$POORHLTH<-ifelse(BRFSS15$POORHLTH<=30,BRFSS15$POORHLTH,
                        ifelse(BRFSS15$POORHLTH==88,0,""))


#PREDIAB1
BRFSS15$PREDIAB1<-ifelse(BRFSS15$PREDIAB1==1,"Yes",
                        ifelse(BRFSS15$PREDIAB1==2,"Yes,during pregnency",
                               ifelse(BRFSS15$PREDIAB1==3,"No","")))


#PREGNANT
BRFSS15$PREGNANT<-ifelse(BRFSS15$PREGNANT==1,"Yes",
                        ifelse(BRFSS15$PREGNANT==2,"No",""))


#PROFEXAM
BRFSS15$PROFEXAM<-ifelse(BRFSS15$PROFEXAM==1,"Yes",
                        ifelse(BRFSS15$PROFEXAM==2,"No",""))

#PSATEST1
BRFSS15$PSATEST1<-ifelse(BRFSS15$PSATEST1==1,"Yes",
                        ifelse(BRFSS15$PSATEST1==2,"No",""))


#PSATIME
BRFSS15$PSATIME<-ifelse(BRFSS15$PSATIME==1,"Within 1 year",
                       ifelse(BRFSS15$PSATIME==2,"1 -2 years",
                              ifelse(BRFSS15$PSATIME==3,"2-3 years",
                                     ifelse(BRFSS15$PSATIME==4,"3-5 years",
                                            ifelse(BRFSS15$PSATIME==5,"before 5 years","")))))


#PVTRESD2
BRFSS15$PVTRESD2<-ifelse(BRFSS15$PVTRESD2==1,"Yes",
                        ifelse(BRFSS15$PVTRESD2==2,"No",""))


#QLACTLM2
BRFSS15$QLACTLM2<-ifelse(BRFSS15$QLACTLM2==1,"Yes",
                        ifelse(BRFSS15$QLACTLM2==2,"No",""))


#QLHLTH2
BRFSS15$QLHLTH2<-NULL


#QLMENTL2
BRFSS15$QLMENTL2<-NULL


#QLSTRES2
BRFSS15$QLSTRES2<-NULL


#QSTLANG
BRFSS15$QSTLANG<-ifelse(BRFSS15$QSTLANG==1,"English",
                       ifelse(BRFSS15$QSTLANG==2,"Spanish",
                              ifelse(BRFSS15$QSTLANG>=3,"other","")))


#RCSGENDR
BRFSS15$RCSGENDR<-ifelse(BRFSS15$RCSGENDR==1,"Boy",
                        ifelse(BRFSS15$RCSGENDR==2,"Girl",""))


#RCSRLTN2
BRFSS15$RCSRLTN2<-ifelse(BRFSS15$RCSRLTN2==1,"Parent",
                        ifelse(BRFSS15$RCSRLTN2==2,"Grandparent",
                               ifelse(BRFSS15$RCSRLTN2==3,"Foster Parent or Guardian",
                                      ifelse(BRFSS15$RCSRLTN2==4,"Sibling",
                                             ifelse(BRFSS15$RCSRLTN2==5,"Other relative",
                                                    ifelse(BRFSS15$RCSRLTN2==6,"Not related in any way",
                                                           ifelse(BRFSS15$RCSRLTN2==7,"Don't know not sure","")))))))



#RENTHOM1
BRFSS15$RENTHOM1<-ifelse(BRFSS15$RENTHOM1==1,"Parent",
                        ifelse(BRFSS15$RENTHOM1==2,"Own",
                               ifelse(BRFSS15$RENTHOM1==3,"Rent",
                                      ifelse(BRFSS15$RENTHOM1==4,"Other Arrangement",""))))


#SCNTLPAD
BRFSS15$SCNTLPAD<-ifelse(BRFSS15$SCNTLPAD==1,"By Salary",
                        ifelse(BRFSS15$SCNTLPAD==2,"Paid by hour",
                               ifelse(BRFSS15$SCNTLPAD==3,"Paid by job/task(commission)",
                                      ifelse(BRFSS15$SCNTLPAD==4,"Paid some other way",""))))



#SCNTLWK1
BRFSS15$SCNTLWK1<- ifelse(BRFSS15$SCNTLWK1==98,0,
                         ifelse(BRFSS15$SCNTLWK1<=96,BRFSS15$SCNTLWK1,""))


#SCNTPAID
BRFSS15$SCNTPAID<-NULL


#SCNTWRK1
BRFSS15$SCNTWRK1<-NULL


#SEATBELT
BRFSS15$SEATBELT<-ifelse(BRFSS15$SEATBELT==1,"always",
                        ifelse(BRFSS15$SEATBELT==2,"nearly always",
                               ifelse(BRFSS15$SEATBELT==3,"sometimes",
                                      ifelse(BRFSS15$SEATBELT==4,"seldom",
                                             ifelse(BRFSS15$SEATBELT==5,"never",
                                                    ifelse(BRFSS15$SEATBELT==8,"Never drive or ride a car",""))))))


#SEQNO
BRFSS15$SEQNO<-NULL


#SEX
BRFSS15$SEX<-ifelse(BRFSS15$SEX==1,"Male",
                   ifelse(BRFSS15$SEX==2,"Female",""))



#SMOKDAY2
BRFSS15$SMOKDAY2<-ifelse(BRFSS15$SMOKDAY2==1,"Everyday",
                        ifelse(BRFSS15$SMOKDAY2==2,"Somedays",
                               ifelse(BRFSS15$SMOKDAY2==3,"Not at all","")))



#SMOKE100
BRFSS15$SMOKE100<-ifelse(BRFSS15$SMOKE100==1,"Yes",
                        ifelse(BRFSS15$SMOKE100==2,"No",""))



#STOPSMK2
BRFSS15$STOPSMK2<-ifelse(BRFSS15$STOPSMK2==1,"Yes",
                        ifelse(BRFSS15$STOPSMK2==2,"No",""))


#USEEQUIP
BRFSS15$USEEQUIP<-ifelse(BRFSS15$USEEQUIP==1,"Yes",
                        ifelse(BRFSS15$USEEQUIP==2,"No",""))


#USENOW3
BRFSS15$USENOW3<-ifelse(BRFSS15$USENOW3==1,"Everyday",
                       ifelse(BRFSS15$USENOW3==2,"Somedays",
                              ifelse(BRFSS15$USENOW3==3,"Not at all","")))



#VETERAN3
BRFSS15$VETERAN3<-ifelse(BRFSS15$VETERAN3==1,"Yes",
                        ifelse(BRFSS15$VETERAN3==2,"No",""))


#WEIGHT2
BRFSS15$WEIGHT2<-ifelse(BRFSS15$WEIGHT2<=1000,round((as.numeric(BRFSS15$WEIGHT2)*0.453592),digits=0),
                       ifelse(BRFSS15$WEIGHT2 %in% c(9000:9998),BRFSS15$WEIGHT2-9000,""))

#WTKG3
BRFSS15$WTKG3<-NULL


##########################################################################################

BRFSS15$HAVARTH3

BRFSS15_ARTHRITIS<- BRFSS15[BRFSS15$HAVARTH3!="",]

nrow(BRFSS15_ARTHRITIS)

nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$AGEG5YR=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$CHLDCNT=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$EDUCAG=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$HCVU651=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$INCOMG=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$PNEUMO2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$RFBING5=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$RFBMI5=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$RFHLTH=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$RFSMOK3=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$SMOKER3=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$STATE=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$TOTINDA=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$ADDEPEV2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$ALCDAY5=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$AVEDRNK2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$CADULT=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$CELLFON2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$CHCKIDNY=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$CPDEMO1=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$DISPCODE=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$EDUCA=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$EMTSUPRT=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$EXERANY2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$GENHLTH=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$HAVARTH3=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$HEIGHT3=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$HLTHPLN1=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$INCOME2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$LSATISFY=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$MARITAL=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$MAXDRNKS=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$MEDCOST=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$MSCODE=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$NUMADULT=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$NUMHHOL2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$NUMPHON2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$PERSDOC2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$PNEUVAC3=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$POORHLTH=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$PREGNANT=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$PROFEXAM=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$PVTRESD2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$QLACTLM2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$QSTLANG=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$RCSGENDR=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$RCSRLTN2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$RENTHOM1=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$SCNTLWK1=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$SEX=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$SMOKDAY2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$STOPSMK2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$USEEQUIP=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$USENOW3=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$VETERAN3=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$WEIGHT2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$ASTHMS1=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$BMI5=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$ASACTLIM=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$ASATTACK=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$ASDRVIST=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$ASINHALR=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$BLDSUGAR=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$CHCOCNCR=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$CHCSCNCR=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$CHECKUP1=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$CHILDREN=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$CHKHEMO3=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$CVDCRHD4=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$CVDINFR4=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$CVDSTRK3=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$DIABETE3=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$FLSHTMY2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$HIVTST6=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$INSULIN=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$LASTSMK2=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$PDIABTST=="",])
nrow(BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$PREDIAB1=="",])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$AGEG5YR),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$CHLDCNT),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$EDUCAG),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$HCVU651),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$INCOMG),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$PNEUMO2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$RFBING5),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$RFBMI5),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$RFHLTH),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$RFSMOK3),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$SMOKER3),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$STATE),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$TOTINDA),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$ADDEPEV2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$ALCDAY5),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$AVEDRNK2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$CADULT),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$CELLFON2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$CHCKIDNY),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$CPDEMO1),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$DISPCODE),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$EDUCA),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$EMTSUPRT),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$EXERANY2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$GENHLTH),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$HAVARTH3),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$HEIGHT3),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$HLTHPLN1),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$INCOME2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$LSATISFY),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$MARITAL),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$MAXDRNKS),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$MEDCOST),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$MSCODE),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$NUMADULT),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$NUMHHOL2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$NUMPHON2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$PERSDOC2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$PNEUVAC3),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$POORHLTH),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$PREGNANT),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$PROFEXAM),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$PVTRESD2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$QLACTLM2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$QSTLANG),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$RCSGENDR),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$RCSRLTN2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$RENTHOM1),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$SCNTLWK1),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$SEX),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$SMOKDAY2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$STOPSMK2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$USEEQUIP),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$USENOW3),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$VETERAN3),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$WEIGHT2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$ASTHMS1),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$BMI5),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$ASACTLIM),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$ASATTACK),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$ASDRVIST),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$ASINHALR),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$BLDSUGAR),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$CHCOCNCR),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$CHCSCNCR),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$CHECKUP1),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$CHILDREN),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$CHKHEMO3),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$CVDCRHD4),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$CVDINFR4),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$CVDSTRK3),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$DIABETE3),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$FLSHTMY2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$HIVTST6),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$INSULIN),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$LASTSMK2),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$PDIABTST),])
nrow(BRFSS15_ARTHRITIS[is.na(BRFSS15_ARTHRITIS$PREDIAB1),])


#########################################################################################


x15<-BRFSS15_ARTHRITIS[BRFSS15_ARTHRITIS$AGEG5YR!="",]
x15<-x15[x15$CHLDCNT!="",]
x15<-x15[x15$EDUCAG!="",]
x15<-x15[x15$RFBING5!="",]
x15<-x15[x15$RFHLTH!="",]
x15<-x15[x15$SMOKER3!="",]
x15<-x15[x15$TOTINDA!="",]
x15<-x15[x15$ADDEPEV2!="",]
x15<-x15[x15$ALCDAY5!="",]
x15<-x15[x15$CHCKIDNY!="",]
x15<-x15[x15$EDUCA!="",]
x15<-x15[x15$EXERANY2!="",]
x15<-x15[x15$GENHLTH!="",]
x15<-x15[x15$HAVARTH3!="",]
x15<-x15[x15$HEIGHT3!="",]
x15<-x15[x15$HLTHPLN1!="",]
x15<-x15[x15$MARITAL!="",]
x15<-x15[x15$MEDCOST!="",]
x15<-x15[x15$PERSDOC2!="",]
x15<-x15[x15$PNEUVAC3!="",]
x15<-x15[x15$PHYSHLTH!="",]
x15<-x15[x15$QLACTLM2!="",]
x15<-x15[x15$QSTLANG!="",]
x15<-x15[x15$SEX!="",]
x15<-x15[x15$VETERAN3!="",]
x15<-x15[x15$WEIGHT2!="",]
x15<-x15[x15$ASTHMS1!="",]
x15<-x15[x15$BMI5!="",]
x15<-x15[x15$CHCOCNCR!="",]
x15<-x15[x15$CVDSTRK3!="",]
x15<-x15[x15$DIABETE3!="",]
x15<-x15[x15$HCVU651!="",]
x15<-x15[x15$INCOME2!="",]
x15<-x15[x15$STATE!="",]
x15<-x15[x15$USEEQUIP!="",]
x15<-x15[x15$USENOW3!="",]
x15<-x15[x15$CHECKUP1!="",]
x15<-x15[x15$CHILDREN!="",]
x15<-x15[x15$CASTHDX2!="",]



a15<-x15[, c(	"AGEG5YR",	"CHLDCNT",	"EDUCAG",	"RFBING5",	"RFHLTH",	"SMOKER3",	"TOTINDA",	"ADDEPEV2",	"ALCDAY5",	"CHCKIDNY",	"EDUCA",	"EXERANY2",	"GENHLTH",	"HAVARTH3",	"HEIGHT3",	"HLTHPLN1",	"MARITAL",	"MEDCOST",	"PERSDOC2",	"PNEUVAC3",	"PHYSHLTH",	"QLACTLM2",	"QSTLANG",	"SEX",	"VETERAN3",	"WEIGHT2",	"ASTHMS1",	"BMI5",	"CHCOCNCR",	"CVDSTRK3",	"DIABETE3",	"HCVU651",	"INCOME2",	"STATE",	"USEEQUIP",	"USENOW3",	"CHECKUP1",	"CHILDREN","CASTHDX2")]
dim(a15)
x15<-a15
BRFSS15_ARTHRITIS<-x15

###########################################################################################
library(readxl)
library(Hmisc)
library(rpart)
library(rpart.plot)
library(plyr)

BRFSS_ARTHRITIS<-rbind(BRFSS11_ARTHRITIS,BRFSS12_ARTHRITIS,BRFSS13_ARTHRITIS,BRFSS14_ARTHRITIS,BRFSS15_ARTHRITIS)
BRFSS_ARTHRITIS$ALCDAY5<-as.numeric(BRFSS_ARTHRITIS$ALCDAY5)
BRFSS_ARTHRITIS$WEIGHT2<-as.numeric(BRFSS_ARTHRITIS$WEIGHT2)
BRFSS_ARTHRITIS$HEIGHT3<-round(as.numeric(BRFSS_ARTHRITIS$HEIGHT3),digits=0)
BRFSS_ARTHRITIS$PHYSHLTH<-as.numeric(BRFSS_ARTHRITIS$PHYSHLTH)
BRFSS_ARTHRITIS$BMI5<-as.numeric(BRFSS_ARTHRITIS$BMI5)
BRFSS_ARTHRITIS$CHILDREN<-as.numeric(BRFSS_ARTHRITIS$CHILDREN)

###############

xBRFSS<-BRFSS_ARTHRITIS[!(row.names(BRFSS_ARTHRITIS) %in% row.names(BRFSS_ARTHRITIS[is.na(BRFSS_ARTHRITIS$HAVARTH3),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$AGEG5YR),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$CHLDCNT),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$EDUCAG),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$RFBING5),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$RFHLTH),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$SMOKER3),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$TOTINDA),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$ADDEPEV2),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$ALCDAY5),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$CHCKIDNY),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$EDUCA),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$EXERANY2),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$GENHLTH),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$HAVARTH3),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$HEIGHT3),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$HLTHPLN1),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$MARITAL),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$MEDCOST),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$PERSDOC2),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$PNEUVAC3),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$PHYSHLTH),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$QLACTLM2),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$QSTLANG),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$SEX),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$VETERAN3),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$WEIGHT2),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$ASTHMS1),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$BMI5),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$CHCOCNCR),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$CVDSTRK3),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$DIABETE3),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$HCVU651),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$INCOME2),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$STATE),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$USEEQUIP),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$USENOW3),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$CHECKUP1),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$CHILDREN),])),]
xBRFSS<-xBRFSS[!(row.names(xBRFSS) %in% row.names(xBRFSS[is.na(xBRFSS$CASTHDX2),])),]



BRFSS_ARTHRITIS<-xBRFSS
BRFSS_ARTHRITIS$ASTHMS1<- ifelse(BRFSS_ARTHRITIS$ASTHMS1=="current","Yes",
                        ifelse(BRFSS_ARTHRITIS$ASTHMS1=="former","Yes","No"))

dim(BRFSS_ARTHRITIS)
set.seed(888)
#Randomize the data and give the probabilities of partitions to specify the partition size 
ind<- sample(2, nrow(BRFSS_ARTHRITIS),
             replace=TRUE,
             prob=c(0.6,0.4))

BRFSS_ASTHAMA_train<- BRFSS_ARTHRITIS[ind==1, ]
dim(BRFSS_ASTHAMA_train)

BRFSS_ASTHAMA_test<- BRFSS_ARTHRITIS[ind==2, ]
dim(BRFSS_ASTHAMA_test)

options("scipen"=100, "digits"=5)

#Proportion of indivisuals who purchased organic products
print(nrow(BRFSS_ARTHRITIS[BRFSS_ARTHRITIS$ASTHMS1=="Yes",])/nrow(BRFSS_ARTHRITIS))
#In percentage
print((nrow(BRFSS_ARTHRITIS[BRFSS_ARTHRITIS$ASTHMS1=="Yes",])*100)/nrow(BRFSS_ARTHRITIS))


#Build decision tree
BRFSS_ASTHAMA_tree <- rpart(ASTHMS1 ~ ., data = BRFSS_ASTHAMA_train, method = "class", control = rpart.control(cp = 0.000001, maxdepth=6))

BRFSS_ASTHAMA_train$pred<-predict(BRFSS_ASTHAMA_tree,BRFSS_ASTHAMA_train,type="class")



library(caret)
library(e1071)

############################################################################




#############################################################################

### Test Model Performance on train data - Creates a 2X2 confusion matrix and associated metrics
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
  }}
  
  Performancetrain <- testModelPerformance(BRFSS_ASTHAMA_tree, BRFSS_ASTHAMA_train, BRFSS_ASTHAMA_train$ASTHMS1)
  
  writeLines("PERFORMANCE EVALUATION FOR")
  writeLines(paste("Model:", deparse(substitute(BRFSS_ASTHAMA_tree))))
  writeLines(paste("Target:", deparse(substitute(BRFSS_ASTHAMA_train))))
  
  writeLines("\n\nConfusion Matrix:")
  confMatrix <- table(Actual = BRFSS_ASTHAMA_train$ASTHMS1, Predicted = BRFSS_ASTHAMA_train$pred)
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
  writeLines(paste("False Positive Rate:", round(falsePosRate, digits = 4)))
  writeLines(paste("False Negative Rate:", round(falseNegRate, digits = 4)))
  writeLines(paste("Precision:", round(precision, digits = 4)))
  
  ###Test model performance on test data
  BRFSS_ASTHAMA_test$pred<-predict(BRFSS_ASTHAMA_tree,BRFSS_ASTHAMA_test,type="class")
  Performancetest <- testModelPerformance(BRFSS_ASTHAMA_tree, BRFSS_ASTHAMA_test, BRFSS_ASTHAMA_test$ASTHMS1)
  
  writeLines("PERFORMANCE EVALUATION FOR")
  writeLines(paste("Model:", deparse(substitute(BRFSS_ASTHAMA_tree))))
  writeLines(paste("Target:", deparse(substitute(BRFSS_ASTHAMA_test))))
  
  writeLines("\n\nConfusion Matrix:")
  confMatrix <- table(Actual = BRFSS_ASTHAMA_test$ASTHMS1, Predicted = BRFSS_ASTHAMA_test$pred)
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
  writeLines(paste("False Positive Rate:", round(falsePosRate, digits = 4)))
  writeLines(paste("False Negative Rate:", round(falseNegRate, digits = 4)))
  writeLines(paste("Precision:", round(precision, digits = 4)))
  
  
  #Compare performance on testing and training data
  
  BRFSS_ASTHAMA_train$correct <- BRFSS_ASTHAMA_train$ASTHMS1 == BRFSS_ASTHAMA_train$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
  traincorrectcount <- length(which(BRFSS_ASTHAMA_train$correct))
  trainincorrectcount <- nrow(BRFSS_ASTHAMA_train) - traincorrectcount
  trainerrorrate <- trainincorrectcount/nrow(BRFSS_ASTHAMA_train)
  trainaccuracy <- 1-trainerrorrate
  
  BRFSS_ASTHAMA_test$correct <- BRFSS_ASTHAMA_test$ASTHMS1 == BRFSS_ASTHAMA_test$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
  testcorrectcount <- length(which(BRFSS_ASTHAMA_test$correct))
  testincorrectcount <- nrow(BRFSS_ASTHAMA_test) - testcorrectcount
  testerrorrate <- testincorrectcount/nrow(BRFSS_ASTHAMA_test)
  testaccuracy <- 1-testerrorrate
  
  #Compare
  paste("TRAIN: Error Rate (", trainerrorrate, ") Accuracy (", trainaccuracy, ")")
  paste("TEST: Error Rate (", testerrorrate, ") Accuracy (", testaccuracy, ")")
  
  rpart.plot(BRFSS_ASTHAMA_tree)
  
  BRFSS_ASTHAMA_test$ASTHMS1<-ifelse(BRFSS_ASTHAMA_test$ASTHMS1=="Yes",1,2)
  BRFSS_ASTHAMA_test$pred<-ifelse(BRFSS_ASTHAMA_test$pred=="Yes",1,2)                      
 
 library(gains)
 #Evaluate model performance on test data
gain <- gains(BRFSS_ASTHAMA_test$ASTHMS1, BRFSS_ASTHAMA_test$pred, groups=length(BRFSS_ASTHAMA_test$pred))
 
  #plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(BRFSS_ASTHAMA_test$ASTHMS1))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(BRFSS_ASTHAMA_test$ASTHMS1))~c(0, dim(BRFSS_ASTHAMA_test)[1]), lty=2)
# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(BRFSS_ASTHAMA_test$ASTHMS1)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9),
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)
  