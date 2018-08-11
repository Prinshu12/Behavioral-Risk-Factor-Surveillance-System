setwd("C:/Users/oduwo/Desktop/BRFSS_Project")
library(rpart)
library(rpart.plot)
library(beepr)
#############################################################
# LOADING THE DATA

#BRFSS_2014<-read.csv(file.choose(),header=T)
BRFSS_2014<-read.csv("2014.csv")

Col_2014<- read.csv("Common columns_new1.csv")

row.names(Col_2014)<-Col_2014[,1]

BRFSS_2014<-BRFSS_2014[,noquote(ifelse(substr(noquote(colnames(BRFSS_2014)),1,2)=='X_',substr(noquote(colnames(BRFSS_2014)),2,nchar(noquote(colnames(BRFSS_2014)))),noquote(colnames(BRFSS_2014)))) %in% noquote(row.names(Col_2014))]

dim(BRFSS_2014)

summary(BRFSS_2014)

colnames(BRFSS_2014)<- ifelse(substr(noquote(colnames(BRFSS_2014)),1,2)=='X_',substr(noquote(colnames(BRFSS_2014)),3,nchar(noquote(colnames(BRFSS_2014)))),noquote(colnames(BRFSS_2014)))

colnames(BRFSS_2014)

BRFSS_2014<-BRFSS_2014[ , order(colnames(BRFSS_2014))]

head(BRFSS_2014)

#####################################

BRFSS_2013<-read.csv("2013.csv")
#BRFSS_2013<-read.csv(file.choose(),header=T)

BRFSS_2013<-BRFSS_2013[,noquote(ifelse(substr(noquote(colnames(BRFSS_2013)),1,2)=='X_',substr(noquote(colnames(BRFSS_2013)),2,nchar(noquote(colnames(BRFSS_2013)))),noquote(colnames(BRFSS_2013)))) %in% noquote(row.names(Col_2014))]

dim(BRFSS_2013)

summary(BRFSS_2013)

colnames(BRFSS_2013)<- ifelse(substr(noquote(colnames(BRFSS_2013)),1,2)=='X_',substr(noquote(colnames(BRFSS_2013)),3,nchar(noquote(colnames(BRFSS_2013)))),noquote(colnames(BRFSS_2013)))

colnames(BRFSS_2013)

BRFSS_2013<-BRFSS_2013[ , order(colnames(BRFSS_2013))]

head(BRFSS_2013)


#####################################

BRFSS_2012<-read.csv("2012.csv")
#BRFSS_2012<-read.csv(file.choose(),header=T)

BRFSS_2012<-BRFSS_2012[,noquote(ifelse(substr(noquote(colnames(BRFSS_2012)),1,2)=='X_',substr(noquote(colnames(BRFSS_2012)),2,nchar(noquote(colnames(BRFSS_2012)))),noquote(colnames(BRFSS_2012)))) %in% noquote(row.names(Col_2014))]

dim(BRFSS_2012)

summary(BRFSS_2012)

colnames(BRFSS_2012)<- ifelse(substr(noquote(colnames(BRFSS_2012)),1,2)=='X_',substr(noquote(colnames(BRFSS_2012)),3,nchar(noquote(colnames(BRFSS_2012)))),noquote(colnames(BRFSS_2012)))

colnames(BRFSS_2012)

BRFSS_2012<-BRFSS_2012[ , order(colnames(BRFSS_2012))]

head(BRFSS_2012)

#####################################

BRFSS_2011<-read.csv("2011.csv")
#BRFSS_2011<-read.csv(file.choose(),header=T)

BRFSS_2011<-BRFSS_2011[,noquote(ifelse(substr(noquote(colnames(BRFSS_2011)),1,2)=='X_',substr(noquote(colnames(BRFSS_2011)),2,nchar(noquote(colnames(BRFSS_2011)))),noquote(colnames(BRFSS_2011)))) %in% noquote(row.names(Col_2014))]

dim(BRFSS_2011)

summary(BRFSS_2011)

colnames(BRFSS_2011)<- ifelse(substr(noquote(colnames(BRFSS_2011)),1,2)=='X_',substr(noquote(colnames(BRFSS_2011)),3,nchar(noquote(colnames(BRFSS_2011)))),noquote(colnames(BRFSS_2011)))

colnames(BRFSS_2011)

BRFSS_2011<-BRFSS_2011[ , order(colnames(BRFSS_2011))]

head(BRFSS_2011)

####################################

BRFSS_2015<-read.csv("2015.csv")
#BRFSS_2015<-read.csv(file.choose(),header=T)

BRFSS_2015<-BRFSS_2015[,noquote(ifelse(substr(noquote(colnames(BRFSS_2015)),1,2)=='X_',substr(noquote(colnames(BRFSS_2015)),2,nchar(noquote(colnames(BRFSS_2015)))),noquote(colnames(BRFSS_2015)))) %in% noquote(row.names(Col_2014))]

dim(BRFSS_2015)

summary(BRFSS_2015)

colnames(BRFSS_2015)<- ifelse(substr(noquote(colnames(BRFSS_2015)),1,2)=='X_',substr(noquote(colnames(BRFSS_2015)),3,nchar(noquote(colnames(BRFSS_2015)))),noquote(colnames(BRFSS_2015)))

colnames(BRFSS_2015)

BRFSS_2015<-BRFSS_2015[ , order(colnames(BRFSS_2015))]

head(BRFSS_2015)


# Picking relvant columns for analysis
HeartColumns<-read.csv("HeartColumns.csv")

column.names<-colnames(HeartColumns)

newHeart_11.df <- BRFSS_2011[colnames(BRFSS_2011) %in% column.names]
newHeart_12.df <- BRFSS_2012[colnames(BRFSS_2012) %in% column.names]
newHeart_13.df <- BRFSS_2013[colnames(BRFSS_2013) %in% column.names]
newHeart_14.df <- BRFSS_2014[colnames(BRFSS_2014) %in% column.names]
newHeart_15.df <- BRFSS_2015[colnames(BRFSS_2015) %in% column.names]

## Checking Dimensions
dim(newHeart_11.df)
dim(newHeart_12.df)
dim(newHeart_13.df)
dim(newHeart_14.df)
dim(newHeart_15.df)


newHeart_11.df[newHeart_11.df==""]<-NA
newHeart_12.df[newHeart_12.df==""]<-NA
newHeart_13.df[newHeart_13.df==""]<-NA
newHeart_14.df[newHeart_14.df==""]<-NA
newHeart_15.df[newHeart_15.df==""]<-NA

# REMOVING NA RECORDS

x14<-newHeart_11.df[!(row.names(newHeart_11.df) %in% row.names(newHeart_11.df[is.na(newHeart_11.df$ADDEPEV2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AGEG5YR),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AIDTST3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$ALCDAY5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$ASTHMA3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AVEDRNK2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHCKIDNY),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHCOCNCR),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHILDREN),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHLDCNT),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CVDINFR4),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CVDSTRK3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$DIABETE3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$DRDXAR1),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EDUCA),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EDUCAG),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EXERANY2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$HAVARTH3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$HLTHPLN1),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$INCOME2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$INCOMG),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$MARITAL),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$MENTHLTH),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PERSDOC2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PHYSHLTH),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PNEUVAC3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFBING5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFBMI5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFSMOK3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$SEX),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$TOTINDA),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$USEEQUIP),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$USENOW3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$VETERAN3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$WEIGHT2),])),]

newHeart_11.df<- x14

#######
x14<-newHeart_12.df[!(row.names(newHeart_12.df) %in% row.names(newHeart_12.df[is.na(newHeart_12.df$ADDEPEV2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AGEG5YR),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AIDTST3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$ALCDAY5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$ASTHMA3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AVEDRNK2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHCKIDNY),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHCOCNCR),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHILDREN),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHLDCNT),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CVDINFR4),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CVDSTRK3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$DIABETE3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$DRDXAR1),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EDUCA),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EDUCAG),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EXERANY2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$HAVARTH3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$HLTHPLN1),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$INCOME2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$INCOMG),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$MARITAL),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$MENTHLTH),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PERSDOC2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PHYSHLTH),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PNEUVAC3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFBING5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFBMI5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFSMOK3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$SEX),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$TOTINDA),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$USEEQUIP),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$USENOW3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$VETERAN3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$WEIGHT2),])),]

newHeart_12.df<- x14
###############################
x14<-newHeart_13.df[!(row.names(newHeart_13.df) %in% row.names(newHeart_13.df[is.na(newHeart_13.df$ADDEPEV2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AGEG5YR),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AIDTST3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$ALCDAY5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$ASTHMA3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AVEDRNK2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHCKIDNY),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHCOCNCR),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHILDREN),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHLDCNT),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CVDINFR4),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CVDSTRK3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$DIABETE3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$DRDXAR1),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EDUCA),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EDUCAG),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EXERANY2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$HAVARTH3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$HLTHPLN1),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$INCOME2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$INCOMG),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$MARITAL),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$MENTHLTH),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PERSDOC2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PHYSHLTH),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PNEUVAC3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFBING5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFBMI5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFSMOK3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$SEX),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$TOTINDA),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$USEEQUIP),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$USENOW3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$VETERAN3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$WEIGHT2),])),]

newHeart_13.df<- x14
#############################
x14<-newHeart_14.df[!(row.names(newHeart_14.df) %in% row.names(newHeart_14.df[is.na(newHeart_14.df$ADDEPEV2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AGEG5YR),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AIDTST3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$ALCDAY5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$ASTHMA3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AVEDRNK2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHCKIDNY),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHCOCNCR),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHILDREN),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHLDCNT),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CVDINFR4),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CVDSTRK3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$DIABETE3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$DRDXAR1),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EDUCA),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EDUCAG),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EXERANY2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$HAVARTH3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$HLTHPLN1),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$INCOME2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$INCOMG),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$MARITAL),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$MENTHLTH),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PERSDOC2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PHYSHLTH),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PNEUVAC3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFBING5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFBMI5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFSMOK3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$SEX),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$TOTINDA),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$USEEQUIP),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$USENOW3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$VETERAN3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$WEIGHT2),])),]

newHeart_14.df<- x14
################################
x14<-newHeart_15.df[!(row.names(newHeart_15.df) %in% row.names(newHeart_15.df[is.na(newHeart_15.df$ADDEPEV2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AGEG5YR),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AIDTST3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$ALCDAY5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$ASTHMA3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$AVEDRNK2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHCKIDNY),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHCOCNCR),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHILDREN),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CHLDCNT),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CVDINFR4),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$CVDSTRK3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$DIABETE3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$DRDXAR1),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EDUCA),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EDUCAG),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$EXERANY2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$HAVARTH3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$HLTHPLN1),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$INCOME2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$INCOMG),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$MARITAL),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$MENTHLTH),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PERSDOC2),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PHYSHLTH),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$PNEUVAC3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFBING5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFBMI5),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$RFSMOK3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$SEX),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$TOTINDA),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$USEEQUIP),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$USENOW3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$VETERAN3),])),]
x14<-x14[!(row.names(x14) %in% row.names(x14[is.na(x14$WEIGHT2),])),]

newHeart_15.df<- x14


#Combining Columns

Heart.df <- rbind(newHeart_11.df,newHeart_12.df,newHeart_13.df,newHeart_14.df,newHeart_15.df)

dim(Heart.df)
#Cleaning the data
testheart.df<-Heart.df

#AGE65YR
testheart.df$AGE65YR<- ifelse(testheart.df$AGE65YR==1,"Yes",
                              ifelse(testheart.df$AGE65YR==2,"No",""))

#ASTHMS1
testheart.df$ASTHMS1<- ifelse(testheart.df$ASTHMS1==1,"current",
                              ifelse(testheart.df$ASTHMS1==2,"former",
                                     ifelse(testheart.df$ASTHMS1==3,"never","")))

#BMI5
testheart.df$BMI5<- testheart.df$BMI5/100

#CASTHM1
testheart.df$CASTHM1<- ifelse(testheart.df$CASTHM1==1,"No",
                              ifelse(testheart.df$CASTHM1==2,"Yes",""))

#CHLDCNT
testheart.df$CHLDCNT<- ifelse(testheart.df$CHLDCNT==1,"no child",
                              ifelse(testheart.df$CHLDCNT==2,"1",
                                     ifelse(testheart.df$CHLDCNT==3,"2",
                                            ifelse(testheart.df$CHLDCNT==4,"3",
                                                   ifelse(testheart.df$CHLDCNT==5,"4",
                                                          ifelse(testheart.df$CHLDCNT==6,"5",""))))))

#CLLCPWT
testheart.df$CLLCPWT<- NULL

#DRDXAR1
testheart.df$DRDXAR1<- ifelse(testheart.df$DRDXAR1==1,"Yes",
                              ifelse(testheart.df$DRDXAR1==2,"No",""))

#EDUCAG
testheart.df$EDUCAG<- ifelse(testheart.df$EDUCAG==1,"no High School",
                             ifelse(testheart.df$EDUCAG==2,"High School Grad",
                                    ifelse(testheart.df$EDUCAG==3,"Attended College","College Grad")))

#HCVU651
testheart.df$HCVU651<- ifelse(testheart.df$HCVU651==1,"Yes",
                              ifelse(testheart.df$HCVU651==2,"No",""))

#INCOMG
testheart.df$INCOMG<- ifelse(testheart.df$INCOMG==1,"less than $15,000",
                             ifelse(testheart.df$INCOMG==2,"$15,000 to less than $25,000",
                                    ifelse(testheart.df$INCOMG==3,"$25,000 to less than $35,000",
                                           ifelse(testheart.df$INCOMG==4,"$35,000 to less than $50,000",
                                                  ifelse(testheart.df$INCOMG==5,"greater than $50,000","")))))

#LLCPWT
testheart.df$LLCPWT<- NULL

#LTASTH1
testheart.df$LTASTH1<- ifelse(testheart.df$LTASTH1==1,"No",
                              ifelse(testheart.df$LTASTH1==2,"Yes",""))

#PNEUMO2
testheart.df$PNEUMO2<- ifelse(testheart.df$PNEUMO2==1,"Yes",
                              ifelse(testheart.df$PNEUMO2==2,"No",""))

#PSU
testheart.df$PSU<- NULL

#PNEUMO2
testheart.df$PNEUMO2<- NULL

#RFBING5
testheart.df$RFBING5<- ifelse(testheart.df$RFBING5==1,"No",
                              ifelse(testheart.df$RFBING5==2,"Yes",""))

#RFBMI5
testheart.df$RFBMI5<- ifelse(testheart.df$RFBMI5==1,"BMI =<25",
                             ifelse(testheart.df$RFBMI5==2,"BMI >25",""))

#RFHLTH
testheart.df$RFHLTH<- ifelse(testheart.df$RFHLTH==1,"good or better health",
                             ifelse(testheart.df$RFHLTH==2,"fair or poor helath",""))

#RFSEAT2
testheart.df$RFSEAT2<- ifelse(testheart.df$RFSEAT2==1,"always wear seat belt",
                              ifelse(testheart.df$RFSEAT2==2,"seldom or never",""))

#RFSEAT3
testheart.df$RFSEAT3<- NULL

#RFSMOK3
testheart.df$RFSMOK3<- ifelse(testheart.df$RFSMOK3==1,"No",
                              ifelse(testheart.df$RFSMOK3==2,"Yes",""))

#SMOKER3
testheart.df$SMOKER3<- ifelse(testheart.df$SMOKER3==1,"current/smokes everyday",
                              ifelse(testheart.df$SMOKER3==2,"current/smokes somedays",
                                     ifelse(testheart.df$SMOKER3==3,"former",
                                            ifelse(testheart.df$SMOKER3==4,"never",""))))

#STATE
testheart.df$STATE<- ifelse(testheart.df$STATE==1,"Alabama",
                            ifelse(testheart.df$STATE==2,"Alaska",
                                   ifelse(testheart.df$STATE==4,"Arizona",
                                          ifelse(testheart.df$STATE==5,"Arkansas",
                                                 ifelse(testheart.df$STATE==6,"California",
                                                        ifelse(testheart.df$STATE==8,"Colorado",
                                                               ifelse(testheart.df$STATE==9,"Connecticut",
                                                                      ifelse(testheart.df$STATE==10,"Delaware",
                                                                             ifelse(testheart.df$STATE==11,"District of Columbia",
                                                                                    ifelse(testheart.df$STATE==12,"Florida",
                                                                                           ifelse(testheart.df$STATE==13,"Georgia",
                                                                                                  ifelse(testheart.df$STATE==15,"Hawaii",
                                                                                                         ifelse(testheart.df$STATE==16,"Idaho",
                                                                                                                ifelse(testheart.df$STATE==17,"Illinois",
                                                                                                                       ifelse(testheart.df$STATE==18,"Indiana",
                                                                                                                              ifelse(testheart.df$STATE==19,"Iowa",
                                                                                                                                     ifelse(testheart.df$STATE==20,"Kansas",
                                                                                                                                            ifelse(testheart.df$STATE==21,"Kentucky",
                                                                                                                                                   ifelse(testheart.df$STATE==22,"Louisiana",
                                                                                                                                                          ifelse(testheart.df$STATE==23,"Maine",
                                                                                                                                                                 ifelse(testheart.df$STATE==24,"Maryland",
                                                                                                                                                                        ifelse(testheart.df$STATE==25,"Massachusetts",
                                                                                                                                                                               ifelse(testheart.df$STATE==26,"Michigan",
                                                                                                                                                                                      ifelse(testheart.df$STATE==27,"Minnesota",
                                                                                                                                                                                             ifelse(testheart.df$STATE==28,"Mississippi",
                                                                                                                                                                                                    ifelse(testheart.df$STATE==29,"Missouri",
                                                                                                                                                                                                           ifelse(testheart.df$STATE==30,"Montana",
                                                                                                                                                                                                                  ifelse(testheart.df$STATE==31,"Nebraska",
                                                                                                                                                                                                                         ifelse(testheart.df$STATE==32,"Nevada",
                                                                                                                                                                                                                                ifelse(testheart.df$STATE==33,"New Hampshire",
                                                                                                                                                                                                                                       ifelse(testheart.df$STATE==34,"New Jersey",
                                                                                                                                                                                                                                              ifelse(testheart.df$STATE==35,"New Mexico",
                                                                                                                                                                                                                                                     ifelse(testheart.df$STATE==36,"New York",
                                                                                                                                                                                                                                                            ifelse(testheart.df$STATE==37,"North Carolina",
                                                                                                                                                                                                                                                                   ifelse(testheart.df$STATE==38,"North Dakota",
                                                                                                                                                                                                                                                                          ifelse(testheart.df$STATE==39,"Ohio",
                                                                                                                                                                                                                                                                                 ifelse(testheart.df$STATE==40,"Oklahoma",
                                                                                                                                                                                                                                                                                        ifelse(testheart.df$STATE==41,"Oregon ",""
                                                                                                                                                                                                                                                                                        ))))))))))))))))))))))))))))))))))))))

#STRWT
testheart.df$STRWT<- NULL

#STSTR
testheart.df$STSTR<- NULL

#TOTINDA
testheart.df$TOTINDA<- ifelse(testheart.df$TOTINDA==1,"Yes",
                              ifelse(testheart.df$TOTINDA==2,"No",""))

#WT2RAKE
testheart.df$WT2RAKE<- NULL

#ADDEPEV2
testheart.df$ADDEPEV2<- ifelse(testheart.df$ADDEPEV2==1,"Yes",
                               ifelse(testheart.df$ADDEPEV2==2,"No",""))

#ALCDAY5 - Convert days per week to approx days per month considering 30 days in a month.
testheart.df$ALCDAY5<- ifelse(testheart.df$ALCDAY5 %in% c(100:199), round((testheart.df$ALCDAY5-100)*4.29,digits=0),
                              ifelse(testheart.df$ALCDAY5 %in% c(200:299),testheart.df$ALCDAY5-200,
                                     ifelse(testheart.df$ALCDAY5 ==888,0,"")))

#ASACTLIM
testheart.df$ASACTLIM<- ifelse(testheart.df$ASACTLIM<=365,testheart.df$ASACTLIM,
                               ifelse(testheart.df$ASACTLIM==888,0,""))

#ASATTACK
testheart.df$ASATTACK<- ifelse(testheart.df$ASATTACK==1,"Yes",
                               ifelse(testheart.df$ASATTACK==2,"No",""))

#ASDRVIST
testheart.df$ASDRVIST<- ifelse(testheart.df$ASDRVIST<=87,testheart.df$ASDRVIST,
                               ifelse(testheart.df$ASDRVIST==88,0,""))

#ASERVIST
testheart.df$ASERVIST<- ifelse(testheart.df$ASERVIST<=87,testheart.df$ASERVIST,
                               ifelse(testheart.df$ASERVIST==88,0,""))

#ASINHALR
testheart.df$ASINHALR<- ifelse(testheart.df$ASINHALR==1,"1-4",
                               ifelse(testheart.df$ASINHALR==2,"5-14",
                                      ifelse(testheart.df$ASINHALR==3,"15-29",
                                             ifelse(testheart.df$ASINHALR==4,"30-59",
                                                    ifelse(testheart.df$ASINHALR==5,"60-99",
                                                           ifelse(testheart.df$ASINHALR==6,">=100",
                                                                  ifelse(testheart.df$ASINHALR==8,"0","")))))))

#ASTHMA3
testheart.df$ASTHMA3<- ifelse(testheart.df$ASTHMA3==1,"Yes",
                              ifelse(testheart.df$ASTHMA3==2,"No",""))

#ASTHMAGE
testheart.df$ASTHMAGE<- ifelse(testheart.df$ASTHMAGE==97,"<=10",
                               ifelse(testheart.df$ASTHMAGE<97,testheart.df$ASTHMAGE,""))

#ASTHMED3
testheart.df$ASTHMED3<- ifelse(testheart.df$ASTHMED3==1,"1-14",
                               ifelse(testheart.df$ASTHMED3==2,"15-24",
                                      ifelse(testheart.df$ASTHMED3==3,"25-30",
                                             ifelse(testheart.df$ASTHMED3==8,"0",""))))

#CADULT
testheart.df$CADULT<- ifelse(testheart.df$CADULT==1,"Yes/Male",
                             ifelse(testheart.df$CADULT==2,"Yes/Female",""))

#CASTHDX2
testheart.df$CASTHDX2<- ifelse(testheart.df$CASTHDX2==1,"Yes",
                               ifelse(testheart.df$CASTHDX2==2,"No",""))

#CASTHNO2
testheart.df$CASTHNO2<- ifelse(testheart.df$CASTHNO2==1,"Yes",
                               ifelse(testheart.df$CASTHNO2==2,"No",""))

#CELLFON2
testheart.df$CELLFON2<- ifelse(testheart.df$CELLFON2==1,"Yes","")

#CHCKIDNY
testheart.df$CHCKIDNY<- ifelse(testheart.df$CHCKIDNY==1,"Yes",
                               ifelse(testheart.df$CHCKIDNY==2,"No",""))

#CHCOCNCR
testheart.df$CHCOCNCR<- ifelse(testheart.df$CHCOCNCR==1,"Yes",
                               ifelse(testheart.df$CHCOCNCR==2,"No",""))

#CHCSCNCR
testheart.df$CHCSCNCR<- ifelse(testheart.df$CHCOCNCR==1,"Yes",
                               ifelse(testheart.df$CHCOCNCR==2,"No",""))

#CHECKUP1
testheart.df$CHECKUP1<- ifelse(testheart.df$CHECKUP1==1,"within past year",
                               ifelse(testheart.df$CHECKUP1==2,"within past 2 years",
                                      ifelse(testheart.df$CHECKUP1==3,"within past 5 years",
                                             ifelse(testheart.df$CHECKUP1==4,"5 or more years before",
                                                    ifelse(testheart.df$CHECKUP1==8,"never","")))))

#CHILDREN
testheart.df$CHILDREN<- ifelse(testheart.df$CHILDREN<=87,testheart.df$CHILDREN,
                               ifelse(testheart.df$CHILDREN==88,"0",""))

#CHKHEMO3
testheart.df$CHKHEMO3<- ifelse(testheart.df$CHKHEMO3<=76,testheart.df$CHKHEMO3,
                               ifelse(testheart.df$CHKHEMO3==88,"0",
                                      ifelse(testheart.df$CHKHEMO3==98,"never heard of the test","")))

#CPDEMO1
testheart.df$CPDEMO1<- ifelse(testheart.df$CPDEMO1==1,"Yes",
                              ifelse(testheart.df$CPDEMO1==2,"No",""))

#CSTATE
testheart.df$CSTATE<- ifelse(testheart.df$CSTATE==1,"Yes",
                             ifelse(testheart.df$CSTATE==2,"No",""))

#CTELENUM
testheart.df$CTELENUM<- NULL

#CTELNUM1
testheart.df$CTELNUM1<- NULL

#CVDCRHD4
testheart.df$CVDCRHD4<- ifelse(testheart.df$CVDCRHD4==1,"Yes",
                               ifelse(testheart.df$CVDCRHD4==2,"No",""))

#CVDINFR4
testheart.df$CVDINFR4<- ifelse(testheart.df$CVDINFR4==1,"Yes",
                               ifelse(testheart.df$CVDINFR4==2,"No",""))

#CVDSTRK3
testheart.df$CVDSTRK3<- ifelse(testheart.df$CVDSTRK3==1,"Yes",
                               ifelse(testheart.df$CVDSTRK3==2,"No",""))

#DIABAGE2
testheart.df$DIABAGE2<- ifelse(testheart.df$DIABAGE2<=97,testheart.df$DIABAGE2,"")

#DIABEDU
testheart.df$DIABEDU<- ifelse(testheart.df$DIABEDU==1,"Yes",
                              ifelse(testheart.df$DIABEDU==2,"No",""))

#DIABETE3
testheart.df$DIABETE3<- ifelse(testheart.df$DIABETE3==1,"Yes",
                               ifelse(testheart.df$DIABETE3==2,"Yes/female during pregnancy",
                                      ifelse(testheart.df$DIABETE3==3,"No",
                                             ifelse(testheart.df$DIABETE3==2,"Borderline diabetes/No current diabetes",""))))

#DIABEYE
testheart.df$DIABEYE<- ifelse(testheart.df$DIABEYE==1,"Yes",
                              ifelse(testheart.df$DIABEYE==2,"No",""))

#DISPCODE
testheart.df$DISPCODE<- NULL

#DOCTDIAB
testheart.df$DOCTDIAB<- ifelse(testheart.df$DOCTDIAB==88,0,
                               ifelse(testheart.df$DOCTDIAB<=76,testheart.df$DOCTDIAB,""))

#DRNK3GE5
testheart.df$DRNK3GE5<- ifelse(testheart.df$DRNK3GE5==88,0,
                               ifelse(testheart.df$DRNK3GE5<=76,testheart.df$DRNK3GE5,""))

#DRNKANY5
testheart.df$DRNKANY5<- ifelse(testheart.df$DRNKANY5==1,"Yes",
                               ifelse(testheart.df$DRNKANY5==2,"No",""))

#DROCDY3_
testheart.df$DROCDY3_<- NULL

#EDUCA
testheart.df$EDUCA<-ifelse(testheart.df$EDUCA==1,"never attended school",
                           ifelse(testheart.df$EDUCA==2,"1-8 elementary",
                                  ifelse(testheart.df$EDUCA==3,"9-11 high school",
                                         ifelse(testheart.df$EDUCA==4,"grade 12 or higher",
                                                ifelse(testheart.df$EDUCA==5,"college year 1-3",
                                                       ifelse(testheart.df$EDUCA==6,"college year 4 or graduate",""))))))

#EMTSUPRT
testheart.df$EMTSUPRT<- ifelse(testheart.df$EMTSUPRT==1,"always",
                               ifelse(testheart.df$EMTSUPRT==2,"usually",
                                      ifelse(testheart.df$EMTSUPRT==3,"sometimes",
                                             ifelse(testheart.df$EMTSUPRT==4,"rarely",
                                                    ifelse(testheart.df$EMTSUPRT==5,"never","")))))

#EXERANY2
testheart.df$EXERANY2<- ifelse(testheart.df$EXERANY2==1,"Yes",
                               ifelse(testheart.df$EXERANY2==2,"No",""))

#EYEEXAM
testheart.df$EYEEXAM<- ifelse(testheart.df$EYEEXAM==1,"within past one month",
                              ifelse(testheart.df$EYEEXAM==2,"within past year",
                                     ifelse(testheart.df$EYEEXAM==3,"within past 2 years",
                                            ifelse(testheart.df$EYEEXAM==4,"2 or more years",
                                                   ifelse(testheart.df$EYEEXAM==8,"never","")))))

#FEETCHK
testheart.df$FEETCHK<- ifelse(testheart.df$FEETCHK==88,0,
                              ifelse(testheart.df$FEETCHK<=76,testheart.df$DOCTDIAB,""))

#FEETCHK2
testheart.df$FEETCHK2<- ifelse(testheart.df$FEETCHK2 %in% c(100:199), round((testheart.df$FEETCHK2-100)*365,digits=0),
                               ifelse(testheart.df$FEETCHK2 %in% c(200:299),round((testheart.df$FEETCHK2-200)*(365/7),digits=0),
                                      ifelse(testheart.df$FEETCHK2 %in% c(300:399),round((testheart.df$FEETCHK2-300)*12,digits=0),
                                             ifelse(testheart.df$FEETCHK2 %in% c(400:499),testheart.df$FEETCHK2,
                                                    ifelse(testheart.df$FEETCHK2 ==888,0,"")))))

#AGE_G
testheart.df$AGE_G<- ifelse(testheart.df$AGE_G ==1, "18-24",
                            ifelse(testheart.df$AGE_G ==2,"25-34",
                                   ifelse(testheart.df$AGE_G ==3,"35-44",
                                          ifelse(testheart.df$AGE_G ==4,"45-54",
                                                 ifelse(testheart.df$AGE_G ==5,"55-64",">=65")))))

#AGEG5YR
testheart.df$AGEG5YR<- ifelse(testheart.df$AGEG5YR ==1, "18-24",
                              ifelse(testheart.df$AGEG5YR ==2,"25-29",
                                     ifelse(testheart.df$AGEG5YR ==3,"30-34",
                                            ifelse(testheart.df$AGEG5YR ==4,"35-39",
                                                   ifelse(testheart.df$AGEG5YR ==5,"40-44",
                                                          ifelse(testheart.df$AGEG5YR ==6,"44-49",
                                                                 ifelse(testheart.df$AGEG5YR ==7,"50-54",
                                                                        ifelse(testheart.df$AGEG5YR ==8,"55-59",
                                                                               ifelse(testheart.df$AGEG5YR ==9,"60-64",
                                                                                      ifelse(testheart.df$AGEG5YR ==10,"65-69",
                                                                                             ifelse(testheart.df$AGEG5YR ==11,"70-74",
                                                                                                    ifelse(testheart.df$AGEG5YR ==12,"75-79",
                                                                                                           ifelse(testheart.df$AGEG5YR ==13,">=80","")))))))))))))

#AIDTST3
testheart.df$AIDTST3<- ifelse(testheart.df$AIDTST3==1,"Yes",
                              ifelse(testheart.df$AIDTST3==2,"No",""))

#BMI5CAT
testheart.df$BMI5CAT<- ifelse(testheart.df$BMI5CAT==1,"Underweight (BMI<18.50)",
                              ifelse(testheart.df$BMI5CAT==2,"Normal Weight (18.50<=BMI< 25.00)",
                                     ifelse(testheart.df$BMI5CAT==3,"Overweight (25.00<=BMI<30.00)",
                                            ifelse(testheart.df$BMI5CAT==4,"Obese (30.00<=BMI<99.99)",""))))

#ASNOSLEP
testheart.df$ASNOSLEP<- ifelse(testheart.df$ASNOSLEP ==1, "1-2",
                               ifelse(testheart.df$ASNOSLEP ==2,"3-4",
                                      ifelse(testheart.df$ASNOSLEP ==3,"5",
                                             ifelse(testheart.df$ASNOSLEP ==4,"6-10",
                                                    ifelse(testheart.df$ASNOSLEP ==5,">=10",
                                                           ifelse(testheart.df$ASNOSLEP ==8,"0",""))))))

#ASRCHKUP
testheart.df$ASRCHKUP<- ifelse(testheart.df$ASRCHKUP==88,0,
                               ifelse(testheart.df$ASRCHKUP<=86,testheart.df$ASRCHKUP,""))

#ASTHNOW
testheart.df$ASTHNOW<- ifelse(testheart.df$ASTHNOW==1,"Yes",
                              ifelse(testheart.df$ASTHNOW==2,"No",""))

#ASYMPTOM
testheart.df$ASYMPTOM<- ifelse(testheart.df$ASYMPTOM ==1, "less than 1 in a week",
                               ifelse(testheart.df$ASYMPTOM ==2,"1-2/week",
                                      ifelse(testheart.df$ASYMPTOM ==3,"3-6/week",
                                             ifelse(testheart.df$ASYMPTOM ==4,"everyday but not all the time",
                                                    ifelse(testheart.df$ASYMPTOM ==5,"everyday all the time",
                                                           ifelse(testheart.df$ASYMPTOM ==8,"never",""))))))

#AVEDRNK2
testheart.df$AVEDRNK2<- ifelse(testheart.df$AVEDRNK2<=76,testheart.df$ASRCHKUP,"")

#BLDSTOOL
testheart.df$BLDSTOOL<- ifelse(testheart.df$BLDSTOOL==1,"Yes",
                               ifelse(testheart.df$BLDSTOOL==2,"No",""))

#BLDSUGAR
testheart.df$BLDSUGAR<- ifelse(testheart.df$BLDSUGAR %in% c(100:199), round((testheart.df$BLDSUGAR-100)*365,digits=0),
                               ifelse(testheart.df$BLDSUGAR %in% c(200:299),round((testheart.df$BLDSUGAR-200)*(365/7),digits=0),
                                      ifelse(testheart.df$BLDSUGAR %in% c(300:399),round((testheart.df$BLDSUGAR-300)*12,digits=0),
                                             ifelse(testheart.df$BLDSUGAR %in% c(400:499),testheart.df$BLDSUGAR,
                                                    ifelse(testheart.df$BLDSUGAR ==888,0,"")))))



#FLSHTMY2
testheart.df$FLSHTMY2<-ifelse(testheart.df$FLSHTMY2==777777,"never",
                              ifelse(testheart.df$FLSHTMY2<999999,paste(substr(testheart.df$FLSHTMY2, 1, ifelse(nchar(testheart.df$FLSHTMY2)==6,2,1)),"-",substr(testheart.df$FLSHTMY2, ifelse(nchar(testheart.df$FLSHTMY2)==6,3,2), nchar(testheart.df$FLSHTMY2))),""))

#GENHLTH
testheart.df$GENHLTH<-ifelse(testheart.df$GENHLTH==1,"Excellent",
                             
                             ifelse(testheart.df$GENHLTH==2,"Very Good",
                                    
                                    ifelse(testheart.df$GENHLTH==3,"Good",
                                           ifelse(testheart.df$GENHLTH==4,"Fair",
                                                  ifelse(testheart.df$GENHLTH==5,"poor","")))))


#HADHYST2
testheart.df$HADHYST2<-ifelse(testheart.df$HADHYST2==1,"Yes",
                              ifelse(testheart.df$HADHYST2==2,"No",""))

#HADMAM
testheart.df$HADMAM<-ifelse(testheart.df$HADMAM==1,"Yes",
                            ifelse(testheart.df$HADMAM==2,"No",""))

#HADPAP2
testheart.df$HADPAP2<-ifelse(testheart.df$HADPAP2==1,"Yes",
                             ifelse(testheart.df$HADPAP2==2,"No",""))


#HADSGCO1
testheart.df$HADSGCO1<-ifelse(testheart.df$HADSGCO1==1,"Sigmoidal",
                              ifelse(testheart.df$HADSGCO1==2,"Colonoscopy",""))

#HADSIGM3
testheart.df$HADSIGM3<-ifelse(testheart.df$HADSIGM3==1,"Yes",
                              ifelse(testheart.df$HADSIGM3==2,"No",""))

#HAVARTH3
testheart.df$HAVARTH3<-ifelse(testheart.df$HAVARTH3==1,"Yes",
                              ifelse(testheart.df$HAVARTH3==2,"No",""))

#HEIGHT3
testheart.df$HEIGHT3<-ifelse(testheart.df$HEIGHT3<=800,((as.numeric(substr(testheart.df$HEIGHT3,1,1))*12) +  as.numeric(substr(testheart.df$HEIGHT3,2,3)))*2.54,
                             ifelse(testheart.df$HEIGHT3 %in% c(9000:9998),substr(testheart.df$HEIGHT3,2,4),""))

#HIVTST6
testheart.df$HIVTST6<-ifelse(testheart.df$HIVTST6==1,"Yes",
                             ifelse(testheart.df$HIVTST6==2,"No",""))

#HIVTSTD3
testheart.df$HIVTSTD3-ifelse(testheart.df$HIVTSTD3<777777,paste(substr(testheart.df$HIVTSTD3, 1, ifelse(nchar(testheart.df$HIVTSTD3)==6,2,1)),"-",substr(testheart.df$HIVTSTD3, ifelse(nchar(testheart.df$HIVTSTD3)==6,3,2), nchar(testheart.df$HIVTSTD3))),"")

#HLTHPLN1
testheart.df$HLTHPLN1<- ifelse(testheart.df$HLTHPLN1==1,"Yes",
                               ifelse(testheart.df$HLTHPLN1==2,"No",""))


#HOWLONG
testheart.df$HOWLONG<- ifelse(testheart.df$HLTHPLN1==1,"within 1 year",
                              ifelse(testheart.df$HLTHPLN1==2,"1-2 years",
                                     ifelse(testheart.df$HLTHPLN1==2,"2-3 years",
                                            ifelse(testheart.df$HLTHPLN1==2,"3-5 years",
                                                   ifelse(testheart.df$HLTHPLN1==2,"before 5 years","")))))

#HPVADSHT
testheart.df$HPVADSHT<- ifelse(testheart.df$HPVADSHT==3,"all shots taken",
                               ifelse(testheart.df$HPVADSHT<=2,paste(testheart.df$HPVADSHT,"shots taken"),""))

#HPVADVC2
testheart.df$HPVADVC2<- ifelse(testheart.df$HPVADVC2==1,"Yes",
                               ifelse(testheart.df$HPVADVC2==2,"No",""))

#HTIN4
testheart.df$HTIN4<- NULL

#HTM4
testheart.df$HTM4<- NULL

#IDATE
testheart.df$IDATE<- NULL

#IMFVPLAC
testheart.df$IMFVPLAC<-ifelse(testheart.df$IMFVPLAC==1,"Doctor's office/Health Management Organization",
                              
                              ifelse(testheart.df$IMFVPLAC==2,"A health department",
                                     
                                     ifelse(testheart.df$IMFVPLAC==3,"Another type of clinic or health center",
                                            ifelse(testheart.df$IMFVPLAC==4,"A senior,recreation or community center",
                                                   ifelse(testheart.df$IMFVPLAC==5,"A store",
                                                          ifelse(testheart.df$IMFVPLAC==6,"A hospital(e.g. in patient)",
                                                                 ifelse(testheart.df$IMFVPLAC==7,"An emergency room",
                                                                        ifelse(testheart.df$IMFVPLAC==8,"Workplace",
                                                                               ifelse(testheart.df$IMFVPLAC==9,"Some other kind of place",
                                                                                      ifelse(testheart.df$IMFVPLAC==10,"Received vaccination in Canada/Mexico",
                                                                                             ifelse(testheart.df$IMFVPLAC==11,"A school","")))))))))))






#INCOME2
testheart.df$INCOME2<-ifelse(testheart.df$INCOME2==1,"Less than $10,000",
                             ifelse(testheart.df$INCOME2==2,"$10,000 to less than $15,000",
                                    ifelse(testheart.df$INCOME2==3,"$15,000 to less than $20,000",
                                           ifelse(testheart.df$INCOME2==4,"$20,000 to less than $25,000",
                                                  ifelse(testheart.df$INCOME2==5,"$25,000 to less than $35,000",
                                                         ifelse(testheart.df$INCOME2==6,"$35,000 to less than $50,000",
                                                                ifelse(testheart.df$INCOME2==7,"$50,000 to less than $75,000",
                                                                       ifelse(testheart.df$INCOME2==8,"$75,000 or more",""))))))))


#INSULIN
testheart.df$INSULIN<-ifelse(testheart.df$INSULIN==1,"Yes",
                             ifelse(testheart.df$INSULIN==2,"No",""))


#IYEAR
testheart.df$IYEAR<-NULL


#LANDLINE
testheart.df$LANDLINE<-ifelse(testheart.df$LANDLINE==1,"Yes",
                              ifelse(testheart.df$LANDLINE==2," No",""))

#LASTPAP2
testheart.df$LASTPAP2<-ifelse(testheart.df$LASTPAP2==1,"Within 1 year",
                              ifelse(testheart.df$LASTPAP2==2,"1 -2 years",
                                     ifelse(testheart.df$LASTPAP2==3,"2-3 years",
                                            ifelse(testheart.df$LASTPAP2==4,"3-5 years",
                                                   ifelse(testheart.df$LASTPAP2==5,"before 5 years","")))))


#LASTSIG3
testheart.df$LASTSIG3<-ifelse(testheart.df$LASTSIG3==1,"Within 1 year",
                              ifelse(testheart.df$LASTSIG3==2,"1-2 years",
                                     ifelse(testheart.df$LASTSIG3==3,"2-3 years",
                                            ifelse(testheart.df$LASTSIG3==4,"3-5 years",
                                                   ifelse(testheart.df$LASTSIG3==5,"5-10 years",
                                                          ifelse(testheart.df$LASTSIG3==6,"before 10 years",""))))))

#LASTSMK2
testheart.df$LASTSMK2<-ifelse(testheart.df$LASTSMK2==1,"Everyday",
                              ifelse(testheart.df$LASTSMK2==2,"Somedays",
                                     ifelse(testheart.df$LASTSMK2==3,"Not at all","")))


#LENGEXAM
testheart.df$LENGEXAM<-ifelse(testheart.df$LENGEXAM==1,"Within 1 year",
                              ifelse(testheart.df$LENGEXAM==2,"1 -2 years",
                                     ifelse(testheart.df$LENGEXAM==3,"2-3 years",
                                            ifelse(testheart.df$LENGEXAM==4,"3-5 years",
                                                   ifelse(testheart.df$LENGEXAM==5,"before 5 years","")))))


#LSATISFY
testheart.df$LSATISFY<-ifelse(testheart.df$LSATISFY==1,"Very satisfied",
                              ifelse(testheart.df$LSATISFY==2,"satisfied",
                                     ifelse(testheart.df$LSATISFY==3,"Dissatisfied",
                                            ifelse(testheart.df$LSATISFY==4,"Very dissatisfied",""))))


#LSTBLDS3
testheart.df$LSTBLDS3<-ifelse(testheart.df$LSTBLDS3==1,"Within 1 year",
                              ifelse(testheart.df$LSTBLDS3==2,"1 -2 years",
                                     ifelse(testheart.df$LSTBLDS3==3,"2-3 years",
                                            ifelse(testheart.df$LSTBLDS3==4,"3-5 years",
                                                   ifelse(testheart.df$LSTBLDS3==5,"before 5 years","")))))


#MARITAL
testheart.df$MARITAL<-ifelse(testheart.df$MARITAL==1,"Married",
                             ifelse(testheart.df$MARITAL==2,"Divorced",
                                    ifelse(testheart.df$MARITAL==3,"Widowed",
                                           ifelse(testheart.df$MARITAL==4,"Separated",
                                                  ifelse(testheart.df$MARITAL==5,"Never married",
                                                         ifelse(testheart.df$MARITAL==6,"A member of an unmarried couple",""))))))



#MAXDRNKS
testheart.df$MAXDRNKS<-ifelse(testheart.df$MAXDRNKS<=76,testheart.df$MAXDRNKS,"")


#MEDCOST
testheart.df$MEDCOST<-ifelse(testheart.df$MEDCOST==1,"Yes",
                             ifelse(testheart.df$MEDCOST==2,"No",""))

#MENTHLTH
testheart.df$MENTHLTH<-ifelse(testheart.df$MENTHLTH<=30,testheart.df$MENTHLTH,
                              ifelse(testheart.df$MENTHLTH==88,0,""))


#MSCODE
testheart.df$MSCODE<-ifelse(testheart.df$MSCODE==1,"In city center",
                            ifelse(testheart.df$MSCODE==2,"outside city center but inside county",
                                   ifelse(testheart.df$MSCODE==3,"Inside the suburban county",
                                          ifelse(testheart.df$MSCODE==5,"Not in an MSA",""))))


#NUMADULT
testheart.df$NUMADULT<-ifelse(testheart.df$NUMADULT<=99,testheart.df$NUMADULT,"")


#NUMHHOL2
testheart.df$NUMHHOL2<-ifelse(testheart.df$NUMADULT==1,"Yes",
                              ifelse(testheart.df$NUMADULT==2,"No",""))

#NUMMEN
testheart.df$NUMMEN<-ifelse(testheart.df$NUMMEN<=99,testheart.df$NUMMEN,"")


#NUMPHON2
testheart.df$NUMPHON2<-NULL


#NUMWOMEN
testheart.df$NUMWOMEN<-ifelse(testheart.df$NUMWOMEN<=99,testheart.df$NUMWOMEN,"")


#PAINACT2
testheart.df$PAINACT2<-NULL


#PDIABTST
testheart.df$PDIABTST<-ifelse(testheart.df$PDIABTST==1,"Yes",
                              ifelse(testheart.df$PDIABTST==2,"No",""))


#PERSDOC2
testheart.df$PERSDOC2<-ifelse(testheart.df$PERSDOC2==1,"YEes, only 1",
                              ifelse(testheart.df$PERSDOC2==2,"More than 1",
                                     ifelse(testheart.df$PERSDOC2==3,"no","")))

#PHYSHLTH
testheart.df$PHYSHLTH<-ifelse(testheart.df$PHYSHLTH<=30,testheart.df$PHYSHLTH,
                              ifelse(testheart.df$PHYSHLTH==88,0,""))

#PNEUVAC3
testheart.df$PNEUVAC3<-ifelse(testheart.df$PNEUVAC3==1,"Yes",
                              ifelse(testheart.df$PNEUVAC3==2,"No",""))


#POORHLTH
testheart.df$POORHLTH<-ifelse(testheart.df$POORHLTH<=30,testheart.df$POORHLTH,
                              ifelse(testheart.df$POORHLTH==88,0,""))


#PREDIAB1
testheart.df$PREDIAB1<-ifelse(testheart.df$PREDIAB1==1,"Yes",
                              ifelse(testheart.df$PREDIAB1==2,"Yes,during pregnency",
                                     ifelse(testheart.df$PREDIAB1==3,"No","")))


#PREGNANT
testheart.df$PREGNANT<-ifelse(testheart.df$PREGNANT==1,"Yes",
                              ifelse(testheart.df$PREGNANT==2,"No",""))


#PROFEXAM
testheart.df$PROFEXAM<-ifelse(testheart.df$PROFEXAM==1,"Yes",
                              ifelse(testheart.df$PROFEXAM==2,"No",""))

#PSATEST1
testheart.df$PSATEST1<-ifelse(testheart.df$PSATEST1==1,"Yes",
                              ifelse(testheart.df$PSATEST1==2,"No",""))


#PSATIME
testheart.df$PSATIME<-ifelse(testheart.df$PSATIME==1,"Within 1 year",
                             ifelse(testheart.df$PSATIME==2,"1 -2 years",
                                    ifelse(testheart.df$PSATIME==3,"2-3 years",
                                           ifelse(testheart.df$PSATIME==4,"3-5 years",
                                                  ifelse(testheart.df$PSATIME==5,"before 5 years","")))))


#PVTRESD2
testheart.df$PVTRESD2<-ifelse(testheart.df$PVTRESD2==1,"Yes",
                              ifelse(testheart.df$PVTRESD2==2,"No",""))


#QLACTLM2
testheart.df$QLACTLM2<-ifelse(testheart.df$QLACTLM2==1,"Yes",
                              ifelse(testheart.df$QLACTLM2==2,"No",""))


#QLHLTH2
testheart.df$QLHLTH2<-NULL


#QLMENTL2
testheart.df$QLMENTL2<-NULL


#QLSTRES2
testheart.df$QLSTRES2<-NULL


#QSTLANG
testheart.df$QSTLANG<-ifelse(testheart.df$QSTLANG==1,"English",
                             ifelse(testheart.df$QSTLANG==2,"Spanish",
                                    ifelse(testheart.df$QSTLANG>=3,"other","")))


#RCSGENDR
testheart.df$RCSGENDR<-ifelse(testheart.df$RCSGENDR==1,"Boy",
                              ifelse(testheart.df$RCSGENDR==2,"Girl",""))


#RCSRLTN2
testheart.df$RCSRLTN2<-ifelse(testheart.df$RCSRLTN2==1,"Parent",
                              ifelse(testheart.df$RCSRLTN2==2,"Grandparent",
                                     ifelse(testheart.df$RCSRLTN2==3,"Foster Parent or Guardian",
                                            ifelse(testheart.df$RCSRLTN2==4,"Sibling",
                                                   ifelse(testheart.df$RCSRLTN2==5,"Other relative",
                                                          ifelse(testheart.df$RCSRLTN2==6,"Not related in any way",
                                                                 ifelse(testheart.df$RCSRLTN2==7,"Don't know not sure","")))))))



#RENTHOM1
testheart.df$RENTHOM1<-ifelse(testheart.df$RENTHOM1==1,"Parent",
                              ifelse(testheart.df$RENTHOM1==2,"Own",
                                     ifelse(testheart.df$RENTHOM1==3,"Rent",
                                            ifelse(testheart.df$RENTHOM1==4,"Other Arrangement",""))))


#SCNTLPAD
testheart.df$SCNTLPAD<-ifelse(testheart.df$SCNTLPAD==1,"By Salary",
                              ifelse(testheart.df$SCNTLPAD==2,"Paid by hour",
                                     ifelse(testheart.df$SCNTLPAD==3,"Paid by job/task(commission)",
                                            ifelse(testheart.df$SCNTLPAD==4,"Paid some other way",""))))



#SCNTLWK1
testheart.df$SCNTLWK1<- ifelse(testheart.df$SCNTLWK1==98,0,
                               ifelse(testheart.df$SCNTLWK1<=96,testheart.df$SCNTLWK1,""))


#SCNTPAID
testheart.df$SCNTPAID<-NULL


#SCNTWRK1
testheart.df$SCNTWRK1<-NULL


#SEATBELT
testheart.df$SEATBELT<-ifelse(testheart.df$SEATBELT==1,"always",
                              ifelse(testheart.df$SEATBELT==2,"nearly always",
                                     ifelse(testheart.df$SEATBELT==3,"sometimes",
                                            ifelse(testheart.df$SEATBELT==4,"seldom",
                                                   ifelse(testheart.df$SEATBELT==5,"never",
                                                          ifelse(testheart.df$SEATBELT==8,"Never drive or ride a car",""))))))


#SEQNO
testheart.df$SEQNO<-NULL


#SEX
testheart.df$SEX<-ifelse(testheart.df$SEX==1,"Male",
                         ifelse(testheart.df$SEX==2,"Female",""))



#SMOKDAY2
testheart.df$SMOKDAY2<-ifelse(testheart.df$SMOKDAY2==1,"Everyday",
                              ifelse(testheart.df$SMOKDAY2==2,"Somedays",
                                     ifelse(testheart.df$SMOKDAY2==3,"Not at all","")))



#SMOKE100
testheart.df$SMOKE100<-ifelse(testheart.df$SMOKE100==1,"Yes",
                              ifelse(testheart.df$SMOKE100==2,"No",""))



#STOPSMK2
testheart.df$STOPSMK2<-ifelse(testheart.df$STOPSMK2==1,"Yes",
                              ifelse(testheart.df$STOPSMK2==2,"No",""))


#USEEQUIP
testheart.df$USEEQUIP<-ifelse(testheart.df$USEEQUIP==1,"Yes",
                              ifelse(testheart.df$USEEQUIP==2,"No",""))


#USENOW3
testheart.df$USENOW3<-ifelse(testheart.df$USENOW3==1,"Everyday",
                             ifelse(testheart.df$USENOW3==2,"Somedays",
                                    ifelse(testheart.df$USENOW3==3,"Not at all","")))



#VETERAN3
testheart.df$VETERAN3<-ifelse(testheart.df$VETERAN3==1,"Yes",
                              ifelse(testheart.df$VETERAN3==2,"No",""))


#WEIGHT2
testheart.df$WEIGHT2<-ifelse(testheart.df$WEIGHT2<=1000,round((as.numeric(testheart.df$WEIGHT2)*0.453592),digits=0),
                             ifelse(testheart.df$WEIGHT2 %in% c(9000:9998),testheart.df$WEIGHT2-9000,""))

#WTKG3
testheart.df$WTKG3<-NULL


nrow(testheart.df[is.na(testheart.df$DIABEYE)==FALSE,])
testheart.df$FLSHTMY2

Heart.df<-testheart.df
####################################
x14<-Heart.df[Heart.df$ADDEPEV2!="",]
x14<-x14[x14$AGEG5YR!="",]
x14<-x14[x14$AIDTST3!="",]
x14<-x14[x14$ALCDAY5!="",]
x14<-x14[x14$ASTHMA3!="",]
x14<-x14[x14$AVEDRNK2!="",]
x14<-x14[x14$CHCKIDNY!="",]
x14<-x14[x14$CHCOCNCR!="",]
x14<-x14[x14$CHILDREN!="",]
x14<-x14[x14$CHLDCNT!="",]
x14<-x14[x14$CVDINFR4!="",]
x14<-x14[x14$CVDSTRK3!="",]
x14<-x14[x14$DIABETE3!="",]
x14<-x14[x14$DRDXAR1!="",]
x14<-x14[x14$EDUCA!="",]
x14<-x14[x14$EDUCAG!="",]
x14<-x14[x14$EXERANY2!="",]
x14<-x14[x14$HAVARTH3!="",]
x14<-x14[x14$HLTHPLN1!="",]
x14<-x14[x14$INCOME2!="",]
x14<-x14[x14$INCOMG!="",]
x14<-x14[x14$MARITAL!="",]
x14<-x14[x14$MENTHLTH!="",]
x14<-x14[x14$PERSDOC2!="",]
x14<-x14[x14$PHYSHLTH!="",]
x14<-x14[x14$PNEUVAC3!="",]
x14<-x14[x14$RFBING5!="",]
x14<-x14[x14$RFBMI5!="",]
x14<-x14[x14$RFSMOK3!="",]
x14<-x14[x14$SEX!="",]
x14<-x14[x14$TOTINDA!="",]
x14<-x14[x14$USEEQUIP!="",]
x14<-x14[x14$USENOW3!="",]
x14<-x14[x14$VETERAN3!="",]
x14<-x14[x14$WEIGHT2!="",]

Heart.df<-x14
Heart_clean.df<-x14
Heart_clean.df$CHCSCNCR <- NULL
Heart_clean.df$HOWLONG <- NULL
#Converting to numeric
Heart_clean.df$AVEDRNK2<-as.numeric(Heart_clean.df$AVEDRNK2)
Heart_clean.df$CHILDREN<-as.numeric(Heart_clean.df$CHILDREN)
Heart_clean.df$MENTHLTH<-as.numeric(Heart_clean.df$MENTHLTH)
Heart_clean.df$PHYSHLTH<-as.numeric(Heart_clean.df$PHYSHLTH)
Heart_clean.df$WEIGHT2<-as.numeric(Heart_clean.df$WEIGHT2)
Heart_clean.df$ALCDAY5<-as.numeric(Heart_clean.df$ALCDAY5)

set.seed(123)
#Randomize the data
rand <- runif(nrow(Heart_clean.df)) # create 150 random numbers
Heartrand <- Heart_clean.df[order(rand), ]
head(Heartrand)

#Partition the data
smp_size <- floor(0.65 * nrow(Heartrand))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(Heartrand)), size = smp_size)

Hearttrain <- Heartrand[train_ind, ]
Hearttest <- Heartrand[-train_ind, ]

#Build decision tree
Hearttree_6 <- rpart(CVDINFR4 ~ ., data = Hearttrain, method = "class",control=rpart.control(cp=0.0000001,maxdepth=6))
rpart.plot(Hearttree_6, extra = 6)

#Create a confusion matrix which shows the accuracy rate of your classification
Hearttrain$pred <- predict(Hearttree_6, Hearttrain, type = "class") #create a prediction using our tree
table(Actual = Hearttrain$CVDINFR4, Predicted = Hearttrain$pred) #create a confusion matrix

Hearttest$pred <- predict(Hearttree_6, Hearttest, type = "class")#create a prediction using our tree
table(Actual = Hearttest$CVDINFR4, Predicted = Hearttest$pred)

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
  writeLines(paste("False Positive Rate:", round(falsePosRate, digits = 4)))
  writeLines(paste("False Negative Rate:", round(falseNegRate, digits = 4)))
  writeLines(paste("Precision:", round(precision, digits = 4)))
  
  dataset
}
#Evaluate tree performance
HeartTrain_perf <- testModelPerformance(Hearttree_6, Hearttrain, Hearttrain$CVDINFR4)
HeartTest_perf <- testModelPerformance(Hearttree_6, Hearttest, Hearttest$CVDINFR4)

##  BUILDING THE LOGISTIC REGRESSION MODEL
set.seed(123)
HeartLogitData<-x14
HeartLogitData$CVDINFR4<- ifelse(HeartLogitData$CVDINFR4=="Yes",1,
                                  ifelse(HeartLogitData$CVDINFR4=="No",2,""))
HeartLogitData<-HeartLogitData[HeartLogitData$CVDINFR4!="",]
dim(HeartLogitData)
HeartLogitData$CHCSCNCR<- NULL
HeartLogitData$HOWLONG<- NULL
dim(HeartLogitData)
HeartLogitData[HeartLogitData==""]<-NA
colnames(HeartLogitData)[colSums(is.na(HeartLogitData)) > 0]
#Randomize the data
rand_logit <- runif(nrow(HeartLogitData)) # create 150 random numbers
Heartrand_logit <- HeartLogitData[order(rand), ]
head(Heartrand_logit)

#Partition the data
smp_size <- floor(0.65 * nrow(Heartrand_logit))

## set the seed to make your partition reproductible
set.seed(123)
train_ind_logit <- sample(seq_len(nrow(Heartrand_logit)), size = smp_size)

Hearttrain_logit <- Heartrand_logit[train_ind_logit, ]
Hearttest_logit <- Heartrand_logit[-train_ind_logit, ]

Heartlogit <- glm(CVDINFR4 ~., data = Hearttrain_logit, family = binomial(link = "logit"))
summary(Heartlogit)

beep("sword")