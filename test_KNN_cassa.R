setwd("C:/Users/miraf/Desktop/Sviluppi_Rating_FFF")

library(readxl)
library(class)
db=read_excel("dataset di partenza.xlsx", na="")

#controllo presenza di missing nei vari campi del db
colSums(is.na(db))

#sostituzione non proprio automatica dei missing con le medie

db$CCROTUT_TM[is.na(db$CCROTUT_TM)]        =mean(db$CCROTUT_TM,na.rm=TRUE)
db$CCU_A_SM[is.na(db$CCU_A_SM)]            =mean(db$CCU_A_SM,na.rm=TRUE)
db$PCNRI_S_SM[is.na(db$CCU_A_SM)]          =mean(db$CCU_A_SM,na.rm=TRUE)
db$PCNRI_S_SM[is.na(db$PCNRI_S_SM)]        =mean(db$PCNRI_S_SM,na.rm=TRUE)
db$PCU_A_SM[is.na(db$PCU_A_SM)]            =mean(db$PCU_A_SM,na.rm=TRUE)
db$PCNM6_SC[is.na(db$PCNM6_SC)]            =mean(db$PCNM6_SC,na.rm=TRUE)
db$ANU_A_SM[is.na(db$ANU_A_SM)]            =mean(db$ANU_A_SM,na.rm=TRUE)
db$ANNM6_SC[is.na(db$ANNM6_SC)]            =mean(db$ANNM6_SC,na.rm=TRUE)
db$RTMCI_A_SM[is.na(db$RTMCI_A_SM)]        =mean(db$RTMCI_A_SM,na.rm=TRUE)
db$RTU_IMPINIZ_AM[is.na(db$RTU_IMPINIZ_AM)]=mean(db$RTU_IMPINIZ_AM,na.rm=TRUE)
db$RTPNM6_NIMP[is.na(db$RTPNM6_NIMP)]      =mean(db$RTPNM6_NIMP,na.rm=TRUE)


#controllo post sostituzione
colSums(is.na(db))


#KNN


#divisione in test e training set

db$NDG_NEW <- NULL
db$DT_RIF <- NULL


#reduction db dimensions and standardization


set.seed(123)
sample1=sample(c(TRUE,FALSE), nrow(db),replace=T,prob = c(0.05,0.95))
db1=db[sample1,]
std.db=as.data.frame(scale(db1[,-11]))


#insample outofsample 

set.seed(123)
sample=sample(c(TRUE,FALSE), nrow(std.db),replace=T,prob = c(0.7,0.3))
train=std.db[sample,]
test=std.db[!sample,]
ind_bad.is=db1$IND_BAD[sample]
ind_bad.os=db1$IND_BAD[!sample]

library(class)
set.seed (1)
knn.pred=knn(train,test,ind_bad.is,k=1)
knn.table=table(knn.pred,ind_bad.os)


err1=knn.table[2,1]/(knn.table[1,1]+knn.table[2,1])
err2=knn.table[1,2]/(knn.table[1,2]+knn.table[2,2])

spec=knn.table[1,1]/(knn.table[1,1]+knn.table[2,1])
sens=knn.table[2,2]/(knn.table[2,2]+knn.table[1,2])

AR=rowSums(knn.table)[1]/(sum(knn.table))
TC=(sens+spec)/2


install.packages("oii")
library(oii)

association.measures(ind_bad.os, knn.pred)


