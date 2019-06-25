setwd("C:/Users/miraf/Desktop/Sviluppi_Rating_FFF")


install.packages('earth')
library(earth)
library(caret)
install.packages('vip')
library(vip)
library(pdp)
library(readxl)

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


#divisione in test e training set

db$NDG_NEW <- NULL
db$DT_RIF <- NULL

db1=db[,-11]

set.seed(123)
sample=sample(c(TRUE,FALSE), nrow(db1),replace=T,prob = c(0.7,0.3))
train=db1[sample,]
test=db1[!sample,]

ind_bad.is=db$IND_BAD[sample]
ind_bad.os=db$IND_BAD[!sample]


mars1=earth(ind_bad.is ~ ., data = train,  glm=list(family=binomial))

print(mars1)
summary(mars1)
plot(mars1,which=1)

mars2=earth(ind_bad.is ~ ., data = train, glm=list(family=binomial), degree=2)

print(mars2)
summary(mars2)
plot(mars2,which=1)

#cross validation per il tuning 

# create a tuning grid
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)

head(hyper_grid)


# for reproducibiity
set.seed(123)

# cross validated model
tuned_mars = train(
  x = subset(train, select = -IND_BAD),
  y = train$IND_BAD,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

# best model
tuned_mars$bestTune


# plot results
ggplot(tuned_mars)

in.sample = predict(mars1,type = "response")
out.of.sample=predict(mars1, newdata = test, type="response")

plot(log(out.of.sample/(1-out.of.sample)),out.of.sample)
lines(log(in.sample/(1-in.sample)),in.sample, type = "p", col="red")

list(
  m.in = table(as.logical(ind_bad.is), in.sample > 0.5) %>% prop.table() %>% round(3),
  m.out = table(as.logical(ind_bad.os), out.of.sample > 0.5) %>% prop.table() %>% round(3)
)
####
install.packages("ROCR")
library(ROCR)

par(mfrow=c(1, 2))

prediction(in.sample, ind_bad.is) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot()

prediction(out.of.sample, ind_bad.os) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot()

####
os.table=table(as.logical(ind_bad.os), out.of.sample > 0.5)

is.table=table(as.logical(ind_bad.is), in.sample > 0.5)

err1.os=os.table[2,1]/(os.table[1,1]+os.table[2,1])
err2.os=os.table[1,2]/(os.table[1,2]+os.table[2,2])

err1.is=is.table[2,1]/(is.table[1,1]+is.table[2,1])
err2.is=is.table[1,2]/(is.table[1,2]+is.table[2,2])


#sensitivity(factor(ind_bad.os,levels=c(1,0)),
#             factor(ifelse(out.of.sample > 0.5, 1, 0),levels=c(1,0)))
#specificity(factor(ind_bad.os,levels=c(1,0)),
#            factor(ifelse(out.of.sample > 0.5, 1, 0),levels=c(1,0)))

spec.os=os.table[1,1]/(os.table[1,1]+os.table[2,1])
sens.os=os.table[2,2]/(os.table[2,2]+os.table[1,2])

spec.is=is.table[1,1]/(is.table[1,1]+is.table[2,1])
sens.is=is.table[2,2]/(is.table[2,2]+is.table[1,2])



AR.os=rowSums(os.table)[1]/(sum(os.table))
TC.os=(sens.os+spec.os)/2

AR.is=rowSums(is.table)[1]/(sum(is.table))
TC.is=(sens.is+spec.is)/2


install.packages("oii")
library(oii)

association.measures(ind_bad.os, ifelse(out.of.sample > 0.5,1,0))



