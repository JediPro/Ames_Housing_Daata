## make submission ####
dsub=as.data.frame(matrix(nrow = nrow(dtest), ncol = 2))
colnames(dsub)=c('Id','SalePrice')
dsub$Id=dtest$Id
dsub$SalePrice=lrstep.f
dsub$SalePrice[dsub$SalePrice<0]=0
write.csv(dsub, "submission.csv",row.names = F)

# load libraries
library(data.table)
library(ggplot2)
library(Metrics)
library(xgboost)
library(corrplot)

# read in datasets
setwd("D:/Datasets/Ames Housing Data")
dtrain=fread("train.csv", data.table = F, stringsAsFactors = T)
dtest=fread("test.csv", data.table = F, stringsAsFactors = T)
str(dtrain)
summary(dtrain)
dtest$SalePrice=NA
dtrain=rbind(dtrain,dtest)

# convert predictors to factor when required
dtrain$MSSubClass=as.factor(dtrain$MSSubClass)
dtrain$OverallQual=as.factor(dtrain$OverallQual)
dtrain$OverallCond=as.factor(dtrain$OverallCond)
dtrain$BsmtFullBath=as.factor(dtrain$BsmtFullBath)
dtrain$BsmtHalfBath=as.factor(dtrain$BsmtHalfBath)
dtrain$FullBath=as.factor(dtrain$FullBath)
dtrain$HalfBath=as.factor(dtrain$HalfBath)
dtrain$BedroomAbvGr=as.factor(dtrain$BedroomAbvGr)
dtrain$KitchenAbvGr=as.factor(dtrain$KitchenAbvGr)
dtrain$TotRmsAbvGrd=as.factor(dtrain$TotRmsAbvGrd)
dtrain$Fireplaces=as.factor(dtrain$Fireplaces)
dtrain$GarageCars=as.factor(dtrain$GarageCars)
dtrain$MoSold=as.factor(dtrain$MoSold)

# recode variable categories ####
# MSSubClass
library(car)
t1=sort(table(dtrain$MSSubClass), decreasing = T)
print(t1)
s1=names(which(t1<3000))
df1$group_1=recode(df1$group_1, "s1='group 0'") # recode factors w/ freq < 2000

#MSZoning
dtrain$MSZoning[is.na(dtrain$MSZoning)]=as.factor('RL')

#LotFrontage
library(lattice)
densityplot(dtrain$LotFrontage)
dtrain$LotFrontage[is.na(dtrain$LotFrontage)]=mean(dtrain$LotFrontage, na.rm = T)

#Year Built
dtrain$YearBuilt=2017-dtrain$YearBuilt
# Year RemodAdd
dtrain$YearRemodAdd=2017-dtrain$YearRemodAdd
# Exterior
dtrain$Exterior1st[is.na(dtrain$Exterior1st)]=as.factor('VinylSd')
dtrain$Exterior2nd[is.na(dtrain$Exterior2nd)]=as.factor('VinylSd')
# MasVnrType
dtrain$MasVnrType[is.na(dtrain$MasVnrType)]=as.factor('None')
#MasVnrArea
densityplot(dtrain$MasVnrArea)
dtrain$MasVnrArea[is.na(dtrain$MasVnrArea)]=median(dtrain$MasVnrArea, na.rm = T)
# Bsmt
dtrain$BsmtQual[is.na(dtrain$BsmtQual)]=as.factor('TA')
dtrain$BsmtCond[is.na(dtrain$BsmtCond)]=as.factor('TA')
dtrain$BsmtExposure=recode(dtrain$BsmtExposure,"NA='NotAvl'")
dtrain$BsmtFinType1=recode(dtrain$BsmtFinType1,"NA='NotAvl'")
dtrain$BsmtFinSF1[is.na(dtrain$BsmtFinSF1)]=0
dtrain$BsmtFinType2=recode(dtrain$BsmtFinType2,"NA='NotAvl'")
dtrain$BsmtFinSF2[is.na(dtrain$BsmtFinSF2)]=0
dtrain$BsmtUnfSF[is.na(dtrain$BsmtUnfSF)]=median(dtrain$BsmtUnfSF, na.rm = T)
dtrain$TotalBsmtSF[is.na(dtrain$TotalBsmtSF)]=median(dtrain$TotalBsmtSF, na.rm = T)
# Electrical
dtrain$Electrical[is.na(dtrain$Electrical)]=as.factor('SBrkr')
# Bsmt
dtrain$BsmtFullBath[is.na(dtrain$BsmtFullBath)]=as.factor('0')
dtrain$BsmtHalfBath[is.na(dtrain$BsmtHalfBath)]=as.factor('0')
# Kitchen
dtrain$KitchenQual[is.na(dtrain$KitchenQual)]=as.factor('TA')
dtrain$Functional[is.na(dtrain$Functional)]=as.factor('Typ')
# Fireplace
dtrain$FireplaceQu=as.character(dtrain$FireplaceQu)
dtrain$FireplaceQu[is.na(dtrain$FireplaceQu)]='NotAv'
dtrain$FireplaceQu=as.factor(dtrain$FireplaceQu)

# Garage
dtrain$GarageType=as.character(dtrain$GarageType)
dtrain$GarageType[is.na(dtrain$GarageType)]='NotAv'
dtrain$GarageType=as.factor(dtrain$GarageType)

dtrain$GarageYrBlt=recode(dtrain$GarageYrBlt,"2207=2007")
dtrain$GarageYrBlt=2017-dtrain$GarageYrBlt
densityplot(dtrain$GarageYrBlt)
dtrain$GarageYrBlt[is.na(dtrain$GarageYrBlt)]=130
dtrain$GarageFinish[is.na(dtrain$GarageFinish)]=as.factor('Unf')
dtrain$GarageCars[is.na(dtrain$GarageCars)]=as.factor('0')
dtrain$GarageArea[is.na(dtrain$GarageArea)]=0

dtrain$GarageQual=as.character(dtrain$GarageQual)
dtrain$GarageQual[is.na(dtrain$GarageQual)]='NotAv'
dtrain$GarageQual=as.factor(dtrain$GarageQual)

dtrain$GarageCond=as.character(dtrain$GarageCond)
dtrain$GarageCond[is.na(dtrain$GarageCond)]='NotAv'
dtrain$GarageCond=as.factor(dtrain$GarageCond)

#PoolQC
dtrain$PoolQC=recode(dtrain$PoolQC, "c(NA,'')='N'; else='Y'")
# Fence
dtrain$PoolQC=recode(dtrain$PoolQC, "NA='N'")
#MiscFeature
dtrain$MiscFeature=recode(dtrain$MiscFeature, "c(NA,'')='N';else='Other'")
# Sale Type
dtrain$SaleType[is.na(dtrain$SaleType)]=as.factor('WD')
# YrSold
dtrain$YrSold=2017-dtrain$YrSold
# Condition1
t1=sort(table(dtrain$Condition1), decreasing = T)
print(t1)
dtrain$Condition1=recode(dtrain$Condition1, "c('RRAn','RRAe')='RRA';c('RRNn','RRNe')='RRN';
                         c('PosN','PosA')='Pos'")

## Recoding factors, Viewing correlations####
library(ggplot2)

ggplot(data = dtrain, aes(x=SalePrice))+geom_density()

#MSSubClass
ggplot(data = dtrain, aes(x=MSSubClass, y = SalePrice))+geom_boxplot(aes(color=MSSubClass))
sort(table(dtrain$MSSubClass), decreasing = T)
dtrain$MSSubClass=recode(dtrain$MSSubClass, "c('20','30','40')='1S'; c('45','50')='1.5S';
                         c('60','70','75')='2S';c('80','85')='SP';c('90','190')='O';else='PUD'")

# MSZoning
ggplot(data = dtrain, aes(x=MSZoning, y = SalePrice))+geom_boxplot(aes(color=MSZoning))

# LotFrontage
ggplot(data = dtrain, aes(x=LotFrontage, y = SalePrice))+geom_point()+geom_smooth()

#LotArea
ggplot(data = dtrain, aes(x=LotArea, y = SalePrice))+geom_point()+geom_smooth()
ggplot(data = dtrain, aes(x=LotArea, y = LotFrontage))+geom_point()+geom_smooth()

#LotShape
dtrain$LotShape=recode(dtrain$LotShape,"c('IR1','IR2','IR3')='IR'")
ggplot(data = dtrain, aes(x=LotShape, y = SalePrice))+geom_point()+geom_boxplot()

# LandContour
ggplot(data = dtrain, aes(x=LandContour, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$LandContour=recode(dtrain$LandContour,"c('Bnk','HLS','Low')='NotLvl'")

# LotConfig
ggplot(data = dtrain, aes(x=LotConfig, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$LotConfig=recode(dtrain$LotConfig,"c('FR2','FR3','Low')='FRm'")

# LandSlope
ggplot(data = dtrain, aes(x=LandSlope, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$LandSlope=recode(dtrain$LandSlope,"c('Mod','Sev','Low')='Slp'")

# Neighborhood
ggplot(data = dtrain, aes(x=Neighborhood, y = SalePrice))+geom_boxplot(aes(color=Neighborhood))
# dtrain$Neighborhood=recode(dtrain$Neighborhood,"c('Mod','Sev','Low')='Slp'")

# Condition1
ggplot(data = dtrain, aes(x=Condition1, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$Condition1=recode(dtrain$Condition1,"c('Artery','Feedr')='Str';c('PosA','PosN')='Pos';
                         c('RRAe','RRAn','RRNe','RRNn')='RR'")

# Condition2
ggplot(data = dtrain, aes(x=Condition2, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$Condition2=recode(dtrain$Condition2,"c('Artery','Feedr')='Str';c('PosA','PosN')='Pos';
                         c('RRAe','RRAn','RRNe','RRNn')='RR'")

#BldgType
ggplot(data = dtrain, aes(x=BldgType, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$BldgType=recode(dtrain$BldgType,"c('2fmCon','Duplex')='2fmDup';c('Twnhs','TwnhsE')='Twnh'")

# HouseStyle
ggplot(data = dtrain, aes(x=HouseStyle, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$HouseStyle=recode(dtrain$HouseStyle,"c('1.5Fin','1.5Unf')='1.5S';
                         c('2.5Fin','2.5Unf')='2.5S'; c('SFoyer','SLvl')='Spl'")
#can code with respect to finished status

# OverallQual
ggplot(data = dtrain, aes(x=OverallQual, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$OverallQual=recode(dtrain$OverallQual,"c('1','2')='E';c('3','4')='D';c('5','6')='C';
                         c('7','8')='B'; c('9','10')='A'")

# OverallCond
ggplot(data = dtrain, aes(x=OverallCond, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$OverallCond=recode(dtrain$OverallCond,"c('1','2')='E';c('3','4')='D';c('5','6')='C';
                          c('7','8')='B'; c('9','10')='A'")

# YearBuilt
ggplot(data = dtrain, aes(x=YearBuilt, y = SalePrice))+geom_point()+geom_smooth()

# YearRemodAdd
ggplot(data = dtrain, aes(x=YearRemodAdd, y = SalePrice))+geom_point()+geom_smooth()
ggplot(data = dtrain, aes(x=YearRemodAdd, y = YearBuilt))+geom_point()+geom_smooth()

# RoofStyle
ggplot(data = dtrain, aes(x=RoofStyle, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$RoofStyle=recode(dtrain$RoofStyle,"c('Flat','Gambrel','Mansard','Shed')='Other'")

# RoofMatl
ggplot(data = dtrain, aes(x=RoofMatl, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$RoofMatl=recode(dtrain$RoofMatl,"c('CompShg')='CompShg';else='Other'")

# Exterior1st
ggplot(data = dtrain, aes(x=Exterior1st, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$Exterior1st=recode(dtrain$Exterior1st,"c('BrkComm','AsphShn','CBlock','Stone','ImStucc')='Other'")

# Exterior2nd
ggplot(data = dtrain, aes(x=Exterior2nd, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$Exterior2nd=recode(dtrain$Exterior2nd,"c('BrkComm','AsphShn','CBlock','Stone','ImStucc')='Other'")

# MasVnrType
ggplot(data = dtrain, aes(x=MasVnrType, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$MasVnrType=recode(dtrain$MasVnrType,"c('BrkComm','AsphShn','CBlock','Stone','ImStucc')='Other'")

# MasVnrArea
ggplot(data = dtrain, aes(x=MasVnrArea, y = SalePrice))+geom_point()+geom_smooth()

# ExterQual
ggplot(data = dtrain, aes(x=ExterQual, y = SalePrice))+geom_point()+geom_boxplot()

# ExterCond
ggplot(data = dtrain, aes(x=ExterCond, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$ExterCond=recode(dtrain$ExterCond,"c('Po')='Fa'")

# Foundation
ggplot(data = dtrain, aes(x=Foundation, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$Foundation=recode(dtrain$Foundation,"c('Slab','Stone','Wood')='Other'")

# BsmtQual 
ggplot(data = dtrain, aes(x=BsmtQual, y = SalePrice))+geom_point()+geom_boxplot()

# BsmtCond
ggplot(data = dtrain, aes(x=BsmtCond, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$BsmtCond=recode(dtrain$BsmtCond,"c('Po')='Fa'")

# BsmtExposure
ggplot(data = dtrain, aes(x=BsmtExposure, y = SalePrice))+geom_point()+geom_boxplot()

# BsmtFinType1
ggplot(data = dtrain, aes(x=BsmtFinType1, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$BsmtFinType1=recode(dtrain$BsmtFinType1,"c('ALQ','BLQ')='Avg';c('LwQ','Rec')='Low'")

# BsmtFinType2
ggplot(data = dtrain, aes(x=BsmtFinType2, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$BsmtFinType2=recode(dtrain$BsmtFinType2,"c('ALQ','BLQ')='Avg';c('LwQ','Rec')='Low'")

# BsmtFinSF1
ggplot(data = dtrain, aes(x=BsmtFinSF1, y = SalePrice))+geom_point()+geom_smooth()

# BsmtFinSF2
ggplot(data = dtrain, aes(x=BsmtFinSF2, y = SalePrice))+geom_point()+geom_smooth()

# TotalBsmtSF
ggplot(data = dtrain, aes(x=TotalBsmtSF, y = SalePrice))+geom_point()+geom_smooth()

# Heating
ggplot(data = dtrain, aes(x=Heating, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$Heating=recode(dtrain$Heating,"c('Floor','Grav','OthW','Wall')='Oth';c('GasW','GasA')='Gas'")

# HeatingQC
ggplot(data = dtrain, aes(x=HeatingQC, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$HeatingQC=recode(dtrain$HeatingQC,"c('Po')='Fa'")

# CentralAir
ggplot(data = dtrain, aes(x=CentralAir, y = SalePrice))+geom_point()+geom_boxplot()

# Electrical
ggplot(data = dtrain, aes(x=Electrical, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$Electrical=recode(dtrain$Electrical,"c('FuseF','FuseP','Mix')='FuseO'")

# 1stFlrSF
ggplot(data = dtrain, aes(x=`1stFlrSF`, y = SalePrice))+geom_point()+geom_smooth()

# 2ndFlrSF
ggplot(data = dtrain, aes(x=`2ndFlrSF`, y = SalePrice))+geom_point()+geom_smooth()

# LowQualFinSF
ggplot(data = dtrain, aes(x=LowQualFinSF, y = SalePrice))+geom_point()+geom_smooth()

# GrLivArea
ggplot(data = dtrain, aes(x=GrLivArea, y = SalePrice))+geom_point()+geom_smooth()

# BsmtFullBath
ggplot(data = dtrain, aes(x=BsmtFullBath, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$BsmtFullBath=recode(dtrain$BsmtFullBath,"c('2','3')='M'")

# BsmtHalfBath
ggplot(data = dtrain, aes(x=BsmtHalfBath, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$BsmtHalfBath=recode(dtrain$BsmtHalfBath,"c('1','2',NA)='P'")

# FullBath
ggplot(data = dtrain, aes(x=FullBath, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$FullBath=recode(dtrain$FullBath,"c('3','4')='M'")

# HalfBath
ggplot(data = dtrain, aes(x=HalfBath, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$HalfBath=recode(dtrain$HalfBath,"c('2')='1'")

# BedroomAbvGr
ggplot(data = dtrain, aes(x=BedroomAbvGr, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$BedroomAbvGr=recode(dtrain$BedroomAbvGr,"c('5','6','8')='M'")

# KitchenAbvGr
ggplot(data = dtrain, aes(x=KitchenAbvGr, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$KitchenAbvGr=recode(dtrain$KitchenAbvGr,"c('0','2','3')='Not1'")

# KitchenQual
ggplot(data = dtrain, aes(x=KitchenQual, y = SalePrice))+geom_point()+geom_boxplot()

# TotRmsAbvGrd
ggplot(data = dtrain, aes(x=TotRmsAbvGrd, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$TotRmsAbvGrd=recode(dtrain$TotRmsAbvGrd,"c('2','3','4')='LTE4';c('11','12','13','14','15')='GT10'")

# Functional
ggplot(data = dtrain, aes(x=Functional, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$Functional=recode(dtrain$Functional,"c('Maj1','Maj2','Sev','Sal')='Maj';c('Min1','Min2')='Min'")

# Fireplaces
ggplot(data = dtrain, aes(x=Fireplaces, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$Fireplaces=recode(dtrain$Fireplaces,"c('3','4')='GTE2'")

# FireplaceQu
ggplot(data = dtrain, aes(x=FireplaceQu, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$FireplaceQu=recode(dtrain$FireplaceQu,"c('Po')='Fa'")

# GarageType
ggplot(data = dtrain, aes(x=GarageType, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$GarageType=recode(dtrain$GarageType,"c('CarPort')='Detchd'")

# GarageYrBlt
ggplot(data = dtrain, aes(x=GarageYrBlt, y = SalePrice))+geom_point()+geom_smooth()

# GarageFinish
ggplot(data = dtrain, aes(x=GarageFinish, y = SalePrice))+geom_point()+geom_boxplot()

# GarageCars
ggplot(data = dtrain, aes(x=GarageCars, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$GarageCars=recode(dtrain$GarageCars,"c('4','5','3')='GTE3'")

# GarageArea 
ggplot(data = dtrain, aes(x=GarageArea, y = SalePrice))+geom_point()+geom_smooth()

# GarageQual
ggplot(data = dtrain, aes(x=GarageQual, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$GarageQual=recode(dtrain$GarageQual,"c('Ex')='Gd';'Po'='Fa'")

# GarageCond
ggplot(data = dtrain, aes(x=GarageCond, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$GarageCond=recode(dtrain$GarageCond,"c('Ex')='Gd';'Po'='Fa'")

# PavedDrive
ggplot(data = dtrain, aes(x=PavedDrive, y = SalePrice))+geom_point()+geom_boxplot()

# WoodDeckSF 
ggplot(data = dtrain, aes(x=WoodDeckSF, y = SalePrice))+geom_point()+geom_smooth()

# OpenPorchSF 
ggplot(data = dtrain, aes(x=OpenPorchSF, y = SalePrice))+geom_point()+geom_smooth()

# EnclosedPorch 
ggplot(data = dtrain, aes(x=EnclosedPorch, y = SalePrice))+geom_point()+geom_smooth()

# 3SsnPorch 
ggplot(data = dtrain, aes(x=`3SsnPorch`, y = SalePrice))+geom_point()+geom_smooth()

# ScreenPorch 
ggplot(data = dtrain, aes(x=ScreenPorch, y = SalePrice))+geom_point()+geom_smooth()

# PoolArea 
ggplot(data = dtrain, aes(x=PoolArea, y = SalePrice))+geom_point()+geom_smooth()

# Fence
ggplot(data = dtrain, aes(x=Fence, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$Fence=recode(dtrain$Fence,"c('GdPrv','GdWo')='Gd';c('MnPrv','MnWw')='Mn';NA='NotAvl'")

# MiscFeature
ggplot(data = dtrain, aes(x=MiscFeature, y = SalePrice))+geom_point()+geom_boxplot()

# MiscVal
ggplot(data = dtrain, aes(x=MiscVal, y = SalePrice))+geom_point()+geom_smooth()

# MoSold
ggplot(data = dtrain, aes(x=MoSold, y = SalePrice))+geom_point()+geom_boxplot()

# YrSold
ggplot(data = dtrain, aes(x=YrSold, y = SalePrice))+geom_point()+geom_boxplot()

# SaleType
ggplot(data = dtrain, aes(x=SaleType, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$SaleType=recode(dtrain$SaleType,"c('CWD','VWD')='WD';c('ConLD','COnLI','ConLw')='Con'")

# SaleCondition
ggplot(data = dtrain, aes(x=SaleCondition, y = SalePrice))+geom_point()+geom_boxplot()
dtrain$SaleCondition=recode(dtrain$SaleCondition,"c('AdjLand')='Alloca'")

# remove predictors with low variation
dtrain=dtrain[,!names(dtrain)%in% c('Street','Alley','Utilities','Condition2','RoofMatl','Heating','PoolQC')]
# rename predictors starting with numbers
names(dtrain)[names(dtrain)=='1stFlrSF']='Flr1stSF'
names(dtrain)[names(dtrain)=='2ndFlrSF']='Flr2ndSF'
names(dtrain)[names(dtrain)=='3SsnPorch']='Ssn3Porch'

## Find correlations among variables ####
# separate numerical and categorical predictors
dnum=dtrain[sapply(dtrain, is.numeric)]
dcat=dtrain[!sapply(dtrain, is.numeric)]
dtrain=cbind.data.frame(dcat,dnum)

# separate train and test sets
dtest=dtrain[is.na(dtrain$SalePrice),]
dtrain2=dtrain[complete.cases(dtrain$SalePrice),]
dtest$SalePrice=0

# split into train and validate
set.seed(2094)
prt=sample(seq_len(nrow(dtrain2)),ceiling(.65*nrow(dtrain2)),replace=F)
dt=dtrain2[prt,]
dv=dtrain2[-prt,]

# create normalized dataset ####
sc.max=apply(dnum[,-1], 2, max, na.rm=T) 
sc.min=apply(dnum[,-1], 2, min, na.rm=T)
dnum.norm <- as.data.frame(scale(dnum[,-1], center =sc.min, scale = sc.max - sc.min))
dtr.norm=cbind(dnum$Id,dcat,dnum.norm)

# separate train and test sets
dts.norm=dtr.norm[is.na(dtr.norm$SalePrice),]
dtr2.norm=dtr.norm[complete.cases(dtr.norm$SalePrice),]
dts.norm$SalePrice=0

# split into train and validate
set.seed(2094)
prt=sample(seq_len(nrow(dtr2.norm)),ceiling(.65*nrow(dtr2.norm)),replace=F)
dt.norm=dtr2.norm[prt,]
dv.norm=dtr2.norm[-prt,]

# find correlations
source("D:/R_Data/functions.R")
corrl(dnum[,-c(11,24)],0)
cor(dnum[,-c(1,24)])
scatterplotMatrix(dnum[,-c(1,24)])


# Build models with all predictors ####
# build Linear Reg model
lr1=lm(SalePrice~., data = dt[,-51])
summary(lr1)
lr1.p=predict(lr1, newdata = dv[,-51])
library(Metrics)
rmsle(dt$SalePrice,lr1.p)
error(dv$SalePrice,lr1.p,nrow(dv$SalePrice),ncol(dv[,-51]))

# Build xgboost model
# form sparse matrices
library(Matrix)
mt= sparse.model.matrix(SalePrice~.-1, data = dt[,-1])
mv=sparse.model.matrix(SalePrice~.-1, data =dv[,-1])

# Run xgBoost model to find feature importance ####
library(xgboost)
set.seed(1984)
parm=list("objective"="reg:linear","eta"=0.3, "max.depth"=6, "eval_metric"="rmse",
          "verbose"=2)
xgmod=xgboost(data = mt, label = dt$SalePrice, nrounds = 20, params = parm)
mod.cv=xgb.cv(data = mt, nfold = 10, label = dt$SalePrice, nrounds = 30, params = parm)
# plot line chart of error
library(ggplot2)
library(reshape2)
mod.cv$id=seq(from=0,to=29,by=1)
mod.cv=as.data.frame(mod.cv)
mod.cv.melt=melt(mod.cv[,c(1,3,5)], id.vars = "id", value.name = "value", variable.name = "rmse")
ggplot(data=mod.cv.melt, aes(x=id, y=value, group = rmse, colour = rmse)) + geom_line() 
#predict
xgmod.p=predict(xgmod, mv)
rmsle(dv$SalePrice,xgmod.p)
mtest=sparse.model.matrix(SalePrice~.-1, data =dtest[,-51])
xgmod.f=predict(xgmod,mtest)

## Second Run; Remove Correlated variables; Introduce interaction terms ####
# predictors to remove
prd.rmv=c('Id','GarageYrBlt','GrLivArea','YearRemodAdd','BsmtFinSF1','BsmtFinSF2','BsmtUnfSF')
# build Linear Reg model
lr2=lm(SalePrice~., data = dt[,!names(dt)%in%prd.rmv])
summary(lr1)
lr2.p=predict(lr2, newdata = dv[,!names(dv)%in%prd.rmv])
library(Metrics)
rmsle(dv$SalePrice,lr1.p)
error(dv$SalePrice,lr2.p,nrow(dv$SalePrice),ncol(dv[,!names(dv)%in%prd.rmv])-1)

# Neural Network
# create matrix of dummy variables
mtr1=model.matrix(~.-1,data = dt.norm)
df.dummy=as.data.frame(mtr1)
nom.vec=colnames(df.dummy)
nom.vec=make.names(nom.vec)  
colnames(df.dummy)=nom.vec
rm(nom.vec)
#build model
library(neuralnet)
n = names(df.dummy[,-1])
f = as.formula(paste("SalePrice ~", paste(n[!n %in% c('SalePrice')], collapse = " + ")))
modnn = neuralnet(f,data=df.dummy[,-1],hidden=c(15),rep=5,linear.output=T)

# predict on validation data
mtr2=model.matrix(~.-1,data = dv.norm)
df.dummy2=as.data.frame(mtr2)
nom.vec=colnames(df.dummy2)
nom.vec=make.names(nom.vec)  
colnames(df.dummy2)=nom.vec
rm(nom.vec)

modnn.p=compute(modnn,df.dummy2[,2:216])
modnn.p2=ifelse(modnn.p$net.result<0,0,modnn.p$net.result)
rmsle(dv$SalePrice,modnn.p2)

# plot actual v predicted
dtemp=as.data.frame(matrix(nrow = nrow(dv), ncol = 2))
colnames(dtemp)=c('Actual','Predicted')
dtemp$Actual=dv$SalePrice
dtemp$Predicted=xgmod.p
ggplot(dtemp, aes(x=Actual,y=Predicted))+geom_point()+geom_smooth()

## removing outliers ####
dtrain.v1=dtrain
dtrain=dtrain[(dtrain$SalePrice<350000),]
dtrain=dtrain[complete.cases(dtrain$SalePrice),]

# removing outlying points for predictors as required
ggplot(data = dtrain, aes(x=LotFrontage))+geom_density()
dtrain=dtrain[(dtrain$LotFrontage<160),]
dtrain=dtrain[(dtrain$LotArea<40000),]
dtrain=dtrain[(dtrain$MasVnrArea<750),]
dtrain=dtrain[(dtrain$TotalBsmtSF<2000),]
dtrain=dtrain[,!(names(dtrain)%in% c('LowQualFinSF'))]
dtrain=dtrain[(dtrain$GrLivArea<3000),]
dtrain=dtrain[,!(names(dtrain)%in% c('BsmtHalfBath'))]
dtrain=dtrain[(dtrain$GarageArea<1000),]
dtrain=dtrain[(dtrain$WoodDeckSF<500),]
dtrain=dtrain[(dtrain$OpenPorchSF<300),]
dtrain=dtrain[!(names(dtrain)%in% c('PoolArea'))]
dtrain=dtrain[!(names(dtrain)%in% c('Fence'))]
dtrain=dtrain[!(names(dtrain)%in% c('MiscVal'))]
dtrain=dtrain[!(names(dtrain)%in% c('MoSold'))]
dtrain=dtrain[!(names(dtrain)%in% c('YrSold'))]

#log-transform sale price
dtrain$SalePrice=log10(dtrain$SalePrice)

# remove above predictors from test data
prd.sel=colnames(dtrain)
dtest=dtest[,names(dtest)%in% prd.sel]

# split into test and validate
set.seed(2094)
prt=sample(seq_len(nrow(dtrain)),ceiling(.65*nrow(dtrain)),replace=F)
dt=dtrain[prt,]
dv=dtrain[-prt,]

# scale data
dnum=dt[sapply(dt, is.numeric)]
dcat=dt[!sapply(dt, is.numeric)]

sc.max=apply(dnum[,-1], 2, max, na.rm=T) 
sc.min=apply(dnum[,-1], 2, min, na.rm=T)
dnum.norm <- as.data.frame(scale(dnum[,-1], center =sc.min, scale = sc.max - sc.min))
dt.norm=cbind(dcat,dnum.norm)

#view correlations
crl=cor(dnum.norm)
corrplot(crl, method = "number", type = "upper")

# build linear regression model
lr3=lm(SalePrice~., data = dt.norm[,-c(7)])
# scale data
dnum.v=dv[sapply(dv, is.numeric)]
dcat.v=dv[!sapply(dv, is.numeric)]

sc.max.v=apply(dnum.v[,-1], 2, max, na.rm=T) 
sc.min.v=apply(dnum.v[,-1], 2, min, na.rm=T)
dnum.v.norm <- as.data.frame(scale(dnum.v[,-1], center =sc.min, scale = sc.max - sc.min))
dv.norm=cbind(dcat.v,dnum.v.norm)

# predict using LinR model
lr3.p=predict(lr3, newdata = dv.norm[,-c(7)])
rmsle(dv.norm$SalePrice,lr3.p)
# final prediction
dnum.f=dtest[sapply(dtest, is.numeric)]
dcat.f=dtest[!sapply(dtest, is.numeric)]
dnum.f.norm = as.data.frame(scale(dnum.f[,-1], center =sc.min, scale = sc.max - sc.min))
dtest.norm=cbind(dcat.f,dnum.f.norm)
lr3.f=predict(lr3, newdata = dtest.norm[,-c(7)])
lr3.f=(lr3.f*(sc.max[[20]] - sc.min[[20]]))+sc.min[[20]]
lr3.f=10^lr3.f #0.13652

# build new xgboost model
library(Matrix)
mt= sparse.model.matrix(SalePrice~.-1, data = dt.norm[,-7])
mv=sparse.model.matrix(SalePrice~.-1, data =dv.norm[,-7])

# Run xgBoost model to find feature importance ####
set.seed(1984)
parm=list("objective"="reg:linear","eta"=0.3, "max.depth"=6, "eval_metric"="rmse",
          "verbose"=2)
xgmod2=xgboost(data = mt, label = dt.norm$SalePrice, nrounds = 10, params = parm)
mod2.cv=xgb.cv(data = mt, nfold = 5, label = dt.norm$SalePrice, nrounds = 30, params = parm)
#predict
xgmod2.p=predict(xgmod2, mv)
rmsle(dv.norm$SalePrice,xgmod2.p)
mtest=sparse.model.matrix(SalePrice~.-1, data =dtest.norm[,-7])
xgmod2.f=predict(xgmod2,mtest)
# convert to original scale
xgmod2.f=(xgmod2.f*(sc.max[[20]] - sc.min[[20]]))+sc.min[[20]]
xgmod2.f=10^xgmod2.f #0.17684

# removing GarageYrBlt, YearRemodAdd, adding Neighbourhood ####
# build model
# build linear regression model
lr4=lm(SalePrice~., data = dt.norm[,-c(50,59)])
lr4.p=predict(lr4, newdata = dv.norm[,-c(50,59)])
rmsle(dv.norm$SalePrice,lr4.p)

# final prediction
lr4.f=predict(lr4, newdata = dtest.norm[,-c(50,59)])
lr4.f=(lr4.f*(sc.max[[20]] - sc.min[[20]]))+sc.min[[20]]
lr4.f=10^lr4.f #0.13273

# run stepwise regression
lrfull=lm(SalePrice~., data = dt.norm)
lrnull=lm(SalePrice~1, data = dt.norm)
lrstep=step(lrfull, scope = list("lrfull","lrnull"), direction = "both")
lrstep.p=predict(lrstep, newdata=dv.norm)
rmsle(dv.norm$SalePrice,lrstep.p)
# final prediction
lrstep.f=predict(lrstep, newdata = dtest.norm)
lrstep.f=(lrstep.f*(sc.max[[20]] - sc.min[[20]]))+sc.min[[20]]
lrstep.f=10^lrstep.f #0.13652
