## ---------------------------------------------------------------------------------------------------------------------
library(readxl)
HUN_FIN_2012_R <- read_excel("C:/Users/bokan/Desktop/Egyetem/Szakdoga/Szakdolgozat/Adatbazis/HUN_FIN_2012_R.xlsx", 
    col_types = c("numeric", "text", "text", 
        "text", "text", "text", "text", "text", 
        "text", "text", "text", "text", "text", 
        "text", "text", "text", "text", "text", 
        "text", "text", "text", "text", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric"))
library("mice")
library("dplyr")
library("car")
library("HH")
library("lmtest")
library("sandwich")
library("corrplot")


## ---- message=FALSE---------------------------------------------------------------------------------------------------
imp_single12 <- mice(HUN_FIN_2012_R, m = 1, method = "pmm")
PISA2012IMP <- complete(imp_single12)



## ---------------------------------------------------------------------------------------------------------------------
TreatsFIN <- subset(PISA2012IMP,  PISA2012IMP$CNT == "FIN", select = -c(CNT))
ControlHUN <- subset(PISA2012IMP,  PISA2012IMP$CNT == "HUN", select = -c(CNT))
summary(TreatsFIN$PV1MATH)
summary(ControlHUN$PV1MATH)
summary(TreatsFIN$PV1READ)
summary(ControlHUN$PV1READ)
summary(TreatsFIN$PV1SCIE)
summary(ControlHUN$PV1SCIE)


## ---------------------------------------------------------------------------------------------------------------------
library(dplyr)
PISA2012 <- data.frame(PISA2012IMP)
PISA2012IMP %>% dplyr::select(CNT) -> PISA2012
PISA2012$Nem = factor(PISA2012IMP$Nem,levels = c(0, 1),labels = c('Nő', 'Férfi'))
for (i in 1:length(PISA2012$CNT))
{
	if (PISA2012IMP$IskKer_Rit[i] ==0 && PISA2012IMP$IskKer_Gyak[i] == 0)
	{
		PISA2012$IskKer[i] = 'Nincs'
	}
    else
    {
        if  (PISA2012IMP$IskKer_Rit[i] ==1)
        {
            PISA2012$IskKer[i] = 'Ritka'
        }
        else
        {
            PISA2012$IskKer[i] = 'Gyakori'
        }
    }
}
for (i in 1:length(PISA2012$CNT))
{
	if (PISA2012IMP$K_25_100[i] ==0 && PISA2012IMP$K_100_200[i] == 0 && PISA2012IMP$K_200_500[i] == 0)
	{
		PISA2012$Könyvek[i] = '0-25'
	}
    else
    {
        if  (PISA2012IMP$K_25_100[i] ==1)
        {
            PISA2012$Könyvek[i] = '25-100'
        }
        else
        {
           if (PISA2012IMP$K_100_200[i] ==1)
           {
               PISA2012$Könyvek[i] = '100-200'
           }
               else
               {
                   PISA2012$Könyvek[i] = '200-500'
               }
           }
        }
}
for (i in 1:length(PISA2012$CNT))
{
	if (PISA2012IMP$TT_Koz[i] ==0 && PISA2012IMP$TT_Kiv[i] == 0)
	{
		PISA2012$TT[i] = 'Rossz'
	}
    else
    {
        if  (PISA2012IMP$TT_Koz[i] ==1)
        {
            PISA2012$TT[i] = 'Közepes'
        }
        else
        {
            PISA2012$TT[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2012$CNT))
{
	if (PISA2012IMP$OV_Koz[i] ==0 && PISA2012IMP$OV_Kiv[i] == 0)
	{
		PISA2012$OV[i] = 'Rossz'
	}
    else
    {
        if  (PISA2012IMP$OV_Koz[i] ==1)
        {
            PISA2012$OV[i] = 'Közepes'
        }
        else
        {
            PISA2012$OV[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2012$CNT))
{
	if (PISA2012IMP$TS_Koz[i] ==0 && PISA2012IMP$TS_Kiv[i] == 0)
	{
		PISA2012$TS[i] = 'Rossz'
	}
    else
    {
        if  (PISA2012IMP$TS_Koz[i] ==1)
        {
            PISA2012$TS[i] = 'Közepes'
        }
        else
        {
            PISA2012$TS[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2012$CNT))
{
	if (PISA2012IMP$IK_Koz[i] ==0 && PISA2012IMP$IK_Kiv[i] == 0)
	{
		PISA2012$IK[i] = 'Rossz'
	}
    else
    {
        if  (PISA2012IMP$IK_Koz[i] ==1)
        {
            PISA2012$IK[i] = 'Közepes'
        }
        else
        {
            PISA2012$IK[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2012$CNT))
{
	if (PISA2012IMP$Isk_Munk_Koz[i] ==0 && PISA2012IMP$Isk_Munk_Kiv[i] == 0)
	{
		PISA2012$Isk_Munk[i] = 'Rossz'
	}
    else
    {
        if  (PISA2012IMP$Isk_Munk_Koz[i] ==1)
        {
            PISA2012$Isk_Munk[i] = 'Közepes'
        }
        else
        {
            PISA2012$Isk_Munk[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2012$CNT))
{
	if (PISA2012IMP$OD_Koz[i] ==0 && PISA2012IMP$OD_Kiv[i] == 0)
	{
		PISA2012$OD[i] = 'Rossz'
	}
    else
    {
        if  (PISA2012IMP$OD_Koz[i] ==1)
        {
            PISA2012$OD[i] = 'Közepes'
        }
        else
        {
            PISA2012$OD[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2012$CNT))
{
	if (PISA2012IMP$ID_Koz[i] ==0 && PISA2012IMP$ID_Kiv[i] == 0)
	{
		PISA2012$ID[i] = 'Rossz'
	}
    else
    {
        if  (PISA2012IMP$ID_Koz[i] ==1)
        {
            PISA2012$ID[i] = 'Közepes'
        }
        else
        {
            PISA2012$ID[i] = 'Kiváló'
        }
    }
}
PISA2012$FISCED <- PISA2012IMP$FISCED
PISA2012$MISCED <- PISA2012IMP$MISCED
PISA2012$BFMJ2 <- PISA2012IMP$BFMJ2
PISA2012$BMMJ1 <- PISA2012IMP$BMMJ1
PISA2012$ESCS <- PISA2012IMP$ESCS
PISA2012$HEDRES <- PISA2012IMP$HEDRES
PISA2012$HOMEPOS <- PISA2012IMP$HOMEPOS
PISA2012$ICTHOME <- PISA2012IMP$ICTHOME
PISA2012$ICTSCH <- PISA2012IMP$ICTSCH
PISA2012$TEACHSUP <- PISA2012IMP$TEACHSUP
PISA2012$WEALTH <- PISA2012IMP$WEALTH
PISA2012$PV1MATH <- PISA2012IMP$PV1MATH
PISA2012$PV1READ <- PISA2012IMP$PV1READ
PISA2012$PV1SCIE <- PISA2012IMP$PV1SCIE
#Factor kell a Random Foresthez
PISA2012$CNT <- as.factor(PISA2012$CNT)
PISA2012$IskKer <- as.factor(PISA2012$IskKer)
PISA2012$Könyvek <- as.factor(PISA2012$Könyvek)
PISA2012$TT <- as.factor(PISA2012$TT)
PISA2012$OV <- as.factor(PISA2012$OV)
PISA2012$TS <- as.factor(PISA2012$TS)
PISA2012$IK <- as.factor(PISA2012$IK)
PISA2012$Isk_Munk <- as.factor(PISA2012$Isk_Munk)
PISA2012$OD <- as.factor(PISA2012$OD)
PISA2012$ID <- as.factor(PISA2012$ID)


## ---------------------------------------------------------------------------------------------------------------------
library("rpart")
library("rpart.plot")
fa <- rpart(CNT~., data=PISA2012, cp=0.03350433  )
prp(fa)
rpart.plot(fa)


## ---------------------------------------------------------------------------------------------------------------------
library("randomForest")
PISA2012$CNT = factor(PISA2012IMP$CNT,levels = c('HUN', 'FIN'),labels = c(0, 1))
erdo <- randomForest(CNT~., data=PISA2012,ntree=600, mtry=34)
plot(erdo)
varImpPlot(erdo)


## ---------------------------------------------------------------------------------------------------------------------
library(caret)
keresztval <- trainControl(method="cv", number=10)
train(CNT~., data=PISA2012, method="rpart", trControl=keresztval)
train(CNT~., data=PISA2012, method="rf", trConrtol=keresztval) #mtry 34 a legjobb 0.9687285 accuracy �rt�kkel


## ---------------------------------------------------------------------------------------------------------------------
PS <- erdo$votes[,2]
PS <- data.frame(PS)
hist(PS$PS[PISA2012$CNT==1], ,main = "Pscores of Response = FIN")
hist(PS$PS[PISA2012$CNT==0], ,main = "Pscores of Response = HUN")


## ---------------------------------------------------------------------------------------------------------------------
xvars <- c("Nem","IskKer","Könyvek","TT","OV","TS","IK","Isk_Munk","OD","ID","FISCED","MISCED","BFMJ2","BMMJ1","ESCS","HEDRES","HOMEPOS","ICTHOME","ICTSCH","TEACHSUP", "WEALTH")
library(tableone)
table1 <- CreateTableOne(vars = xvars,strata = "CNT",data = PISA2012, test = FALSE)
print(table1, smd = TRUE)


## ---------------------------------------------------------------------------------------------------------------------
library("MatchIt")
scores <- c(PS$PS)
#match <- matchit(CNT~., data = PISA2012, distance = PS$PS, method = "nearest", ratio = 2, replace = T, caliper = 0.1)
match <- matchit(CNT~.-PV1MATH-PV1READ-PV1SCIE, data = PISA2012, distance="randomforest", mtry=34, method = "full")
plot(match, type="jitter")
plot(match, type="hist")
summary(match, standardized=T)
summary(match, standardized=T) -> BalanceNN
TableNN <- BalanceNN$sum.matched


## ---------------------------------------------------------------------------------------------------------------------
summary(match, standardized=T) -> fullmatching
tablefull <- fullmatching$sum.matched
match.data(match) -> data.full
table(data.full$subclass)
table(data.full$weights)
sum(data.full$weights)
library(survey)
design.full <- svydesign(ids=~1, weights=~weights, data=data.full)
model.full <- svyglm(PV1MATH~CNT, design.full, family = gaussian())
summary(model.full)
#2
design.full2 <- svydesign(ids=~subclass, weights=~weights, data=data.full)
design.full2 <- as.svrepdesign(design.full2, type="bootstrap", replicates=1000)
model.full2 <- svyglm(PV1MATH~CNT, design.full2, family = gaussian())
summary(model.full2)


## ---------------------------------------------------------------------------------------------------------------------
matchdata <- match.data(match)
table_match2 <- CreateTableOne(vars = xvars,strata = "CNT",data = matchdata,test = FALSE)
print(table_match2, smd = TRUE)
library(cobalt)
bal.tab(match, treshold=0.1)


## ---------------------------------------------------------------------------------------------------------------------
library(cobalt)
bal.plot(match,var.name = 'PV1MATH',which = 'both')
bal.plot(match,var.name = 'PV1READ',which = 'both')
bal.plot(match,var.name = 'PV1SCIE',which = 'both')
bal.plot(match,var.name = 'ID',which = 'both')

## ---------------------------------------------------------------------------------------------------------------------
with(matchdata, t.test(PV1MATH ~ CNT))
with(PISA2012, t.test(PV1MATH ~ CNT))


## ---------------------------------------------------------------------------------------------------------------------
with(matchdata, t.test(WEALTH ~ CNT))


## ---------------------------------------------------------------------------------------------------------------------
mean(matchdata$PV1MATH[matchdata$CNT == 1]) - mean(matchdata$PV1MATH[matchdata$CNT == 0])


## ---------------------------------------------------------------------------------------------------------------------
m.sum <- summary(match)
plot(m.sum, var.order = "unmatched")
love.plot(match, binary = "std")
