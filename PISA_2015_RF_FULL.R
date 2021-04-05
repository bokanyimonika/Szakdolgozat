## ---------------------------------------------------------------------------------------------------------------------
library(readxl)
HUN_FIN_2015_R <- read_excel("C:/Users/bokan/Desktop/Egyetem/Szakdoga/Szakdolgozat/Adatbazis/HUN_FIN_2015_R.xlsx")
library("mice")
library("dplyr")
library("car")
library("HH")
library("lmtest")
library("sandwich")
library("corrplot")


## ---- message=FALSE---------------------------------------------------------------------------------------------------
imp_single15 <- mice(HUN_FIN_2015_R, m = 1, method = "pmm")
PISA2015IMP <- complete(imp_single15)



## ---------------------------------------------------------------------------------------------------------------------
TreatsFIN <- subset(PISA2015IMP,  PISA2015IMP$CNT == "FIN", select = -c(CNT))
ControlHUN <- subset(PISA2015IMP,  PISA2015IMP$CNT == "HUN", select = -c(CNT))
summary(TreatsFIN$PV1MATH)
summary(ControlHUN$PV1MATH)
summary(TreatsFIN$PV1READ)
summary(ControlHUN$PV1READ)
summary(TreatsFIN$PV1SCIE)
summary(ControlHUN$PV1SCIE)


## ---------------------------------------------------------------------------------------------------------------------
library(dplyr)
PISA2015IMP %>% dplyr::select(CNT) -> PISA2015
PISA2015$Nem = factor(PISA2015IMP$Nem,levels = c(0, 1),labels = c('Nő', 'Férfi'))
for (i in 1:length(PISA2015$CNT))
{
	if (PISA2015IMP$OF_Jo[i] ==0 && PISA2015IMP$OF_Kiv[i] == 0)
	{
		PISA2015$OF[i] = 'Rossz'
	}
    else
    {
        if  (PISA2015IMP$OF_Jo[i] ==1)
        {
            PISA2015$OF[i] = 'Jó'
        }
        else
        {
            PISA2015$OF[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2015$CNT))
{
	if (PISA2015IMP$K_25_100[i] ==0 && PISA2015IMP$K_100_200[i] == 0 && PISA2015IMP$K_200_500[i] == 0)
	{
		PISA2015$Könyvek[i] = '0-25'
	}
    else
    {
        if  (PISA2015IMP$K_25_100[i] ==1)
        {
            PISA2015$Könyvek[i] = '25-100'
        }
        else
        {
           if (PISA2015IMP$K_100_200[i] ==1)
           {
               PISA2015$Könyvek[i] = '100-200'
           }
               else
               {
                   PISA2015$Könyvek[i] = '200-500'
               }
           }
        }
}
for (i in 1:length(PISA2015$CNT))
{
	if (PISA2015IMP$SzT_Koz[i] ==0 && PISA2015IMP$SzT_Kiv[i] == 0)
	{
		PISA2015$SzT[i] = 'Rossz'
	}
    else
    {
        if  (PISA2015IMP$SzT_Koz[i] ==1)
        {
            PISA2015$SzT[i] = 'Közepes'
        }
        else
        {
            PISA2015$SzT[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2015$CNT))
{
	if (PISA2015IMP$IK_Koz[i] ==0 && PISA2015IMP$IK_Jo[i] == 0 && PISA2015IMP$IK_Kiv[i] == 0)
	{
		PISA2015$IK[i] = 'Rossz'
	}
    else
    {
        if  (PISA2015IMP$IK_Koz[i] ==1)
        {
            PISA2015$IK[i] = 'Közepes'
        }
        else
        {
           if (PISA2015IMP$IK_Jo[i] ==1)
           {
               PISA2015$IK[i] = 'Jó'
           }
               else
               {
                   PISA2015$IK[i] = 'Kiváló'
               }
           }
        }
}
for (i in 1:length(PISA2015$CNT))
{
	if (PISA2015IMP$TB_Koz[i] ==0 && PISA2015IMP$TB_Nincs[i] == 0)
	{
		PISA2015$TB[i] = 'Van'
	}
    else
    {
        if  (PISA2015IMP$TB_Koz[i] ==1)
        {
            PISA2015$TB[i] = 'Közepes'
        }
        else
        {
            PISA2015$TB[i] = 'Nincs'
        }
    }
}
for (i in 1:length(PISA2015$CNT))
{
	if (PISA2015IMP$OV_Koz[i] ==0 && PISA2015IMP$OV_Kiv[i] == 0)
	{
		PISA2015$OV[i] = 'Rossz'
	}
    else
    {
        if  (PISA2015IMP$OV_Koz[i] ==1)
        {
            PISA2015$OV[i] = 'Közepes'
        }
        else
        {
            PISA2015$OV[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2015$CNT))
{
	if (PISA2015IMP$TK_Koz[i] ==0 && PISA2015IMP$TK_Kiv[i] == 0)
	{
		PISA2015$TK[i] = 'Rossz'
	}
    else
    {
        if  (PISA2015IMP$TK_Koz[i] ==1)
        {
            PISA2015$TK[i] = 'Közepes'
        }
        else
        {
            PISA2015$TK[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2015$CNT))
{
	if (PISA2015IMP$TS_Koz[i] ==0 && PISA2015IMP$TS_Kiv[i] == 0)
	{
		PISA2015$TS[i] = 'Rossz'
	}
    else
    {
        if  (PISA2015IMP$TS_Koz[i] ==1)
        {
            PISA2015$TS[i] = 'Közepes'
        }
        else
        {
            PISA2015$TS[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2015$CNT))
{
	if (PISA2015IMP$TO_Koz[i] ==0 && PISA2015IMP$TO_Kiv[i] == 0)
	{
		PISA2015$TO[i] = 'Rossz'
	}
    else
    {
        if  (PISA2015IMP$TO_Koz[i] ==1)
        {
            PISA2015$TO[i] = 'Közepes'
        }
        else
        {
            PISA2015$TO[i] = 'Kiváló'
        }
    }
}
PISA2015$TanÉsMunka = factor(PISA2015IMP$T_M_Jo,levels = c(0, 1),labels = c('Rossz', 'Jó'))
for (i in 1:length(PISA2015$CNT))
{
	if (PISA2015IMP$OED_Koz[i] ==0 && PISA2015IMP$OED_Sok[i] == 0)
	{
		PISA2015$OED[i] = 'Kevés'
	}
    else
    {
        if  (PISA2015IMP$OED_Koz[i] ==1)
        {
            PISA2015$OED[i] = 'Közepes'
        }
        else
        {
            PISA2015$OED[i] = 'Sok'
        }
    }
}
for (i in 1:length(PISA2015$CNT))
{
	if (PISA2015IMP$DE_Kev[i] ==0 && PISA2015IMP$DE_Van[i] == 0)
	{
		PISA2015$DE[i] = 'Nincs'
	}
    else
    {
        if  (PISA2015IMP$DE_Kev[i] ==1)
        {
            PISA2015$DE[i] = 'Van'
        }
        else
        {
            PISA2015$DE[i] = 'Sok'
        }
    }
}
PISA2015$WeeklyClass <- PISA2015IMP$WeeklyClass
PISA2015$MISCED <- PISA2015IMP$MISCED
PISA2015$FISCED <- PISA2015IMP$FISCED
PISA2015$BMMJ_Mother <- PISA2015IMP$BMMJ_Mother
PISA2015$BFMJ_Father <- PISA2015IMP$BFMJ_Father
PISA2015$MINS_Maths <- PISA2015IMP$MINS_Maths
PISA2015$MINS_Language <- PISA2015IMP$MINS_Language
PISA2015$ESCS <- PISA2015IMP$ESCS
PISA2015$ICT_Home <- PISA2015IMP$ICT_Home
PISA2015$ICT_School <- PISA2015IMP$ICT_School
PISA2015$WLE_Cultural <- PISA2015IMP$WLE_Cultural
PISA2015$WLE_Home <- PISA2015IMP$WLE_Home
PISA2015$WLE_Wealth <- PISA2015IMP$WLE_Wealth
PISA2015$WLE_INSTRUCTION <- PISA2015IMP$WLE_INSTRUCTION
PISA2015$WLE_TeacherFairness <- PISA2015IMP$WLE_TeacherFairness
PISA2015$WLE_Emotional <- PISA2015IMP$WLE_Emotional
PISA2015$WLE_SchoolWellBeing <- PISA2015IMP$WLE_SchoolWellBeing
PISA2015$CPSVALUE <- PISA2015IMP$CPSVALUE
PISA2015$PERFEED <- PISA2015IMP$PERFEED
PISA2015$PV1READ <- PISA2015IMP$PV1READ
PISA2015$PV1MATH <- PISA2015IMP$PV1MATH
PISA2015$PV1SCIE <- PISA2015IMP$PV1SCIE
#Factor kell a Random Foresthez
PISA2015$CNT <- as.factor(PISA2015$CNT)
PISA2015$OF <- as.factor(PISA2015$OF)
PISA2015$Könyvek <- as.factor(PISA2015$Könyvek)
PISA2015$SzT <- as.factor(PISA2015$SzT)
PISA2015$IK <- as.factor(PISA2015$IK)
PISA2015$TB <- as.factor(PISA2015$TB)
PISA2015$OV <- as.factor(PISA2015$OV)
PISA2015$TK <- as.factor(PISA2015$TK)
PISA2015$TS <- as.factor(PISA2015$TS)
PISA2015$TO <- as.factor(PISA2015$TO)
PISA2015$TanÉsMunka <- as.factor(PISA2015$TanÉsMunka)
PISA2015$OED <- as.factor(PISA2015$OED)
PISA2015$DE <- as.factor(PISA2015$DE)


## ---------------------------------------------------------------------------------------------------------------------
library("rpart")
library("rpart.plot")
fa <- rpart(CNT~., data=PISA2015, cp=0.01247494)
prp(fa)
rpart.plot(fa)


## ---------------------------------------------------------------------------------------------------------------------
library("randomForest")
PISA2015$CNT = factor(PISA2015IMP$CNT,levels = c('HUN', 'FIN'),labels = c(0, 1))
erdo <- randomForest(CNT~., data=PISA2015, mtry=25) 
plot(erdo) 
varImpPlot(erdo)


## ---------------------------------------------------------------------------------------------------------------------
library(caret)
keresztval <- trainControl(method="cv", number=10)
train(CNT~., data=PISA2015, method="rpart", trControl=keresztval)
train(CNT~., data=PISA2015, method="rf", trConrtol=keresztval) #mtry 25 a legjobb, 0.9011904 accuracy


## ---------------------------------------------------------------------------------------------------------------------
PS <- erdo$votes[,2]
PS <- data.frame(PS)
PS$PS <- PS$PS
hist(PS$PS[PISA2015$CNT==1], ,main = "Pscores of Response = FIN")
hist(PS$PS[PISA2015$CNT==0], ,main = "Pscores of Response = HUN")


## ---------------------------------------------------------------------------------------------------------------------
xvars <- c("Nem","OF","Könyvek","SzT","IK","TB","OV","TK","TS","TO","TanÉsMunka","OED","DE","WeeklyClass","MISCED","FISCED","BMMJ_Mother","BFMJ_Father","MINS_Maths","MINS_Language","ESCS","ICT_Home","ICT_School","WLE_Cultural","WLE_Home","WLE_Wealth","WLE_INSTRUCTION","WLE_TeacherFairness","WLE_Emotional","WLE_SchoolWellBeing","CPSVALUE","PERFEED")
library(tableone)
table1 <- CreateTableOne(vars = xvars,strata = "CNT",data = PISA2015, test = FALSE)
print(table1, smd = TRUE)


## ---------------------------------------------------------------------------------------------------------------------
library("MatchIt")
#match <- matchit(CNT~., data = PISA2015, distance=PS$PS, method = "nearest")
match <- matchit(CNT~.-PV1MATH-PV1READ-PV1SCIE, data = PISA2015, distance="randomforest", mtry=25, method = "full")
plot(match, type="jitter")
plot(match, type="hist")
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
bal.plot(match,var.name = 'OED',which = 'both')


## ---------------------------------------------------------------------------------------------------------------------
plot(match, type = "qq", which.xs = c("WLE_SchoolWellBeing","ICT_Home"))
plot(match, type = "ecdf", which.xs = c("BFMJ_Father","TB"))



## ---------------------------------------------------------------------------------------------------------------------
with(matchdata, t.test(PV1MATH ~ CNT))


## ---------------------------------------------------------------------------------------------------------------------
mean(matchdata$PV1MATH[matchdata$CNT == 1]) - mean(matchdata$PV1MATH[matchdata$CNT == 0])


## ---------------------------------------------------------------------------------------------------------------------
m.sum <- summary(match)
plot(m.sum, var.order = "unmatched")
love.plot(match, binary = "std")

