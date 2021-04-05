## ---------------------------------------------------------------------------------------------------------------------
library(readxl)
HUN_FIN_2018_R <- read_excel("C:/Users/bokan/Desktop/Egyetem/Szakdoga/Szakdolgozat/Adatbazis/HUN_FIN_2018_R.xlsx")
HUN_FIN_2018_R$Sorszám <- NULL
library("mice")
library("dplyr")
library("car")
library("HH")
library("lmtest")
library("sandwich")
library("corrplot")


## ---- message=FALSE---------------------------------------------------------------------------------------------------
imp_single18 <- mice(HUN_FIN_2018_R, m = 1, method = "pmm")
PISA2018IMP <- complete(imp_single18)
PISA2018IMP$CNT = factor(PISA2018IMP$CNT,levels = c('HUN', 'FIN'),labels = c(0, 1))
names(PISA2018IMP)[13] <- "KO_Koz"
names(PISA2018IMP)[47] <- "WLE_TeacherInterest"
names(PISA2018IMP)[50] <- "WLE_SubjectiveWellBeing"


## ---------------------------------------------------------------------------------------------------------------------
library(dplyr)
PISA2018IMP %>% dplyr::select(CNT) -> PISA2018
PISA2018$Nem = factor(PISA2018IMP$Nem,levels = c(0, 1),labels = c('Nő', 'Férfi'))
PISA2018$CNT = factor(PISA2018IMP$CNT,levels = c(0, 1),labels = c('HUN', 'FIN'))
for (i in 1:length(PISA2018$CNT))
{
	if (PISA2018IMP$OF_Jo[i] ==0 && PISA2018IMP$OF_Kiv[i] == 0)
	{
		PISA2018$OF[i] = 'Rossz'
	}
    else
    {
        if  (PISA2018IMP$OF_Jo[i] ==1)
        {
            PISA2018$OF[i] = 'Jó'
        }
        else
        {
            PISA2018$OF[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2018$CNT))
{
	if (PISA2018IMP$K_25_100[i] ==0 && PISA2018IMP$K_100_200[i] == 0 && PISA2018IMP$K_200_500[i] == 0)
	{
		PISA2018$Könyvek[i] = '0-25'
	}
    else
    {
        if  (PISA2018IMP$K_25_100[i] ==1)
        {
            PISA2018$Könyvek[i] = '25-100'
        }
        else
        {
           if (PISA2018IMP$K_100_200[i] ==1)
           {
               PISA2018$Könyvek[i] = '100-200'
           }
               else
               {
                   PISA2018$Könyvek[i] = '200-500'
               }
           }
        }
}
for (i in 1:length(PISA2018$CNT))
{
	if (PISA2018IMP$TH_Koz[i] ==0 && PISA2018IMP$TH_Kiv[i] == 0)
	{
		PISA2018$TH[i] = 'Rossz'
	}
    else
    {
        if  (PISA2018IMP$TH_Koz[i] ==1)
        {
            PISA2018$TH[i] = 'Közepes'
        }
        else
        {
            PISA2018$TH[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2018$CNT))
{
	if (PISA2018IMP$TT_Koz[i] ==0 && PISA2018IMP$TT_Jo[i] == 0 && PISA2018IMP$TT_Kiv[i] == 0)
	{
		PISA2018$TT[i] = 'Rossz'
	}
    else
    {
        if  (PISA2018IMP$TT_Koz[i] ==1)
        {
            PISA2018$TT[i] = 'Közepes'
        }
        else
        {
           if (PISA2018IMP$TT_Jo[i] ==1)
           {
               PISA2018$TT[i] = 'Jó'
           }
               else
               {
                   PISA2018$TT[i] = 'Kiváló'
               }
           }
        }
}
for (i in 1:length(PISA2018$CNT))
{
	if (PISA2018IMP$KO_Koz[i] ==0 && PISA2018IMP$KO_Kiv[i] == 0)
	{
		PISA2018$KO[i] = 'Nincs'
	}
    else
    {
        if  (PISA2018IMP$KO_Koz[i] ==1)
        {
            PISA2018$KO[i] = 'Közepes'
        }
        else
        {
            PISA2018$KO[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2018$CNT))
{
	if (PISA2018IMP$OI_Koz[i] ==0 && PISA2018IMP$OI_Sok[i] == 0)
	{
		PISA2018$OI[i] = 'Kevés'
	}
    else
    {
        if  (PISA2018IMP$OI_Koz[i] ==1)
        {
            PISA2018$OI[i] = 'Közepes'
        }
        else
        {
            PISA2018$OI[i] = 'Sok'
        }
    }
}
for (i in 1:length(PISA2018$CNT))
{
	if (PISA2018IMP$IK_Koz[i] ==0 && PISA2018IMP$IK_Jo[i] == 0 && PISA2018IMP$IK_Kiv[i] == 0)
	{
		PISA2018$IK[i] = 'Rossz'
	}
    else
    {
        if  (PISA2018IMP$IK_Koz[i] ==1)
        {
            PISA2018$IK[i] = 'Közepes'
        }
        else
        {
           if (PISA2018IMP$IK_Jo[i] ==1)
           {
               PISA2018$IK[i] = 'Jó'
           }
               else
               {
                   PISA2018$IK[i] = 'Kiváló'
               }
           }
        }
}
for (i in 1:length(PISA2018$CNT))
{
	if (PISA2018IMP$SzT_Koz[i] ==0 && PISA2018IMP$SzT_Kiv[i] == 0)
	{
		PISA2018$SzT[i] = 'Rossz'
	}
    else
    {
        if  (PISA2018IMP$SzT_Koz[i] ==1)
        {
            PISA2018$SzT[i] = 'Közepes'
        }
        else
        {
            PISA2018$SzT[i] = 'Kiváló'
        }
    }
}
for (i in 1:length(PISA2018$CNT))
{
	if (PISA2018IMP$Ver_Koz[i] ==0 && PISA2018IMP$Ver_Nagy[i] == 0)
	{
		PISA2018$Ver[i] = 'Kicsi'
	}
    else
    {
        if  (PISA2018IMP$Ver_Koz[i] ==1)
        {
            PISA2018$Ver[i] = 'Közepes'
        }
        else
        {
            PISA2018$Ver[i] = 'Nagy'
        }
    }
}
for (i in 1:length(PISA2018$CNT))
{
	if (PISA2018IMP$Bull_Soh[i] ==0 && PISA2018IMP$Bull_Neh[i] == 0)
	{
		PISA2018$Bull[i] = 'Gyakran'
	}
    else
    {
        if  (PISA2018IMP$Bull_Neh[i] ==1)
        {
            PISA2018$Bull[i] = 'Néha'
        }
        else
        {
            PISA2018$Bull[i] = 'Soha'
        }
    }
}
for (i in 1:length(PISA2018$CNT))
{
	if (PISA2018IMP$OED_Koz[i] ==0 && PISA2018IMP$OED_Sok[i] == 0)
	{
		PISA2018$OED[i] = 'Kevés'
	}
    else
    {
        if  (PISA2018IMP$OED_Koz[i] ==1)
        {
            PISA2018$OED[i] = 'Közepes'
        }
        else
        {
            PISA2018$OED[i] = 'Sok'
        }
    }
}
for (i in 1:length(PISA2018$CNT))
{
	if (PISA2018IMP$DE_Van[i] ==0 && PISA2018IMP$DE_Kev[i] == 0)
	{
		PISA2018$DE[i] = 'Nincs'
	}
    else
    {
        if  (PISA2018IMP$DE_Kev[i] ==1)
        {
            PISA2018$DE[i] = 'Kevés'
        }
        else
        {
            PISA2018$DE[i] = 'Sok'
        }
    }
}
PISA2018$Satisfact <- PISA2018IMP$Satisfact
PISA2018$WeeklyClass <- PISA2018IMP$WeeklyClass
PISA2018$MISCED <- PISA2018IMP$MISCED
PISA2018$FISCED <- PISA2018IMP$FISCED
PISA2018$BMMJ_Mother <- PISA2018IMP$BMMJ_Mother
PISA2018$BFMJ_Father <- PISA2018IMP$BFMJ_Father
PISA2018$MINS_Maths <- PISA2018IMP$MINS_Maths
PISA2018$MINS_Language <- PISA2018IMP$MINS_Language
PISA2018$ESCS <- PISA2018IMP$ESCS
PISA2018$ICT_Home <- PISA2018IMP$ICT_Home
PISA2018$ICT_School <- PISA2018IMP$ICT_School
PISA2018$WLE_Cultural <- PISA2018IMP$WLE_Cultural
PISA2018$WLE_Home <- PISA2018IMP$WLE_Home
PISA2018$WLE_Wealth <- PISA2018IMP$WLE_Wealth
PISA2018$WLE_TeacherSupport <- PISA2018IMP$WLE_TeacherSupport
PISA2018$WLE_TeacherInstruction <- PISA2018IMP$WLE_TeacherInstruction
PISA2018$WLE_Emotional <- PISA2018IMP$WLE_Emotional
PISA2018$WLE_TeacherInterest <- PISA2018IMP$WLE_TeacherInterest
PISA2018$WLE_Attitude <- PISA2018IMP$WLE_Attitude
PISA2018$WLE_Competitiveness <- PISA2018IMP$WLE_Competitiveness
PISA2018$WLE_SubjectiveWellBeing <- PISA2018IMP$WLE_SubjectiveWellBeing
PISA2018$WLE_GoalOrientation <- PISA2018IMP$WLE_GoalOrientation
PISA2018$WLE_SchoolWellBeing <- PISA2018IMP$WLE_SchoolWellBeing
PISA2018$PERCOOP <- PISA2018IMP$PERCOOP
PISA2018$GFOFAIL <- PISA2018IMP$GFOFAIL
PISA2018$PERFEED <- PISA2018IMP$PERFEED
PISA2018$RESILIENCE <- PISA2018IMP$RESILIENCE
PISA2018$PV1READ <- PISA2018IMP$PV1READ
PISA2018$PV1MATH <- PISA2018IMP$PV1MATH
PISA2018$PV1SCIE <- PISA2018IMP$PV1SCIE
PISA2018$OF <- as.factor(PISA2018$OF)
PISA2018$Könyvek <- as.factor(PISA2018$Könyvek)
PISA2018$TH <- as.factor(PISA2018$TH)
PISA2018$TT <- as.factor(PISA2018$TT)
PISA2018$KO <- as.factor(PISA2018$KO)
PISA2018$OI <- as.factor(PISA2018$OI)
PISA2018$IK <- as.factor(PISA2018$IK)
PISA2018$SzT <- as.factor(PISA2018$SzT)
PISA2018$Ver <- as.factor(PISA2018$Ver)
PISA2018$Bull <- as.factor(PISA2018$Bull)
PISA2018$OED <- as.factor(PISA2018$OED)
PISA2018$DE <- as.factor(PISA2018$DE)


## ---------------------------------------------------------------------------------------------------------------------
TreatsFIN <- subset(PISA2018IMP,  PISA2018IMP$CNT == 1, select = -c(CNT))
ControlHUN <- subset(PISA2018IMP,  PISA2018IMP$CNT == 0, select = -c(CNT))
summary(TreatsFIN$PV1MATH)
summary(ControlHUN$PV1MATH)
summary(TreatsFIN$PV1READ)
summary(ControlHUN$PV1READ)
summary(TreatsFIN$PV1SCIE)
summary(ControlHUN$PV1SCIE)


## ---------------------------------------------------------------------------------------------------------------------
with(PISA2018IMP, t.test(PV1MATH ~ CNT))
with(PISA2018IMP, t.test(PV1READ ~ CNT))
with(PISA2018IMP, t.test(PV1SCIE ~ CNT))
PISA2018IMP_cov <- c("FISCED", "MISCED", "ESCS")
lapply(PISA2018IMP_cov, function(v) {t.test(PISA2018IMP[, v] ~ PISA2018IMP[, 'CNT'])})


## ---------------------------------------------------------------------------------------------------------------------
library("rpart")
library("rpart.plot")
fa <- rpart(CNT~., data=PISA2018, cp=0.01247494)
prp(fa)
rpart.plot(fa)


## ---------------------------------------------------------------------------------------------------------------------
PISA2018$CNT = factor(PISA2018$CNT,levels = c('HUN', 'FIN'),labels = c(0, 1))
library("randomForest")
erdo <- randomForest(CNT~., data=PISA2018, mtry=30)
plot(erdo) #elsimul, jó!
varImpPlot(erdo)


## ---------------------------------------------------------------------------------------------------------------------
library(caret)
keresztval <- trainControl(method="cv", number=10)
train(CNT~., data=PISA2018, method="rpart", trControl=keresztval) # 0.03484730 cp a legjobb, accuarcy:  0.8224031
train(CNT~., data=PISA2018, method="rf", trConrtol=keresztval) # 30-as mtry a legjobb


## ---------------------------------------------------------------------------------------------------------------------
PS <- erdo$votes[,2]
PS <- data.frame(PS)
PS$PS <- PS$PS
hist(PS$PS[PISA2018$CNT==1], ,main = "Pscores of Response = FIN")
hist(PS$PS[PISA2018$CNT==0], ,main = "Pscores of Response = HUN")


## ---------------------------------------------------------------------------------------------------------------------
xvars <- c("Nem","OF","Könyvek","TH","TT","KO","OI","IK","SzT","Ver","Bull","OED","DE","Satisfact","WeeklyClass","MISCED","FISCED","BMMJ_Mother","BFMJ_Father","MINS_Maths","MINS_Language","ESCS","ICT_Home","ICT_School","WLE_Cultural","WLE_Home","WLE_Wealth","WLE_TeacherSupport","WLE_TeacherInstruction","WLE_Emotional","WLE_TeacherInterest","WLE_Attitude","WLE_Competitiveness","WLE_SubjectiveWellBeing","WLE_GoalOrientation","WLE_SchoolWellBeing","PERCOOP","GFOFAIL","PERFEED","RESILIENCE")
library(tableone)
table1 <- CreateTableOne(vars = xvars,strata = "CNT",data = PISA2018, test = FALSE)
print(table1, smd = TRUE)


## ---------------------------------------------------------------------------------------------------------------------

library("MatchIt")
#match <- matchit(CNT~., data = PISA2015, distance=PS$PS, method = "nearest")
match <- matchit(CNT~.-PV1MATH-PV1READ-PV1SCIE, data = PISA2018, distance="randomforest", mtry=30, method = "full")
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



## ---------------------------------------------------------------------------------------------------------------------
with(matchdata, t.test(PV1MATH ~ CNT))


## ---------------------------------------------------------------------------------------------------------------------
mean(matchdata$PV1MATH[matchdata$CNT == 1]) - mean(matchdata$PV1MATH[matchdata$CNT == 0])


## ---------------------------------------------------------------------------------------------------------------------
m.sum <- summary(match)
plot(m.sum, var.order = "unmatched")
love.plot(match, binary = "std")