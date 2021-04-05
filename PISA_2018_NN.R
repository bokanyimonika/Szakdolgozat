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
pscores.model <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ,family = binomial("logit"),data = PISA2018)
summary(pscores.model)


## ---------------------------------------------------------------------------------------------------------------------
PISA2018[abs(rstudent(pscores.model))>3,] -> kilogok
PISA2018  <- PISA2018[abs(rstudent(pscores.model))<=3, ]


## ---------------------------------------------------------------------------------------------------------------------
vif(pscores.model)
corr <- PISA2018[, c(15, 16, 17,18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37)]
cor(corr)
corr2 <- PISA2018[, c(17,18, 19, 20, 23, 24, 26, 27, 28, 31)] 
cor(corr2)

## ---------------------------------------------------------------------------------------------------------------------
fokomp1 <- princomp(scale(PISA2018[ , c(17,18, 19, 20, 23, 24, 26, 27, 28, 31)]))
fokomp1$sdev^2
summary(fokomp1)
#Kaiser-kritérium alapján az első 2-t hagyjuk meg
PISA2018 <- cbind(PISA2018, fokomp1$scores[ ,1:2])


## ---------------------------------------------------------------------------------------------------------------------
fokomp1$loadings


## ---------------------------------------------------------------------------------------------------------------------
colnames(PISA2018)[45:46] <- c("SocioecStat", "ParentalEduc")


## ---------------------------------------------------------------------------------------------------------------------
pscores.model2 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Emotional-WLE_Wealth+SocioecStat+ParentalEduc,family = binomial("logit"),data = PISA2018)
summary(pscores.model2)
vif(pscores.model2)


## ---------------------------------------------------------------------------------------------------------------------
PISA2018$TT <- as.character(PISA2018$TT)
PISA2018$TT[PISA2018$TT=="Kiváló"] <- "Jó"
PISA2018$TT <- as.factor(PISA2018$TT)

pscores.model3 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Emotional-WLE_Wealth+SocioecStat+ParentalEduc,family = binomial("logit"),data = PISA2018)
summary(pscores.model3)

PISA2018$Könyvek <- as.character(PISA2018$Könyvek)
PISA2018$Könyvek[PISA2018$Könyvek=="100-200"] <- "100-500"
PISA2018$Könyvek[PISA2018$Könyvek=="200-500"] <- "100-500"
PISA2018$Könyvek <- as.factor(PISA2018$Könyvek)

pscores.model4 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Emotional-WLE_Wealth+SocioecStat+ParentalEduc,family = binomial("logit"),data = PISA2018)
summary(pscores.model4)
vif(pscores.model4)


## ---------------------------------------------------------------------------------------------------------------------
resettest(pscores.model4) 
crPlots(pscores.model4, ~Nem)
crPlots(pscores.model4, ~OF)
crPlots(pscores.model4, ~Könyvek)
crPlots(pscores.model4, ~TH)
crPlots(pscores.model4, ~TT)
crPlots(pscores.model4, ~KO)
crPlots(pscores.model4, ~OI)
crPlots(pscores.model4, ~IK)
crPlots(pscores.model4, ~SzT)
crPlots(pscores.model4, ~Ver)
crPlots(pscores.model4, ~Bull)
crPlots(pscores.model4, ~OED)
crPlots(pscores.model4, ~DE)
crPlots(pscores.model4, ~Satisfact)
crPlots(pscores.model4, ~WeeklyClass)
crPlots(pscores.model4, ~MINS_Maths)
crPlots(pscores.model4, ~MINS_Language)
crPlots(pscores.model4, ~ICT_School)
crPlots(pscores.model4, ~WLE_TeacherSupport)
crPlots(pscores.model4, ~WLE_TeacherInstruction)
crPlots(pscores.model4, ~WLE_TeacherInterest)
crPlots(pscores.model4, ~WLE_Attitude)
crPlots(pscores.model4, ~WLE_Competitiveness)
crPlots(pscores.model4, ~WLE_SubjectiveWellBeing)
crPlots(pscores.model4, ~WLE_GoalOrientation)
crPlots(pscores.model4, ~WLE_SchoolWellBeing)
crPlots(pscores.model4, ~PERCOOP)
crPlots(pscores.model4, ~GFOFAIL)
crPlots(pscores.model4, ~PERFEED)
crPlots(pscores.model4, ~RESILIENCE)
crPlots(pscores.model4, ~SocioecStat)
crPlots(pscores.model4, ~ParentalEduc)


## ---------------------------------------------------------------------------------------------------------------------
pscores.model5 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Emotional-WLE_Wealth+SocioecStat+ParentalEduc+pmax(MINS_Maths-875, 0),family = binomial("logit"),data = PISA2018)
summary(pscores.model5)
BIC(pscores.model5) 
AIC(pscores.model5)
resettest(pscores.model5)


## ---------------------------------------------------------------------------------------------------------------------

pscores.model4 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Emotional-WLE_Wealth+SocioecStat+ParentalEduc,family = binomial("logit"),data = PISA2018)
summary(pscores.model4)
AIC(pscores.model4)
BIC(pscores.model4)
vif(pscores.model4)

pscores.model6 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Emotional-WLE_Wealth+SocioecStat+ParentalEduc-SzT,family = binomial("logit"),data = PISA2018)
summary(pscores.model6)
AIC(pscores.model6)
BIC(pscores.model6)
vif(pscores.model6)

pscores.model7 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Emotional-WLE_Wealth+SocioecStat+ParentalEduc-SzT-PERFEED,family = binomial("logit"),data = PISA2018)
summary(pscores.model7)
AIC(pscores.model7)
BIC(pscores.model7)
vif(pscores.model7)

pscores.model8 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Emotional-WLE_Wealth+SocioecStat+ParentalEduc-SzT-PERFEED-WLE_Competitiveness,family = binomial("logit"),data = PISA2018)
summary(pscores.model8)
AIC(pscores.model8)
BIC(pscores.model8) 
vif(pscores.model8)

pscores.model9 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Emotional-WLE_Wealth+SocioecStat+ParentalEduc-SzT-PERFEED-WLE_Competitiveness-PERCOOP,family = binomial("logit"),data = PISA2018)
summary(pscores.model9)
AIC(pscores.model9)
BIC(pscores.model9)
vif(pscores.model9)

PropScores <- fitted(glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Emotional-WLE_Wealth+SocioecStat+ParentalEduc-SzT-PERFEED-WLE_Competitiveness-PERCOOP,family = binomial("logit"),data = PISA2018))


## ---------------------------------------------------------------------------------------------------------------------
becsles <- predict(pscores.model9, PISA2018, type="response")
PISA2018$becsult <- ifelse(becsles>0.5, 1,0)
xtabs(~CNT+becsult, PISA2018)

sum(PISA2018$CNT==PISA2018$becsult)/nrow(PISA2018)

library("pROC")
ROCgorbe <- roc(PISA2018$CNT~becsles)
plot(ROCgorbe)
2*auc(ROCgorbe)-1


## ---------------------------------------------------------------------------------------------------------------------
PISA2018$PScores <- pscores.model6$fitted.values
hist(PISA2018$PScores[PISA2018$CNT==1],main = "Pscores of Response = 1")
hist(PISA2018$PScores[PISA2018$CNT==0],main = "Pscores of Response = 0")


## ---------------------------------------------------------------------------------------------------------------------
xvars <- c("Nem","OF","Könyvek","TH","TT","KO","OI","IK","Ver","Bull","OED","DE","Satisfact","WeeklyClass","MINS_Maths","MINS_Language","ICT_School", "WLE_TeacherSupport", "WLE_TeacherInstruction", "WLE_TeacherInterest", "WLE_Attitude", "WLE_SubjectiveWellBeing", "WLE_GoalOrientation", "WLE_SchoolWellBeing", "GFOFAIL", "RESILIENCE", "SocioecStat", "ParentalEduc")
library(tableone)
table1 <- CreateTableOne(vars = xvars,strata = "CNT",data = PISA2018, test = FALSE)
print(table1, smd = TRUE)


## ---------------------------------------------------------------------------------------------------------------------
PISA2018$becsult <- NULL
library(MatchIt)
match <- matchit(CNT~.-PV1MATH-PV1READ-PV1SCIE-PScores-PERCOOP, data = PISA2018, distance=PropScores, method = "nearest")
plot(match, type="jitter")
plot(match, type="hist")
summary(match, standardized=T)
summary(match, standardized=T) -> BalanceNN
TableNN <- BalanceNN$sum.matched


## ---------------------------------------------------------------------------------------------------------------------
matchdata <- match.data(match)
table_match2 <- CreateTableOne(vars = xvars,strata = "CNT",data = matchdata,test = FALSE)
print(table_match2, smd = TRUE)


## ---------------------------------------------------------------------------------------------------------------------
with(matchdata, t.test(PV1MATH ~ CNT))
lm_treat1 <- lm(PV1MATH ~ CNT, data = matchdata)
summary(lm_treat1)


## ---------------------------------------------------------------------------------------------------------------------
mean(matchdata$PV1MATH[matchdata$CNT == 1]) - mean(matchdata$PV1MATH[matchdata$CNT == 0])


## ---------------------------------------------------------------------------------------------------------------------
library(cobalt)
m.sum <- summary(match)
plot(m.sum, var.order = "unmatched", cex=0.75)
love.plot(match, binary = "std")

