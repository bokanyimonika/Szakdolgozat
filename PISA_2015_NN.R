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
PISA2015IMP$CNT = factor(PISA2015IMP$CNT,levels = c('HUN', 'FIN'),labels = c(0, 1))


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
TreatsFIN <- subset(PISA2015IMP,  PISA2015IMP$CNT == 1, select = -c(CNT))
ControlHUN <- subset(PISA2015IMP,  PISA2015IMP$CNT == 0, select = -c(CNT))
summary(TreatsFIN$PV1MATH)
summary(ControlHUN$PV1MATH)
summary(TreatsFIN$PV1READ)
summary(ControlHUN$PV1READ)
summary(TreatsFIN$PV1SCIE)
summary(ControlHUN$PV1SCIE)


## ---------------------------------------------------------------------------------------------------------------------
with(PISA2015IMP, t.test(PV1MATH ~ CNT)) 
with(PISA2015IMP, t.test(PV1READ ~ CNT))
with(PISA2015IMP, t.test(PV1SCIE ~ CNT))
PISA2015IMP_cov <- c("FISCED", "MISCED", "ESCS")
lapply(PISA2015IMP_cov, function(v) {t.test(PISA2015IMP[, v] ~ PISA2015IMP[, 'CNT'])})


## ---------------------------------------------------------------------------------------------------------------------
pscores.model <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ,family = binomial("logit"),data = PISA2015)
summary(pscores.model)


## ---------------------------------------------------------------------------------------------------------------------
PISA2015[abs(rstudent(pscores.model))>3,] -> kilogok
PISA2015  <- PISA2015[abs(rstudent(pscores.model))<=3, ]


## ---------------------------------------------------------------------------------------------------------------------
vif(pscores.model)
corr <- PISA2015[, c(15, 16, 17,18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33)]
cor(corr)
corr2 <- PISA2015[, c(16, 17,18, 19, 22, 23, 25, 26, 27)]
cor(corr2)


## ---------------------------------------------------------------------------------------------------------------------
fokomp1 <- princomp(scale(PISA2015[ , c(16, 17,18, 19, 22, 23, 25, 26, 27)]))
fokomp1$sdev^2
summary(fokomp1)

PISA2015 <- cbind(PISA2015, fokomp1$scores[ ,1:3])


## ---------------------------------------------------------------------------------------------------------------------
fokomp1$loadings


## ---------------------------------------------------------------------------------------------------------------------
colnames(PISA2015)[37:39] <- c("SocioecStat", "ParentalEduc", "Vagyon")


## ---------------------------------------------------------------------------------------------------------------------
pscores.model2 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Wealth+SocioecStat+ParentalEduc+Vagyon,family = binomial("logit"),data = PISA2015)
summary(pscores.model2)


## ---------------------------------------------------------------------------------------------------------------------
PISA2015$Könyvek <- as.character(PISA2015$Könyvek)
PISA2015$Könyvek[PISA2015$Könyvek=="100-200"] <- "100-500"
PISA2015$Könyvek[PISA2015$Könyvek=="200-500"] <- "100-500"
PISA2015$Könyvek <- as.factor(PISA2015$Könyvek)

pscores.model3 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Wealth+SocioecStat+ParentalEduc+Vagyon,family = binomial("logit"),data = PISA2015)
summary(pscores.model3)

PISA2015$OF <- as.character(PISA2015$OF)
PISA2015$OF[PISA2015$OF=="Rossz"] <- "Közepes"
PISA2015$OF[PISA2015$OF=="Jó"] <- "Közepes"
PISA2015$OF <- as.factor(PISA2015$OF)

pscores.model4 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Wealth+SocioecStat+ParentalEduc+Vagyon,family = binomial("logit"),data = PISA2015)
summary(pscores.model4)

PISA2015$IK <- as.character(PISA2015$IK)
PISA2015$IK[PISA2015$IK=="Rossz"] <- "Elégséges"
PISA2015$IK[PISA2015$IK=="Közepes"] <- "Elégséges"
PISA2015$IK <- as.factor(PISA2015$IK)

pscores.model5 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Wealth+SocioecStat+ParentalEduc+Vagyon,family = binomial("logit"),data = PISA2015)
summary(pscores.model5)

PISA2015$TB <- as.character(PISA2015$TB)
PISA2015$TB[PISA2015$TB=="Nincs"] <- "Néha"
PISA2015$TB[PISA2015$TB=="Közepes"] <- "Néha"
PISA2015$TB <- as.factor(PISA2015$TB)

pscores.model6 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Wealth+SocioecStat+ParentalEduc+Vagyon,family = binomial("logit"),data = PISA2015)
summary(pscores.model6)

PISA2015$OV <- as.character(PISA2015$OV)
PISA2015$OV[PISA2015$OV=="Rossz"] <- "Ingadozó"
PISA2015$OV[PISA2015$OV=="Közepes"] <- "Ingadozó"
PISA2015$OV <- as.factor(PISA2015$OV)

pscores.model7 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Wealth+SocioecStat+ParentalEduc+Vagyon,family = binomial("logit"),data = PISA2015)
summary(pscores.model7)


## ---------------------------------------------------------------------------------------------------------------------
resettest(pscores.model7)
crPlots(pscores.model7, ~Nem)
crPlots(pscores.model7, ~OF)
crPlots(pscores.model7, ~Könyvek)
crPlots(pscores.model7, ~SzT)
crPlots(pscores.model7, ~IK)
crPlots(pscores.model7, ~TB)
crPlots(pscores.model7, ~OV)
crPlots(pscores.model7, ~TK)
crPlots(pscores.model7, ~TS)
crPlots(pscores.model7, ~TO)
crPlots(pscores.model7, ~TanÉsMunka)
crPlots(pscores.model7, ~OED)
crPlots(pscores.model7, ~DE)
crPlots(pscores.model7, ~WeeklyClass)
crPlots(pscores.model7, ~MINS_Maths)
crPlots(pscores.model7, ~MINS_Language)
crPlots(pscores.model7, ~ICT_School)
crPlots(pscores.model7, ~WLE_INSTRUCTION)
crPlots(pscores.model7, ~WLE_TeacherFairness)
crPlots(pscores.model7, ~WLE_Emotional)
crPlots(pscores.model7, ~WLE_SchoolWellBeing)
crPlots(pscores.model7, ~CPSVALUE)
crPlots(pscores.model7, ~PERFEED)
crPlots(pscores.model7, ~SocioecStat)
crPlots(pscores.model7, ~ParentalEduc)
crPlots(pscores.model7, ~Vagyon)


## ---------------------------------------------------------------------------------------------------------------------
pscores.model8 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Wealth+SocioecStat+ParentalEduc+Vagyon+pmax(WLE_INSTRUCTION-0.7, 0),family = binomial("logit"),data = PISA2015)
summary(pscores.model8)
BIC(pscores.model8)
AIC(pscores.model8)
resettest(pscores.model8)


## ---------------------------------------------------------------------------------------------------------------------
pscores.model9 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Wealth+SocioecStat+ParentalEduc+Vagyon+pmax(WLE_INSTRUCTION-0.7, 0)-Nem,family = binomial("logit"),data = PISA2015)
summary(pscores.model9)
BIC(pscores.model9)
AIC(pscores.model9)

pscores.model10 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Wealth+SocioecStat+ParentalEduc+Vagyon+pmax(WLE_INSTRUCTION-0.7, 0)-Nem-TO,family = binomial("logit"),data = PISA2015)
summary(pscores.model10)
BIC(pscores.model10)
AIC(pscores.model10)

pscores.model11 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Wealth+SocioecStat+ParentalEduc+Vagyon+pmax(WLE_INSTRUCTION-0.7, 0)-Nem-TO-ICT_School,family = binomial("logit"),data = PISA2015)
summary(pscores.model11)
BIC(pscores.model11)
AIC(pscores.model11)

pscores.model12 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Wealth+SocioecStat+ParentalEduc+Vagyon+pmax(WLE_INSTRUCTION-0.7, 0)-Nem-TO-ICT_School-WLE_SchoolWellBeing,family = binomial("logit"),data = PISA2015)
summary(pscores.model12) 
BIC(pscores.model12)
AIC(pscores.model12)

pscores.model13 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Wealth+SocioecStat+ParentalEduc+Vagyon+pmax(WLE_INSTRUCTION-0.7, 0)-Nem-TO-ICT_School-WLE_SchoolWellBeing-ParentalEduc,family = binomial("logit"),data = PISA2015)
summary(pscores.model13)
BIC(pscores.model13)
AIC(pscores.model13)

PropScore <- fitted(glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-MISCED-FISCED-BMMJ_Mother-BFMJ_Father-ESCS-ICT_Home-WLE_Cultural-WLE_Home-WLE_Wealth+SocioecStat+ParentalEduc+Vagyon+pmax(WLE_INSTRUCTION-0.7, 0)-Nem-TO-ICT_School-WLE_SchoolWellBeing-ParentalEduc,family = binomial("logit"),data = PISA2015))


## ---------------------------------------------------------------------------------------------------------------------
becsles <- predict(pscores.model13, PISA2015, type="response")
PISA2015$becsult <- ifelse(becsles>0.5, 1,0)
xtabs(~CNT+becsult, PISA2015)

sum(PISA2015$CNT==PISA2015$becsult)/nrow(PISA2015)

library("pROC")
ROCgorbe <- roc(PISA2015$CNT~becsles)
plot(ROCgorbe)
2*auc(ROCgorbe)-1


## ---------------------------------------------------------------------------------------------------------------------
PISA2015$PScores <- pscores.model13$fitted.values
hist(PISA2015$PScores[PISA2015$CNT==1],main = "Pscores of Response = 1")
hist(PISA2015$PScores[PISA2015$CNT==0],main = "Pscores of Response = 0")


## ---------------------------------------------------------------------------------------------------------------------
xvars <- c("OF","Könyvek","SzT","IK","TB","OV","TK","TS","TanÉsMunka","OED","DE","WeeklyClass","MINS_Maths","MINS_Language","WLE_INSTRUCTION","WLE_TeacherFairness","WLE_Emotional","CPSVALUE","PERFEED","SocioecStat","Vagyon")
library(tableone)
table1 <- CreateTableOne(vars = xvars,strata = "CNT",data = PISA2015, test = FALSE)
print(table1, smd = TRUE)


## ---------------------------------------------------------------------------------------------------------------------
PISA2015$becsult <- NULL
library(MatchIt)
match <- matchit(CNT~.-PV1MATH-PV1READ-PV1SCIE-PScores, data = PISA2015, distance=PropScore, method = "nearest")
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

