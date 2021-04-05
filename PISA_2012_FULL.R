## ---------------------------------------------------------------------------------------------------------------------
library(readxl)
HUN_FIN_2012_R <- read_excel("C:/Users/bokan/Desktop/Egyetem/Szakdoga/Szakdolgozat/Adatbazis/HUN_FIN_2012_R.xlsx")
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
PISA2012IMP$CNT = factor(PISA2012IMP$CNT,levels = c('HUN', 'FIN'),labels = c(0, 1))


## ---------------------------------------------------------------------------------------------------------------------
library(dplyr)
PISA2012 <- data.frame(PISA2012IMP)
PISA2012IMP %>% dplyr::select(CNT) -> PISA2012
PISA2012$Nem = factor(PISA2012IMP$Nem,levels = c(0, 1),labels = c('Nő', 'Férfi'))
for (i in 1:8644)
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
for (i in 1:8644)
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
for (i in 1:8644)
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
for (i in 1:8644)
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
for (i in 1:8644)
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
for (i in 1:8644)
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
for (i in 1:8644)
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
for (i in 1:8644)
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
for (i in 1:8644)
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
TreatsFIN <- subset(PISA2012,  PISA2012$CNT == 1, select = -c(CNT))
ControlHUN <- subset(PISA2012,  PISA2012$CNT == 0, select = -c(CNT))
summary(TreatsFIN$PV1MATH)
summary(ControlHUN$PV1MATH)
summary(TreatsFIN$PV1READ)
summary(ControlHUN$PV1READ)
summary(TreatsFIN$PV1SCIE)
summary(ControlHUN$PV1SCIE)


## ---------------------------------------------------------------------------------------------------------------------
with(PISA2012IMP, t.test(PV1MATH ~ CNT))
with(PISA2012IMP, t.test(PV1READ ~ CNT))
with(PISA2012IMP, t.test(PV1SCIE ~ CNT))
PISA2012IMP_cov <- c("FISCED", "MISCED", "HEDRES", "ESCS")
lapply(PISA2012IMP_cov, function(v) {t.test(PISA2012IMP[, v] ~ PISA2012IMP[, 'CNT'])})


## ---------------------------------------------------------------------------------------------------------------------
pscores.model <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ,family = binomial("logit"),data = PISA2012)
summary(pscores.model)




## ---------------------------------------------------------------------------------------------------------------------
PISA2012[abs(rstudent(pscores.model))>3,] -> kilogok
PISA2012  <- PISA2012[abs(rstudent(pscores.model))<=3, ]


## ---------------------------------------------------------------------------------------------------------------------
vif(pscores.model)
corr <- PISA2012[, c(12, 13, 14, 15, 16, 17,18, 19, 20, 21, 22)] #csak numerikus
cor(corr)
corr2 <- PISA2012[, c(12, 13, 14, 15, 16, 17,18, 19, 22)]
cor(corr2)


## ---------------------------------------------------------------------------------------------------------------------
fokomp1 <- princomp(scale(PISA2012[ , c(12, 13, 14, 15, 16, 17,18, 19, 22)]))
fokomp1$sdev^2
summary(fokomp1)
PISA2012 <- cbind(PISA2012, fokomp1$scores[ ,1:2])


## ---------------------------------------------------------------------------------------------------------------------
fokomp1$loadings


## ---------------------------------------------------------------------------------------------------------------------
colnames(PISA2012)[26:27] <- c("SocioecStat", "ParentalEduc")


## ---------------------------------------------------------------------------------------------------------------------
pscores.model2 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-FISCED-MISCED-BFMJ2-BMMJ1-ESCS-HEDRES-HOMEPOS-ICTHOME-WEALTH+SocioecStat+ParentalEduc,family = binomial("logit"),data = PISA2012)
summary(pscores.model2)
vif(pscores.model2) 


## ---------------------------------------------------------------------------------------------------------------------
PISA2012[PISA2012$TT=="Közepes", "TT"] <- "Rossz"
PISA2012[PISA2012$TS=="Közepes", "TS"] <- "Rossz"
PISA2012[PISA2012$OD=="Közepes", "OD"] <- "Rossz"
pscores.model3 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-FISCED-MISCED-BFMJ2-BMMJ1-ESCS-HEDRES-HOMEPOS-ICTHOME-WEALTH+SocioecStat+ParentalEduc,family = binomial("logit"),data = PISA2012)
summary(pscores.model3)


## ---------------------------------------------------------------------------------------------------------------------
resettest(pscores.model3)
crPlots(pscores.model3, ~Nem)
crPlots(pscores.model3, ~IskKer)
crPlots(pscores.model3, ~Könyvek)
crPlots(pscores.model3, ~TT)
crPlots(pscores.model3, ~OV)
crPlots(pscores.model3, ~TS)
crPlots(pscores.model3, ~IK)
crPlots(pscores.model3, ~Isk_Munk)
crPlots(pscores.model3, ~OD)
crPlots(pscores.model3, ~ID)
crPlots(pscores.model3, ~ICTSCH)
crPlots(pscores.model3, ~TEACHSUP)
crPlots(pscores.model3, ~SocioecStat)
crPlots(pscores.model3, ~ParentalEduc)


## ---------------------------------------------------------------------------------------------------------------------
pscores.model4 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-FISCED-MISCED-BFMJ2-BMMJ1-ESCS-HEDRES-HOMEPOS-ICTHOME-WEALTH+SocioecStat+ParentalEduc+pmax(ICTSCH-0.9, 0),family = binomial("logit"),data = PISA2012) #nem javított, BIC nőtt a törésponttól, kiveszem
summary(pscores.model4)
AIC(pscores.model4)
BIC(pscores.model4)
AIC(pscores.model3)
BIC(pscores.model3)
resettest(pscores.model4)


## ---------------------------------------------------------------------------------------------------------------------
pscores.model5 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-FISCED-MISCED-BFMJ2-BMMJ1-ESCS-HEDRES-HOMEPOS-ICTHOME-WEALTH+SocioecStat+ParentalEduc-Isk_Munk,family = binomial("logit"),data = PISA2012) #Isk_Munk kivétel javít
summary(pscores.model5)
AIC(pscores.model5)
BIC(pscores.model5)

pscores.model6 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-FISCED-MISCED-BFMJ2-BMMJ1-ESCS-HEDRES-HOMEPOS-ICTHOME-WEALTH+SocioecStat+ParentalEduc-Isk_Munk-TT,family = binomial("logit"),data = PISA2012) #TT kivétel javít
summary(pscores.model6)
AIC(pscores.model6)
BIC(pscores.model6)

pscores.model7 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-FISCED-MISCED-BFMJ2-BMMJ1-ESCS-HEDRES-HOMEPOS-ICTHOME-WEALTH+SocioecStat+ParentalEduc-Isk_Munk-TT-Nem,family = binomial("logit"),data = PISA2012) #Nem kivétel javít
summary(pscores.model7)
AIC(pscores.model7)
BIC(pscores.model7)

PISA2012[PISA2012$IskKer=="Nincs", "IskKer"] <- "Ritka"

pscores.model8 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-FISCED-MISCED-BFMJ2-BMMJ1-ESCS-HEDRES-HOMEPOS-ICTHOME-WEALTH+SocioecStat+ParentalEduc-Isk_Munk-TT-Nem,family = binomial("logit"),data = PISA2012) #Nem kivétel javít
summary(pscores.model8)
AIC(pscores.model8)
BIC(pscores.model8)

library(dplyr)
PISA2012 %>% rename(OED=OD, DE=ID, WLE_TeachSupport=TEACHSUP, WLE_Wealth=WEALTH, ICT_Home=ICTHOME, ICT_School=ICTSCH, WLE_Home=HOMEPOS) -> PISA2012

pscores.model9 <- glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-FISCED-MISCED-BFMJ2-BMMJ1-ESCS-HEDRES-WLE_Home-ICT_Home-WLE_Wealth+SocioecStat+ParentalEduc-Isk_Munk-TT-Nem-IskKer,family = binomial("logit"),data = PISA2012) #Nem kivétel javít
summary(pscores.model9)
AIC(pscores.model9)
BIC(pscores.model9)


## ---------------------------------------------------------------------------------------------------------------------
becsles <- predict(pscores.model9, PISA2012, type="response")
PISA2012$becsult <- ifelse(becsles>0.5, 1,0)
xtabs(~CNT+becsult, PISA2012)

sum(PISA2012$CNT==PISA2012$becsult)/nrow(PISA2012)
library("pROC")
ROCgorbe <- roc(PISA2012$CNT~becsles)
plot(ROCgorbe)
auc(ROCgorbe)
2*auc(ROCgorbe)-1 
library(caret)
keresztval <- trainControl(method="cv", number=10)
train(CNT~.-PV1MATH-PV1SCIE-PV1READ-FISCED-MISCED-BFMJ2-BMMJ1-ESCS-HEDRES-WLE_Home-ICT_Home-WLE_Wealth+SocioecStat+ParentalEduc-Isk_Munk-TT-Nem-IskKer, data=PISA2012, method="glm", trControl=keresztval)


## ---------------------------------------------------------------------------------------------------------------------
library(precrec)
precrec_obj <- evalmod(scores = becsles, labels = PISA2012$CNT)
autoplot(precrec_obj)


## ---------------------------------------------------------------------------------------------------------------------
PISA2012$PScores <- pscores.model9$fitted.values
ps <- pscores.model9$fitted.values
ps <- data.frame(ps)
hist(PISA2012$PScores[PISA2012$CNT==1],main = "Pscores of Response = 1")
hist(PISA2012$PScores[PISA2012$CNT==0], ,main = "Pscores of Response = 0")


## ---------------------------------------------------------------------------------------------------------------------
xvars <- c("Könyvek","OV","TS","IK","OD","ID","ICTSCH","TEACHSUP","SocioecStat","ParentalEduc")
library(tableone)
table1 <- CreateTableOne(vars = xvars,strata = "CNT",data = PISA2012, test = FALSE)
print(table1, smd = TRUE)


## ---------------------------------------------------------------------------------------------------------------------

PISA2012$becsult <- NULL
PropScores <- fitted(glm(as.factor(CNT) ~.-PV1MATH-PV1SCIE-PV1READ-FISCED-MISCED-BFMJ2-BMMJ1-ESCS-HEDRES-WLE_Home-ICT_Home-WLE_Wealth+SocioecStat+ParentalEduc-Isk_Munk-TT-IskKer,family = binomial("logit"),data = PISA2012))

library(MatchIt)
match <- matchit(CNT~.-PV1MATH-PV1READ-PV1SCIE-PScores, data = PISA2012, distance=PropScores, method = "full")
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
?as.svrepdesign


## ---------------------------------------------------------------------------------------------------------------------
library(tableone)
matchdata <- match.data(match)
table_match2 <- CreateTableOne(vars = xvars,strata = "CNT",data = matchdata,test = FALSE)
print(table_match2, smd = TRUE)


## ---------------------------------------------------------------------------------------------------------------------
with(matchdata, t.test(PV1MATH ~ CNT))
lm_treat1 <- lm(PV1MATH ~ CNT, data = matchdata)
summary(lm_treat1)


## ---------------------------------------------------------------------------------------------------------------------
mean(matchdata$PV1MATH[matchdata$CNT == 1]) - mean(matchdata$PV1MATH[matchdata$CNT == 0])
mean(matchdata$PV1READ[matchdata$CNT == 1]) - mean(matchdata$PV1READ[matchdata$CNT == 0])
mean(matchdata$PV1SCIE[matchdata$CNT == 1]) - mean(matchdata$PV1SCIE[matchdata$CNT == 0])


## ---------------------------------------------------------------------------------------------------------------------
library(cobalt)
m.sum <- summary(match)
plot(m.sum, var.order = "unmatched")
love.plot(match, binary = "std")

