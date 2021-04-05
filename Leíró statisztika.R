## --------------------------------------------------------------------------------------------------------------------
library(readxl)
HUN_FIN_2012_R <- read_excel("C:/Users/bokan/Desktop/Egyetem/Szakdoga/Szakdolgozat/Adatbazis/HUN_FIN_2012_R.xlsx")
library("mice")
library("dplyr")
library("car")
library("HH")
library("lmtest")
library("sandwich")
library("corrplot")


## ---- message=FALSE--------------------------------------------------------------------------------------------------
imp_single12 <- mice(HUN_FIN_2012_R, m = 1, method = "pmm")
PISA2012IMP <- complete(imp_single12)
PISA2012IMP$CNT = factor(PISA2012IMP$CNT,levels = c('HUN', 'FIN'),labels = c(0, 1))


## --------------------------------------------------------------------------------------------------------------------
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


## --------------------------------------------------------------------------------------------------------------------
HUN_FIN_2015_R <- read_excel("C:/Users/bokan/Desktop/Egyetem/Szakdoga/Szakdolgozat/Adatbazis/HUN_FIN_2015_R.xlsx")


## ---- message=FALSE--------------------------------------------------------------------------------------------------
imp_single15 <- mice(HUN_FIN_2015_R, m = 1, method = "pmm")
PISA2015IMP <- complete(imp_single15)
PISA2015IMP$CNT = factor(PISA2015IMP$CNT,levels = c('HUN', 'FIN'),labels = c(0, 1))


## --------------------------------------------------------------------------------------------------------------------
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


## --------------------------------------------------------------------------------------------------------------------
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


## ---- message=FALSE--------------------------------------------------------------------------------------------------
imp_single18 <- mice(HUN_FIN_2018_R, m = 1, method = "pmm")
PISA2018IMP <- complete(imp_single18)
PISA2018IMP$CNT = factor(PISA2018IMP$CNT,levels = c('HUN', 'FIN'),labels = c(0, 1))
names(PISA2018IMP)[13] <- "KO_Koz"
names(PISA2018IMP)[47] <- "WLE_TeacherInterest"
names(PISA2018IMP)[50] <- "WLE_SubjectiveWellBeing"


## --------------------------------------------------------------------------------------------------------------------
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
#PISA2018$CNT = factor(PISA2018IMP$Nem,levels = c(0, 1),labels = c('HUN', 'FIN'))
#PISA2015$CNT = factor(PISA2015IMP$Nem,levels = c(0, 1),labels = c('HUN', 'FIN'))
#PISA2012$CNT = factor(PISA2012IMP$Nem,levels = c(0, 1),labels = c('HUN', 'FIN'))


## --------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(ggeasy)
ggplot(PISA2012, aes(x=WEALTH, fill=CNT, col=I("black")))+geom_histogram(bins=11, position="fill")+ggtitle("Vagyon és ország kapcsolata 2012-es felmérésben")
ggplot(PISA2015, aes(x=WLE_Wealth, fill=CNT, col=I("black")))+geom_histogram(bins=11, position="fill")+ggtitle("Vagyon és ország kapcsolata 2015-ös felmérésben")
ggplot(PISA2018, aes(x=WLE_Wealth, fill=CNT, col=I("black")))+geom_histogram(bins=11, position="fill")+ggtitle("Vagyon és ország kapcsolata 2018-as felmérésben")

ggplot(PISA2012, aes(x=WEALTH, fill=CNT, col=I("black")))+geom_boxplot(bins=11)+ theme_minimal() + theme_bw()+scale_fill_discrete("Ország")+labs(x = "Vagyon")
ggplot(PISA2015, aes(x=WLE_Wealth, fill=CNT, col=I("black")))+geom_boxplot(bins=11)+ theme_minimal() + theme_bw()+scale_fill_discrete("Ország")+labs(x = "Vagyon")

ggplot(PISA2018, aes(x=WLE_Wealth, fill=CNT))+geom_boxplot(bins=11)+ theme_minimal() + theme_bw()+scale_fill_discrete("Ország")+labs(x = "Vagyon")


## --------------------------------------------------------------------------------------------------------------------
#2012
ggplot(PISA2012, aes(x=PV1MATH, y=1, fill=I("darkgreen")))+geom_boxplot()+geom_violin(alpha=0.3)
ggplot(PISA2012, aes(x=PV1READ, y=1, fill=I("darkgreen")))+geom_boxplot()+geom_violin(alpha=0.3)
ggplot(PISA2012, aes(x=PV1SCIE, y=1, fill=I("darkgreen")))+geom_boxplot()+geom_violin(alpha=0.3)
#2012 országonként
ggplot(PISA2012, aes(x=PV1MATH, y=CNT, fill=I("darkgreen")))+geom_boxplot()+geom_violin(alpha=0.3)
ggplot(PISA2012, aes(x=PV1READ, y=CNT, fill=I("darkgreen")))+geom_boxplot()+geom_violin(alpha=0.3)
ggplot(PISA2012, aes(x=PV1SCIE, y=CNT, fill=I("darkgreen")))+geom_boxplot()+geom_violin(alpha=0.3)
#2015
ggplot(PISA2015, aes(x=PV1MATH, y=1, fill=I("lightblue")))+geom_boxplot()+geom_violin(alpha=0.3)
ggplot(PISA2015, aes(x=PV1READ, y=1, fill=I("lightblue")))+geom_boxplot()+geom_violin(alpha=0.3)
ggplot(PISA2015, aes(x=PV1SCIE, y=1, fill=I("lightblue")))+geom_boxplot()+geom_violin(alpha=0.3)
#2015 országonként
ggplot(PISA2015, aes(x=PV1MATH, y=CNT, fill=I("lightblue")))+geom_boxplot()+geom_violin(alpha=0.3)
ggplot(PISA2015, aes(x=PV1READ, y=CNT, fill=I("lightblue")))+geom_boxplot()+geom_violin(alpha=0.3)
ggplot(PISA2015, aes(x=PV1SCIE, y=CNT, fill=I("lightblue")))+geom_boxplot()+geom_violin(alpha=0.3)
#2018
ggplot(PISA2018, aes(x=PV1MATH, y=1, fill=I("orange")))+geom_boxplot()+geom_violin(alpha=0.3)
ggplot(PISA2018, aes(x=PV1READ, y=1, fill=I("orange")))+geom_boxplot()+geom_violin(alpha=0.3)
ggplot(PISA2018, aes(x=PV1SCIE, y=1, fill=I("orange")))+geom_boxplot()+geom_violin(alpha=0.3)
#2018 országonként
ggplot(PISA2018, aes(x=PV1MATH, y=CNT, fill=I("white")))+geom_boxplot()+geom_violin(alpha=0.3)
ggplot(PISA2018, aes(x=PV1READ, y=CNT, fill=I("white")))+geom_boxplot()+geom_violin(alpha=0.3)
ggplot(PISA2018, aes(x=PV1SCIE, y=CNT, fill=I("white")))+geom_boxplot()+geom_violin(alpha=0.3)


## --------------------------------------------------------------------------------------------------------------------
summary(PISA2012$PV1MATH)
summary(PISA2012$PV1MATH[PISA2012$CNT==1])
summary(PISA2012$PV1MATH[PISA2012$CNT==0])
sd(PISA2012$PV1MATH)
sd(PISA2012$PV1MATH[PISA2012$CNT==1])
sd(PISA2012$PV1MATH[PISA2012$CNT==0])

summary(PISA2012$PV1READ)
summary(PISA2012$PV1READ[PISA2012$CNT==1])
summary(PISA2012$PV1READ[PISA2012$CNT==0])
sd(PISA2012$PV1READ)
sd(PISA2012$PV1READ[PISA2012$CNT==1])
sd(PISA2012$PV1READ[PISA2012$CNT==0])

summary(PISA2012$PV1SCIE)
summary(PISA2012$PV1SCIE[PISA2012$CNT==1])
summary(PISA2012$PV1SCIE[PISA2012$CNT==0])
sd(PISA2012$PV1SCIE)
sd(PISA2012$PV1SCIE[PISA2012$CNT==1])
sd(PISA2012$PV1SCIE[PISA2012$CNT==0])


## --------------------------------------------------------------------------------------------------------------------
summary(PISA2015$PV1MATH)
summary(PISA2015$PV1MATH[PISA2015$CNT==1])
summary(PISA2015$PV1MATH[PISA2015$CNT==0])
sd(PISA2015$PV1MATH)
sd(PISA2015$PV1MATH[PISA2015$CNT==1])
sd(PISA2015$PV1MATH[PISA2015$CNT==0])

summary(PISA2015$PV1READ)
summary(PISA2015$PV1READ[PISA2015$CNT==1])
summary(PISA2015$PV1READ[PISA2015$CNT==0])
sd(PISA2015$PV1READ)
sd(PISA2015$PV1READ[PISA2015$CNT==1])
sd(PISA2015$PV1READ[PISA2015$CNT==0])

summary(PISA2015$PV1SCIE)
summary(PISA2015$PV1SCIE[PISA2015$CNT==1])
summary(PISA2015$PV1SCIE[PISA2015$CNT==0])
sd(PISA2015$PV1SCIE)
sd(PISA2015$PV1SCIE[PISA2015$CNT==1])
sd(PISA2015$PV1SCIE[PISA2015$CNT==0])


## --------------------------------------------------------------------------------------------------------------------
summary(PISA2018$PV1MATH)
summary(PISA2018$PV1MATH[PISA2018$CNT==1])
summary(PISA2018$PV1MATH[PISA2018$CNT==0])
sd(PISA2018$PV1MATH)
sd(PISA2018$PV1MATH[PISA2018$CNT==1])
sd(PISA2018$PV1MATH[PISA2018$CNT==0])

summary(PISA2018$PV1READ)
summary(PISA2018$PV1READ[PISA2018$CNT==1])
summary(PISA2018$PV1READ[PISA2018$CNT==0])
sd(PISA2018$PV1READ)
sd(PISA2018$PV1READ[PISA2018$CNT==1])
sd(PISA2018$PV1READ[PISA2018$CNT==0])

summary(PISA2018$PV1SCIE)
summary(PISA2018$PV1SCIE[PISA2018$CNT==1])
summary(PISA2018$PV1SCIE[PISA2018$CNT==0])
sd(PISA2018$PV1SCIE)
sd(PISA2018$PV1SCIE[PISA2018$CNT==1])
sd(PISA2018$PV1SCIE[PISA2018$CNT==0])


## --------------------------------------------------------------------------------------------------------------------
ggplot(PISA2012, aes(x=WEALTH, y=PV1MATH))+geom_point()+geom_smooth()+geom_smooth(method="lm", col=I("red"))
ggplot(PISA2015, aes(x=WLE_Wealth, y=PV1MATH))+geom_point()+geom_smooth()+geom_smooth(method="lm", col=I("red"))
ggplot(PISA2018, aes(x=WLE_Wealth, y=PV1MATH))+geom_point()+geom_smooth()+geom_smooth(method="lm", col=I("red"))
cor.test(x=PISA2012$WEALTH, y=PISA2012$PV1MATH)
cor.test(x=PISA2015$WLE_Wealth, y=PISA2015$PV1MATH)
cor.test(x=PISA2018$WLE_Wealth, y=PISA2018$PV1MATH)

ggplot(PISA2012, aes(x=WEALTH, y=PV1MATH))+geom_bin2d()+geom_density_2d()+geom_smooth(col=I("red"))
ggplot(PISA2015, aes(x=WLE_Wealth, y=PV1MATH))+geom_bin2d()+geom_density_2d()+geom_smooth(col=I("red"))
ggplot(PISA2018, aes(x=WLE_Wealth, y=PV1MATH))+geom_bin2d()+geom_density_2d()+geom_smooth(col=I("red"))


## --------------------------------------------------------------------------------------------------------------------
library(ggplot2)
ggplot(PISA2012, aes(x=ESCS, y=PV1MATH))+geom_point()+geom_smooth()+geom_smooth(method="lm", col=I("red"))
ggplot(PISA2015, aes(x=ESCS, y=PV1MATH))+geom_point()+geom_smooth()+geom_smooth(method="lm", col=I("red"))

ggplot(PISA2018, aes(x=ESCS, y=PV1MATH))+geom_point()+geom_smooth()+geom_smooth(method="lm", col=I("red"))+facet_grid(~CNT)+ theme_minimal() + theme_bw()+labs(x = "ESCS index", y= "Matematika pontszám")
cor.test(x=PISA2012$ESCS[PISA2012$CNT==1], y=PISA2012$PV1MATH[PISA2012$CNT==1])
cor.test(x=PISA2012$ESCS[PISA2012$CNT==0], y=PISA2012$PV1MATH[PISA2012$CNT==0])
cor.test(x=PISA2015$ESC[PISA2015$CNT==1], y=PISA2015$PV1MATH[PISA2015$CNT==1])
cor.test(x=PISA2015$ESC[PISA2015$CNT==0], y=PISA2015$PV1MATH[PISA2015$CNT==0])
cor.test(x=PISA2018$ESCS[PISA2018$CNT==1], y=PISA2018$PV1MATH[PISA2018$CNT==1])
cor.test(x=PISA2018$ESCS[PISA2018$CNT==0], y=PISA2018$PV1MATH[PISA2018$CNT==0])
ggplot(PISA2015, aes(x=ESCS, y=PV1MATH))+geom_point()+geom_smooth()+geom_smooth(method="lm", col=I("red"))+facet_grid(~CNT)+ theme_minimal() + theme_bw()
ggplot(PISA2012, aes(x=ESCS, y=PV1MATH))+geom_point()+geom_smooth()+geom_smooth(method="lm", col=I("red"))+facet_grid(~CNT)+ theme_minimal() + theme_bw()


## --------------------------------------------------------------------------------------------------------------------
library(ggplot2)
ggplot(PISA2015, aes(x=WeeklyClass, y=PV1MATH))+geom_point()+geom_smooth()+geom_smooth(method="lm", col=I("red"))
ggplot(PISA2018, aes(x=WeeklyClass, y=PV1MATH))+geom_point()+geom_smooth()+geom_smooth(method="lm", col=I("red"))
cor.test(x=PISA2015$WeeklyClass, y=PISA2015$PV1MATH)
cor.test(x=PISA2018$WeeklyClass, y=PISA2018$PV1MATH)


## --------------------------------------------------------------------------------------------------------------------
y <- mean(PISA2018$PV1SCIE[PISA2018$CNT==1])
x <- mean(PISA2015$PV1SCIE[PISA2015$CNT==1])
sy2 <- (sd(PISA2018$PV1SCIE[PISA2018$CNT==1]))^2
sx2 <- (sd(PISA2015$PV1SCIE[PISA2015$CNT==1]))^2
z1 <- (y-x)/(sqrt((sy2/10574)+(sx2/10216)))
z1

y <- mean(PISA2018$PV1READ[PISA2018$CNT==1])
x <- mean(PISA2015$PV1READ[PISA2015$CNT==1])
sy2 <- (sd(PISA2018$PV1READ[PISA2018$CNT==1]))^2
sx2 <- (sd(PISA2015$PV1READ[PISA2015$CNT==1]))^2
z2 <- (y-x)/(sqrt((sy2/10574)+(sx2/10216)))
z2

y <- mean(PISA2018$PV1MATH[PISA2018$CNT==1])
x <- mean(PISA2015$PV1MATH[PISA2015$CNT==1])
sy2 <- (sd(PISA2018$PV1MATH[PISA2018$CNT==1]))^2
sx2 <- (sd(PISA2015$PV1MATH[PISA2015$CNT==1]))^2
z3 <- (y-x)/(sqrt((sy2/10574)+(sx2/10216)))
z3

pnorm(-8.8864, 0, 1)
pnorm(-5.230747, 0, 1)
pnorm(-1.832456, 0, 1)

