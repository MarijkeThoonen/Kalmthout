#Data preparation

#Description of variables
  # EXC_ID = exclosure ID, BEH_TYP = begraasd/niet-begraasd, M = Month, Y = Year)
  #DS_105 = Drooggewicht bij 105°, VOCHT_105 = Vocht vrijgekomen bij 105°
    #DS_105 + Vocht_105 = 1000 g/kg
  # VEG_TYP = vegetatietype (A = andere, F = fijne grassen, H = heide, P = pijpenstro, S = strooisel)
    #ADL Acid Detergent Lignin ~ % lignine ~ onverteerbare bestanddelen van celwanden
    #ADF = Acid Detergent Fiber ~ % cellulose + % lignine ~ moeilijk verteerbare bestanddelen van celwanden
    #NDF = Neutral Detergent Fiber ~ % hemicellulose + % cellulose + % lignine ~ totaal celwandbestanddelen
    #NDF - ADF = % hemicellulose 
    #ADF - ADL = % cellulose
  #N_DS = totaalstikstof/DS (g N/kg droog staal), 
        #methode = Kjeldahl_N
        #org_N + NH4_N, niet NO2 + NO3 (wellicht verwaarloosbaar) 
        #N_DS*DS_105 = gewicht voorgedroogd staal (40°)
  #RE = Ruw eiwit (%) bepaald met de NIR (near infrared) methoden 
        #naam wordt RE_NIR
        #verschil tussen RE en RE_DS is te wijten aan verschil in analysemethode
  #RE_DS = Ruw eiwit/DS (%) bepaald met de natchemische methode 
        #naam wordt RE_NATCHEM
        #N_DS * 0,001 * 6,25 (omrekeningsfactor, eiwit bevat 16% (100/16) N
  
library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(plyr)

#Import and control of dataset
VEGCOM.KH <- read.csv2(
  "Data/Basisdatasets/Veg_2011_2012_2013_CLEAN_27nov2018.csv",
  stringsAsFactors = FALSE)

#Verkennen datatype en frame
str(VEGCOM.KH)
dim(VEGCOM.KH)

#Deleten van de IDnummer
VEGCOM.KH$ID <- NULL

#berekenen van % hemicellulose
VEGCOM.KH$HEMCEL_pct <- (VEGCOM.KH$NDF - VEGCOM.KH$ADF)

#berekenen van % cellulose
VEGCOM.KH$CEL_pct <- (VEGCOM.KH$ADF - VEGCOM.KH$ADL)

#kolomnaam veranderen kolomnaam ADL naar LIG_pct
VEGCOM.KH <- rename(VEGCOM.KH,c("ADL" = "LIG_pct"))

#kolomnaam RE veranderen in RE_NIR
VEGCOM.KH <- rename(VEGCOM.KH, c("RE" = "RE_NIR_pct"))

#RE_DS naam veranderen in RE_NATCHEM_DS_pct
VEGCOM.KH <- rename(VEGCOM.KH, c("RE_DS" = "RE_NATCHEM_DS_pct"))

#DS_105, VOCHT_105, N_DS omrekenen vanuit g/kg naar %
VEGCOM.KH$DS_pct <- (VEGCOM.KH$DS_105*0.1)
VEGCOM.KH$VOCHT_pct <- (VEGCOM.KH$VOCHT_105*0.1)
VEGCOM.KH$N_DS_pct <- (VEGCOM.KH$N_DS*0.1)

#RE uitdrukken op droge stofgehalte voor vergelijking met RE_NATCHEM
VEGCOM.KH$RE_NIR_DS_pct <- (VEGCOM.KH$RE_NIR*(1000/VEGCOM.KH$DS_105))

#maanden uitdrukken in cijfers
VEGCOM.KH$M <- revalue(VEGCOM.KH$M, c("jun" = "6", "jul" = "7", "aug" = "8",
                                      "sep" = "9"))
#of

#VEGCOM.KH$M[VEGCOM.KH$M == "jun"] <- "6"
#VEGCOM.KH$M[VEGCOM.KH$M == "jul"] <- "7"
#VEGCOM.KH$M[VEGCOM.KH$M == "aug"] <- "8"
#VEGCOM.KH$M[VEGCOM.KH$M == "sep"] <- "9"

#nieuwe kolom maken voor Brand in 2011
VEGCOM.KH <- VEGCOM.KH %>% 
  mutate(Brand2011 = ifelse(EXC_ID %in% c("WD1","WD2", "LV1", "LV2"), "Ja", "Nee"))
         
VEGCOM.KH <- VEGCOM.KH %>% 
  select(EXC_ID, BEH_TYP, M, Y, VEG_TYP, DS_pct, VOCHT_pct, 
                      LIG_pct, CEL_pct, HEMCEL_pct, N_DS_pct, RE_NIR_DS_pct, 
                      RE_NATCHEM_DS_pct, Brand2011)

# Wegschrijven van de dataset
write.csv2(VEGCOM.KH, row.names = FALSE,
           file = "Data/Afgeleide datasets/VEGCOM.KH.csv")


##########################################
#niet gebruikt
##library(stringr)

#nieuwe variabele maken met de jaarlocatie
##VEGCOM.KH$yearlocation <- paste(VEGCOM.KH$EXC_ID, VEGCOM.KH$Y, sep = " ")

#nieuwe variabele maken met de maandlocatie
##VEGCOM.KH$monthlocation <- paste(VEGCOM.KH$EXC_ID, VEGCOM.KH$M, sep = " ")

#nieuwe variabele maken met de maand en jaar
##VEGCOM.KH$obs_M_Y <- paste(VEGCOM.KH$M, VEGCOM.KH$Y, sep = " ")

#nieuwe variabele maken met de maand, datum en locatie
##VEGCOM.KH$datelocation <- paste(VEGCOM.KH$EXC_ID, VEGCOM.KH$obs_M_Y, sep = " ")

#nieuwe variabele maken met beheertype per locatie
##VEGCOM.KH$EXC_ID_BEH_TYP <- paste(VEGCOM.KH$EXC_ID, VEGCOM.KH$BEH_TYP, sep = " ")

#nieuwe variabele maken met vegetatietype per locatie
##VEGCOM.KH$EXC_ID_VEG_TYP <- paste(VEGCOM.KH$EXC_ID, VEGCOM.KH$VEG_TYP, sep = " ")

#nieuwe variabele maken met unieke sleutel per staal EXC_ID BEH_TYP VEG_TYP M Y 
#VEGCOM.KH$SAMPLE_ID <- paste(VEGCOM.KH$EXC_ID, VEGCOM.KH$BEH_TYP,
                             #VEGCOM.KH$VEG_TYP, VEGCOM.KH$M, VEGCOM.KH$Y, 
                             #sep = " ")






