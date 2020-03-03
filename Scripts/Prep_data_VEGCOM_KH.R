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
vegcom_kh <- read.csv2(
  "Data/Basisdatasets/Veg_2011_2012_2013_CLEAN_27nov2018.csv",
  stringsAsFactors = FALSE)

#Verkennen datatype en frame
str(vegcom_kh)
dim(vegcom_kh)

#Deleten van de IDnummer
vegcom_kh$ID <- NULL

#berekenen van % hemicellulose
vegcom_kh$HEMCEL_pct <- (vegcom_kh$NDF - vegcom_kh$ADF)

#berekenen van % cellulose
vegcom_kh$CEL_pct <- (vegcom_kh$ADF - vegcom_kh$ADL)

#kolomnaam veranderen kolomnaam ADL naar LIG_pct
vegcom_kh <- rename(vegcom_kh,c("ADL" = "LIG_pct"))

#kolomnaam RE veranderen in RE_NIR
vegcom_kh <- rename(vegcom_kh, c("RE" = "RE_NIR_pct"))

#RE_DS naam veranderen in RE_NATCHEM_DS_pct
vegcom_kh <- rename(vegcom_kh, c("RE_DS" = "RE_NATCHEM_DS_pct"))

#DS_105, VOCHT_105, N_DS omrekenen vanuit g/kg naar %
vegcom_kh$DS_pct <- (vegcom_kh$DS_105*0.1)
vegcom_kh$VOCHT_pct <- (vegcom_kh$VOCHT_105*0.1)
vegcom_kh$N_DS_pct <- (vegcom_kh$N_DS*0.1)

#RE uitdrukken op droge stofgehalte voor vergelijking met RE_NATCHEM
vegcom_kh$RE_NIR_DS_pct <- (vegcom_kh$RE_NIR*(1000/vegcom_kh$DS_105))

#maanden uitdrukken in cijfers
vegcom_kh$M <- revalue(vegcom_kh$M, c("jun" = "6", "jul" = "7", "aug" = "8",
                                      "sep" = "9"))
#of

#vegcom_kh$M[vegcom_kh$M == "jun"] <- "6"
#vegcom_kh$M[vegcom_kh$M == "jul"] <- "7"
#vegcom_kh$M[vegcom_kh$M == "aug"] <- "8"
#vegcom_kh$M[vegcom_kh$M == "sep"] <- "9"

#nieuwe kolom maken voor Brand in 2011
vegcom_kh <- vegcom_kh %>% 
  mutate(Brand2011 = ifelse(EXC_ID %in% c("WD1","WD2", "LV1", "LV2"), "ja", "nee"))
         
vegcom_kh <- vegcom_kh %>% 
  select(EXC_ID, BEH_TYP, M, Y, VEG_TYP, DS_pct, VOCHT_pct, 
                      LIG_pct, CEL_pct, HEMCEL_pct, N_DS_pct, RE_NIR_DS_pct, 
                      RE_NATCHEM_DS_pct, Brand2011)

#kolomnamen naar kleine letters
install.packages("janitor")
library(janitor)

vegcom_kh <- vegcom_kh %>%
  clean_names()

# Wegschrijven van de dataset
write.csv2(vegcom_kh, row.names = FALSE,
           file = "Data/Afgeleide datasets/vegcom_kh.csv")


##########################################
#niet gebruikt
##library(stringr)

#nieuwe variabele maken met de jaarlocatie
##vegcom_kh$yearlocation <- paste(vegcom_kh$EXC_ID, vegcom_kh$Y, sep = " ")

#nieuwe variabele maken met de maandlocatie
##vegcom_kh$monthlocation <- paste(vegcom_kh$EXC_ID, vegcom_kh$M, sep = " ")

#nieuwe variabele maken met de maand en jaar
##vegcom_kh$obs_M_Y <- paste(vegcom_kh$M, vegcom_kh$Y, sep = " ")

#nieuwe variabele maken met de maand, datum en locatie
##vegcom_kh$datelocation <- paste(vegcom_kh$EXC_ID, vegcom_kh$obs_M_Y, sep = " ")

#nieuwe variabele maken met beheertype per locatie
##vegcom_kh$EXC_ID_BEH_TYP <- paste(vegcom_kh$EXC_ID, vegcom_kh$BEH_TYP, sep = " ")

#nieuwe variabele maken met vegetatietype per locatie
##vegcom_kh$EXC_ID_VEG_TYP <- paste(vegcom_kh$EXC_ID, vegcom_kh$VEG_TYP, sep = " ")

#nieuwe variabele maken met unieke sleutel per staal EXC_ID BEH_TYP VEG_TYP M Y 
#vegcom_kh$SAMPLE_ID <- paste(vegcom_kh$EXC_ID, vegcom_kh$BEH_TYP,
                             #vegcom_kh$VEG_TYP, vegcom_kh$M, vegcom_kh$Y, 
                             #sep = " ")






