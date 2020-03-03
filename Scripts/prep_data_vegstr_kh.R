library(tidyverse)
library(plyr)

#Import and control of dataset
vegstr_kh <- read.csv2("./Data/Basisdatasets/HTbl_Exclosure_opname_2011_2014.csv",
  stringsAsFactors = FALSE)

#Verkennen datatype en frame
str(vegstr_kh)
dim(vegstr_kh)

#veranderen ID_Excosure in afgekorte namen
vegstr_kh$ID_Exclosure <- mapvalues(vegstr_kh$ID_Exclosure, from = c(1, 2, 3, 4, 
                                                                     5, 6, 7, 8, 
                                                                     9),
                                    to = c("GB1", "GB2", "WD1", "WD2","LV1", 
                                           "LV2", "VEN1", "VEN2", "HD"))

#Deleten van de ID_Exclosure_opname, Deelgebied, Opmerking, ...
vegstr_kh <- vegstr_kh %>% 
  select(-ID_Exclosure_opname, -Deelgebied, -Opmerking, - Afstand_m, 
         -Hoek_gr, -Bedekking.mos, -Bedekking.kale.bodem, -strooisel.ingezameld,
         -bodemstalen.ingezameld, -H1_cm, -H2_cm, -H3_cm, -H4_cm, 
         -H5_cm, -H6_cm, -H7_cm, -H8_cm)

#kolomnamen hernoemen, let op er zijn twee functies rename in tidyverse en dplyr,
# daarom specifiek deze van dplyr oproepen door dplyr::rename, anders error!
vegstr_kh <- dplyr::rename(vegstr_kh, exc_id = ID_Exclosure)
vegstr_kh <- dplyr::rename(vegstr_kh, beh_typ = Begraasd.Niet.Begr, 
                           brand2011 = Afgebrand.2011,
                           y = jaar, m = maand, zone = ID_Exclosure_zone)
vegstr_kh <- dplyr::rename(vegstr_kh, veg_aanw = Vegetatie.aanwezig)

#waarden kol beh_typ hernoemen
distinct(vegstr_kh, beh_typ)
vegstr_kh$beh_typ <- mapvalues(vegstr_kh$beh_typ, from = c("NB", "B"),
                                    to = c("EXC", "BGR"))

#waarden kol veg_aanw en kol brand2011 hernoemen
distinct(vegstr_kh, veg_aanw)
vegstr_kh$veg_aanw <- mapvalues(vegstr_kh$veg_aanw, from = c("WAAR", "ONWAAR"),
                               to = c("ja", "nee"))

distinct(vegstr_kh, veg_aanw)
vegstr_kh$brand2011 <- mapvalues(vegstr_kh$brand2011, from = c("WAAR", "ONWAAR"),
                                to = c("ja", "nee"))
