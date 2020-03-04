library(lubridate)
library(tidyverse)
library(plyr)
library(readr)
library(readxl)
library(janitor)

###inlezen Excel met invoerdata en toevoegen toestel_id
vegbed_kh <- read_excel("./Data/Basisdatasets/HTbl_Vegetatie_2011_2014.xlsx",
                                 sheet = 1,.name_repair = make_clean_names)

#Deleten onnodige velden
vegbed_kh <- vegbed_kh %>% 
  select(-id_vegetatie_opname, -id_exclosure_opname, -deelgebied, -id_exclosure, 
         -exclosure_zone,-id_hoofdcategorie, -id_bedekkingschaal, -bedekkingschaal,
         -bloeistengels_aanwezig, -opmerking, -bedekking_mos, -mos_bedekking, 
         -mos_bedekking_getal, -bedekking_kale_bodem, -kaal_bedekking, 
         -kaal_bedekking_getal, -jaar, -maand)

#na is wellicht niet aanwezig...

#kolomnamen hernoemen, let op er zijn twee functies rename in tidyverse en dplyr,
# daarom specifiek deze van dplyr oproepen door dplyr::rename, anders error!
vegbed_kh <- dplyr::rename(vegbed_kh, exc_id = afkorting, 
                           brand2011 = afgebrand_voorjaar_2011, 
                           beh_typ = begraasd_niet_begr, 
                           aant_bloeist_pijpenstro = aantal_bloeistengels,
                           zone = id_exclosure_zone, mos = mos_oppervlakte, 
                           kale_bodem = kale_oppervlakte)

#waarden kol brand2011 hernoemen
vegbed_kh$brand2011 <- mapvalues(vegbed_kh$brand2011, from = c(TRUE, FALSE),
                                 to = c("ja", "nee"))
vegbed_kh$beh_typ <- mapvalues(vegbed_kh$beh_typ, from = c("B", "NB"),
                                 to = c("BGR", "EXC"))

#omzetten van lang naar breed formaat voor soort
unique(vegbed_kh$soort)
vegbed_kh <- vegbed_kh %>% 
spread(soort, bedekking_getal,fill = 0)

vegbed_kh <- clean_names(vegbed_kh)

#ordenen variabelen
vegbed_kh <- select(vegbed_kh, -totale_bedekking, -staal_ingezameld)

#wegschrijven .csv
write.csv2(vegbed_kh, row.names = FALSE,
           file = "Data/Afgeleide datasets/vegbed_kh.csv")
