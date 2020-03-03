# aanmaken van een .csv met de data van de vegetatiestructuur

#nieuwe kolom maken voor type begrazing
summarise(VEGCOM.KH, unique(EXC_ID))

mutate(BEG_TYP = ifelse(EXC_ID %in% c("WD1","WD2", "LV1", "LV2"), "extensief", "gehoed" ))