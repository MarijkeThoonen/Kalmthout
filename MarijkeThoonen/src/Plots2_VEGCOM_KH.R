library(tidyverse)

# path aangepast
VEGCOM.KH <- read.csv2("data/VEGCOM.KH.csv",
                       stringsAsFactors = FALSE)

str(VEGCOM.KH)

##GEM_M_VEG_TYP <- VEGCOM.KH %>%
  ##count (M, VEG_TYP)

### Grafiek van de gemiddelde waarden per maand en per vegetatietype
  GEM_M_BEH_VEG_TYP <- VEGCOM.KH %>%
  group_by(BEH_TYP, M, VEG_TYP) %>%
  summarize(GEM_LIG_pct = mean(LIG_pct, na.rm = TRUE),
            MAX_LIG_pct = max(LIG_pct, na.rm = TRUE),
            MIN_LIG_pct = min(LIG_pct, na.rm = TRUE),
            GEM_CEL_pct = mean(CEL_pct, na.rm = TRUE),
            MAX_CEL_pct = max(CEL_pct, na.rm = TRUE),
            MIN_CEL_pct = min(CEL_pct, na.rm = TRUE),
            GEM_HEMCEL_pct = mean(HEMCEL_pct, na.rm = TRUE),
            MAX_HEMCEL_pct = max(HEMCEL_pct, na.rm = TRUE),
            MIN_HEMCEL_pct = min(HEMCEL_pct, na.rm = TRUE),
            GEM_N_DS_pct = mean(N_DS_pct, na.rm = TRUE),
            MAX_N_DS_pct = max(N_DS_pct, na.rm = TRUE),
            MIN_N_DS_pct = min(N_DS_pct, na.rm = TRUE),
            GEM_RE_NIR_DS_pct = mean(RE_NIR_DS_pct, na.rm = TRUE),
            MAX_RE_NIR_DS_pct = max(RE_NIR_DS_pct, na.rm = TRUE),
            MIN_RE_NIR_DS_pct = min(RE_NIR_DS_pct, na.rm = TRUE),
            GEM_RE_NATCHEM_DS_pct = mean(RE_NATCHEM_DS_pct, na.rm = TRUE),
            MAX_RE_NATCHEM_DS_pct = max(RE_NATCHEM_DS_pct, na.rm = TRUE),
            MIN_RE_NATCHEM_DS_pct = min(RE_NATCHEM_DS_pct, na.rm = TRUE))

# er zijn vier van de 40 groepen waar je NA of -Inf hebt
GEM_M_BEH_VEG_TYP %>%
  filter(is.na(GEM_LIG_pct)) %>%
  View()

# deze groepen in detail bekijken welke ruwe data erin zitten
probleemgevallen <- GEM_M_BEH_VEG_TYP %>%
  filter(is.na(GEM_LIG_pct)) %>%
  select(BEH_TYP, M, VEG_TYP) %>%
  ungroup()


VEGCOM.KH %>%
  semi_join(probleemgevallen) %>%
  arrange(BEH_TYP, M, VEG_TYP)


# dit is normaal, zie ?max: For numeric x max(x) == -Inf and min(x) == +Inf whenever length(x) == 0 (after removing missing values if requested).

# je kan de figuren ook rechtstreeks maken vanaf de ruwe data

VEGCOM.KH %>%
  ggplot(aes(x = M, y = LIG_pct, color = BEH_TYP)) +
  stat_summary(fun.y = "mean", fun.ymax = "max", fun.ymin = "min", na.rm = TRUE,
               position=position_dodge(0.2)) +
  facet_wrap (~ VEG_TYP)

# of exact hetzelfde via
VEGCOM.KH %>%
  ggplot(aes(x = M, y = LIG_pct, color = BEH_TYP)) +
  stat_summary(fun.y = "mean", na.rm = TRUE,
               position=position_dodge(0.2),
               geom = "line") +
  stat_summary(fun.y = "mean", na.rm = TRUE,
               position=position_dodge(0.2),
               geom = "point") +
  stat_summary(fun.ymax = "max", fun.ymin = "min", na.rm = TRUE,
               position=position_dodge(0.2),
               geom = "errorbar", width = 0.2) +
  facet_wrap (~ VEG_TYP)



#door na.rm = TRUE toe te voegen gooi je ontbrekende waarden weg
#controle: print(GEM_M_BEH_VEG_TYP)

MEANS_KH_GROUP_MVEGBEH <- filter(GEM_M_BEH_VEG_TYP, GEM_LIG_pct != "NaN")

  # Wegschrijven van de dataset
  write.csv2(MEANS_KH_GROUP_MVEGBEH, row.names = FALSE,
             file = "Data/Afgeleide datasets/MEANS_KH_GROUP_MVEGBEH.csv")
read.csv2("Data/Afgeleide datasets/MEANS_KH_GROUP_MVEGBEH.csv")

##GEM_LIG_pct
  ggplot(data = MEANS_KH_GROUP_MVEGBEH,
  mapping = aes(x = M, y = GEM_LIG_pct, color = BEH_TYP)) +
  geom_line() +
  geom_errorbar(aes(ymin = MIN_LIG_pct, ymax = MAX_LIG_pct), width=.2,
                position=position_dodge(0.05)) +
  facet_wrap (~ VEG_TYP)
filename = paste("Gemiddeld percentage lignine per maand", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

##GEM_CEL_pct
ggplot(data = MEANS_KH_GROUP_MVEGBEH,
       mapping = aes(x = M, y = GEM_CEL_pct, color = BEH_TYP)) +
  geom_line() +
  geom_errorbar(aes(ymin = MIN_CEL_pct, ymax = MAX_CEL_pct), width=.2,
                position=position_dodge(0.05)) +
  facet_wrap (~ VEG_TYP)
filename = paste("Gemiddeld percentage cellulose per maand", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

##GEM_HEMCEL_pct
ggplot(data = MEANS_KH_GROUP_MVEGBEH,
       mapping = aes(x = M, y = GEM_HEMCEL_pct, color = BEH_TYP)) +
  geom_line() +
  geom_errorbar(aes(ymin = MIN_HEMCEL_pct, ymax = MAX_HEMCEL_pct), width=.2,
                position=position_dodge(0.05)) +
  facet_wrap (~ VEG_TYP)
filename = paste("Gemiddeld percentage hemicellulose per maand", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

##GEM_N_DS_pct
ggplot(data = MEANS_KH_GROUP_MVEGBEH,
       mapping = aes(x = M, y = GEM_N_DS_pct, color = BEH_TYP)) +
  geom_line() +
  geom_errorbar(aes(ymin = MIN_N_DS_pct, ymax = MAX_N_DS_pct), width=.2,
                position=position_dodge(0.05)) +
  facet_wrap (~ VEG_TYP)
filename = paste("Gemiddeld percentage stikstof (op droge stof) per maand", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

##GEM_RE_NIR_DS_pct
ggplot(data = MEANS_KH_GROUP_MVEGBEH,
       mapping = aes(x = M, y = GEM_RE_NIR_DS_pct, color = BEH_TYP)) +
  geom_line() +
  geom_errorbar(aes(ymin = MIN_RE_NIR_DS_pct, ymax = MAX_RE_NIR_DS_pct), width=.2,
                position=position_dodge(0.05)) +
  facet_wrap (~ VEG_TYP)
filename = paste("Gemiddeld percentage ruw eiwit (op droge stof) per maand NIR",
                 ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

##GEM_RE_NATCHEM_DS_pct
ggplot(data = MEANS_KH_GROUP_MVEGBEH,
       mapping = aes(x = M, y = GEM_RE_NATCHEM_DS_pct, color = BEH_TYP)) +
  geom_line() +
  geom_errorbar(aes(ymin = MIN_RE_NATCHEM_DS_pct, ymax = MAX_RE_NATCHEM_DS_pct), width=.2,
                position=position_dodge(0.05)) +
  facet_wrap (~ VEG_TYP)
filename = paste("Gemiddeld percentage ruw eiwit (op droge stof) per maand natchemisch",
                 ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

# extra toevoeging om plots op te splitsen: facet_wrap(~ VEG_TYP)
