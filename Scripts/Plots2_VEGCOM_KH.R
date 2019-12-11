library(tidyverse)

# path aangepast
VEGCOM.KH <- read.csv2("Data/Afgeleide datasets/VEGCOM.KH.csv",
                       stringsAsFactors = FALSE)

str(VEGCOM.KH)

# je kan de figuren ook rechtstreeks maken vanaf de ruwe data

  VEGCOM.KH$Y <- as.factor(VEGCOM.KH$Y)

VEGCOM.KH %>%
  ggplot(aes(x = M, y = LIG_pct, color = Y)) +
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


##GEM_LIG_pct
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
filename = paste("Gemiddeld percentage lignine per maand vs beheertype", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

VEGCOM.KH %>%
  ggplot(aes(x = M, y = LIG_pct, color = Y)) +
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
filename = paste("Gemiddeld percentage lignine per maand vs jaar", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

##GEM_CEL_pct
VEGCOM.KH %>%
  ggplot(aes(x = M, y = CEL_pct, color = BEH_TYP)) +
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
filename = paste("Gemiddeld percentage cellulose per maand vs. beheertype", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

VEGCOM.KH %>%
  ggplot(aes(x = M, y = CEL_pct, color = Y)) +
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
filename = paste("Gemiddeld percentage cellulose per maand vs. jaar", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

##GEM_HEMCEL_pct
VEGCOM.KH %>%
  ggplot(aes(x = M, y = HEMCEL_pct, color = BEH_TYP)) +
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
filename = paste("Gemiddeld percentage hemicellulose per maand vs. beheertype", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

VEGCOM.KH %>%
  ggplot(aes(x = M, y = HEMCEL_pct, color = Y)) +
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
filename = paste("Gemiddeld percentage hemicellulose per maand vs. jaar", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

##GEM_N_DS_pct
VEGCOM.KH %>%
  ggplot(aes(x = M, y = N_DS_pct, color = BEH_TYP)) +
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
filename = paste("Gemiddeld percentage stikstof per maand vs. beheertype", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

VEGCOM.KH %>%
  ggplot(aes(x = M, y = N_DS_pct, color = Y)) +
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
filename = paste("Gemiddeld percentage stikstof per maand vs. jaar", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

##GEM_RE_NIR_DS_pct
VEGCOM.KH %>%
  ggplot(aes(x = M, y = RE_NIR_DS_pct, color = BEH_TYP)) +
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
filename = paste("Gemiddeld percentage ruw eiwit (NIR methode) per maand vs. beheertype", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

VEGCOM.KH %>%
  ggplot(aes(x = M, y = RE_NIR_DS_pct, color = Y)) +
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
filename = paste("Gemiddeld percentage ruw eiwit (NIR methode) per maand vs. jaar", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

##GEM_RE_NATCHEM_DS_pct
VEGCOM.KH %>%
  ggplot(aes(x = M, y = RE_NATCHEM_DS_pct, color = BEH_TYP)) +
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
filename = paste("Gemiddeld percentage ruw eiwit (natchemische methode) per maand vs. beheertype", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

VEGCOM.KH %>%
  ggplot(aes(x = M, y = RE_NATCHEM_DS_pct, color = Y)) +
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
filename = paste("Gemiddeld percentage ruw eiwit (natchemische methode) per maand vs. jaar", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

#################################################################################
### Zwaar ondiep verbrand in 2011 of niet"
##GEM_LIG_pct
VEGCOM.KH %>%
  ggplot(aes(x = M, y = LIG_pct, color = Brand2011)) +
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
filename = paste("Gemiddeld percentage lignine per maand vs Brand2011", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)


##GEM_CEL_pct
VEGCOM.KH %>%
  ggplot(aes(x = M, y = CEL_pct, color = Brand2011)) +
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
filename = paste("Gemiddeld percentage cellulose per maand vs. Brand2011", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

##GEM_HEMCEL_pct
VEGCOM.KH %>%
  ggplot(aes(x = M, y = HEMCEL_pct, color = Brand2011)) +
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
filename = paste("Gemiddeld percentage hemicellulose per maand vs. Brand2011", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)


##GEM_N_DS_pct
VEGCOM.KH %>%
  ggplot(aes(x = M, y = N_DS_pct, color = Brand2011)) +
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
filename = paste("Gemiddeld percentage stikstof per maand vs. Brand2011", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

##GEM_RE_NIR_DS_pct
VEGCOM.KH %>%
  ggplot(aes(x = M, y = RE_NIR_DS_pct, color = Brand2011)) +
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
filename = paste("Gemiddeld percentage ruw eiwit (NIR methode) per maand vs. Brand2011", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)

##GEM_RE_NATCHEM_DS_pct
VEGCOM.KH %>%
  ggplot(aes(x = M, y = RE_NATCHEM_DS_pct, color = Brand2011)) +
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
filename = paste("Gemiddeld percentage ruw eiwit (natchemische methode) per maand vs. Brand2011", ".tiff", sep = '')
ggsave(file = filename,
       path = "Output/Plots_per_maand/",
       width = 7.5, height = 5.5, dpi = 300)


# extra toevoeging om plots op te splitsen: facet_wrap(~ VEG_TYP)
##GEM_M_VEG_TYP <- VEGCOM.KH %>%
##count (BEH_TYP, M, VEG_TYP)

# er zijn vier van de 40 groepen waar je NA of -Inf hebt
#EM_M_BEH_VEG_TYP %>%
#filter(is.na(GEM_LIG_pct)) %>%
#View()

# deze groepen in detail bekijken welke ruwe data erin zitten
#probleemgevallen <- GEM_M_BEH_VEG_TYP %>%
#filter(is.na(GEM_LIG_pct)) %>%
#select(BEH_TYP, M, VEG_TYP) %>%
#ungroup()


#VEGCOM.KH %>%
#semi_join(probleemgevallen) %>%
#arrange(BEH_TYP, M, VEG_TYP)

# dit is normaal, zie ?max: For numeric x max(x) == -Inf and min(x) == +Inf
# whenever length(x) == 0 (after removing missing values if requested).

### Grafiek van de gemiddelde waarden per beheertype, pper maand
### en per vegetatietype

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
