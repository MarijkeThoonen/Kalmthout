#Loading packages
#ggplot 2 is included in the tydiverse package

library(tidyverse)
library(ggplot2)
library(scales)
library(grid)
library(plyr)


#inlezen afgeleide dataset

vegcom_kh <- read.csv2(file = "./Data/Afgeleide datasets/vegcom_kh.csv")

#aantal behandelingen waar een droogstaal van werd genomen
vegcom_kh_opnames <- distinct(vegcom_kh, exc_id, beh_typ, y, m)
write.csv2(vegcom_kh_opnames, row.names = FALSE, 
           file = "Data/Afgeleide datasets/vegcom_kh_opnames.csv")

#omzetten van jaartal in een discrete variabele: een factor
vegcom_kh$Y <- as.factor(vegcom_kh$Y)

#make boxplots for the dataframe vegcom_kh

#Plantenvariabelen per vegetatietype voor elk beheertype 
################################################################################
#discrete X (BEH_TYP), continuous Y (N_DS_PRT)
graphs <- dlply(
  vegcom_kh,  
  ~ VEG_TYP,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = N_DS_PRT)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Stikstof (%)", breaks = c(0,2.5,5,7.5,7.5,10)) +
            coord_cartesian(ylim = c(0, 10)) +
            theme_bw(13) + 
                theme(axis.text = element_text(size = rel(0.8)), 
                      axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Stikstof (%)", "in", deel[1,"VEG_TYP"])))
    filename = paste("Stikstof (prt)", " ", deel[1,"VEG_TYP"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/Stikstof(PRT)_per vegetatietype/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (BEH_TYP), continuous Y (DS_PRT)
graphs <- dlply(
  vegcom_kh,  
  ~ VEG_TYP,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = DS_PRT)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Droge stof (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(80,100)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Droge stof (%)", "in", deel[1,"VEG_TYP"])))
    filename = paste("Droge stof (prt)", " ", deel[1,"VEG_TYP"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/Droge stof_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (BEH_TYP), continuous Y (RE_NIR_DS)
graphs <- dlply(
  vegcom_kh,  
  ~ VEG_TYP,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = RE_NIR_DS)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Ruw eiwit NIR (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Ruw eiwit NIR (%)", "in", deel[1,"VEG_TYP"])))
    filename = paste("Ruw eiwit NIR (prt)", " ", deel[1,"VEG_TYP"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/Ruw eiwit_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (BEH_TYP), continuous Y (RE_NATCHEM_DS)
graphs <- dlply(
  vegcom_kh,  
  ~ VEG_TYP,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = RE_NATCHEM_DS)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Ruw eiwit NATCHEM (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Ruw eiwit NATCHEM (%)", "in", deel[1,"VEG_TYP"])))
    filename = paste("Ruw eiwit NATCHEM (prt)", " ", deel[1,"VEG_TYP"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/Ruw eiwit_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (BEH_TYP), continuous Y (CEL)
graphs <- dlply(
  vegcom_kh,  
  ~ VEG_TYP,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = CEL)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Cellulose (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Cellulose (%)", "in", deel[1,"VEG_TYP"])))
    filename = paste("Cellulose (prt)", " ", deel[1,"VEG_TYP"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/Celwandcomp_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (BEH_TYP), continuous Y (HEMCEL)
graphs <- dlply(
  vegcom_kh,  
  ~ VEG_TYP,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = HEMCEL)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Hemicellulose (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Hemicellulose (%)", "in", deel[1,"VEG_TYP"])))
    filename = paste("Hemicellulose (prt)", " ", deel[1,"VEG_TYP"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/Celwandcomp_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
  })
#discrete X (BEH_TYP), continuous Y (LIG)
graphs <- dlply(
  vegcom_kh,  
  ~ VEG_TYP,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = LIG)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Lignine (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Lignine (%)", "in", deel[1,"VEG_TYP"])))
    filename = paste("Lignine (prt)", " ", deel[1,"VEG_TYP"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/Celwandcomp_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
  })

#plots per EXC_ID
#discrete X (BEH_TYP), continuous Y (N_DS_PRT)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = N_DS_PRT)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Stikstof (%)", breaks = c(0,2.5,5,7.5,10)) +
            coord_cartesian(ylim = c(0, 10)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Stikstof (%)", "in", deel[1,"EXC_ID"])))
    filename = paste("Stikstof (prt)", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/Stikstof(PRT)_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (BEH_TYP), continuous Y (RE_NIR_DS)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = RE_NIR_DS)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Ruw eiwit NIR (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0, 60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Ruw eiwit NIR (%)", "in", deel[1,"EXC_ID"])))
    filename = paste("Ruw eiwit NIR (prt)", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
      ggsave(file = filename, path = "./Output/Plots_per_locatie/Ruw eiwit_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (BEH_TYP), continuous Y (RE_NATCHEM_DS)
#discrete X (BEH_TYP), continuous Y (RE_NIR_DS)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = RE_NATCHEM_DS)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Ruw eiwit NATCHEM (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0, 60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Ruw eiwit NATCHEM (%)", "in", deel[1,"EXC_ID"])))
    filename = paste("Ruw eiwit NATCHEM (prt)", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/Ruw eiwit_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (BEH_TYP), continuous Y (DS_PRT)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = DS_PRT)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Droge stof (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(80, 100)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Droge stof (%)", "in", deel[1,"EXC_ID"])))
    filename = paste("Droge stof (prt)", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/Droge stof_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (BEH_TYP), continuous Y (CEL)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = CEL)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Cellulose (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0, 70)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Cellulose (%)", "in", deel[1,"EXC_ID"])))
    filename = paste("Cellulose (prt)", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/Celwandcomp_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (BEH_TYP), continuous Y (HEMCEL)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = HEMCEL)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Hemicellulose (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0, 70)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Hemicellulose (%)", "in", deel[1,"EXC_ID"])))
    filename = paste("Hemicellulose (prt)", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/Celwandcomp_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })
#discrete X (BEH_TYP), continuous Y (LIG)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = LIG)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Lignine (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0, 70)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Lignine (%)", "in", deel[1,"EXC_ID"])))
    filename = paste("Lignine (prt)", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/Celwandcomp_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })



################################################################################
#Plantenvariabelen per jaar voor elk beheertype
################################################################################
#plots per jaar
#per Y, discrete X (BEH_TYP), continuous Y (N_DS_PRT)
graphs <- dlply(
  vegcom_kh,  
  ~ Y,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = N_DS_PRT)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Stikstof (%)", breaks = c(0,2.5,5,7.5,10)) +
            coord_cartesian(ylim = c(0, 10)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Stikstof (%)", "in", deel[1,"Y"])))
    filename = paste("Stikstof (prt)", " ", deel[1,"Y"], ".tiff", sep = '')
      ggsave(file = filename, path = "./Output/Plots_per_jaar/Stikstof(PRT)_per_jaar/", width = 7, height = 5.5, dpi = 100)
  })

#per Y, discrete X (BEH_TYP), continuous Y (RE_NIR_DS)
graphs <- dlply(
  vegcom_kh,  
  ~ Y,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = RE_NIR_DS)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Ruw eiwit NIR (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0, 60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Ruw eiwit NIR (%)", "in", deel[1,"Y"])))
    filename = paste("Ruw eiwit NIR (prt)", " ", deel[1,"Y"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_jaar/Ruwe eiwit (PRT)_per jaar/", width = 7, height = 5.5, dpi = 100)
  })
  
#per Y, discrete X (BEH_TYP), continuous Y (RE_NATCHEM_DS)
graphs <- dlply(
  vegcom_kh,  
  ~ Y,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = RE_NATCHEM_DS)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Ruw eiwit NATCHEM (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0, 60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Ruw eiwit NATCHEM (%)", "in", deel[1,"Y"])))
    filename = paste("Ruw eiwit NATCHEM (prt)", " ", deel[1,"Y"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_jaar/Ruwe eiwit (PRT)_per jaar/", width = 7, height = 5.5, dpi = 100)
  })

#per Y, discrete X (BEH_TYP), continuous Y (DS_105)
graphs <- dlply(
  vegcom_kh,  
  ~ Y,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = DS_PRT)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Droge stof (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(80, 100)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Droge stof (%)", "in", deel[1,"Y"])))
    filename = paste("Droge stof (prt)", " ", deel[1,"Y"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_jaar/Droge stof(PRT)_per_jaar/", width = 7, height = 5.5, dpi = 100)
  })

#per Y, discrete X (BEH_TYP), continuous Y (VOCHT_105)

#per Y, discrete X (BEH_TYP), continuous Y (CEL)
graphs <- dlply(
  vegcom_kh,  
  ~ Y,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = CEL)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Cellulose (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0, 70)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Cellulose (%)", "in", deel[1,"Y"])))
    filename = paste("Cellulose (prt)", " ", deel[1,"Y"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_jaar/Celwandcomp(PRT)_per_jaar/", width = 7, height = 5.5, dpi = 100)
  })

#per Y, discrete X (BEH_TYP), continuous Y (HEMCEL)
graphs <- dlply(
  vegcom_kh,  
  ~ Y,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = HEMCEL)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Hemicellulose (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0, 70)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Hemicellulose (%)", "in", deel[1,"Y"])))
    filename = paste("Hemicellulose (prt)", " ", deel[1,"Y"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_jaar/Celwandcomp(PRT)_per_jaar/", width = 7, height = 5.5, dpi = 100)
  })

#per Y, discrete X (BEH_TYP), continuous Y (LIG)
graphs <- dlply(
  vegcom_kh,  
  ~ Y,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = LIG)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Lignine (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0, 70)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Lignine (%)", "in", deel[1,"Y"])))
    filename = paste("Lignine (prt)", " ", deel[1,"Y"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_jaar/Celwandcomp(PRT)_per_jaar/", width = 7, height = 5.5, dpi = 100)
  })




################################################################################
#Plantenvariabelen voor de verschillende locaties
################################################################################
#discrete X (EXC_ID), continuous Y (N_DS_PRT)
graphs <-  
    print(ggplot(vegcom_kh,aes(x = EXC_ID, y = N_DS_PRT)) + 
            geom_boxplot() +
            scale_x_discrete("Locatie") +
            scale_y_continuous("Stikstof (%)", breaks = c(0,2.5,5,7.5,10)) +
            coord_cartesian(ylim = c(0, 10)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle("Stikstof (%) per locatie"))
    filename = "Stikstof (prt) per locatie.tiff"
    ggsave(file = filename, path = "./Output/Plots_per_locatie/", width = 7, height = 5.5, dpi = 100)
    
#discrete X (EXC_ID), continuous Y (RE_NIR_DS)
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = EXC_ID, y = RE_NIR_DS)) + 
              geom_boxplot() +
              scale_x_discrete("Locatie") +
              scale_y_continuous("Ruw eiwit NIR (%)", breaks = c(0,10,20,30,40,50,60)) +
              coord_cartesian(ylim = c(0,60)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Ruw eiwit NIR (%) per locatie"))
    filename = "Ruw eiwit NIR (PRT).tiff"
    ggsave(file = filename, path = "./Output/Plots_per_locatie/", width = 7, height = 5.5, dpi = 100)
    
#discrete X (EXC_ID), continuous Y (RE_NATCHEM_DS)
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = EXC_ID, y = RE_NATCHEM_DS)) + 
              geom_boxplot() +
              scale_x_discrete("Locatie") +
              scale_y_continuous("Ruw eiwit NATCHEM (%)", breaks = c(0,10,20,30,40,50,60)) +
              coord_cartesian(ylim = c(0,60)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Ruw eiwit NATCHEM (%) per locatie"))
    filename = "Ruw eiwit NATCHEM (PRT).tiff"
    ggsave(file = filename, path = "./Output/Plots_per_locatie/", width = 7, height = 5.5, dpi = 100)
    
#discrete X (EXC_ID), continuous Y (CEL)
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = EXC_ID, y = CEL)) + 
              geom_boxplot() +
              scale_x_discrete("Locatie") +
              scale_y_continuous("Cellulose (%)", breaks = c(0,10,20,30,40,50,60,70)) +
              coord_cartesian(ylim = c(0,70)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Cellulose (%) per locatie"))
    filename = "Cellulose (PRT).tiff"
    ggsave(file = filename, path = "./Output/Plots_per_locatie/", width = 7, height = 5.5, dpi = 100)
    
#discrete X (EXC_ID), continuous Y (HEMCEL)  
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = EXC_ID, y = HEMCEL)) + 
              geom_boxplot() +
              scale_x_discrete("Locatie") +
              scale_y_continuous("Hemicellulose (%)", breaks = c(0,10,20,30,40,50,60,70)) +
              coord_cartesian(ylim = c(0,70)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Hemicellulose (%) per locatie"))
    filename = "Hemicellulose (PRT).tiff"
    ggsave(file = filename, path = "./Output/Plots_per_locatie/", width = 7, height = 5.5, dpi = 100)
    
#discrete X (EXC_ID), continuous Y (LIG)
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = EXC_ID, y = LIG)) + 
              geom_boxplot() +
              scale_x_discrete("Locatie") +
              scale_y_continuous("Lignine (%)", breaks = c(0,10,20,30,40,50,60,70)) +
              coord_cartesian(ylim = c(0,40)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Lignine (%) per locatie"))
    filename = "Lignine (PRT).tiff"
    ggsave(file = filename, path = "./Output/Plots_per_locatie/", width = 7, height = 5.5, dpi = 100)
    

################################################################################
#Plantenvariabelen voor de verschillende jaren
################################################################################
#discrete X (Y), continuous Y (N_DS_PRT)
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = Y, y = N_DS_PRT)) + 
              geom_boxplot() +
              scale_x_discrete("Jaar") +
              scale_y_continuous("Stikstof (%)", breaks = c(0,2.5,5,7.5,7.5,10)) +
              coord_cartesian(ylim = c(0, 10)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Stikstof (%) per jaar"))
    filename = "Stikstof (prt) per jaar.tiff"
    ggsave(file = filename, path = "./Output/Plots_per_jaar", width = 7, height = 5.5, width = 7, height = 5.5, dpi = 100)
    
    #discrete X (Y), continuous Y (RE_NIR_DS)
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = Y, y = RE_NIR_DS)) + 
              geom_boxplot() +
              scale_x_discrete("Jaar") +
              scale_y_continuous("Ruw eiwit NIR (%)", breaks = c(0,10,20,30,40,50,60)) +
              coord_cartesian(ylim = c(0,60)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Ruw eiwit NIR (%) per jaar"))
    filename = "Ruw eiwit NIR (PRT) per jaar.tiff"
    ggsave(file = filename, path = "./Output/Plots_per_jaar/", width = 7, height = 5.5, dpi = 100)
    
    #discrete X (Y), continuous Y (RE_NATCHEM_DS)
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = Y, y = RE_NATCHEM_DS)) + 
              geom_boxplot() +
              scale_x_discrete("Jaar") +
              scale_y_continuous("Ruw eiwit NATCHEM (%)", breaks = c(0,10,20,30,40,50,60)) +
              coord_cartesian(ylim = c(0,60)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Ruw eiwit NATCHEM (%) per jaar"))
    filename = "Ruw eiwit NATCHEM (PRT) per jaar.tiff"
    ggsave(file = filename, path = "./Output/Plots_per_jaar/", width = 7, height = 5.5, dpi = 100)
    
    #discrete X (Y), continuous Y (CEL)
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = Y, y = CEL)) + 
              geom_boxplot() +
              scale_x_discrete("Jaar") +
              scale_y_continuous("Cellulose (%)", breaks = c(0,10,20,30,40,50,60,70)) +
              coord_cartesian(ylim = c(0,70)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Cellulose (%) per jaar"))
    filename = "Cellulose (PRT) per jaar.tiff"
    ggsave(file = filename, path = "./Output/Plots_per_jaar/", width = 7, height = 5.5, dpi = 100)
    
    #discrete X (Y), continuous Y (HEMCEL)  
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = Y, y = HEMCEL)) + 
              geom_boxplot() +
              scale_x_discrete("Jaar") +
              scale_y_continuous("Hemicellulose (%)", breaks = c(0,10,20,30,40,50,60,70)) +
              coord_cartesian(ylim = c(0,70)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Hemicellulose (%) per jaar"))
    filename = "Hemicellulose (PRT) per jaar.tiff"
    ggsave(file = filename, path = "./Output/Plots_per_jaar/", width = 7, height = 5.5, dpi = 100)
    
    #discrete X (Y), continuous Y (LIG)
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = Y, y = LIG)) + 
              geom_boxplot() +
              scale_x_discrete("Jaar") +
              scale_y_continuous("Lignine (%)", breaks = c(0,10,20,30,40,50,60,70)) +
              coord_cartesian(ylim = c(0,40)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Lignine (%) per jaar"))
    filename = "Lignine (PRT) per jaar.tiff"
    ggsave(file = filename, path = "./Output/Plots_per_jaar/", width = 7, height = 5.5, dpi = 100)


################################################################################
#Plantenvariabelen voor de verschillende vegetatietypes
################################################################################
#discrete X (VEG_TYP), continuous Y (N_DS_PRT)
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = VEG_TYP, y = N_DS_PRT)) + 
              geom_boxplot() +
              scale_x_discrete("Vegetatietype") +
              scale_y_continuous("Stikstof (%)", breaks = c(0,2.5,5,7.5,7.5,10)) +
              coord_cartesian(ylim = c(0, 10)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Stikstof (%) per vegetatietype"))
    filename = "Stikstof (prt) per vegetatietype.tiff"
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
    
    #discrete X (VEG_TYP), continuous Y (RE_NIR_DS)
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = VEG_TYP, y = RE_NIR_DS)) + 
              geom_boxplot() +
              scale_x_discrete("Vegetatietype") +
              scale_y_continuous("Ruw eiwit NIR (%)", breaks = c(0,10,20,30,40,50,60)) +
              coord_cartesian(ylim = c(0,60)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Ruw eiwit NIR (%) per vegetatietype"))
    filename = "Ruw eiwit NIR (PRT) per vegetatietype.tiff"
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
    
    #discrete X (VEG_TYP), continuous Y (RE_NATCHEM_DS)
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = VEG_TYP, y = RE_NATCHEM_DS)) + 
              geom_boxplot() +
              scale_x_discrete("Vegetatietype") +
              scale_y_continuous("Ruw eiwit NATCHEM (%)", breaks = c(0,10,20,30,40,50,60)) +
              coord_cartesian(ylim = c(0,60)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Ruw eiwit NATCHEM (%) per vegetatietype"))
    filename = "Ruw eiwit NATCHEM (PRT) per vegetatietype.tiff"
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
    
    #discrete X (VEG_TYP), continuous Y (CEL)
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = VEG_TYP, y = CEL)) + 
              geom_boxplot() +
              scale_x_discrete("Vegetatietype") +
              scale_y_continuous("Cellulose (%)", breaks = c(0,10,20,30,40,50,60,70)) +
              coord_cartesian(ylim = c(0,70)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Cellulose (%) per vegetatietype"))
    filename = "Cellulose (PRT) per vegetatietype.tiff"
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
    
    #discrete X (VEG_TYP), continuous Y (HEMCEL)  
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = VEG_TYP, y = HEMCEL)) + 
              geom_boxplot() +
              scale_x_discrete("Vegetatietype") +
              scale_y_continuous("Hemicellulose (%)", breaks = c(0,10,20,30,40,50,60,70)) +
              coord_cartesian(ylim = c(0,70)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Hemicellulose (%) per vegetatietype"))
    filename = "Hemicellulose (PRT) per vegetatietype.tiff"
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
    
    #discrete X (VEG_TYP), continuous Y (LIG)
    graphs <-  
      print(ggplot(vegcom_kh,aes(x = VEG_TYP, y = LIG)) + 
              geom_boxplot() +
              scale_x_discrete("Vegetatietype") +
              scale_y_continuous("Lignine (%)", breaks = c(0,10,20,30,40,50,60,70)) +
              coord_cartesian(ylim = c(0,40)) +
              theme_bw(13) + 
              theme(axis.text = element_text(size = rel(0.8)), 
                    axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
              ggtitle("Lignine (%) per vegetatietype"))
    filename = "Lignine (PRT) per vegetatietype.tiff"
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)


################################################################################
#Plantenvariabelen per locatie voor elk beheertype
################################################################################
#per EXC_ID, discrete X (BEH_TYP), continuous Y (N_DS_PRT)
    graphs <- dlply(
      vegcom_kh,  
      ~ EXC_ID,
      function(deel){
        print(ggplot(deel,aes(x = BEH_TYP, y = N_DS_PRT)) + 
                geom_boxplot() +
                scale_x_discrete("Beheertype") +
                scale_y_continuous("Stikstof (%)", breaks = c(0,2.5,5,7.5,10)) +
                coord_cartesian(ylim = c(0,10)) +
                theme_bw(13) + 
                theme(axis.text = element_text(size = rel(0.8)), 
                      axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
                ggtitle(paste("Stikstof (%)", "in", deel[1,"EXC_ID"])))
        filename = paste("Stikstof (PRT)", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
        ggsave(file = filename, path = "./Output/Plots_per_locatie/Ruw eiwit_per_locatie/", width = 7, height = 5.5, dpi = 100)
      })
    
#per EXC_ID, discrete X (BEH_TYP), continuous Y (RE_NIR_DS)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = RE_NIR_DS)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Ruw eiwit NIR (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Ruw eiwit NIR (%)", "in", deel[1,"EXC_ID"])))
    filename = paste("Ruw eiwit NIR (prt)", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/Ruw eiwit_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#per EXC_ID, discrete X (BEH_TYP), continuous Y (RE_NATCHEM_DS)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = RE_NATCHEM_DS)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Ruw eiwit NATCHEM (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Ruw eiwit NATCHEM (%)", "in", deel[1,"EXC_ID"])))
    filename = paste("Ruw eiwit NATCHEM (prt)", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/Ruw eiwit_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#per EXC_ID, discrete X (BEH_TYP), continuous Y (DS_105)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = DS_PRT)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Droge stof (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(80,100)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Droge stof (%)", "in", deel[1,"EXC_ID"])))
    filename = paste("Droge stof (prt)", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/Droge stof_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#per EXC_ID, discrete X (BEH_TYP), continuous Y (VOCHT_105)

#per EXC_ID, discrete X (BEH_TYP), continuous Y (CEL)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = CEL)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Cellulose (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0, 70)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Cellulose (%)", "in", deel[1,"EXC_ID"])))
    filename = paste("Cellulose (prt)", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/Celwandcomp_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#per EXC_ID, discrete X (BEH_TYP), continuous Y (HEMCEL)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = HEMCEL)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Hemicellulose (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0, 70)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Hemicellulose (%)", "in", deel[1,"EXC_ID"])))
    filename = paste("Hemicellulose (prt)", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/Celwandcomp_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#per EXC_ID, discrete X (BEH_TYP), continuous Y (LIG)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = LIG)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Lignine (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0, 70)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Lignine (%)", "in", deel[1,"EXC_ID"])))
    filename = paste("Lignine (prt)", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/Celwandcomp_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })


################################################################################
#Plantenvariabelen per maand (af te werken)
################################################################################

#per month, discrete X (BEH_TYP), continuous Y (N_DS_PRT)
graphs <- dlply(
  vegcom_kh,  
  ~ M,
  function(deel){
    print(ggplot(deel,aes(x = BEH_TYP, y = N_DS_PRT)) + 
            geom_boxplot() +
            scale_x_discrete("Beheertype") +
            scale_y_continuous("Stikstof (%)", breaks = c(0,2.5,5,7.5,10)) +
            coord_cartesian(ylim = c(0, 10)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Stikstof (%)", "in", deel[1,"M"])))
    filename = paste("Stikstof (prt)", " ", deel[1,"M"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_maand/Stikstof(PRT)_per_maand/", width = 7, height = 5.5, dpi = 100)
  })

####
#Plantenvariabelen per jaar voor elk vegetatietype
################################################################################
#discrete X (Y), continuous Y (N_DS_PRT)
graphs <- dlply(
  vegcom_kh,  
  ~ VEG_TYP,
  function(deel){
    print(ggplot(deel,aes(x = Y, y = N_DS_PRT)) + 
            geom_boxplot() +
            scale_x_discrete("Jaar") +
            scale_y_continuous("Stikstof (%)", breaks = c(0,2.5,5,7.5,10)) +
            coord_cartesian(ylim = c(0,10)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Stikstof (%) per jaar", "in", deel[1,"VEG_TYP"])))
    filename = paste("Stikstof (prt) per jaar", " ", deel[1,"VEG_TYP"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (Y), continuous Y (DS_PRT)
graphs <- dlply(
  vegcom_kh,  
  ~ VEG_TYP,
  function(deel){
    print(ggplot(deel,aes(x = Y, y = DS_PRT)) + 
            geom_boxplot() +
            scale_x_discrete("Jaar") +
            scale_y_continuous("Droge stof (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(80,100)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Droge stof (%)  per jaar", "in", deel[1,"VEG_TYP"])))
    filename = paste("Droge stof (prt) per jaar", " ", deel[1,"VEG_TYP"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (Y), continuous Y (RE_NIR_DS)
graphs <- dlply(
  vegcom_kh,  
  ~ VEG_TYP,
  function(deel){
    print(ggplot(deel,aes(x = Y, y = RE_NIR_DS)) + 
            geom_boxplot() +
            scale_x_discrete("Jaar") +
            scale_y_continuous("Ruw eiwit NIR (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Ruw eiwit NIR (%) per jaar", "in", deel[1,"VEG_TYP"])))
    filename = paste("Ruw eiwit NIR (prt) per jaar", " ", deel[1,"VEG_TYP"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (Y), continuous Y (RE_NATCHEM_DS)
graphs <- dlply(
  vegcom_kh,  
  ~ VEG_TYP,
  function(deel){
    print(ggplot(deel,aes(x = Y, y = RE_NATCHEM_DS)) + 
            geom_boxplot() +
            scale_x_discrete("Jaar") +
            scale_y_continuous("Ruw eiwit NATCHEM (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Ruw eiwit NATCHEM (%) per jaar", "in", deel[1,"VEG_TYP"])))
    filename = paste("Ruw eiwit NATCHEM (PRT) per jaar", " ", deel[1,"VEG_TYP"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (Y), continuous Y (CEL)
graphs <- dlply(
  vegcom_kh,  
  ~ VEG_TYP,
  function(deel){
    print(ggplot(deel,aes(x = Y, y = CEL)) + 
            geom_boxplot() +
            scale_x_discrete("Jaar") +
            scale_y_continuous("Cellulose (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Cellulose (%) per jaar", "in", deel[1,"VEG_TYP"])))
    filename = paste("Cellulose (prt) per jaar", " ", deel[1,"VEG_TYP"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (Y), continuous Y (HEMCEL)
graphs <- dlply(
  vegcom_kh,  
  ~ VEG_TYP,
  function(deel){
    print(ggplot(deel,aes(x = Y, y = HEMCEL)) + 
            geom_boxplot() +
            scale_x_discrete("Jaar") +
            scale_y_continuous("Hemicellulose (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Hemicellulose (%) per jaar", "in", deel[1,"VEG_TYP"])))
    filename = paste("Hemicellulose (prt) per jaar", " ", deel[1,"VEG_TYP"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (Y), continuous Y (LIG)
graphs <- dlply(
  vegcom_kh,  
  ~ VEG_TYP,
  function(deel){
    print(ggplot(deel,aes(x = Y, y = LIG)) + 
            geom_boxplot() +
            scale_x_discrete("Jaar") +
            scale_y_continuous("Lignine (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,30)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Lignine (%) per jaar", "in", deel[1,"VEG_TYP"])))
    filename = paste("Lignine (prt) per jaar", " ", deel[1,"VEG_TYP"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_vegetatietype/", width = 7, height = 5.5, dpi = 100)
  })

####
#Plantenvariabelen per jaar voor elke locatie
################################################################################
#discrete X (Y), continuous Y (N_DS_PRT)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = Y, y = N_DS_PRT)) + 
            geom_boxplot() +
            scale_x_discrete("Jaar") +
            scale_y_continuous("Stikstof (%)", breaks = c(0,2.5,5,7.5,10)) +
            coord_cartesian(ylim = c(0,10)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Stikstof (%) per jaar", "in", deel[1,"EXC_ID"])))
    filename = paste("Stikstof (prt) per jaar", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (Y), continuous Y (DS_PRT)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = Y, y = DS_PRT)) + 
            geom_boxplot() +
            scale_x_discrete("Jaar") +
            scale_y_continuous("Droge stof (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(80,100)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Droge stof (%) per jaar", "in", deel[1,"EXC_ID"])))
    filename = paste("Droge stof (prt) per jaar", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (Y), continuous Y (RE_NIR_DS)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = Y, y = RE_NIR_DS)) + 
            geom_boxplot() +
            scale_x_discrete("Jaar") +
            scale_y_continuous("Ruw eiwit NIR (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Ruw eiwit NIR (%) per jaar", "in", deel[1,"EXC_ID"])))
    filename = paste("Ruw eiwit NIR (prt) per jaar", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (Y), continuous Y (RE_NATCHEM_DS)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = Y, y = RE_NATCHEM_DS)) + 
            geom_boxplot() +
            scale_x_discrete("Jaar") +
            scale_y_continuous("Ruw eiwit NATCHEM (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Ruw eiwit NATCHEM (%) per jaar", "in", deel[1,"EXC_ID"])))
    filename = paste("Ruw eiwit NATCHEM (prt) per jaar", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (Y), continuous Y (CEL)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = Y, y = CEL)) + 
            geom_boxplot() +
            scale_x_discrete("Jaar") +
            scale_y_continuous("Cellulose (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Cellulose (%) per jaar", "in", deel[1,"EXC_ID"])))
    filename = paste("Cellulose (prt) per jaar", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (Y), continuous Y (HEMCEL)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = Y, y = HEMCEL)) + 
            geom_boxplot() +
            scale_x_discrete("Jaar") +
            scale_y_continuous("Hemicellulose (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Hemicellulose (%) per jaar", "in", deel[1,"EXC_ID"])))
    filename = paste("Hemicellulose (prt) per jaar", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })

#discrete X (Y), continuous Y (LIG)
graphs <- dlply(
  vegcom_kh,  
  ~ EXC_ID,
  function(deel){
    print(ggplot(deel,aes(x = Y, y = LIG)) + 
            geom_boxplot() +
            scale_x_discrete("Jaar") +
            scale_y_continuous("Lignine (%)", breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
            coord_cartesian(ylim = c(0,60)) +
            theme_bw(13) + 
            theme(axis.text = element_text(size = rel(0.8)), 
                  axis.text.x = element_text(hjust = 0.5, vjust = 0.5)) +
            ggtitle(paste("Lignine (%) per jaar", "in", deel[1,"EXC_ID"])))
    filename = paste("Lignine (prt) per jaar", " ", deel[1,"EXC_ID"], ".tiff", sep = '')
    ggsave(file = filename, path = "./Output/Plots_per_locatie/", width = 7, height = 5.5, dpi = 100)
  })