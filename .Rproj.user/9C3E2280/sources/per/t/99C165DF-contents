### Jacob Adams
### Full DoD DO Record Script
### 7/6/2022

#Packages I think I might perhaps maybe need...
library(ggpubr)
library(anytime)
library(googlesheets4)
library(ggpmisc)
library(plyr)

library(dplyr)
library(lubridate)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

library(zoo)
library(xts)
library(forecast)
library(googledrive)
library(streamMetabolizer)
library(readr)

#calculate mg/L DO from Psat

# Identify which records do not have proper DO data for METAB run




########### POKE ###########

### 2019 ###
#Read cleaned CSVs from DoD 2019 Script 
POKE_EXO_cl.2019 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/EXO_processed/POKE.EXO.cl.csv")
View(POKE_EXO_cl)


#### 2020 ####
exo.all.2020.url <- "https://drive.google.com/drive/u/1/folders/1nNKoIdgP-fdCNRGUbGca_zVujF_16QEM"
exo.all.2020.prt1 <- drive_get(as_id(exo.all.2020.url))
exo.all.2020.glist <- drive_ls(exo.all.2020.prt1, pattern = "EXO.ALL.csv")
walk(exo.all.2020.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
exo.all.2020 <- read.csv("EXO.ALL.csv",)


poke.exo.2020 <- exo.all.2020 %>% filter(site.ID == "POKE")



poke.exo.2020$ODO.mgL.calcuated <- 
  
  #calc from YSI %Sat to MGL spreadsheet
  as.numeric(poke.exo.2020$ODO.Psat) * (0.01* exp(
    (-862194900000*(1/(poke.exo.2020$Temp.C+273.15))^4+12438000000*(1/(poke.exo.2020$Temp.C+273.15))^3-66423080*(1/(poke.exo.2020$Temp.C+273.15))^2+157570.1*(1/(poke.exo.2020$Temp.C+273.15))-139.344)
      -0* (2140.7*(1/(poke.exo.2020$Temp.C+273.15))^2-10.754*(1/(poke.exo.2020$Temp.C+273.15))+0.017674 )))


# salinity temp and do percent value 


#### 2021 ####

#Read processed CSV from DoD 2021 Script 
exo.processed.2021 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2021/EXO_data/from_internal_harddrive/processed/EXO.processed.csv")

poke.exo.2021 <- exo.processed.2021 %>% filter(site.ID == "POKE")




#STRT
STRT_EXO_cl <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/EXO_processed/STRT.EXO.cl.csv")
View(STRT_EXO_cl)

#MOOS
MOOS_EXO_cl <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/EXO_processed/MOOS.EXO.cl.csv")
View(MOOS_EXO_cl)

#VAUL
VAUL_EXO_cl <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/EXO_processed/VAUL.EXO.cl.csv")
View(VAUL_EXO_cl)

#FRCH
FRCH_EXO_cl <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/EXO_processed/FRCH.EXO.cl.csv")
View(FRCH_EXO_cl)
