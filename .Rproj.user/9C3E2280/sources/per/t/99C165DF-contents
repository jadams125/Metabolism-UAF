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

# Have to rename rows so they done have "mean" in them so it can be combined for three straight years. Data is not cleaned.

POKE_EXO_cl.2019.renamed <- POKE_EXO_cl.2019 %>%
  dplyr::rename(ODO.mgL = ODO.mgL.mn, ODO.Psat = ODO.Psat.mn, ODO.Ploc = ODO.Ploc.mn, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)



#ROUGHLY convert %LOC to mg/L with Air pressure at time of install
poke.2019.air.P.url <- "https://drive.google.com/drive/u/1/folders/12-av--A9_rqcyvwOl9EiB0MSZdmWnPcj"
poke_19.ap <- drive_get(as_id(poke.2019.air.P.url))
poke_19.ap_glist <- drive_ls(poke_19.ap, pattern = "191017_20005936_POKE_ATM.csv")
walk(poke_19.ap_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke.ap.2019.Data <- read.csv("191017_20005936_POKE_ATM.csv",
                              skip = 1, header = TRUE)
poke.ap.2019.Data$DateTime <- as.POSIXct(strptime(poke.ap.2019.Data$Date.Time..GMT.08.00, "%m/%d/%y %I:%M:%S %p"))

poke.ap.2019.Data$DateTime <- lubridate::round_date(poke.ap.2019.Data$DateTime, "15 minutes") 

#row 2 is not a real row
poke.ap.2019.Data <- poke.ap.2019.Data[-c(2), ]


#convert to mmHg
poke.ap.2019.Data$pressure.mmHg <- poke.ap.2019.Data$Abs.Pres..kPa..LGR.S.N..20005936..SEN.S.N..20005936..LBL..P. * 7.50062

#air pressure at start time: may 10, 2019 at 12:30
airPressureInstall <- poke.ap.2019.Data %>% filter(DateTime == "2019-05-10 12:30:00")




POKE_EXO_cl.2019.renamed$datetimeAK <- force_tz(POKE_EXO_cl.2019.renamed$datetimeAK, "America/Anchorage")

poke.exo.telemFilled.2020 <- POKE_EXO_cl.2019.renamed %>% filter(datetimeAK >= "2019-08-22 15:00:00" & datetimeAK <= "2019-09-11 11:30:00")

poke.exo.telemFilled.2020$ODO.Psat <-  729.8703 / 760 *  poke.exo.telemFilled.2020$ODO.Ploc




#Plot It

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/poke19filled.pdf")

plot(POKE_EXO_cl.2019.renamed$datetimeAK, POKE_EXO_cl.2019.renamed$ODO.Psat ,type="l",col="black", xlab = "date", ylab = "ODO %Sat",  ylim=c(93,107),)

lines(poke.exo.telemFilled.2020$datetimeAK, poke.exo.telemFilled.2020$ODO.Psat ,col="blue")

legend(2, 4, legend=c("Equation 1", "Equation 2"), 
       fill = c("blue","red")
)

dev.off()


#### 2020 ####
exo.all.2020.url <- "https://drive.google.com/drive/u/1/folders/1nNKoIdgP-fdCNRGUbGca_zVujF_16QEM"
exo.all.2020.prt1 <- drive_get(as_id(exo.all.2020.url))
exo.all.2020.glist <- drive_ls(exo.all.2020.prt1, pattern = "EXO.ALL.csv")
walk(exo.all.2020.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
exo.all.2020 <- read.csv("EXO.ALL.csv",)

poke.exo.2020 <- exo.all.2020 %>% filter(site.ID == "POKE")

poke.exo.2020$ODO.mgL <- 
  
  #calc from YSI %Sat to MGL spreadsheet
  as.numeric(poke.exo.2020$ODO.Psat) * (0.01* exp(
    (-862194900000*(1/(poke.exo.2020$Temp.C+273.15))^4+12438000000*(1/(poke.exo.2020$Temp.C+273.15))^3-66423080*(1/(poke.exo.2020$Temp.C+273.15))^2+157570.1*(1/(poke.exo.2020$Temp.C+273.15))-139.344)
      -0* (2140.7*(1/(poke.exo.2020$Temp.C+273.15))^2-10.754*(1/(poke.exo.2020$Temp.C+273.15))+0.017674 )))


poke.exo.2020$datetimeAK <- as.POSIXct(strptime(poke.exo.2020$datetimeAK, "%m/%e/%y %H:%M"))




# salinity temp and do percent value 


#### 2021 ####

#Read processed CSV from DoD 2021 Script 
exo.processed.2021 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2021/EXO_data/from_internal_harddrive/processed/EXO.processed.csv")

poke.exo.2021 <- exo.processed.2021 %>% filter(site.ID == "POKE")
poke.exo.2021 <- as.data.frame(poke.exo.2021)


#mean to when burst was taken

#mean bursts
mean2021odoMGL <- aggregate( ODO.mgL ~ datetimeAK, poke.exo.2021, mean)
mean2021odoPSAT <- aggregate( ODO.Psat ~ datetimeAK, poke.exo.2021, mean)
mean2021odoPLOC <- aggregate( ODO.Ploc ~ datetimeAK, poke.exo.2021, mean)
mean2021odoTEMPC <- aggregate( Temp.C ~ datetimeAK, poke.exo.2021, mean)

means2021ODO <- plyr::join(mean2021odoMGL, mean2021odoPSAT, by = "datetimeAK")
means2021ODO <- plyr::join(means2021ODO, mean2021odoPLOC, by = "datetimeAK")
means2021ODO <- plyr::join(means2021ODO, mean2021odoTEMPC, by = "datetimeAK")


#Put together
selected.poke.exo.2020 <- poke.exo.2020 %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)

selected.poke.exo.2021 <- means2021ODO %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)


All.years.poke.MESSY <- rbind(POKE_EXO_cl.2019.renamed, selected.poke.exo.2020, selected.poke.exo.2021)


#keep in mind 2019 is clean data and 2020 and 2021 are not.
pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/PokerODO.pdf")

testPlotPoke <- ggplot(data = All.years.poke.MESSY,
                       mapping = aes(x = datetimeAK, y = as.numeric(ODO.Psat))) + geom_point() + labs(x = "Date", y = "Percent Saturation")
testPlotPoke
dev.off()


###### STUART ######

#### 2019 ####

#Read cleaned CSVs from DoD 2019 Script 
STRT_EXO_cl.2019 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/EXO_processed/STRT.EXO.cl.csv")

# Have to rename rows so they done have "mean" in them so it can be combined for three straight years. Data is not cleaned.

STRT_EXO_cl.2019.renamed <- STRT_EXO_cl.2019 %>%
  dplyr::rename(ODO.mgL = ODO.mgL.mn, ODO.Psat = ODO.Psat.mn, ODO.Ploc = ODO.Ploc.mn, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)




#### 2020 ####

strt.exo.2020 <- exo.all.2020 %>% filter(site.ID == "STRT")

#convert Bursts to means



strt.exo.2020$datetimeAK <- as.POSIXct(strptime(strt.exo.2020$datetimeAK, "%m/%e/%y %H:%M"))



strt.exo.2020$Temp.C <- as.numeric(strt.exo.2020$Temp.C)
strt.exo.2020$ODO.Psat <- as.numeric(strt.exo.2020$ODO.Psat)
strt.exo.2020$ODO.mgL <- as.numeric(strt.exo.2020$ODO.mgL)
strt.exo.2020$ODO.Ploc <- as.numeric(strt.exo.2020$ODO.Ploc)



#mean bursts
STRTmean2020odoMGL <- aggregate( ODO.mgL ~ datetimeAK, strt.exo.2020, mean)
STRTmean2020odoPSAT <- aggregate( ODO.Psat ~ datetimeAK, strt.exo.2020, mean)
STRTmean2020odoPLOC <- aggregate( ODO.Ploc ~ datetimeAK, strt.exo.2020, mean)
STRTmean2020odoTEMPC <- aggregate( Temp.C ~ datetimeAK, strt.exo.2020, mean)

STRTmeans2020ODO <- plyr::join(STRTmean2020odoMGL, STRTmean2020odoPSAT, by = "datetimeAK")
STRTmeans2020ODO <- plyr::join(STRTmeans2020ODO, STRTmean2020odoPLOC, by = "datetimeAK")
STRTmeans2020ODO <- plyr::join(STRTmeans2020ODO, STRTmean2020odoTEMPC, by = "datetimeAK")

#### 2021 ####
strt.exo.2021 <- exo.processed.2021 %>% filter(site.ID == "STRT")


strt.exo.2021$datetimeAK <- lubridate::round_date(strt.exo.2021$datetimeAK, "15 minutes")
strt.exo.2021$datetimeAK <- force_tz(strt.exo.2021$datetimeAK, "America/Anchorage")

strt.exo.2021$Temp.C <- as.numeric(strt.exo.2021$Temp.C)
strt.exo.2021$ODO.Psat <- as.numeric(strt.exo.2021$ODO.Psat)
strt.exo.2021$ODO.mgL <- as.numeric(strt.exo.2021$ODO.mgL)
strt.exo.2021$ODO.Ploc <- as.numeric(strt.exo.2021$ODO.Ploc)



#mean bursts
STRTmean2021odoMGL <- aggregate( ODO.mgL ~ datetimeAK, strt.exo.2021, mean)
STRTmean2021odoPSAT <- aggregate( ODO.Psat ~ datetimeAK, strt.exo.2021, mean)
STRTmean2021odoPLOC <- aggregate( ODO.Ploc ~ datetimeAK, strt.exo.2021, mean)
STRTmean2021odoTEMPC <- aggregate( Temp.C ~ datetimeAK, strt.exo.2021, mean)

STRTmeans2021ODO <- plyr::join(STRTmean2021odoMGL, STRTmean2021odoPSAT, by = "datetimeAK")
STRTmeans2021ODO <- plyr::join(STRTmeans2021ODO, STRTmean2021odoPLOC, by = "datetimeAK")
STRTmeans2021ODO <- plyr::join(STRTmeans2021ODO, STRTmean2021odoTEMPC, by = "datetimeAK")



#### Put together ####

All.years.strt.MESSY <- rbind(STRT_EXO_cl.2019.renamed, STRTmeans2020ODO, STRTmeans2021ODO)


#keep in mind 2019 is clean data and 2020 and 2021 are not.
pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/StuartODO.pdf")

testPlotSTRT <- ggplot(data = All.years.strt.MESSY,
                       mapping = aes(x = datetimeAK, y = as.numeric(ODO.Psat))) + geom_point() + labs(x = "Date", y = "Percent Saturation")
testPlotSTRT
dev.off()





####### VAULT #######

#### 2019 ####
VAUL_EXO_cl.2019 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/EXO_processed/VAUL.EXO.cl.csv")

# Have to rename rows so they done have "mean" in them so it can be combined for three straight years. Data is not cleaned.

VAUL_EXO_cl.2019.renamed <- VAUL_EXO_cl.2019 %>%
  dplyr::rename(ODO.mgL = ODO.mgL.mn, ODO.Psat = ODO.Psat.mn, ODO.Ploc = ODO.Ploc.mn, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)



### 2020 ####

vaul.exo.2020 <- exo.all.2020 %>% filter(site.ID == "VAUL")

#convert Bursts to means



vaul.exo.2020$datetimeAK <- as.POSIXct(strptime(vaul.exo.2020$datetimeAK, "%m/%e/%y %H:%M"))



vaul.exo.2020$Temp.C <- as.numeric(vaul.exo.2020$Temp.C)
vaul.exo.2020$ODO.Psat <- as.numeric(vaul.exo.2020$ODO.Psat)
vaul.exo.2020$ODO.mgL <- as.numeric(vaul.exo.2020$ODO.mgL)
vaul.exo.2020$ODO.Ploc <- as.numeric(vaul.exo.2020$ODO.Ploc)



#mean bursts
VAULmean2020odoMGL <- aggregate( ODO.mgL ~ datetimeAK, vaul.exo.2020, mean)
VAULmean2020odoPSAT <- aggregate( ODO.Psat ~ datetimeAK, vaul.exo.2020, mean)
VAULmean2020odoPLOC <- aggregate( ODO.Ploc ~ datetimeAK, vaul.exo.2020, mean)
VAULmean2020odoTEMPC <- aggregate( Temp.C ~ datetimeAK, vaul.exo.2020, mean)

VAULmeans2020ODO <- plyr::join(VAULmean2020odoMGL, VAULmean2020odoPSAT, by = "datetimeAK")
VAULmeans2020ODO <- plyr::join(VAULmeans2020ODO, VAULmean2020odoPLOC, by = "datetimeAK")
VAULmeans2020ODO <- plyr::join(VAULmeans2020ODO, VAULmean2020odoTEMPC, by = "datetimeAK")



















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
