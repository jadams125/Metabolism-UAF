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
library(data.table)

#calculate mg/L DO from Psat

# Identify which records do not have proper DO data for METAB run




########### POKE ###########

### 2019 ###
#Read cleaned CSVs from DoD 2019 Script 

SondeData2019 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/processed_sensor_dat/SUNA.EXO.int.corr.csv")

SondeData2019$datetimeAK <- force_tz(as.POSIXct(SondeData2019$datetimeAK), "America/Anchorage")

# Have to rename rows so they dont have "mean" in them so it can be combined for three straight years. Data is cleaned.

SondeData2019.renamed <- SondeData2019 %>%
  dplyr::rename(ODO.mgL = ODO.mgL.mn, ODO.Psat = ODO.Psat.mn, ODO.Ploc = ODO.Ploc.mn, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc, site.ID)


POKE_EXO_cl.2019.renamed <- SondeData2019.renamed %>% filter(site.ID == "POKE")

# 
# 
# POKE_EXO_cl.2019 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/EXO_processed/POKE.EXO.cl.csv")
# 
# POKE_EXO_cl.2019$datetimeAK <- force_tz(as.POSIXct(POKE_EXO_cl.2019$datetimeAK), "America/Anchorage")
# # Have to rename rows so they dont have "mean" in them so it can be combined for three straight years. Data is not cleaned.
# 
# POKE_EXO_cl.2019.renamed <- POKE_EXO_cl.2019 %>%
#   dplyr::rename(ODO.mgL = ODO.mgL.mn, ODO.Psat = ODO.Psat.mn, ODO.Ploc = ODO.Ploc.mn, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)
# 


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


# #Get missed out of water point
# POKE_EXO_cl.2019.renamed.1 <- POKE_EXO_cl.2019.renamed %>% filter(datetimeAK <= "2019-08-22 14:15:00")
# 
# 
# POKE_EXO_cl.2019.renamed.2 <- POKE_EXO_cl.2019.renamed %>% filter(datetimeAK >= "2019-08-22 16:00:00")
# 
# POKE_EXO_cl.2019.renamed <- rbind(POKE_EXO_cl.2019.renamed.1,POKE_EXO_cl.2019.renamed.2)




poke.exo.telemFilled.2019 <- POKE_EXO_cl.2019.renamed %>% filter(datetimeAK >= "2019-08-22 15:00:00" & datetimeAK <= "2019-09-11 11:30:00")

#change pressure to match current record
poke.exo.telemFilled.2019$ODO.Psat <-  758.85 / 760 *  poke.exo.telemFilled.2019$ODO.Ploc


#convert from PSAT to mg/L using the formula the EXO uses
poke.exo.telemFilled.2019$ODO.mgL <- as.numeric(poke.exo.telemFilled.2019$ODO.Psat) * (0.01* exp(
  (-862194900000*(1/(poke.exo.telemFilled.2019$Temp.C+273.15))^4+12438000000*(1/(poke.exo.telemFilled.2019$Temp.C+273.15))^3-66423080*(1/(poke.exo.telemFilled.2019$Temp.C+273.15))^2+157570.1*(1/(poke.exo.telemFilled.2019$Temp.C+273.15))-139.344)
  -0* (2140.7*(1/(poke.exo.telemFilled.2019$Temp.C+273.15))^2-10.754*(1/(poke.exo.telemFilled.2019$Temp.C+273.15))+0.017674 )))




#combine

#Plot It

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/poke19filled.pdf")

plot(POKE_EXO_cl.2019.renamed$datetimeAK, POKE_EXO_cl.2019.renamed$ODO.Psat ,type="l",col="black", xlab = "date", ylab = "ODO %Sat",  ylim=c(93,107),)

lines(poke.exo.telemFilled.2019$datetimeAK, poke.exo.telemFilled.2019$ODO.Psat ,col="blue")

legend(2, 4, legend=c("Equation 1", "Equation 2"),
       fill = c("blue","red")
)

dev.off()

#Put it together

POKE_EXO_cl.2019.renamed.3 <- POKE_EXO_cl.2019.renamed %>% filter(datetimeAK <= "2019-08-22 14:45:00")
POKE_EXO_cl.2019.renamed.4 <- POKE_EXO_cl.2019.renamed %>% filter(datetimeAK >= "2019-09-11 11:45:00")

final.Poke.DO.2019 <- rbind(POKE_EXO_cl.2019.renamed.3,POKE_EXO_cl.2019.renamed.4, poke.exo.telemFilled.2019)

final.Poke.DO.2019 <- dplyr::arrange(final.Poke.DO.2019, datetimeAK)

plot(final.Poke.DO.2019$datetimeAK,final.Poke.DO.2019$ODO.Psat, type="l",col="black")
# 
# # Poke:
# 
# #download flowmeter data
# WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
# WR.19.1 <- drive_get(as_id(WR_19.url))
# poke.wr19_glist <- drive_ls(WR.19.1, pattern = "poke_2019_flowmeter_Q_for_R_JAA.csv")
# walk(poke.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# poke_WR_19.Data <- read.csv("poke_2019_flowmeter_Q_for_R_JAA.csv",
#                             skip = 1, header = TRUE, na.strings=c("","NA","blank"))
# 
# poke_WR_19.Data <- poke_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# poke_WR_19.Data <- poke_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# poke_WR_19.Data$datetimeAK <- as.POSIXct(paste(poke_WR_19.Data$Date, poke_WR_19.Data$Time), format="%m/%d/%Y %H:%M")
# 
# 
# poke_WR_19.Data <- poke_WR_19.Data %>%
#   select(Depth..cm., datetimeAK)
# 
# Poke_depth_19 <- ddply(na.omit(poke_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))
# 
# # poke_wr.lm <- lm(meanDepth~datetimeAK, Poke_depth_19)
# 
# 
# Poke_depth_19 <- setDT(Poke_depth_19)
# 
# Poke_depth_19 <- Poke_depth_19 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# final.Poke.DO.2019 <- setDT(final.Poke.DO.2019)
# 
# setDT(Poke_depth_19)
# setDT(final.Poke.DO.2019)
# 
# final.Poke.DO.2019$datetimeAK1 <- final.Poke.DO.2019$datetimeAK
# 
# setkey( final.Poke.DO.2019, datetimeAK )
# setkey( Poke_depth_19, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_poke19 <- final.Poke.DO.2019[ Poke_depth_19, roll = "nearest" ]
# 
# rounded.dates_poke19 <- rounded.dates_poke19 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_poke19 <- rounded.dates_poke19 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_poke19 <- rounded.dates_poke19 %>%
#   select(meanDepth, datetimeAK)
# 
# Poke.2019.DO.Depth <- merge(final.Poke.DO.2019, rounded.dates_poke19, by = "datetimeAK", all = TRUE)
# Poke.2019.DO.Depth$meanDepth1 <- Poke.2019.DO.Depth$meanDepth
# 
# testmod <- lm(meanDepth ~ datetimeAK, Poke.2019.DO.Depth)
# 
# summary(testmod)
# 
# Poke.2019.DO.Depth <- Poke.2019.DO.Depth %>% 
#   mutate(predictedDepth = predict(testmod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))


final.Poke.DO.2019 <- final.Poke.DO.2019 %>% filter(datetimeAK < "2019-10-17 11:45:00")




### 2020 ###
# exo.all.2020.url <- "https://drive.google.com/drive/u/1/folders/1nNKoIdgP-fdCNRGUbGca_zVujF_16QEM"
# exo.all.2020.prt1 <- drive_get(as_id(exo.all.2020.url))
# exo.all.2020.glist <- drive_ls(exo.all.2020.prt1, pattern = "EXO.ALL.csv")
# walk(exo.all.2020.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# exo.all.2020 <- read.csv("EXO.ALL.csv",)

poke.exo.2020 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2020/EXO_processed/POKE.EXO.cl.csv")


poke.exo.2020 <- poke.exo.2020 %>%
  dplyr::rename(ODO.mgL = ODO.mgL.mn.adj, ODO.Psat = ODO.Psat.mn.adj, ODO.Ploc = ODO.Ploc.mn.adj, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc, site.ID)

poke.exo.2020$datetimeAK <- force_tz(as.POSIXct(poke.exo.2020$datetimeAK), "America/Anchorage")

# poke.exo.2020$ODO.mgL <- 
#   
#   #calc from YSI %Sat to MGL spreadsheet
#   as.numeric(poke.exo.2020$ODO.Psat) * (0.01* exp(
#     (-862194900000*(1/(poke.exo.2020$Temp.C+273.15))^4+12438000000*(1/(poke.exo.2020$Temp.C+273.15))^3-66423080*(1/(poke.exo.2020$Temp.C+273.15))^2+157570.1*(1/(poke.exo.2020$Temp.C+273.15))-139.344)
#       -0* (2140.7*(1/(poke.exo.2020$Temp.C+273.15))^2-10.754*(1/(poke.exo.2020$Temp.C+273.15))+0.017674 )))

# 
# poke.exo.2020$datetimeAK <- as.POSIXct(strptime(poke.exo.2020$datetimeAK, "%m/%e/%y %H:%M"))


plot(poke.exo.2020$datetimeAK, poke.exo.2020$ODO.mgL)

# salinity temp and do percent value 


# 
# 
#  DEPTH #
# 
# 
# # Poke:
# 
# #download flowmeter data
# WR_20.url <- "https://drive.google.com/drive/u/1/folders/1S2L8Qg08AIhQo1ZdKdaxlJliz4bi1ttr"
# WR.20.1 <- drive_get(as_id(WR_20.url))
# poke.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_POKE_for_R_JAA.csv")
# walk(poke.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# poke_WR_20.Data <- read.csv("R_Flowmeter Q calculation_POKE_for_R_JAA.csv",
#                             skip = 0, header = TRUE, na.strings=c("","NA","blank"))
# 
# 
# poke_WR_20.Data <- poke_WR_20.Data %>%
#   dplyr::rename(Date = ï..Date)
# 
# poke_WR_20.Data <- poke_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# poke_WR_20.Data <- poke_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# poke_WR_20.Data$datetimeAK <- as.POSIXct(paste(poke_WR_20.Data$Date, poke_WR_20.Data$Time), format="%y%m%d %H:%M")
# 
# 
# poke_WR_20.Data <- poke_WR_20.Data %>%
#   select(Depth, datetimeAK)
# 
# Poke_depth_20 <- ddply(na.omit(poke_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth)))
# 
# # poke_wr.lm <- lm(meanDepth~datetimeAK, Poke_depth_20)
# 
# 
# Poke_depth_20 <- setDT(Poke_depth_20)
# 
# Poke_depth_20 <- Poke_depth_20 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# poke.exo.2020 <- setDT(poke.exo.2020)
# 
# setDT(Poke_depth_20)
# setDT(poke.exo.2020)
# 
# poke.exo.2020$datetimeAK1 <- poke.exo.2020$datetimeAK
# 
# setkey( poke.exo.2020, datetimeAK )
# setkey( Poke_depth_20, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_poke20 <- poke.exo.2020[ Poke_depth_20, roll = "nearest" ]
# 
# rounded.dates_poke20 <- rounded.dates_poke20 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_poke20 <- rounded.dates_poke20 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_poke20 <- rounded.dates_poke20 %>%
#   select(meanDepth, datetimeAK)
# 
# Poke.2020.DO.Depth <- merge(poke.exo.2020, rounded.dates_poke20, by = "datetimeAK", all = TRUE)
# Poke.2020.DO.Depth$meanDepth1 <- Poke.2020.DO.Depth$meanDepth
# 
# poke20mod <- lm(meanDepth ~ datetimeAK, Poke.2020.DO.Depth)
# 
# summary(poke20mod)
# 
# Poke.2020.DO.Depth <- Poke.2020.DO.Depth %>% 
#   mutate(predictedDepth = predict(poke20mod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))
# 
# 





### 2021 ###

#Read processed CSV from DoD 2021 Script 
exo.processed.2021 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2021/EXO_data/from_internal_harddrive/processed/EXO.processed.csv")

poke.exo.2021 <- exo.processed.2021 %>% filter(site.ID == "POKE")
poke.exo.2021 <- as.data.frame(poke.exo.2021)


#mean to when burst was taken

#mean bursts
poke.exo.2021$datetimeAK <- lubridate::round_date(as.POSIXct( poke.exo.2021$datetimeAK), "15 minutes")

pokemean2021odoMGL <- aggregate( ODO.mgL ~ datetimeAK, poke.exo.2021, mean)
pokemean2021odoPSAT <- aggregate( ODO.Psat ~ datetimeAK, poke.exo.2021, mean)
pokemean2021odoPLOC <- aggregate( ODO.Ploc ~ datetimeAK, poke.exo.2021, mean)
pokemean2021odoTEMPC <- aggregate( Temp.C ~ datetimeAK, poke.exo.2021, mean)

pokemeans2021ODO <- plyr::join(pokemean2021odoMGL, pokemean2021odoPSAT, by = "datetimeAK")
pokemeans2021ODO <- plyr::join(pokemeans2021ODO, pokemean2021odoPLOC, by = "datetimeAK")
pokemeans2021ODO <- plyr::join(pokemeans2021ODO, pokemean2021odoTEMPC, by = "datetimeAK")




# 
# DEPTH #
# 
# 
# # Poke:
# 
# #download flowmeter data
# WR_21.url <- "https://drive.google.com/drive/u/1/folders/18z6vSz6SE3DEvUVDyfM8I3gqkGaxQqOl" 
# WR.21.1 <- drive_get(as_id(WR_21.url))
# poke.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter Q calculation_POKE_for_R_JAA_2021.csv")
# walk(poke.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# poke_WR_21.Data <- read.csv("R_Flowmeter Q calculation_POKE_for_R_JAA_2021.csv",
#                             skip = 0, header = TRUE, na.strings=c("","NA","blank"))
# 
# 
# 
# poke_WR_21.Data <- poke_WR_21.Data %>%
#   dplyr::rename(Date = ï..Date)
# 
# poke_WR_21.Data <- poke_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# poke_WR_21.Data <- poke_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# poke_WR_21.Data$datetimeAK <- as.POSIXct(paste(poke_WR_21.Data$Date, poke_WR_21.Data$Time), format="%y%m%d %H:%M")
# 
# 
# poke_WR_21.Data <- poke_WR_21.Data %>%
#   select(Depth..cm., datetimeAK)
# 
# Poke_depth_21 <- ddply(na.omit(poke_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))
# 
# # poke_wr.lm <- lm(meanDepth~datetimeAK, Poke_depth_21)
# 
# 
# Poke_depth_21 <- setDT(Poke_depth_21)
# 
# Poke_depth_21 <- Poke_depth_21 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# pokemeans2021ODO <- setDT(pokemeans2021ODO)
# 
# setDT(Poke_depth_21)
# setDT(pokemeans2021ODO)
# 
# pokemeans2021ODO$datetimeAK1 <- pokemeans2021ODO$datetimeAK
# 
# setkey( pokemeans2021ODO, datetimeAK )
# setkey( Poke_depth_21, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_poke21 <- pokemeans2021ODO[Poke_depth_21, roll = "nearest" ]
# 
# rounded.dates_poke21 <- rounded.dates_poke21 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_poke21 <- rounded.dates_poke21 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_poke21 <- rounded.dates_poke21 %>%
#   select(meanDepth, datetimeAK)
# 
# Poke.2021.DO.Depth <- merge(pokemeans2021ODO, rounded.dates_poke21, by = "datetimeAK", all = TRUE)
# Poke.2021.DO.Depth$meanDepth1 <- Poke.2021.DO.Depth$meanDepth
# 
# poke21mod <- lm(meanDepth ~ datetimeAK, Poke.2021.DO.Depth)
# 
# summary(poke21mod)
# 
# Poke.2021.DO.Depth <- Poke.2021.DO.Depth %>% 
#   mutate(predictedDepth = predict(poke21mod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))
# 




#Put together
final.Poke.DO.2019 <- final.Poke.DO.2019 %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)

poke.exo.2020 <- poke.exo.2020 %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)

pokemeans2021ODO <- pokemeans2021ODO %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)


All.years.poke.MESSY <- rbind(final.Poke.DO.2019, poke.exo.2020, pokemeans2021ODO)

All.years.poke.MESSY <- All.years.poke.MESSY %>%
  dplyr::rename(DO.obs = ODO.mgL)

All.years.poke.MESSY <- All.years.poke.MESSY %>%
  dplyr::rename(temp.water = Temp.C)


#plot all

#keep in mind 2019 and 2020 is clean data and 2021 is not.
pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/PokerODO.pdf")

testPlotPoke <- ggplot(data = All.years.poke.MESSY,
                       mapping = aes(x = datetimeAK, y = as.numeric(ODO.Psat))) + geom_point() + labs(x = "Date", y = "Percent Saturation")
testPlotPoke
dev.off()













###### STUART ######

#### 2019 ####

SondeData2019 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/processed_sensor_dat/SUNA.EXO.int.corr.csv")

SondeData2019$datetimeAK <- force_tz(as.POSIXct(SondeData2019$datetimeAK), "America/Anchorage")

# Have to rename rows so they dont have "mean" in them so it can be combined for three straight years. Data is cleaned.

SondeData2019.renamed <- SondeData2019 %>%
  dplyr::rename(ODO.mgL = ODO.mgL.mn, ODO.Psat = ODO.Psat.mn, ODO.Ploc = ODO.Ploc.mn, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc, site.ID)


STRT_EXO_cl.2019.renamed <- SondeData2019.renamed %>% filter(site.ID == "STRT")


# 
# 
# ########## DEPTH ############ 
# 
# 
# # Strt:
# 
# #download flowmeter data
# WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
# WR.19.1 <- drive_get(as_id(WR_19.url))
# strt.wr19_glist <- drive_ls(WR.19.1, pattern = "STRT_2019_flowmeter_Q_for_R_JAA.csv")
# walk(strt.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# strt_WR_19.Data <- read.csv("STRT_2019_flowmeter_Q_for_R_JAA.csv",
#                             skip = 0, header = TRUE, na.strings=c("","NA","blank","can't read note here, image 190807_2"))
# 
# strt_WR_19.Data <- strt_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# strt_WR_19.Data <- strt_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# strt_WR_19.Data$datetimeAK <- as.POSIXct(paste(strt_WR_19.Data$Date, strt_WR_19.Data$Time), format="%m/%d/%Y %H:%M")
# 
# 
# strt_WR_19.Data <- strt_WR_19.Data %>%
#   select(Depth..cm., datetimeAK)
# 
# Strt_depth_19 <- ddply(na.omit(strt_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))
# 
# # strt_wr.lm <- lm(meanDepth~datetimeAK, Strt_depth_19)
# 
# 
# Strt_depth_19 <- setDT(Strt_depth_19)
# 
# Strt_depth_19 <- Strt_depth_19 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# STRT_EXO_cl.2019.renamed <- setDT(STRT_EXO_cl.2019.renamed)
# 
# setDT(Strt_depth_19)
# setDT(STRT_EXO_cl.2019.renamed)
# 
# STRT_EXO_cl.2019.renamed$datetimeAK1 <- STRT_EXO_cl.2019.renamed$datetimeAK
# 
# setkey( STRT_EXO_cl.2019.renamed, datetimeAK )
# setkey( Strt_depth_19, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_strt19 <- STRT_EXO_cl.2019.renamed[ Strt_depth_19, roll = "nearest" ]
# 
# rounded.dates_strt19 <- rounded.dates_strt19 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_strt19 <- rounded.dates_strt19 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_strt19 <- rounded.dates_strt19 %>%
#   select(meanDepth, datetimeAK)
# 
# Strt.2019.DO.Depth <- merge(STRT_EXO_cl.2019.renamed, rounded.dates_strt19, by = "datetimeAK", all = TRUE)
# Strt.2019.DO.Depth$meanDepth1 <- Strt.2019.DO.Depth$meanDepth
# 
# strt19mod <- lm(meanDepth ~ datetimeAK, Strt.2019.DO.Depth)
# 
# summary(strt19mod)
# 
# Strt.2019.DO.Depth <- Strt.2019.DO.Depth %>% 
#   mutate(predictedDepth = predict(strt19mod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))


#USE TELEM FILLED DATA TO CALCULATE MG/L 


#calc from YSI %Sat to MGL spreadsheet

# # DO data collected as concentrations can be 
# converted to percent saturation using temperature and salinity data collected in conjunction with 
# the DO measurements using the equations as provided in APHA, 1989:


# DOsat = (Exp((-139.34411 + (157570.1/Temp) - (66423080/Temp2
# ) + (12438000000/Temp3
# ) - 
#   (862194900000/Temp4
#   )) - (Sal * (0.017674-(10.754/Temp)+(2140.7/Temp2
#   )))))
# % DO = (DOmeasure/ DOsat)*100 


# Where:
#   DOsat = DO concentration in mg/L at 100 % saturation, 
# Temp = water temperature in °K (°C + 273.15 = °K) 
# DOmeasure = Measured DO concentration in mg/L.
# Sal = Salinity in part per thousand (ppt)

#Constants: 
# 862194900000
# 12438000000
# 66423080
# 157570.1
# 139.344
#

#We change this according to how YSI does it in the EXO



#in this case, we are finding the DO mg/L per percent saturation, and multiplying it by our percent sat from telem. This percent sat accounts for pressure as well I believe. 


STRT_EXO_cl.2019.renamed$ODO.mgL.Calc <-as.numeric(STRT_EXO_cl.2019.renamed$ODO.Psat) * 
  #DO mg/L per sat
  (0.01* exp(
    #p1
    (-862194900000*(1/(STRT_EXO_cl.2019.renamed$Temp.C+273.15))^4+12438000000*(1/(STRT_EXO_cl.2019.renamed$Temp.C+273.15))^3-66423080*(1/(STRT_EXO_cl.2019.renamed$Temp.C+273.15))^2+157570.1*(1/(STRT_EXO_cl.2019.renamed$Temp.C+273.15))-139.344)
    #salinity
    -0* 
      #p2
      (2140.7*(1/(STRT_EXO_cl.2019.renamed$Temp.C+273.15))^2-10.754*(1/(STRT_EXO_cl.2019.renamed$Temp.C+273.15))+0.017674 )))


#fill missing rows with calculated MGL
STRT_EXO_cl.2019.renamed <- STRT_EXO_cl.2019.renamed %>% 
  mutate(ODO.mgL = coalesce(ODO.mgL,ODO.mgL.Calc))






#### 2020 ####


#### 2020 ####
# exo.all.2020.url <- "https://drive.google.com/drive/u/1/folders/1nNKoIdgP-fdCNRGUbGca_zVujF_16QEM"
# exo.all.2020.prt1 <- drive_get(as_id(exo.all.2020.url))
# exo.all.2020.glist <- drive_ls(exo.all.2020.prt1, pattern = "EXO.ALL.csv")
# walk(exo.all.2020.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# exo.all.2020 <- read.csv("EXO.ALL.csv",)

strt.exo.2020 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2020/EXO_processed/STRT.EXO.cl.csv")


strt.exo.2020 <- strt.exo.2020 %>%
  dplyr::rename(ODO.mgL = ODO.mgL.mn.adj, ODO.Psat = ODO.Psat.mn.adj, ODO.Ploc = ODO.Ploc.mn.adj, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc, site.ID)

strt.exo.2020$datetimeAK <- force_tz(as.POSIXct(strt.exo.2020$datetimeAK), "America/Anchorage")













# strt.exo.2020 <- exo.all.2020 %>% filter(site.ID == "STRT")
# 
# #convert Bursts to means
# 
# 
# 
# strt.exo.2020$datetimeAK <- as.POSIXct(strptime(strt.exo.2020$datetimeAK, "%m/%e/%y %H:%M"))
# 
# 
# 
# strt.exo.2020$Temp.C <- as.numeric(strt.exo.2020$Temp.C)
# strt.exo.2020$ODO.Psat <- as.numeric(strt.exo.2020$ODO.Psat)
# strt.exo.2020$ODO.mgL <- as.numeric(strt.exo.2020$ODO.mgL)
# strt.exo.2020$ODO.Ploc <- as.numeric(strt.exo.2020$ODO.Ploc)
# 
# 
# 
# #mean bursts
# STRTmean2020odoMGL <- aggregate( ODO.mgL ~ datetimeAK, strt.exo.2020, mean)
# STRTmean2020odoPSAT <- aggregate( ODO.Psat ~ datetimeAK, strt.exo.2020, mean)
# STRTmean2020odoPLOC <- aggregate( ODO.Ploc ~ datetimeAK, strt.exo.2020, mean)
# STRTmean2020odoTEMPC <- aggregate( Temp.C ~ datetimeAK, strt.exo.2020, mean)
# 
# STRTmeans2020ODO <- plyr::join(STRTmean2020odoMGL, STRTmean2020odoPSAT, by = "datetimeAK")
# STRTmeans2020ODO <- plyr::join(STRTmeans2020ODO, STRTmean2020odoPLOC, by = "datetimeAK")
# STRTmeans2020ODO <- plyr::join(STRTmeans2020ODO, STRTmean2020odoTEMPC, by = "datetimeAK")



########## DEPTH ############ 

# 
# # Strt:
# 
# #download flowmeter data
# WR_20.url <- "https://drive.google.com/drive/u/1/folders/1x_E4gaPvjRLDcrM8lN2ao4_o0bzdMBrb"
# WR.20.1 <- drive_get(as_id(WR_20.url))
# strt.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_STRT_for_R_JAA.csv")
# walk(strt.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# strt_WR_20.Data <- read.csv("R_Flowmeter Q calculation_STRT_for_R_JAA.csv",
#                             skip = 0, header = TRUE, na.strings=c("","NA","blank"))
# 
# 
# strt_WR_20.Data <- strt_WR_20.Data %>%
#   dplyr::rename(Date = ï..Date)
# 
# strt_WR_20.Data <- strt_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# strt_WR_20.Data <- strt_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# strt_WR_20.Data$datetimeAK <- as.POSIXct(paste(strt_WR_20.Data$Date, strt_WR_20.Data$Time), format="%y%m%d %H:%M")
# 
# 
# strt_WR_20.Data <- strt_WR_20.Data %>%
#   select(Depth..cm., datetimeAK)
# 
# Strt_depth_20 <- ddply(na.omit(strt_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))
# 
# # strt_wr.lm <- lm(meanDepth~datetimeAK, Strt_depth_20)
# 
# 
# Strt_depth_20 <- setDT(Strt_depth_20)
# 
# Strt_depth_20 <- Strt_depth_20 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# STRTmeans2020ODO <- setDT(STRTmeans2020ODO)
# 
# setDT(Strt_depth_20)
# setDT(STRTmeans2020ODO)
# 
# STRTmeans2020ODO$datetimeAK1 <- STRTmeans2020ODO$datetimeAK
# 
# setkey( STRTmeans2020ODO, datetimeAK )
# setkey( Strt_depth_20, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_strt20 <- STRTmeans2020ODO[ Strt_depth_20, roll = "nearest" ]
# 
# rounded.dates_strt20 <- rounded.dates_strt20 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_strt20 <- rounded.dates_strt20 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_strt20 <- rounded.dates_strt20 %>%
#   select(meanDepth, datetimeAK)
# 
# Strt.2020.DO.Depth <- merge(STRTmeans2020ODO, rounded.dates_strt20, by = "datetimeAK", all = TRUE)
# Strt.2020.DO.Depth$meanDepth1 <- Strt.2020.DO.Depth$meanDepth
# 
# strt20mod <- lm(meanDepth ~ datetimeAK, Strt.2020.DO.Depth)
# 
# summary(strt20mod)
# 
# Strt.2020.DO.Depth <- Strt.2020.DO.Depth %>% 
#   mutate(predictedDepth = predict(strt20mod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))




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

# ########## DEPTH ############ 
# 
# 
# # Strt:
# 
# #download flowmeter data
# WR_21.url <- "https://drive.google.com/drive/u/1/folders/1LTD4EFX3_Yas0ZCF8rKLl6dxSeZDvkDY"
# WR.21.1 <- drive_get(as_id(WR_21.url))
# strt.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter Q calculation_STRT_for_R_JAA_2021.csv")
# walk(strt.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# strt_WR_21.Data <- read.csv("R_Flowmeter Q calculation_STRT_for_R_JAA_2021.csv",
#                             skip = 0, header = TRUE, na.strings=c("","NA","blank"))
# 
# 
# strt_WR_21.Data <- strt_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# strt_WR_21.Data <- strt_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# strt_WR_21.Data$datetimeAK <- as.POSIXct(paste(strt_WR_21.Data$Date, strt_WR_21.Data$Time), format="%y%m%d %H:%M")
# 
# 
# strt_WR_21.Data <- strt_WR_21.Data %>%
#   select(Depth..cm., datetimeAK)
# 
# Strt_depth_21 <- ddply(na.omit(strt_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))
# 
# # strt_wr.lm <- lm(meanDepth~datetimeAK, Strt_depth_21)
# 
# 
# Strt_depth_21 <- setDT(Strt_depth_21)
# 
# Strt_depth_21 <- Strt_depth_21 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# STRTmeans2021ODO <- setDT(STRTmeans2021ODO)
# 
# setDT(Strt_depth_21)
# setDT(STRTmeans2021ODO)
# 
# STRTmeans2021ODO$datetimeAK1 <- STRTmeans2021ODO$datetimeAK
# 
# setkey( STRTmeans2021ODO, datetimeAK )
# setkey( Strt_depth_21, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_strt21 <- STRTmeans2021ODO[ Strt_depth_21, roll = "nearest" ]
# 
# rounded.dates_strt21 <- rounded.dates_strt21 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_strt21 <- rounded.dates_strt21 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_strt21 <- rounded.dates_strt21 %>%
#   select(meanDepth, datetimeAK)
# 
# Strt.2021.DO.Depth <- merge(STRTmeans2021ODO, rounded.dates_strt21, by = "datetimeAK", all = TRUE)
# Strt.2021.DO.Depth$meanDepth1 <- Strt.2021.DO.Depth$meanDepth
# 
# strt21mod <- lm(meanDepth ~ datetimeAK, Strt.2021.DO.Depth)
# 
# summary(strt21mod)
# 
# Strt.2021.DO.Depth <- Strt.2021.DO.Depth %>% 
#   mutate(predictedDepth = predict(strt21mod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))



#Put together
STRT_EXO_cl.2019.renamed <- STRT_EXO_cl.2019.renamed %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)

strt.exo.2020 <- strt.exo.2020 %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)

STRTmeans2021ODO <- STRTmeans2021ODO %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)


All.years.strt.MESSY <- rbind(STRT_EXO_cl.2019.renamed, strt.exo.2020, STRTmeans2021ODO)

All.years.strt.MESSY <- All.years.strt.MESSY %>%
  dplyr::rename(DO.obs = ODO.mgL)

All.years.strt.MESSY <- All.years.strt.MESSY %>%
  dplyr::rename(temp.water = Temp.C)



#keep in mind 2019 and 2020 is clean data and 2021 are not.
pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/StuartODO.pdf")

testPlotSTRT <- ggplot(data = All.years.strt.MESSY,
                       mapping = aes(x = datetimeAK, y = as.numeric(ODO.Psat))) + geom_point() + labs(x = "Date", y = "Dissolved Oxygen (%Sat)")
testPlotSTRT
dev.off()




####### VAULT #######

### 2019 ###
#Read cleaned CSVs from DoD 2019 Script 

SondeData2019 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/processed_sensor_dat/SUNA.EXO.int.corr.csv")

SondeData2019$datetimeAK <- force_tz(as.POSIXct(SondeData2019$datetimeAK), "America/Anchorage")

# Have to rename rows so they dont have "mean" in them so it can be combined for three straight years. Data is cleaned.

SondeData2019.renamed <- SondeData2019 %>%
  dplyr::rename(ODO.mgL = ODO.mgL.mn, ODO.Psat = ODO.Psat.mn, ODO.Ploc = ODO.Ploc.mn, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc, site.ID)


VAUL_EXO_cl.2019.renamed <- SondeData2019.renamed %>% filter(site.ID == "VAUL")




# 
# #### 2019 ####
# VAUL_EXO_cl.2019 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/EXO_processed/VAUL.EXO.cl.csv")
# 
# VAUL_EXO_cl.2019$datetimeAK <- force_tz(as.POSIXct(VAUL_EXO_cl.2019$datetimeAK), "America/Anchorage")
# 
# # Have to rename rows so they done have "mean" in them so it can be combined for three straight years. Data is not cleaned.
# 
# VAUL_EXO_cl.2019.renamed <- VAUL_EXO_cl.2019 %>%
#   dplyr::rename(ODO.mgL = ODO.mgL.mn, ODO.Psat = ODO.Psat.mn, ODO.Ploc = ODO.Ploc.mn, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)

# 
# #########DEPTH ################
# # Vaul:
# 
# #download flowmeter data
# WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
# WR.19.1 <- drive_get(as_id(WR_19.url))
# vaul.wr19_glist <- drive_ls(WR.19.1, pattern = "vaul_2019_flowmeter_Q_for_R_JAA.csv")
# walk(vaul.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# vaul_WR_19.Data <- read.csv("vaul_2019_flowmeter_Q_for_R_JAA.csv",
#                             skip = 1, header = TRUE, na.strings=c("","NA","blank"))
# 
# vaul_WR_19.Data <- vaul_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# vaul_WR_19.Data <- vaul_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# vaul_WR_19.Data$datetimeAK <- as.POSIXct(paste(vaul_WR_19.Data$Date, vaul_WR_19.Data$Time), format="%m/%d/%Y %H:%M")
# 
# 
# vaul_WR_19.Data <- vaul_WR_19.Data %>%
#   select(Depth..cm., datetimeAK)
# 
# Vaul_depth_19 <- ddply(na.omit(vaul_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))
# 
# # vaul_wr.lm <- lm(meanDepth~datetimeAK, Vaul_depth_19)
# 
# 
# Vaul_depth_19 <- setDT(Vaul_depth_19)
# 
# Vaul_depth_19 <- Vaul_depth_19 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# VAUL_EXO_cl.2019.renamed <- setDT(VAUL_EXO_cl.2019.renamed)
# 
# setDT(Vaul_depth_19)
# setDT(VAUL_EXO_cl.2019.renamed)
# 
# VAUL_EXO_cl.2019.renamed$datetimeAK1 <- VAUL_EXO_cl.2019.renamed$datetimeAK
# 
# setkey( VAUL_EXO_cl.2019.renamed, datetimeAK )
# setkey( Vaul_depth_19, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_vaul19 <- VAUL_EXO_cl.2019.renamed[ Vaul_depth_19, roll = "nearest" ]
# 
# rounded.dates_vaul19 <- rounded.dates_vaul19 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_vaul19 <- rounded.dates_vaul19 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_vaul19 <- rounded.dates_vaul19 %>%
#   select(meanDepth, datetimeAK)
# 
# Vaul.2019.DO.Depth <- merge(VAUL_EXO_cl.2019.renamed, rounded.dates_vaul19, by = "datetimeAK", all = TRUE)
# Vaul.2019.DO.Depth$meanDepth1 <- Vaul.2019.DO.Depth$meanDepth
# 
# vaul19mod <- lm(meanDepth ~ datetimeAK, Vaul.2019.DO.Depth)
# 
# summary(vaul19mod)
# 
# Vaul.2019.DO.Depth <- Vaul.2019.DO.Depth %>% 
#   mutate(predictedDepth = predict(vaul19mod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))
# 






#USE TELEM FILLED DATA TO CALCULATE MG/L 


#calc from YSI %Sat to MGL spreadsheet

# # DO data collected as concentrations can be 
# converted to percent saturation using temperature and salinity data collected in conjunction with 
# the DO measurements using the equations as provided in APHA, 1989:


# DOsat = (Exp((-139.34411 + (157570.1/Temp) - (66423080/Temp2
# ) + (12438000000/Temp3
# ) - 
#   (862194900000/Temp4
#   )) - (Sal * (0.017674-(10.754/Temp)+(2140.7/Temp2
#   )))))
# % DO = (DOmeasure/ DOsat)*100 


# Where:
#   DOsat = DO concentration in mg/L at 100 % saturation, 
# Temp = water temperature in °K (°C + 273.15 = °K) 
# DOmeasure = Measured DO concentration in mg/L.
# Sal = Salinity in part per thousand (ppt)

#Constants: 
# 862194900000
# 12438000000
# 66423080
# 157570.1
# 139.344
#

#We change this according to how YSI does it in the EXO



#in this case, we are finding the DO mg/L per percent saturation, and multiplying it by our percent sat from telem. This percent sat accounts for pressure as well I believe. 


VAUL_EXO_cl.2019.renamed$ODO.mgL.Calc <-as.numeric(VAUL_EXO_cl.2019.renamed$ODO.Psat) * 
  #DO mg/L per sat
  (0.01* exp(
        #p1
      (-862194900000*(1/(VAUL_EXO_cl.2019.renamed$Temp.C+273.15))^4+12438000000*(1/(VAUL_EXO_cl.2019.renamed$Temp.C+273.15))^3-66423080*(1/(VAUL_EXO_cl.2019.renamed$Temp.C+273.15))^2+157570.1*(1/(VAUL_EXO_cl.2019.renamed$Temp.C+273.15))-139.344)
  #salinity
  -0* 
        #p2
        (2140.7*(1/(VAUL_EXO_cl.2019.renamed$Temp.C+273.15))^2-10.754*(1/(VAUL_EXO_cl.2019.renamed$Temp.C+273.15))+0.017674 )))


VAUL_EXO_cl.2019.renamed <- VAUL_EXO_cl.2019.renamed %>% 
  mutate(ODO.mgL = coalesce(ODO.mgL,ODO.mgL.Calc))



### 2020 ####


# exo.all.2020.url <- "https://drive.google.com/drive/u/1/folders/1nNKoIdgP-fdCNRGUbGca_zVujF_16QEM"
# exo.all.2020.prt1 <- drive_get(as_id(exo.all.2020.url))
# exo.all.2020.glist <- drive_ls(exo.all.2020.prt1, pattern = "EXO.ALL.csv")
# walk(exo.all.2020.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# exo.all.2020 <- read.csv("EXO.ALL.csv",)

vaul.exo.2020 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2020/EXO_processed/VAUL.EXO.cl.csv")


vaul.exo.2020 <- vaul.exo.2020 %>%
  dplyr::rename(ODO.mgL = ODO.mgL.mn.adj, ODO.Psat = ODO.Psat.mn.adj, ODO.Ploc = ODO.Ploc.mn.adj, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc, site.ID)

vaul.exo.2020$datetimeAK <- force_tz(as.POSIXct(vaul.exo.2020$datetimeAK), "America/Anchorage")



# 
# vaul.exo.2020 <- exo.all.2020 %>% filter(site.ID == "VAUL")


# #remove bad data point
# vaul.exo.2020 <- vaul.exo.2020[-c(56619)]


#convert Bursts to means

# 
# 
# vaul.exo.2020$datetimeAK <- as.POSIXct(strptime(vaul.exo.2020$datetimeAK, "%m/%e/%y %H:%M"))
# 
# 
# 
# vaul.exo.2020$Temp.C <- as.numeric(vaul.exo.2020$Temp.C)
# vaul.exo.2020$ODO.Psat <- as.numeric(vaul.exo.2020$ODO.Psat)
# vaul.exo.2020$ODO.mgL <- as.numeric(vaul.exo.2020$ODO.mgL)
# vaul.exo.2020$ODO.Ploc <- as.numeric(vaul.exo.2020$ODO.Ploc)
# 
# 
# 
# #mean bursts
# VAULmean2020odoMGL <- aggregate( ODO.mgL ~ datetimeAK, vaul.exo.2020, mean)
# VAULmean2020odoPSAT <- aggregate( ODO.Psat ~ datetimeAK, vaul.exo.2020, mean)
# VAULmean2020odoPLOC <- aggregate( ODO.Ploc ~ datetimeAK, vaul.exo.2020, mean)
# VAULmean2020odoTEMPC <- aggregate( Temp.C ~ datetimeAK, vaul.exo.2020, mean)
# 
# VAULmeans2020ODO <- plyr::join(VAULmean2020odoMGL, VAULmean2020odoPSAT, by = "datetimeAK")
# VAULmeans2020ODO <- plyr::join(VAULmeans2020ODO, VAULmean2020odoPLOC, by = "datetimeAK")
# VAULmeans2020ODO <- plyr::join(VAULmeans2020ODO, VAULmean2020odoTEMPC, by = "datetimeAK")
# 
# #remove outlier point
# VAULmeans2020ODO.1 <- VAULmeans2020ODO %>% filter(datetimeAK <= "2020-08-09 22:30:00")
# VAULmeans2020ODO.2 <- VAULmeans2020ODO %>% filter(datetimeAK > "2020-08-09 23:00:00")
# 
# VAULmeans2020ODO <- rbind(VAULmeans2020ODO.1,VAULmeans2020ODO.2)                                                  
# VAULmeans2020ODO$datetimeAK <-  lubridate::round_date(VAULmeans2020ODO$datetimeAK, "15 minutes") 
# 
# VAULmeans2020ODO <- distinct(VAULmeans2020ODO)
# 
# #duplicate rows meaned
# library(plyr)
# VAULmeans2020ODO = ddply(
#   VAULmeans2020ODO,
#   .(datetimeAK),
#   function(df_section) {
#     res_df = data.frame(datetimeAK=df_section$datetimeAK[1], ODO.mgL=mean(df_section$ODO.mgL), ODO.Ploc = mean(df_section$ODO.Ploc), ODO.Psat = mean(df_section$ODO.Psat), Temp.C = mean(df_section$Temp.C))
#   }
# )
# 
# ########## DEPTH ############ 
# 
# 
# # Vaul:
# 
# #download flowmeter data
# WR_20.url <- "https://drive.google.com/drive/u/1/folders/1l-QIICuviZvugbNGrvoLfkCDgo1HyDMg"
# WR.20.1 <- drive_get(as_id(WR_20.url))
# vaul.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_VAUL_for_R_JAA.csv")
# walk(vaul.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# vaul_WR_20.Data <- read.csv("R_Flowmeter Q calculation_VAUL_for_R_JAA.csv",
#                             skip = 0, header = TRUE, na.strings=c("","NA","blank"))
# 
# 
# vaul_WR_20.Data <- vaul_WR_20.Data %>%
#   dplyr::rename(Date = ï..Date)
# 
# vaul_WR_20.Data <- vaul_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# vaul_WR_20.Data <- vaul_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# vaul_WR_20.Data$datetimeAK <- as.POSIXct(paste(vaul_WR_20.Data$Date, vaul_WR_20.Data$Time), format="%y%m%d %H:%M")
# 
# 
# vaul_WR_20.Data <- vaul_WR_20.Data %>%
#   select(Depth..cm., datetimeAK)
# 
# Vaul_depth_20 <- ddply(na.omit(vaul_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))
# 
# # vaul_wr.lm <- lm(meanDepth~datetimeAK, Vaul_depth_20)
# 
# 
# Vaul_depth_20 <- setDT(Vaul_depth_20)
# 
# Vaul_depth_20 <- Vaul_depth_20 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# VAULmeans2020ODO <- setDT(VAULmeans2020ODO)
# 
# setDT(Vaul_depth_20)
# setDT(VAULmeans2020ODO)
# 
# VAULmeans2020ODO$datetimeAK1 <- VAULmeans2020ODO$datetimeAK
# 
# setkey( VAULmeans2020ODO, datetimeAK )
# setkey( Vaul_depth_20, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_vaul20 <- VAULmeans2020ODO[ Vaul_depth_20, roll = "nearest" ]
# 
# rounded.dates_vaul20 <- rounded.dates_vaul20 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_vaul20 <- rounded.dates_vaul20 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_vaul20 <- rounded.dates_vaul20 %>%
#   select(meanDepth, datetimeAK)
# 
# Vaul.2020.DO.Depth <- merge(VAULmeans2020ODO, rounded.dates_vaul20, by = "datetimeAK", all = TRUE)
# Vaul.2020.DO.Depth$meanDepth1 <- Vaul.2020.DO.Depth$meanDepth
# 
# vaul20mod <- lm(meanDepth ~ datetimeAK, Vaul.2020.DO.Depth)
# 
# summary(vaul20mod)
# 
# Vaul.2020.DO.Depth <- Vaul.2020.DO.Depth %>% 
#   mutate(predictedDepth = predict(vaul20mod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))
# 




                                  
#### 2021 ####
vaul.exo.2021 <- exo.processed.2021 %>% filter(site.ID == "VAUL")


vaul.exo.2021$datetimeAK <- lubridate::round_date(vaul.exo.2021$datetimeAK, "15 minutes")
vaul.exo.2021$datetimeAK <- force_tz(vaul.exo.2021$datetimeAK, "America/Anchorage")

vaul.exo.2021$Temp.C <- as.numeric(vaul.exo.2021$Temp.C)
vaul.exo.2021$ODO.Psat <- as.numeric(vaul.exo.2021$ODO.Psat)
vaul.exo.2021$ODO.mgL <- as.numeric(vaul.exo.2021$ODO.mgL)
vaul.exo.2021$ODO.Ploc <- as.numeric(vaul.exo.2021$ODO.Ploc)



#mean bursts
VAULmean2021odoMGL <- aggregate( ODO.mgL ~ datetimeAK, vaul.exo.2021, mean)
VAULmean2021odoPSAT <- aggregate( ODO.Psat ~ datetimeAK, vaul.exo.2021, mean)
VAULmean2021odoPLOC <- aggregate( ODO.Ploc ~ datetimeAK, vaul.exo.2021, mean)
VAULmean2021odoTEMPC <- aggregate( Temp.C ~ datetimeAK, vaul.exo.2021, mean)

VAULmeans2021ODO <- plyr::join(VAULmean2021odoMGL, VAULmean2021odoPSAT, by = "datetimeAK")
VAULmeans2021ODO <- plyr::join(VAULmeans2021ODO, VAULmean2021odoPLOC, by = "datetimeAK")
VAULmeans2021ODO <- plyr::join(VAULmeans2021ODO, VAULmean2021odoTEMPC, by = "datetimeAK")

# 
# ########## DEPTH ############ 
# 
# 
# # Vaul:
# 
# #download flowmeter data
# WR_21.url <- "https://drive.google.com/drive/u/1/folders/13avby555rryYttGDgO8sKE7umZPmo5uB"
# WR.21.1 <- drive_get(as_id(WR_21.url))
# vaul.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter Q calculation_VAUL_for_R_JAA_2021.csv")
# walk(vaul.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# vaul_WR_21.Data <- read.csv("R_Flowmeter Q calculation_VAUL_for_R_JAA_2021.csv",
#                             skip = 0, header = TRUE, na.strings=c("","NA","blank"))
# 
# 
# vaul_WR_21.Data <- vaul_WR_21.Data %>%
#   dplyr::rename(Date = ï..Date)
# 
# vaul_WR_21.Data <- vaul_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# vaul_WR_21.Data <- vaul_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# vaul_WR_21.Data$datetimeAK <- as.POSIXct(paste(vaul_WR_21.Data$Date, vaul_WR_21.Data$Time), format="%y%m%d %H:%M")
# 
# 
# vaul_WR_21.Data <- vaul_WR_21.Data %>%
#   select(Depth..cm., datetimeAK)
# 
# Vaul_depth_21 <- ddply(na.omit(vaul_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))
# 
# # vaul_wr.lm <- lm(meanDepth~datetimeAK, Vaul_depth_21)
# 
# 
# Vaul_depth_21 <- setDT(Vaul_depth_21)
# 
# Vaul_depth_21 <- Vaul_depth_21 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# VAULmeans2021ODO <- setDT(VAULmeans2021ODO)
# 
# setDT(Vaul_depth_21)
# setDT(VAULmeans2021ODO)
# 
# VAULmeans2021ODO$datetimeAK1 <- VAULmeans2021ODO$datetimeAK
# 
# setkey( VAULmeans2021ODO, datetimeAK )
# setkey( Vaul_depth_21, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_vaul21 <- VAULmeans2021ODO[ Vaul_depth_21, roll = "nearest" ]
# 
# rounded.dates_vaul21 <- rounded.dates_vaul21 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_vaul21 <- rounded.dates_vaul21 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_vaul21 <- rounded.dates_vaul21 %>%
#   select(meanDepth, datetimeAK)
# 
# Vaul.2021.DO.Depth <- merge(VAULmeans2021ODO, rounded.dates_vaul21, by = "datetimeAK", all = TRUE)
# Vaul.2021.DO.Depth$meanDepth1 <- Vaul.2021.DO.Depth$meanDepth
# 
# vaul21mod <- lm(meanDepth ~ datetimeAK, Vaul.2021.DO.Depth)
# 
# summary(vaul21mod)
# 
# Vaul.2021.DO.Depth <- Vaul.2021.DO.Depth %>% 
#   mutate(predictedDepth = predict(vaul21mod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))



#Put together
VAUL_EXO_cl.2019.renamed <- VAUL_EXO_cl.2019.renamed %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)

vaul.exo.2020 <- vaul.exo.2020 %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)


VAULmeans2021ODO <- VAULmeans2021ODO %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)


All.years.vaul.MESSY <- rbind(VAUL_EXO_cl.2019.renamed, vaul.exo.2020, VAULmeans2021ODO)

All.years.vaul.MESSY <- All.years.vaul.MESSY %>%
  dplyr::rename(DO.obs = ODO.mgL)

All.years.vaul.MESSY <- All.years.vaul.MESSY %>%
  dplyr::rename(temp.water = Temp.C)


# All.years.vaul.MESSY <- All.years.vaul.MESSY[-c(19166)]


#keep in mind 2019 and 2020 is clean data and 2021 is not.
pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/VaultODO.pdf")

testPlotVAUL <- ggplot(data = All.years.vaul.MESSY,
                       mapping = aes(x = datetimeAK, y = as.numeric(ODO.Psat))) + geom_point() + labs(x = "Date", y = "Dissolved Oxygen (%Sat)")
testPlotVAUL
dev.off()





####### MOOS #######

#### 2019 ####

SondeData2019 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/processed_sensor_dat/SUNA.EXO.int.corr.csv")

SondeData2019$datetimeAK <- force_tz(as.POSIXct(SondeData2019$datetimeAK), "America/Anchorage")

# Have to rename rows so they dont have "mean" in them so it can be combined for three straight years. Data is cleaned.

SondeData2019.renamed <- SondeData2019 %>%
  dplyr::rename(ODO.mgL = ODO.mgL.mn, ODO.Psat = ODO.Psat.mn, ODO.Ploc = ODO.Ploc.mn, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc, site.ID)


MOOS_EXO_cl.2019.renamed <- SondeData2019.renamed %>% filter(site.ID == "MOOS")


# 
# MOOS_EXO_cl.2019 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/EXO_processed/MOOS.EXO.cl.csv")
# 
# MOOS_EXO_cl.2019$datetimeAK <- force_tz(as.POSIXct(MOOS_EXO_cl.2019$datetimeAK), "America/Anchorage")
# 
# # Have to rename rows so they done have "mean" in them so it can be combined for three straight years. Data is not cleaned.
# 
# MOOS_EXO_cl.2019.renamed <- MOOS_EXO_cl.2019 %>%
#   dplyr::rename(ODO.mgL = ODO.mgL.mn, ODO.Psat = ODO.Psat.mn, ODO.Ploc = ODO.Ploc.mn, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)
# 

# 
# ########## DEPTH ############ 
# 
# 
# # Moos:
# 
# #download flowmeter data
# WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
# WR.19.1 <- drive_get(as_id(WR_19.url))
# moos.wr19_glist <- drive_ls(WR.19.1, pattern = "moos_2019_flowmeter_Q_for_R_JAA.csv")
# walk(moos.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# moos_WR_19.Data <- read.csv("moos_2019_flowmeter_Q_for_R_JAA.csv",
#                             skip = 1, header = TRUE, na.strings=c("","NA","blank"))
# 
# moos_WR_19.Data <- moos_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# moos_WR_19.Data <- moos_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# moos_WR_19.Data$datetimeAK <- as.POSIXct(paste(moos_WR_19.Data$Date, moos_WR_19.Data$Time), format="%m/%d/%Y %H:%M")
# 
# 
# moos_WR_19.Data <- moos_WR_19.Data %>%
#   select(Depth..cm., datetimeAK)
# 
# Moos_depth_19 <- ddply(na.omit(moos_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))
# 
# # moos_wr.lm <- lm(meanDepth~datetimeAK, Moos_depth_19)
# 
# 
# Moos_depth_19 <- setDT(Moos_depth_19)
# 
# Moos_depth_19 <- Moos_depth_19 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# MOOS_EXO_cl.2019.renamed <- setDT(MOOS_EXO_cl.2019.renamed)
# 
# setDT(Moos_depth_19)
# setDT(MOOS_EXO_cl.2019.renamed)
# 
# MOOS_EXO_cl.2019.renamed$datetimeAK1 <- MOOS_EXO_cl.2019.renamed$datetimeAK
# 
# setkey( MOOS_EXO_cl.2019.renamed, datetimeAK )
# setkey( Moos_depth_19, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_moos19 <- MOOS_EXO_cl.2019.renamed[ Moos_depth_19, roll = "nearest" ]
# 
# rounded.dates_moos19 <- rounded.dates_moos19 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_moos19 <- rounded.dates_moos19 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_moos19 <- rounded.dates_moos19 %>%
#   select(meanDepth, datetimeAK)
# 
# Moos.2019.DO.Depth <- merge(MOOS_EXO_cl.2019.renamed, rounded.dates_moos19, by = "datetimeAK", all = TRUE)
# Moos.2019.DO.Depth$meanDepth1 <- Moos.2019.DO.Depth$meanDepth
# 
# moos19mod <- lm(meanDepth ~ datetimeAK, Moos.2019.DO.Depth)
# 
# summary(moos19mod)
# 
# Moos.2019.DO.Depth <- Moos.2019.DO.Depth %>% 
#   mutate(predictedDepth = predict(moos19mod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))



#USE TELEM FILLED DATA TO CALCULATE MG/L 


#calc from YSI %Sat to MGL spreadsheet

# # DO data collected as concentrations can be 
# converted to percent saturation using temperature and salinity data collected in conjunction with 
# the DO measurements using the equations as provided in APHA, 1989:


# DOsat = (Exp((-139.34411 + (157570.1/Temp) - (66423080/Temp2
# ) + (12438000000/Temp3
# ) - 
#   (862194900000/Temp4
#   )) - (Sal * (0.017674-(10.754/Temp)+(2140.7/Temp2
#   )))))
# % DO = (DOmeasure/ DOsat)*100 


# Where:
#   DOsat = DO concentration in mg/L at 100 % saturation, 
# Temp = water temperature in °K (°C + 273.15 = °K) 
# DOmeasure = Measured DO concentration in mg/L.
# Sal = Salinity in part per thousand (ppt)

#Constants: 
# 862194900000
# 12438000000
# 66423080
# 157570.1
# 139.344
#

#We change this according to how YSI does it in the EXO



#in this case, we are finding the DO mg/L per percent saturation, and multiplying it by our percent sat from telem. This percent sat accounts for pressure as well I believe. 


MOOS_EXO_cl.2019.renamed$ODO.mgL.Calc <-as.numeric(MOOS_EXO_cl.2019.renamed$ODO.Psat) * 
  #DO mg/L per sat
  (0.01* exp(
    #p1
    (-862194900000*(1/(MOOS_EXO_cl.2019.renamed$Temp.C+273.15))^4+12438000000*(1/(MOOS_EXO_cl.2019.renamed$Temp.C+273.15))^3-66423080*(1/(MOOS_EXO_cl.2019.renamed$Temp.C+273.15))^2+157570.1*(1/(MOOS_EXO_cl.2019.renamed$Temp.C+273.15))-139.344)
    #salinity
    -0* 
      #p2
      (2140.7*(1/(MOOS_EXO_cl.2019.renamed$Temp.C+273.15))^2-10.754*(1/(MOOS_EXO_cl.2019.renamed$Temp.C+273.15))+0.017674 )))

#fill missing rows with calculated MGL
MOOS_EXO_cl.2019.renamed <- MOOS_EXO_cl.2019.renamed %>% 
  mutate(ODO.mgL = coalesce(ODO.mgL,ODO.mgL.Calc))





### 2020 ####

# exo.all.2020.url <- "https://drive.google.com/drive/u/1/folders/1nNKoIdgP-fdCNRGUbGca_zVujF_16QEM"
# exo.all.2020.prt1 <- drive_get(as_id(exo.all.2020.url))
# exo.all.2020.glist <- drive_ls(exo.all.2020.prt1, pattern = "EXO.ALL.csv")
# walk(exo.all.2020.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# exo.all.2020 <- read.csv("EXO.ALL.csv",)

moos.exo.2020 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2020/EXO_processed/MOOS.EXO.cl.csv")


moos.exo.2020 <- moos.exo.2020 %>%
  dplyr::rename(ODO.mgL = ODO.mgL.mn.adj, ODO.Psat = ODO.Psat.mn.adj, ODO.Ploc = ODO.Ploc.mn.adj, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc, site.ID)

moos.exo.2020$datetimeAK <- force_tz(as.POSIXct(moos.exo.2020$datetimeAK), "America/Anchorage")
# 
# moos.exo.2020 <- exo.all.2020 %>% filter(site.ID == "MOOS")
# 
# #convert Bursts to means
# 
# 
# 
# moos.exo.2020$datetimeAK <- as.POSIXct(strptime(moos.exo.2020$datetimeAK, "%m/%e/%y %H:%M"))
# 
# 
# 
# moos.exo.2020$Temp.C <- as.numeric(moos.exo.2020$Temp.C)
# moos.exo.2020$ODO.Psat <- as.numeric(moos.exo.2020$ODO.Psat)
# moos.exo.2020$ODO.mgL <- as.numeric(moos.exo.2020$ODO.mgL)
# moos.exo.2020$ODO.Ploc <- as.numeric(moos.exo.2020$ODO.Ploc)
# 
# 
# 
# #mean bursts
# MOOSmean2020odoMGL <- aggregate( ODO.mgL ~ datetimeAK, moos.exo.2020, mean)
# MOOSmean2020odoPSAT <- aggregate( ODO.Psat ~ datetimeAK, moos.exo.2020, mean)
# MOOSmean2020odoPLOC <- aggregate( ODO.Ploc ~ datetimeAK, moos.exo.2020, mean)
# MOOSmean2020odoTEMPC <- aggregate( Temp.C ~ datetimeAK, moos.exo.2020, mean)
# 
# MOOSmeans2020ODO <- plyr::join(MOOSmean2020odoMGL, MOOSmean2020odoPSAT, by = "datetimeAK")
# MOOSmeans2020ODO <- plyr::join(MOOSmeans2020ODO, MOOSmean2020odoPLOC, by = "datetimeAK")
# MOOSmeans2020ODO <- plyr::join(MOOSmeans2020ODO, MOOSmean2020odoTEMPC, by = "datetimeAK")
# 
# 
# 
# ########## DEPTH ############ 
# 
# 
# # Moos:
# 
# #download flowmeter data
# WR_20.url <- "https://drive.google.com/drive/u/1/folders/1O28nv-6gsmC_xsAwUFRjjouY9hS29FtK"
# WR.20.1 <- drive_get(as_id(WR_20.url))
# moos.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_MOOS_for_R_JAA.csv")
# walk(moos.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# moos_WR_20.Data <- read.csv("R_Flowmeter Q calculation_MOOS_for_R_JAA.csv",
#                             skip = 0, header = TRUE, na.strings=c("","NA","blank"))
# 
# 
# moos_WR_20.Data <- moos_WR_20.Data %>%
#   dplyr::rename(Date = ï..Date)
# 
# moos_WR_20.Data <- moos_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# moos_WR_20.Data <- moos_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# moos_WR_20.Data$datetimeAK <- as.POSIXct(paste(moos_WR_20.Data$Date, moos_WR_20.Data$Time), format="%y%m%d %H:%M")
# 
# 
# moos_WR_20.Data <- moos_WR_20.Data %>%
#   select(Depth..cm., datetimeAK)
# 
# Moos_depth_20 <- ddply(na.omit(moos_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))
# 
# # moos_wr.lm <- lm(meanDepth~datetimeAK, Moos_depth_20)
# 
# 
# Moos_depth_20 <- setDT(Moos_depth_20)
# 
# Moos_depth_20 <- Moos_depth_20 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# MOOSmeans2020ODO <- setDT(MOOSmeans2020ODO)
# 
# setDT(Moos_depth_20)
# setDT(MOOSmeans2020ODO)
# 
# MOOSmeans2020ODO$datetimeAK1 <- MOOSmeans2020ODO$datetimeAK
# 
# setkey( MOOSmeans2020ODO, datetimeAK )
# setkey( Moos_depth_20, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_moos20 <- MOOSmeans2020ODO[ Moos_depth_20, roll = "nearest" ]
# 
# rounded.dates_moos20 <- rounded.dates_moos20 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_moos20 <- rounded.dates_moos20 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_moos20 <- rounded.dates_moos20 %>%
#   select(meanDepth, datetimeAK)
# 
# Moos.2020.DO.Depth <- merge(MOOSmeans2020ODO, rounded.dates_moos20, by = "datetimeAK", all = TRUE)
# Moos.2020.DO.Depth$meanDepth1 <- Moos.2020.DO.Depth$meanDepth
# 
# moos20mod <- lm(meanDepth ~ datetimeAK, Moos.2020.DO.Depth)
# 
# summary(moos20mod)
# 
# Moos.2020.DO.Depth <- Moos.2020.DO.Depth %>% 
#   mutate(predictedDepth = predict(moos20mod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))
# 




#### 2021 ####
moos.exo.2021 <- exo.processed.2021 %>% filter(site.ID == "MOOS")


moos.exo.2021$datetimeAK <- lubridate::round_date(moos.exo.2021$datetimeAK, "15 minutes")
moos.exo.2021$datetimeAK <- force_tz(moos.exo.2021$datetimeAK, "America/Anchorage")

moos.exo.2021$Temp.C <- as.numeric(moos.exo.2021$Temp.C)
moos.exo.2021$ODO.Psat <- as.numeric(moos.exo.2021$ODO.Psat)
moos.exo.2021$ODO.mgL <- as.numeric(moos.exo.2021$ODO.mgL)
moos.exo.2021$ODO.Ploc <- as.numeric(moos.exo.2021$ODO.Ploc)



#mean bursts
MOOSmean2021odoMGL <- aggregate( ODO.mgL ~ datetimeAK, moos.exo.2021, mean)
MOOSmean2021odoPSAT <- aggregate( ODO.Psat ~ datetimeAK, moos.exo.2021, mean)
MOOSmean2021odoPLOC <- aggregate( ODO.Ploc ~ datetimeAK, moos.exo.2021, mean)
MOOSmean2021odoTEMPC <- aggregate( Temp.C ~ datetimeAK, moos.exo.2021, mean)

MOOSmeans2021ODO <- plyr::join(MOOSmean2021odoMGL, MOOSmean2021odoPSAT, by = "datetimeAK")
MOOSmeans2021ODO <- plyr::join(MOOSmeans2021ODO, MOOSmean2021odoPLOC, by = "datetimeAK")
MOOSmeans2021ODO <- plyr::join(MOOSmeans2021ODO, MOOSmean2021odoTEMPC, by = "datetimeAK")
# 
# ########## DEPTH ############ 
# 
# 
# # Moos:
# 
# #download flowmeter data
# WR_21.url <- "https://drive.google.com/drive/u/1/folders/1-S_ixEutlA7RKfrvhi15aIBLrjYjg5O_"
# WR.21.1 <- drive_get(as_id(WR_21.url))
# moos.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter Q calculation_MOOS_for_R_JAA_2021.csv")
# walk(moos.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# moos_WR_21.Data <- read.csv("R_Flowmeter Q calculation_MOOS_for_R_JAA_2021.csv",
#                             skip = 0, header = TRUE, na.strings=c("","NA","blank"))
# 
# 
# moos_WR_21.Data <- moos_WR_21.Data %>%
#   dplyr::rename(Date = ï..Date)
# 
# moos_WR_21.Data <- moos_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# moos_WR_21.Data <- moos_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# moos_WR_21.Data$datetimeAK <- as.POSIXct(paste(moos_WR_21.Data$Date, moos_WR_21.Data$Time), format="%y%m%d %H:%M")
# 
# 
# moos_WR_21.Data <- moos_WR_21.Data %>%
#   select(Depth..cm., datetimeAK)
# 
# Moos_depth_21 <- ddply(na.omit(moos_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))
# 
# # moos_wr.lm <- lm(meanDepth~datetimeAK, Moos_depth_21)
# 
# 
# Moos_depth_21 <- setDT(Moos_depth_21)
# 
# Moos_depth_21 <- Moos_depth_21 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# MOOSmeans2021ODO <- setDT(MOOSmeans2021ODO)
# 
# setDT(Moos_depth_21)
# setDT(MOOSmeans2021ODO)
# 
# MOOSmeans2021ODO$datetimeAK1 <- MOOSmeans2021ODO$datetimeAK
# 
# setkey( MOOSmeans2021ODO, datetimeAK )
# setkey( Moos_depth_21, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_moos21 <- MOOSmeans2021ODO[ Moos_depth_21, roll = "nearest" ]
# 
# rounded.dates_moos21 <- rounded.dates_moos21 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_moos21 <- rounded.dates_moos21 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_moos21 <- rounded.dates_moos21 %>%
#   select(meanDepth, datetimeAK)
# 
# Moos.2021.DO.Depth <- merge(MOOSmeans2021ODO, rounded.dates_moos21, by = "datetimeAK", all = TRUE)
# Moos.2021.DO.Depth$meanDepth1 <- Moos.2021.DO.Depth$meanDepth
# 
# moos21mod <- lm(meanDepth ~ datetimeAK, Moos.2021.DO.Depth)
# 
# summary(moos21mod)
# 
# Moos.2021.DO.Depth <- Moos.2021.DO.Depth %>% 
#   mutate(predictedDepth = predict(moos21mod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))
# 



#Put together
MOOS_EXO_cl.2019.renamed <- MOOS_EXO_cl.2019.renamed %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)

moos.exo.2020 <- moos.exo.2020 %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)

MOOSmeans2021ODO <- MOOSmeans2021ODO %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)


All.years.moos.MESSY <- rbind(MOOS_EXO_cl.2019.renamed, moos.exo.2020, MOOSmeans2021ODO)

All.years.moos.MESSY <- All.years.moos.MESSY %>%
  dplyr::rename(DO.obs = ODO.mgL)

All.years.moos.MESSY <- All.years.moos.MESSY %>%
  dplyr::rename(temp.water = Temp.C)



#keep in mind 2019 and 2020 is clean data and 2021 are not.
pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/MoosODO.pdf")

testPlotMOOS <- ggplot(data = All.years.moos.MESSY,
                       mapping = aes(x = datetimeAK, y = as.numeric(ODO.Psat))) + geom_point() + labs(x = "Date", y = "Dissolved Oxygen (%Sat)")
testPlotMOOS
dev.off()







####### FRCH #######

#### 2019 ####

#Read cleaned CSVs from DoD 2019 Script 

SondeData2019 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/processed_sensor_dat/SUNA.EXO.int.corr.csv")

SondeData2019$datetimeAK <- force_tz(as.POSIXct(SondeData2019$datetimeAK), "America/Anchorage")

# Have to rename rows so they dont have "mean" in them so it can be combined for three straight years. Data is cleaned.

SondeData2019.renamed <- SondeData2019 %>%
  dplyr::rename(ODO.mgL = ODO.mgL.mn, ODO.Psat = ODO.Psat.mn, ODO.Ploc = ODO.Ploc.mn, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc, site.ID)


FRCH_EXO_cl.2019.renamed <- SondeData2019.renamed %>% filter(site.ID == "FRCH")




# 
# 
# FRCH_EXO_cl.2019 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/EXO_processed/FRCH.EXO.cl.csv")
# 
# FRCH_EXO_cl.2019$datetimeAK <- force_tz(as.POSIXct(FRCH_EXO_cl.2019$datetimeAK), "America/Anchorage")
# 
# # Have to rename rows so they done have "mean" in them so it can be combined for three straight years. Data is not cleaned.
# 
# FRCH_EXO_cl.2019.renamed <- FRCH_EXO_cl.2019 %>%
#   dplyr::rename(ODO.mgL = ODO.mgL.mn, ODO.Psat = ODO.Psat.mn, ODO.Ploc = ODO.Ploc.mn, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)
# 
# 
# ########## DEPTH ############ 
# 
# 
# # Frch:
# 
# #download flowmeter data
# WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
# WR.19.1 <- drive_get(as_id(WR_19.url))
# frch.wr19_glist <- drive_ls(WR.19.1, pattern = "Frch_2019_flowmeter_Q_for_R_JAA.csv")
# walk(frch.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# frch_WR_19.Data <- read.csv("Frch_2019_flowmeter_Q_for_R_JAA.csv",
#                             skip = 1, header = TRUE, na.strings=c("","NA","blank"))
# 
# frch_WR_19.Data <- frch_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# frch_WR_19.Data <- frch_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# frch_WR_19.Data$datetimeAK <- as.POSIXct(paste(frch_WR_19.Data$Date, frch_WR_19.Data$Time), format="%m/%d/%Y %H:%M")
# 
# 
# frch_WR_19.Data <- frch_WR_19.Data %>%
#   select(Depth..cm., datetimeAK)
# 
# Frch_depth_19 <- ddply(na.omit(frch_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))
# 
# # frch_wr.lm <- lm(meanDepth~datetimeAK, Frch_depth_19)
# 
# 
# Frch_depth_19 <- setDT(Frch_depth_19)
# 
# Frch_depth_19 <- Frch_depth_19 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# FRCH_EXO_cl.2019.renamed <- setDT(FRCH_EXO_cl.2019.renamed)
# 
# setDT(Frch_depth_19)
# setDT(FRCH_EXO_cl.2019.renamed)
# 
# FRCH_EXO_cl.2019.renamed$datetimeAK1 <- FRCH_EXO_cl.2019.renamed$datetimeAK
# 
# setkey( FRCH_EXO_cl.2019.renamed, datetimeAK )
# setkey( Frch_depth_19, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_frch19 <- FRCH_EXO_cl.2019.renamed[ Frch_depth_19, roll = "nearest" ]
# 
# rounded.dates_frch19 <- rounded.dates_frch19 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_frch19 <- rounded.dates_frch19 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_frch19 <- rounded.dates_frch19 %>%
#   select(meanDepth, datetimeAK)
# 
# Frch.2019.DO.Depth <- merge(FRCH_EXO_cl.2019.renamed, rounded.dates_frch19, by = "datetimeAK", all = TRUE)
# Frch.2019.DO.Depth$meanDepth1 <- Frch.2019.DO.Depth$meanDepth
# 
# frch19mod <- lm(meanDepth ~ datetimeAK, Frch.2019.DO.Depth)
# 
# summary(frch19mod)
# 
# Frch.2019.DO.Depth <- Frch.2019.DO.Depth %>% 
#   mutate(predictedDepth = predict(frch19mod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))



#USE TELEM FILLED DATA TO CALCULATE MG/L 


#calc from YSI %Sat to MGL spreadsheet

# # DO data collected as concentrations can be 
# converted to percent saturation using temperature and salinity data collected in conjunction with 
# the DO measurements using the equations as provided in APHA, 1989:


# DOsat = (Exp((-139.34411 + (157570.1/Temp) - (66423080/Temp2
# ) + (12438000000/Temp3
# ) - 
#   (862194900000/Temp4
#   )) - (Sal * (0.017674-(10.754/Temp)+(2140.7/Temp2
#   )))))
# % DO = (DOmeasure/ DOsat)*100 


# Where:
#   DOsat = DO concentration in mg/L at 100 % saturation, 
# Temp = water temperature in °K (°C + 273.15 = °K) 
# DOmeasure = Measured DO concentration in mg/L.
# Sal = Salinity in part per thousand (ppt)

#Constants: 
# 862194900000
# 12438000000
# 66423080
# 157570.1
# 139.344
#

#We change this according to how YSI does it in the EXO



#in this case, we are finding the DO mg/L per percent saturation, and multiplying it by our percent sat from telem. This percent sat accounts for pressure as well I believe. 


FRCH_EXO_cl.2019.renamed$ODO.mgL.Calc <-as.numeric(FRCH_EXO_cl.2019.renamed$ODO.Psat) * 
  #DO mg/L per sat
  (0.01* exp(
    #p1
    (-862194900000*(1/(FRCH_EXO_cl.2019.renamed$Temp.C+273.15))^4+12438000000*(1/(FRCH_EXO_cl.2019.renamed$Temp.C+273.15))^3-66423080*(1/(FRCH_EXO_cl.2019.renamed$Temp.C+273.15))^2+157570.1*(1/(FRCH_EXO_cl.2019.renamed$Temp.C+273.15))-139.344)
    #salinity
    -0* 
      #p2
      (2140.7*(1/(FRCH_EXO_cl.2019.renamed$Temp.C+273.15))^2-10.754*(1/(FRCH_EXO_cl.2019.renamed$Temp.C+273.15))+0.017674 )))

#fill missing rows with calculated MGL
FRCH_EXO_cl.2019.renamed <- FRCH_EXO_cl.2019.renamed %>% 
  mutate(ODO.mgL = coalesce(ODO.mgL,ODO.mgL.Calc))





### 2020 ####

frch.exo.2020 <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2020/EXO_processed/FRCH.EXO.cl.csv")


frch.exo.2020 <- frch.exo.2020 %>%
  dplyr::rename(ODO.mgL = ODO.mgL.mn.adj, ODO.Psat = ODO.Psat.mn.adj, ODO.Ploc = ODO.Ploc.mn.adj, Temp.C = Temp.C.mn) %>% select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc, site.ID)

frch.exo.2020$datetimeAK <- force_tz(as.POSIXct(frch.exo.2020$datetimeAK), "America/Anchorage")


# frch.exo.2020 <- exo.all.2020 %>% filter(site.ID == "FRCH")
# 
# #convert Bursts to means
# 
# frch.exo.2020$Temp.C <- as.numeric(frch.exo.2020$Temp.C)
# frch.exo.2020$ODO.Psat <- as.numeric(frch.exo.2020$ODO.Psat)
# frch.exo.2020$ODO.mgL <- as.numeric(frch.exo.2020$ODO.mgL)
# frch.exo.2020$ODO.Ploc <- as.numeric(frch.exo.2020$ODO.Ploc)
# 
# frch.exo.2020$datetimeAK <- lubridate::round_date(frch.exo.2020$DateTime, "15 minutes") 
# 
# #mean bursts
# frch.exo.2020$datetimeAK <- as.POSIXct(paste(frch.exo.2020$Date, frch.exo.2020$Time), format="%m/%e/%y %H:%M:%S")
# frch.exo.2020$datetimeAK <- lubridate::round_date(frch.exo.2020$datetimeAK, "15 minutes") 
# 
# FRCHmean2020odoMGL <- aggregate( ODO.mgL ~ datetimeAK, frch.exo.2020, mean)
# FRCHmean2020odoPSAT <- aggregate( ODO.Psat ~ datetimeAK, frch.exo.2020, mean)
# FRCHmean2020odoPLOC <- aggregate( ODO.Ploc ~ datetimeAK, frch.exo.2020, mean)
# FRCHmean2020odoTEMPC <- aggregate( Temp.C ~ datetimeAK, frch.exo.2020, mean)
# 
# FRCHmeans2020ODO <- plyr::join(FRCHmean2020odoMGL, FRCHmean2020odoPSAT, by = "datetimeAK")
# FRCHmeans2020ODO <- plyr::join(FRCHmeans2020ODO, FRCHmean2020odoPLOC, by = "datetimeAK")
# FRCHmeans2020ODO <- plyr::join(FRCHmeans2020ODO, FRCHmean2020odoTEMPC, by = "datetimeAK")
# 
# 
# FRCHmeans2020ODO.1 <- FRCHmeans2020ODO %>% filter(datetimeAK < "2020-06-13 18:45:00" & datetimeAK > "2020-06-11 13:45:00)")
# 
# FRCHmeans2020ODO.2 <- FRCHmeans2020ODO %>% filter(datetimeAK >= "2020-09-02 13:00:00")
# 
# FRCHmeans2020ODO <- rbind(FRCHmeans2020ODO.1, FRCHmeans2020ODO.2)
# 
# 
# 
# ########## DEPTH ############ 
# 
# 
# # Frch:
# 
# #download flowmeter data
# WR_20.url <- "https://drive.google.com/drive/u/1/folders/1tlcGKOm11j4nPqgBeTa1Hj6DFTrbZTvy"
# WR.20.1 <- drive_get(as_id(WR_20.url))
# frch.wr20_glist <- drive_ls(WR.20.1, pattern = "R_Flowmeter Q calculation_FRCH_for_R_JAA.csv")
# walk(frch.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# frch_WR_20.Data <- read.csv("R_Flowmeter Q calculation_FRCH_for_R_JAA.csv",
#                             skip = 0, header = TRUE, na.strings=c("","NA","blank"))
# 
# frch_WR_20.Data <- frch_WR_20.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# frch_WR_20.Data <- frch_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# frch_WR_20.Data$datetimeAK <- as.POSIXct(paste(frch_WR_20.Data$Date, frch_WR_20.Data$Time), format="%y%m%d %H:%M")
# 
# 
# frch_WR_20.Data <- frch_WR_20.Data %>%
#   select(Depth..cm., datetimeAK)
# 
# Frch_depth_20 <- ddply(na.omit(frch_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))
# 
# # frch_wr.lm <- lm(meanDepth~datetimeAK, Frch_depth_20)
# 
# 
# Frch_depth_20 <- setDT(Frch_depth_20)
# 
# Frch_depth_20 <- Frch_depth_20 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# FRCHmeans2020ODO <- setDT(FRCHmeans2020ODO)
# 
# setDT(Frch_depth_20)
# setDT(FRCHmeans2020ODO)
# 
# FRCHmeans2020ODO$datetimeAK1 <- FRCHmeans2020ODO$datetimeAK
# 
# setkey( FRCHmeans2020ODO, datetimeAK )
# setkey( Frch_depth_20, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_frch20 <- FRCHmeans2020ODO[ Frch_depth_20, roll = "nearest" ]
# 
# rounded.dates_frch20 <- rounded.dates_frch20 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_frch20 <- rounded.dates_frch20 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_frch20 <- rounded.dates_frch20 %>%
#   select(meanDepth, datetimeAK)
# 
# Frch.2020.DO.Depth <- merge(FRCHmeans2020ODO, rounded.dates_frch20, by = "datetimeAK", all = TRUE)
# Frch.2020.DO.Depth$meanDepth1 <- Frch.2020.DO.Depth$meanDepth
# 
# frch20mod <- lm(meanDepth ~ datetimeAK, Frch.2020.DO.Depth)
# 
# summary(frch20mod)
# 
# Frch.2020.DO.Depth <- Frch.2020.DO.Depth %>% 
#   mutate(predictedDepth = predict(frch20mod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))
# 



#### 2021 ####
frch.exo.2021 <- exo.processed.2021 %>% filter(site.ID == "FRCH")


frch.exo.2021$datetimeAK <- lubridate::round_date(frch.exo.2021$datetimeAK, "15 minutes")
frch.exo.2021$datetimeAK <- force_tz(frch.exo.2021$datetimeAK, "America/Anchorage")

frch.exo.2021$Temp.C <- as.numeric(frch.exo.2021$Temp.C)
frch.exo.2021$ODO.Psat <- as.numeric(frch.exo.2021$ODO.Psat)
frch.exo.2021$ODO.mgL <- as.numeric(frch.exo.2021$ODO.mgL)
frch.exo.2021$ODO.Ploc <- as.numeric(frch.exo.2021$ODO.Ploc)



#mean bursts
FRCHmean2021odoMGL <- aggregate( ODO.mgL ~ datetimeAK, frch.exo.2021, mean)
FRCHmean2021odoPSAT <- aggregate( ODO.Psat ~ datetimeAK, frch.exo.2021, mean)
FRCHmean2021odoPLOC <- aggregate( ODO.Ploc ~ datetimeAK, frch.exo.2021, mean)
FRCHmean2021odoTEMPC <- aggregate( Temp.C ~ datetimeAK, frch.exo.2021, mean)

FRCHmeans2021ODO <- plyr::join(FRCHmean2021odoMGL, FRCHmean2021odoPSAT, by = "datetimeAK")
FRCHmeans2021ODO <- plyr::join(FRCHmeans2021ODO, FRCHmean2021odoPLOC, by = "datetimeAK")
FRCHmeans2021ODO <- plyr::join(FRCHmeans2021ODO, FRCHmean2021odoTEMPC, by = "datetimeAK")
# 
# ########## DEPTH ############ 
# 
# 
# # Frch:
# 
# #download flowmeter data
# WR_21.url <- "https://drive.google.com/drive/u/1/folders/1MrFabu9Mzuv3v4naPl2-iCFaMj_DjkZG"
# WR.21.1 <- drive_get(as_id(WR_21.url))
# frch.wr21_glist <- drive_ls(WR.21.1, pattern = "R_Flowmeter_Q_calulation_FRCH_for_R_JAA_2021.csv")
# walk(frch.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# frch_WR_21.Data <- read.csv("R_Flowmeter_Q_calulation_FRCH_for_R_JAA_2021.csv",
#                             skip = 0, header = TRUE, na.strings=c("","NA","blank"))
# 
# 
# frch_WR_21.Data <- frch_WR_21.Data %>%
#   dplyr::rename(Date = ï..Date)
# 
# frch_WR_21.Data <- frch_WR_21.Data %>% tidyr::fill(Date, .direction = ("down"))
# 
# frch_WR_21.Data <- frch_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))
# 
# 
# frch_WR_21.Data$datetimeAK <- as.POSIXct(paste(frch_WR_21.Data$Date, frch_WR_21.Data$Time), format="%y%m%d %H:%M")
# 
# 
# frch_WR_21.Data <- frch_WR_21.Data %>%
#   select(Depth..cm., datetimeAK)
# 
# Frch_depth_21 <- ddply(na.omit(frch_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))
# 
# # frch_wr.lm <- lm(meanDepth~datetimeAK, Frch_depth_21)
# 
# 
# Frch_depth_21 <- setDT(Frch_depth_21)
# 
# Frch_depth_21 <- Frch_depth_21 %>%
#   dplyr::rename(datetimeAK = datetimeAK)
# 
# FRCHmeans2021ODO <- setDT(FRCHmeans2021ODO)
# 
# setDT(Frch_depth_21)
# setDT(FRCHmeans2021ODO)
# 
# FRCHmeans2021ODO$datetimeAK1 <- FRCHmeans2021ODO$datetimeAK
# 
# setkey( FRCHmeans2021ODO, datetimeAK )
# setkey( Frch_depth_21, datetimeAK )
# 
# #WR was taken when EXO out of water. round depth point to nearest in data record
# rounded.dates_frch21 <- FRCHmeans2021ODO[ Frch_depth_21, roll = "nearest" ]
# 
# rounded.dates_frch21 <- rounded.dates_frch21 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_frch21 <- rounded.dates_frch21 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_frch21 <- rounded.dates_frch21 %>%
#   select(meanDepth, datetimeAK)
# 
# Frch.2021.DO.Depth <- merge(FRCHmeans2021ODO, rounded.dates_frch21, by = "datetimeAK", all = TRUE)
# Frch.2021.DO.Depth$meanDepth1 <- Frch.2021.DO.Depth$meanDepth
# 
# frch21mod <- lm(meanDepth ~ datetimeAK, Frch.2021.DO.Depth)
# 
# summary(frch21mod)
# 
# Frch.2021.DO.Depth <- Frch.2021.DO.Depth %>% 
#   mutate(predictedDepth = predict(frch21mod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))
# 
# 
# 
# 
#Put together
FRCH_EXO_cl.2019.renamed <- FRCH_EXO_cl.2019.renamed %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)

frch.exo.2020 <- frch.exo.2020 %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)

# #outliers
# Frch.2020.DO.Depth.1 <- Frch.2020.DO.Depth %>% filter(datetimeAK < "2020-09-02 13:00:00")
# Frch.2020.DO.Depth.2 <- Frch.2020.DO.Depth %>% filter(datetimeAK >= "2020-09-02 13:30:00") %>% filter(datetimeAK < "2020-09-15 11:30:00")
# Frch.2020.DO.Depth.3 <- Frch.2020.DO.Depth %>% filter(datetimeAK >= "2020-09-15 12:45:00")
# 
# Frch.2020.DO.Depth <- rbind(Frch.2020.DO.Depth.1,Frch.2020.DO.Depth.2,Frch.2020.DO.Depth.3)


FRCHmeans2021ODO <- FRCHmeans2021ODO %>%
  select(datetimeAK, Temp.C, ODO.mgL, ODO.Psat, ODO.Ploc)


All.years.frch.MESSY <- rbind(FRCH_EXO_cl.2019.renamed, frch.exo.2020, FRCHmeans2021ODO)

All.years.frch.MESSY <- All.years.frch.MESSY %>%
  dplyr::rename(DO.obs = ODO.mgL)

All.years.frch.MESSY <- All.years.frch.MESSY %>%
  dplyr::rename(temp.water = Temp.C)

# All.years.frch.MESSY <- All.years.frch.MESSY %>%
#   dplyr::rename(depth = predictedDepth)
# 
# All.years.frch.MESSY$depth <- All.years.frch.MESSY$depth / 100

#keep in mind 2019 and 2020 is clean data and 2021 are not.
pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/FrchODO.pdf")

testPlotFRCH <- ggplot(data = All.years.frch.MESSY,
                       mapping = aes(x = datetimeAK, y = as.numeric(ODO.Psat))) + geom_point() + labs(x = "Date", y = "Dissolved Oxygen (%Sat)")
testPlotFRCH
dev.off()







