# # Jacob Adams
# Metabolism run: Summer 2022

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


##### 2019 Data import #####


### EXO DATA
#Run "01_EXO_Input_clean_DoD_2019.R" 
#download Poker EXO data from this script

#POKE
POKE_EXO_cl <- read_csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_2019/EXO_processed/POKE.EXO.cl.csv")
View(POKE_EXO_cl)

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




### Light Data 2019
## I need to go through and look at field date time: data is uncleaned.
#POKE

#may through Nov 17th
poke.loggers.2019.url <- "https://drive.google.com/drive/u/1/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
poke_19.1 <- drive_get(as_id(poke.loggers.2019.url))
poke_19_glist <- drive_ls(poke_19.1, pattern = "191017_11619_POKE.CSV")
walk(poke_19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke.loggers.2019.Data <- read.csv("191017_11619_POKE.CSV",
                           skip = 8, header = FALSE)


poke.loggers.2019.Data$dateTime <- paste(poke.loggers.2019.Data$V2, poke.loggers.2019.Data$V3, sep="")

poke.loggers.2019.Data$dateTime <-  dmy_hms(poke.loggers.2019.Data$dateTime)
poke.loggers.2019.Data$dateTime <- force_tz(poke.loggers.2019.Data$dateTime, "America/Anchorage")

#poke first good PAR @11:30, No last good listed. Using water sample time as last good.
poke.loggers.2019.Data.trim <- subset(poke.loggers.2019.Data,
                                      poke.loggers.2019.Data$dateTime >= as.POSIXct('2019-05-14 11:30',
                                             tz = "America/Anchorage") &
                                        poke.loggers.2019.Data$dateTime <= as.POSIXct('2019-05-22 13:45',
                                               tz = "America/Anchorage"))

poke.loggers.2019.Data$DateTime <- force_tz(poke.loggers.2019.Data$dateTime, "America/Anchorage")
#Calibrate to known LICOR values
poke.loggers.2019.Data$UMOL_PAR <- poke.loggers.2019.Data$V5 * 0.035







#STRT

#September 17 - November 
strt.loggers.2019.url <- "https://drive.google.com/drive/u/1/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
strt_19.1 <- drive_get(as_id(strt.loggers.2019.url))
strt_19_glist <- drive_ls(strt_19.1, pattern = "191016_11620_PAR_STRT.CSV")
walk(strt_19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt.loggers.2019.Data.1 <- read.csv("191016_11620_PAR_STRT.CSV",
                                   skip = 8, header = FALSE)

strt.loggers.2019.Data.1$dateTime <- paste(strt.loggers.2019.Data.1$V2, strt.loggers.2019.Data.1$V3, sep="")



#may 21 - aug 29
strt.loggers.2019.url <- "https://drive.google.com/drive/u/1/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
strt_19.1 <- drive_get(as_id(strt.loggers.2019.url))
strt_19_glist2 <- drive_ls(strt_19.1, pattern = "190829_11620_PAR_STRT.CSV")
walk(strt_19_glist2$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt.loggers.2019.Data.2 <- read.csv("190829_11620_PAR_STRT.CSV",
                                     skip = 8, header = FALSE)

strt.loggers.2019.Data.2$dateTime <- paste(strt.loggers.2019.Data.2$V2, strt.loggers.2019.Data.2$V3, sep="")


#may 5 - may 21
strt.loggers.2019.url <- "https://drive.google.com/drive/u/1/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
strt_19.1 <- drive_get(as_id(strt.loggers.2019.url))
strt_19_glist3 <- drive_ls(strt_19.1, pattern = "190521_11620_STRT.CSV")
walk(strt_19_glist3$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt.loggers.2019.Data.3 <- read.csv("190521_11620_STRT.CSV",
                                     skip = 8, header = FALSE)

strt.loggers.2019.Data.3$dateTime <- paste(strt.loggers.2019.Data.3$V2, strt.loggers.2019.Data.3$V3, sep="")

merge


strt.loggers.2019.Data.combined <- rbind(strt.loggers.2019.Data.3, strt.loggers.2019.Data.2, strt.loggers.2019.Data.1)

strt.loggers.2019.Data.combined$DateTime <- dmy_hms(strt.loggers.2019.Data.combined$dateTime)



      #Calibrate to known LICOR values
strt.loggers.2019.Data.combined$UMOL_PAR <- strt.loggers.2019.Data.combined$V5 * 0.036




#data gap in September

# 
# #???
# 
# strt.loggers.2019.Data <- paste(strt.loggers.2019.Data$V2, strt.loggers.2019.Data$V3, sep="")
# 
# strt.loggers.2019.Data$dateTime <-  dmy_hms(strt.loggers.2019.Data$dateTime)
# strt.loggers.2019.Data$dateTime <- force_tz(strt.loggers.2019.Data$dateTime, "America/Anchorage")


#MOOS

#may 8th through Nov 23th
moos.loggers.2019.url <- "https://drive.google.com/drive/u/1/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
moos_19.1 <- drive_get(as_id(moos.loggers.2019.url))
moos_19_glist <- drive_ls(moos_19.1, pattern = "191022_11617_MOOS.CSV")
walk(moos_19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos.loggers.2019.Data <- read.csv("191022_11617_MOOS.CSV",
                                   skip = 8, header = FALSE)


moos.loggers.2019.Data$dateTime <- paste(moos.loggers.2019.Data$V2, poke.loggers.2019.Data$V3, sep="")

moos.loggers.2019.Data$dateTime <-  dmy_hms(moos.loggers.2019.Data$dateTime)
moos.loggers.2019.Data$DateTime <- force_tz(moos.loggers.2019.Data$dateTime, "America/Anchorage")
      #Calibrate to known LICOR values
moos.loggers.2019.Data$UMOL_PAR <- moos.loggers.2019.Data$V5 * 0.037





#VAUL 
#may 10th through Nov 18th
vaul.loggers.2019.url <- "https://drive.google.com/drive/u/1/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
vaul_19.1 <- drive_get(as_id(vaul.loggers.2019.url))
vaul_19_glist <- drive_ls(vaul_19.1, pattern = "191017_11616_VAUL.CSV")
walk(vaul_19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul.loggers.2019.Data <- read.csv("191017_11616_VAUL.CSV",
                                   skip = 8, header = FALSE)


vaul.loggers.2019.Data$dateTime <- paste(vaul.loggers.2019.Data$V2, poke.loggers.2019.Data$V3, sep="")

vaul.loggers.2019.Data$dateTime <-  dmy_hms(vaul.loggers.2019.Data$dateTime)
vaul.loggers.2019.Data$dateTime <- force_tz(vaul.loggers.2019.Data$dateTime, "America/Anchorage")


vaul.loggers.2019.Data$DateTime <- force_tz(vaul.loggers.2019.Data$dateTime, "America/Anchorage")
#Calibrate to known LICOR values
vaul.loggers.2019.Data$UMOL_PAR <- vaul.loggers.2019.Data$V5 * 0.032



#FRCH
#April 29th (?) through Nov 10th
frch.loggers.2019.url <- "https://drive.google.com/drive/u/1/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
frch_19.1 <- drive_get(as_id(frch.loggers.2019.url))
frch_19_glist <- drive_ls(frch_19.1, pattern = "191010_11615_FRCH.CSV")
walk(frch_19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch.loggers.2019.Data <- read.csv("191010_11615_FRCH.CSV",
                                   skip = 8, header = FALSE)


frch.loggers.2019.Data$dateTime <- paste(frch.loggers.2019.Data$V2, poke.loggers.2019.Data$V3, sep="")

frch.loggers.2019.Data$dateTime <-  dmy_hms(frch.loggers.2019.Data$dateTime)
frch.loggers.2019.Data$dateTime <- force_tz(frch.loggers.2019.Data$dateTime, "America/Anchorage")


frch.loggers.2019.Data$DateTime <- force_tz(frch.loggers.2019.Data$dateTime, "America/Anchorage")
#Calibrate to known LICOR values
frch.loggers.2019.Data$UMOL_PAR <- frch.loggers.2019.Data$V5 * 0.031



#### Discharge 2019:
# Should be cleaned already from Jake or TKH 


#FRCH
Q.frch.2019.url <- "https://drive.google.com/drive/u/1/folders/1r1dyD_-Lp5vPw18ESbe_6DeIM1UHkCHq"
Q.frch_19.1 <- drive_get(as_id(Q.frch.2019.url))
frch_Q_19_glist <- drive_ls(Q.frch_19.1, pattern = "final_frch_Q.csv")
walk(frch_Q_19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch.Q.2019.Data <- read.csv("final_frch_Q.csv",
                                   skip = 0, header = TRUE)

#STRT
Q.strt.2019.url <- "https://drive.google.com/drive/u/1/folders/1JrlqybmSuWnYQ0QrC0POjH16dHd5ZJC1"
Q.strt_19.1 <- drive_get(as_id(Q.strt.2019.url))
strt_Q_19_glist <- drive_ls(Q.strt_19.1, pattern = "final_strt_Q.csv")
walk(strt_Q_19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt.Q.2019.Data <- read.csv("final_strt_Q.csv",
                             skip = 0, header = TRUE)

#POKE
Q.poke.2019.url <- "https://drive.google.com/drive/u/1/folders/1AurL6qNOf8vxmQ8-D12CrH4HZF30qZAQ"
Q.poke_19.1 <- drive_get(as_id(Q.poke.2019.url))
poke_Q_19_glist <- drive_ls(Q.poke_19.1, pattern = "final_poke_Q.csv")
walk(poke_Q_19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke.Q.2019.Data <- read.csv("final_poke_Q.csv",
                             skip = 0, header = TRUE)

#VAUL
Q.vaul.2019.url <- "https://drive.google.com/drive/u/1/folders/1gUgATjIf5UN-UGTwkFxrDyOH4Y2QlKvz"
Q.vaul_19.1 <- drive_get(as_id(Q.vaul.2019.url))
vaul_Q_19_glist <- drive_ls(Q.vaul_19.1, pattern = "final_vaul_Q.csv")
walk(vaul_Q_19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul.Q.2019.Data <- read.csv("final_vaul_Q.csv",
                             skip = 0, header = TRUE)

#MOOS
Q.moos.2019.url <- " https://drive.google.com/drive/u/1/folders/1-4ZmwqR8xWGZ2qgmT-bRtp6PcSiqw4TP"
Q.moos_19.1 <- drive_get(as_id(Q.moos.2019.url))
moos_Q_19_glist <- drive_ls(Q.moos_19.1, pattern = "final_moos_Q.csv")
walk(moos_Q_19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos.Q.2019.Data <- read.csv("final_moos_Q.csv",
                             skip = 0, header = TRUE)



### DEPTH ####
#Having issues getting this to work... lets backtrack from JAKE'S code...
#Open up "01_PT_data.R and run the script"


#All PT data...
PT.2019.url <- "https://drive.google.com/drive/u/1/folders/1VdtpYHtfxSqp2DRyWTCu4NorvQ5bx_i4"
pt.19.1 <- drive_get(as_id(PT.2019.url))
pt.19_glist <- drive_ls(pt.19.1, pattern = "all.pt.2019.csv")
walk(pt.19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
pt.2019.Data <- read.csv("all.pt.2019.csv",
                             skip = 0, header = TRUE)


#separate out into individual sites to create a mean PT depth 

frch.data1 <- pt.2019.Data %>% filter(Site == "FRCH1")
frch.data2 <- pt.2019.Data %>% filter(Site == "FRCH2")
frch.2019.pt <- inner_join(frch.data1, frch.data2, by = "DateTime")
frch.2019.pt$DateTime <- as.POSIXct(paste(frch.2019.pt$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
frch.2019.pt$AvgAbsDepth <- (frch.2019.pt$AbsPTDepth.x + frch.2019.pt$AbsPTDepth.y)/2 



poke.data1 <- pt.2019.Data %>% filter(Site == "POKE1")
poke.data2 <- pt.2019.Data %>% filter(Site == "POKE2")
poke.2019.pt <- inner_join(poke.data1, poke.data2, by = "DateTime")
poke.2019.pt$DateTime <- as.POSIXct(paste(poke.2019.pt$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
poke.2019.pt$AvgAbsDepth <- (poke.2019.pt$AbsPTDepth.x + poke.2019.pt$AbsPTDepth.y)/2 



strt.data1 <- pt.2019.Data %>% filter(Site == "STRT1")
strt.data2 <- pt.2019.Data %>% filter(Site == "STRT2")
strt.data2$DateTime <- as.POSIXct(paste(strt.data2$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
strt.data2$DateTime <- as.character(strt.data2$DateTime)
strt.2019.pt <- inner_join(strt.data1, strt.data2, by = "DateTime")
strt.data2$DateTime <- as.POSIXct(strt.data2$DateTime)
strt.2019.pt$AvgAbsDepth <- (strt.2019.pt$AbsPTDepth.x + strt.2019.pt$AbsPTDepth.y)/2 


#MOOS and VAUL do not have a second PT in 2019
moos.data1 <- pt.2019.Data %>% filter(Site == "MOOS1")
moos.data1$DateTime <- as.POSIXct(paste(moos.data1$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
moos.2019.pt <- moos.data1



vaul.data1 <- pt.2019.Data %>% filter(Site == "VAUL1")
vaul.data1$DateTime <- as.POSIXct(paste(vaul.data1$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
vaul.2019.pt <- vaul.data1


#air pressure????
#STRT 2019
 strt.2019.air.P <- "https://drive.google.com/drive/u/1/folders/12-av--A9_rqcyvwOl9EiB0MSZdmWnPcj"
strt_19.ap <- drive_get(as_id(strt.2019.air.P))
strt_19.ap_glist <- drive_ls(strt_19.ap, pattern = "191016_20005934_STRT_ATM_0.csv")
walk(strt_19.ap_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt.ap.2019.Data <- read.csv("191016_20005934_STRT_ATM_0.csv",
                              skip = 1, header = TRUE)
strt.ap.2019.Data$DateTime <- strptime(strt.ap.2019.Data$Date.Time..GMT.08.00, "%m/%d/%y %I:%M:%S %p")

#MOOS
moos.2019.air.P.url <- "https://drive.google.com/drive/u/1/folders/12-av--A9_rqcyvwOl9EiB0MSZdmWnPcj"
moos_19.ap <- drive_get(as_id(moos.2019.air.P.url))
moos_19.ap_glist <- drive_ls(moos_19.ap, pattern = "191022_10710340_MOOS_ATM.csv")
walk(moos_19.ap_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos.ap.2019.Data <- read.csv("191022_10710340_MOOS_ATM.csv",
                              skip = 1, header = TRUE)
moos.ap.2019.Data$DateTime <- as.POSIXct(strptime(moos.ap.2019.Data$Date.Time..GMT.08.00, "%m/%d/%y %I:%M:%S %p"))

#POKE
poke.2019.air.P.url <- "https://drive.google.com/drive/u/1/folders/12-av--A9_rqcyvwOl9EiB0MSZdmWnPcj"
poke_19.ap <- drive_get(as_id(poke.2019.air.P.url))
poke_19.ap_glist <- drive_ls(poke_19.ap, pattern = "191017_20005936_POKE_ATM.csv")
walk(poke_19.ap_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke.ap.2019.Data <- read.csv("191017_20005936_POKE_ATM.csv",
                              skip = 1, header = TRUE)
poke.ap.2019.Data$DateTime <- as.POSIXct(strptime(poke.ap.2019.Data$Date.Time..GMT.08.00, "%m/%d/%y %I:%M:%S %p"))

poke.ap.2019.Data$DateTime <- lubridate::round_date(poke.ap.2019.Data$DateTime, "15 minutes") 

#FRCH
frch.2019.air.P.url <- "https://drive.google.com/drive/u/1/folders/12-av--A9_rqcyvwOl9EiB0MSZdmWnPcj"
frch_19.ap <- drive_get(as_id(frch.2019.air.P.url))
frch_19.ap_glist <- drive_ls(frch_19.ap, pattern = "191010_10710335_FRCH_ATM.csv")
walk(frch_19.ap_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch.ap.2019.Data <- read.csv("191010_10710335_FRCH_ATM.csv",
                              skip = 1, header = TRUE)
frch.ap.2019.Data$DateTime <- as.POSIXct(strptime(frch.ap.2019.Data$Date.Time..GMT.08.00, "%m/%d/%y %I:%M:%S %p"))

frch.ap.2019.Data$DateTime <- lubridate::round_date(frch.ap.2019.Data$DateTime, "15 minutes")

#VAUL
vaul.2019.air.P.url <- "https://drive.google.com/drive/u/1/folders/12-av--A9_rqcyvwOl9EiB0MSZdmWnPcj"
vaul_19.ap <- drive_get(as_id(vaul.2019.air.P.url))
vaul_19.ap_glist <- drive_ls(vaul_19.ap, pattern = "191017_20574425_VAUL_ATM.csv")
walk(vaul_19.ap_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul.ap.2019.Data <- read.csv("191017_20574425_VAUL_ATM.csv",
                              skip = 1, header = TRUE)
vaul.ap.2019.Data$DateTime <- as.POSIXct(strptime(vaul.ap.2019.Data$Date.Time..GMT.08.00, "%m/%d/%y %I:%M:%S %p"))

vaul.ap.2019.Data$DateTime <- lubridate::round_date(vaul.ap.2019.Data$DateTime, "15 minutes")



#more EXO
STRT_EXO_cl$DateTime <- STRT_EXO_cl$datetimeAK

MOOS_EXO_cl$DateTime <- MOOS_EXO_cl$datetimeAK

POKE_EXO_cl$DateTime <- POKE_EXO_cl$datetimeAK

FRCH_EXO_cl$DateTime <- FRCH_EXO_cl$datetimeAK

VAUL_EXO_cl$DateTime <- VAUL_EXO_cl$datetimeAK

#join it all together
Final.Strt.2019 <- plyr::join(STRT_EXO_cl, strt.2019.pt, by = "DateTime")
Final.Strt.2019 <- plyr::join(Final.Strt.2019, strt.Q.2019.Data, by = "DateTime")
Final.Strt.2019 <- plyr::join(Final.Strt.2019, strt.loggers.2019.Data.combined, by = "DateTime")
Final.Strt.2019 <- plyr::join(Final.Strt.2019, strt.ap.2019.Data, by = "DateTime")

Final.Moos.2019 <- plyr::join(MOOS_EXO_cl, moos.2019.pt, by = "DateTime")
Final.Moos.2019 <- plyr::join(Final.Moos.2019, moos.Q.2019.Data, by = "DateTime")
Final.Moos.2019 <- plyr::join(Final.Moos.2019, moos.loggers.2019.Data, by = "DateTime")
Final.Moos.2019 <- plyr::join(Final.Moos.2019, moos.ap.2019.Data, by = "DateTime")


Final.Poke.2019 <- plyr::join(POKE_EXO_cl, poke.2019.pt, by = "DateTime")
Final.Poke.2019 <- plyr::join(Final.Poke.2019, poke.Q.2019.Data, by = "DateTime")
Final.Poke.2019 <- plyr::join(Final.Poke.2019, poke.loggers.2019.Data, by = "DateTime")
Final.Poke.2019 <- plyr::join(Final.Poke.2019, poke.ap.2019.Data, by = "DateTime")

Final.Frch.2019 <- plyr::join(FRCH_EXO_cl, frch.2019.pt, by = "DateTime")
Final.Frch.2019 <- plyr::join(Final.Frch.2019, frch.Q.2019.Data, by = "DateTime")
Final.Frch.2019 <- plyr::join(Final.Frch.2019, frch.loggers.2019.Data, by = "DateTime")
Final.Frch.2019 <- plyr::join(Final.Frch.2019, frch.ap.2019.Data, by = "DateTime")

Final.Moos.2019 <- plyr::join(MOOS_EXO_cl, moos.2019.pt, by = "DateTime")
Final.Moos.2019 <- plyr::join(Final.Moos.2019, moos.Q.2019.Data, by = "DateTime")
Final.Moos.2019 <- plyr::join(Final.Moos.2019, moos.loggers.2019.Data, by = "DateTime")
Final.Moos.2019 <- plyr::join(Final.Moos.2019, moos.ap.2019.Data, by = "DateTime")

Final.Vaul.2019 <- plyr::join(VAUL_EXO_cl, vaul.2019.pt, by = "DateTime")
Final.Vaul.2019 <- plyr::join(Final.Vaul.2019, vaul.Q.2019.Data, by = "DateTime")
Final.Vaul.2019 <- plyr::join(Final.Vaul.2019, vaul.loggers.2019.Data, by = "DateTime")
Final.Vaul.2019 <- plyr::join(Final.Vaul.2019, vaul.ap.2019.Data, by = "DateTime")


#Rename it for stream metabolizer 
strt.run.2019 <- na.omit(Final.Strt.2019)

strt.run.2019 <- strt.run.2019 %>%
  dplyr::rename(depth = AvgAbsDepth)

strt.run.2019 <- strt.run.2019 %>%
  dplyr::rename(DO.obs = ODO.mgL.mn)

strt.run.2019 <- strt.run.2019 %>%
  dplyr::rename(temp.water = Temp.C.mn)

strt.run.2019 <- strt.run.2019 %>%
  dplyr::rename(light = V5)

strt.run.2019 <- strt.run.2019 %>%
  dplyr::rename(discharge = MeanDischarge)

strt.run.2019 <- strt.run.2019 %>%
  dplyr::rename(solar.time = DateTime)

strt.run.2019 <- strt.run.2019 %>%
  dplyr::rename(pressure.air = Abs.Pres..kPa..LGR.S.N..20005934..SEN.S.N..20005934..LBL..P.)

strt.run.2019 <- na.omit(strt.run.2019)


#moos
colnames(Final.Moos.2019)[25] <- c("site_1")
moos.run.2019 <- na.omit(Final.Moos.2019)

moos.run.2019 <- moos.run.2019 %>%
  dplyr::rename(depth = AbsPTDepth)

moos.run.2019 <- moos.run.2019 %>%
  dplyr::rename(DO.obs = ODO.mgL.mn)

moos.run.2019 <- moos.run.2019 %>%
  dplyr::rename(temp.water = Temp.C.mn)

moos.run.2019 <- moos.run.2019 %>%
  dplyr::rename(light = V5)

moos.run.2019 <- moos.run.2019 %>%
  dplyr::rename(discharge = MeanDischarge)

moos.run.2019 <- moos.run.2019 %>%
  dplyr::rename(solar.time = DateTime)

colnames(moos.run.2019)[40] <- c("pressure.air")

moos.run.2019 <- na.omit(moos.run.2019)


#poke
#poke
poke.run.2019 <- na.omit(Final.Poke.2019)

poke.run.2019 <- poke.run.2019 %>%
  dplyr::rename(depth = AvgAbsDepth)

poke.run.2019 <- poke.run.2019 %>%
  dplyr::rename(DO.obs = ODO.mgL.mn)

poke.run.2019 <- poke.run.2019 %>%
  dplyr::rename(temp.water = Temp.C.mn)

poke.run.2019 <- poke.run.2019 %>%
  dplyr::rename(light = V5)

poke.run.2019 <- poke.run.2019 %>%
  dplyr::rename(discharge = MeanDischarge)

poke.run.2019 <- poke.run.2019 %>%
  dplyr::rename(solar.time = DateTime)

poke.run.2019 <- poke.run.2019 %>%
  dplyr::rename(pressure.air = Abs.Pres..kPa..LGR.S.N..20005936..SEN.S.N..20005936..LBL..P.)

poke.run.2019 <- na.omit(poke.run.2019)

#FRCH
frch.run.2019 <- na.omit(Final.Frch.2019)

frch.run.2019 <- frch.run.2019 %>%
  dplyr::rename(depth = AvgAbsDepth)

frch.run.2019 <- frch.run.2019 %>%
  dplyr::rename(DO.obs = ODO.mgL.mn)

frch.run.2019 <- frch.run.2019 %>%
  dplyr::rename(temp.water = Temp.C.mn)

frch.run.2019 <- frch.run.2019 %>%
  dplyr::rename(light = V5)

frch.run.2019 <- frch.run.2019 %>%
  dplyr::rename(discharge = MeanDischarge)

frch.run.2019 <- frch.run.2019 %>%
  dplyr::rename(solar.time = DateTime)

frch.run.2019 <- frch.run.2019 %>%
  dplyr::rename(pressure.air = Abs.Pres..kPa..LGR.S.N..10710335..SEN.S.N..10710335..LBL..P.)

frch.run.2019 <- na.omit(frch.run.2019)


#VAUL
vaul.run.2019 <- na.omit(Final.Vaul.2019)
colnames(vaul.run.2019)[25] <- c("site_1")


vaul.run.2019 <- vaul.run.2019 %>%
  dplyr::rename(depth = AbsPTDepth)

vaul.run.2019 <- vaul.run.2019 %>%
  dplyr::rename(DO.obs = ODO.mgL.mn)

vaul.run.2019 <- vaul.run.2019 %>%
  dplyr::rename(temp.water = Temp.C.mn)

vaul.run.2019 <- vaul.run.2019 %>%
  dplyr::rename(light = V5)

vaul.run.2019 <- vaul.run.2019 %>%
  dplyr::rename(discharge = MeanDischarge)

vaul.run.2019 <- vaul.run.2019 %>%
  dplyr::rename(solar.time = DateTime)

vaul.run.2019 <- vaul.run.2019 %>%
  dplyr::rename(pressure.air = Abs.Pres..kPa..LGR.S.N..20574425..SEN.S.N..20574425.)

vaul.run.2019 <- na.omit(vaul.run.2019)




##Calc DO at saturation (not %sat), convert to mbar from Kpa?
#strt
strt.run.2019$DO.sat <- calc_DO_sat(strt.run.2019$temp.water, (strt.run.2019$pressure.air*10), model = "garcia-benson")

#moos
moos.run.2019$DO.sat <- calc_DO_sat(moos.run.2019$temp.water, (moos.run.2019$pressure.air*10), model = "garcia-benson")

#poke
poke.run.2019$DO.sat <- calc_DO_sat(poke.run.2019$temp.water, (poke.run.2019$pressure.air*10), model = "garcia-benson")

#frch
frch.run.2019$DO.sat <- calc_DO_sat(frch.run.2019$temp.water, (frch.run.2019$pressure.air*10), model = "garcia-benson")

#vaul
vaul.run.2019$DO.sat <- calc_DO_sat(vaul.run.2019$temp.water, (vaul.run.2019$pressure.air*10), model = "garcia-benson")


bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_name


bayes_specs <- specs(bayes_name, 
                     burnin_steps=500, saved_steps=1000, n_cores=8, 
                     GPP_daily_lower = 0, ER_daily_upper = 0)

#STRT FINAL RUN
    # Cut off strt date bc of no good light data
strt.run.2019 <- subset(strt.run.2019, solar.time<= "2019-08-15")


data.strt.mm <- na.omit(strt.run.2019) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

mm.test.strt.2019 <- metab(bayes_specs, data=data.strt.mm)

Predict <- predict_metab(mm.test.strt.2019)
strt.metab.plot.19 <- plot_metab_preds(mm.test.strt.2019, style = "ggplot2")
strt.metab.plot.19 + labs(title="strt 2019 metab")

#MOOS FINAL RUN
data.moos.mm <- na.omit(moos.run.2019) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

mm.test.moos.2019 <- metab(bayes_specs, data=data.moos.mm)

Predict <- predict_metab(mm.test.moos.2019)
moos.metab.plot.19 <- plot_metab_preds(mm.test.moos.2019)
moos.metab.plot.19+ labs(title="moos 2019 metab")



#POKE FINAL RUN
data.poke.mm <- na.omit(poke.run.2019) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

mm.test.poke.2019 <- metab(bayes_specs, data=data.poke.mm)

Predict <- predict_metab(mm.test.poke.2019)
poke.metab.plot.19 <- plot_metab_preds(mm.test.poke.2019)
poke.metab.plot.19+ labs(title="poke 2019 metab")

#FRCH FINAL RUN

# Cut off strt date bc of no good light data


data.frch.mm <- na.omit(frch.run.2019) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

data.frch.mm <- data.frch.mm %>% distinct()

mm.test.frch.2019 <- metab(bayes_specs, data=data.frch.mm)

Predict <- predict_metab(mm.test.frch.2019)
frch.metab.plot.19 <- plot_metab_preds(mm.test.frch.2019)
frch.metab.plot.19+ labs(title="frch 2019 metab")

#VAUL FINAL RUN

data.vaul.mm <- na.omit(vaul.run.2019) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

data.vaul.mm <- data.vaul.mm %>% distinct()

mm.test.vaul.2019 <- metab(bayes_specs, data=data.vaul.mm)

Predict <- predict_metab(mm.test.vaul.2019)
vaul.metab.plot.19 <- plot_metab_preds(mm.test.vaul.2019)
vaul.metab.plot.19+ labs(title="vaul 2019 metab")

