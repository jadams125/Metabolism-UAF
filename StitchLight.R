##### Stitch Light Data #####
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


#### POKE 2019 ####
PAR.2019.url <- "https://drive.google.com/drive/u/1/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
PAR_19.prt1 <- drive_get(as_id(PAR.2019.url))
poke_par_glist <- drive_ls(PAR_19.prt1, pattern = "191017_11619_POKE.CSV")
walk(poke_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke.par.2019.Data <- read.csv("191017_11619_POKE.CSV",
                               skip = 8, header = FALSE)
poke.par.2019.Data <- poke.par.2019.Data %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
poke.par.2019.Data$DateTime <- paste(poke.par.2019.Data$Date, poke.par.2019.Data$Time, sep="")

poke.par.2019.Data$DateTime <-  dmy_hms(poke.par.2019.Data$DateTime)
poke.par.2019.Data$DateTime <- force_tz(poke.par.2019.Data$DateTime, "America/Anchorage")

#Calibrate logger 11619 to LICOR
poke.par.2019.Data$CalibratedValue <- poke.par.2019.Data$CalibratedValue * 0.035


#### VAUL 2019 ####
PAR.2019.url <- "https://drive.google.com/drive/u/1/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
PAR_19.prt1 <- drive_get(as_id(PAR.2019.url))
vaul_par_glist <- drive_ls(PAR_19.prt1, pattern = "191017_11616_VAUL.CSV")
walk(vaul_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul.par.2019.Data <- read.csv("191017_11616_VAUL.CSV",
                               skip = 8, header = FALSE)
vaul.par.2019.Data <- vaul.par.2019.Data %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
vaul.par.2019.Data$DateTime <- paste(vaul.par.2019.Data$Date, vaul.par.2019.Data$Time, sep="")

vaul.par.2019.Data$DateTime <-  dmy_hms(vaul.par.2019.Data$DateTime)
vaul.par.2019.Data$DateTime <- force_tz(vaul.par.2019.Data$DateTime, "America/Anchorage")

#Calibrate logger 11616 to LICOR
vaul.par.2019.Data$CalibratedValue <- vaul.par.2019.Data$CalibratedValue * 0.032



#### MOOS 2019 ####
PAR.2019.url <- "https://drive.google.com/drive/u/1/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
PAR_19.prt1 <- drive_get(as_id(PAR.2019.url))
moos_par_glist <- drive_ls(PAR_19.prt1, pattern = "191022_11617_MOOS.CSV")
walk(moos_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos.par.2019.Data <- read.csv("191022_11617_MOOS.CSV",
                              skip = 8, header = FALSE)
moos.par.2019.Data <- moos.par.2019.Data %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
moos.par.2019.Data$DateTime <- paste(moos.par.2019.Data$Date, moos.par.2019.Data$Time, sep="")

moos.par.2019.Data$DateTime <-  dmy_hms(moos.par.2019.Data$DateTime)
moos.par.2019.Data$DateTime <- force_tz(moos.par.2019.Data$DateTime, "America/Anchorage")

#Calibrate logger 11617 to LICOR
moos.par.2019.Data$CalibratedValue <- moos.par.2019.Data$CalibratedValue * 0.037 



#### STRT 2019 ####
PAR.2019.url <- "https://drive.google.com/drive/u/1/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
PAR_19.prt1 <- drive_get(as_id(PAR.2019.url))
strt_par_glist <- drive_ls(PAR_19.prt1, pattern = "191016_11620_PAR_STRT.CSV")
walk(strt_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt.par.2019.Data <- read.csv("191016_11620_PAR_STRT.CSV",
                               skip = 8, header = FALSE)
strt.par.2019.Data <- strt.par.2019.Data %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
strt.par.2019.Data$DateTime <- paste(strt.par.2019.Data$Date, strt.par.2019.Data$Time, sep="")

strt.par.2019.Data$DateTime <-  dmy_hms(strt.par.2019.Data$DateTime)
strt.par.2019.Data$DateTime <- force_tz(strt.par.2019.Data$DateTime, "America/Anchorage")

#Calibrate logger 11620 to LICOR
strt.par.2019.Data$CalibratedValue <- strt.par.2019.Data$CalibratedValue * 0.036



#### FRCH 2019 ####
PAR.2019.url <- "https://drive.google.com/drive/u/1/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
PAR_19.prt1 <- drive_get(as_id(PAR.2019.url))
frch_par_glist <- drive_ls(PAR_19.prt1, pattern = "191010_11615_FRCH.CSV")
walk(frch_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch.par.2019.Data <- read.csv("191010_11615_FRCH.CSV",
                               skip = 8, header = FALSE)
frch.par.2019.Data <- frch.par.2019.Data %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
frch.par.2019.Data$DateTime <- paste(frch.par.2019.Data$Date, frch.par.2019.Data$Time, sep="")

frch.par.2019.Data$DateTime <-  dmy_hms(frch.par.2019.Data$DateTime)
frch.par.2019.Data$DateTime <- force_tz(frch.par.2019.Data$DateTime, "America/Anchorage")

#Calibrate logger 11615 to LICOR
frch.par.2019.Data$CalibratedValue <- frch.par.2019.Data$CalibratedValue * 0.031







##### 2020 #####

#### POKE 2020 ####
PAR.2020.url <- "https://drive.google.com/drive/u/1/folders/1udve7khOM2EX0l8BZQK3yehb2HUbwFtR"
PAR_2020.prt1 <- drive_get(as_id(PAR.2020.url))
poke_par_glist <- drive_ls(PAR_2020.prt1, pattern = "11619_002_002_POKE_EndOfSeason.CSV")
walk(poke_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke.par.2020.Data <- read.csv("11619_002_002_POKE_EndOfSeason.CSV",
                               skip = 9, header = FALSE)
poke.par.2020.Data <- poke.par.2020.Data %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
poke.par.2020.Data$DateTime <- paste(poke.par.2020.Data$Date, poke.par.2020.Data$Time, sep="")

poke.par.2020.Data$DateTime <-  dmy_hms(poke.par.2020.Data$DateTime)
poke.par.2020.Data$DateTime <- force_tz(poke.par.2020.Data$DateTime, "America/Anchorage")

#Calibrate logger 11619 to LICOR
poke.par.2020.Data$CalibratedValue <- poke.par.2020.Data$CalibratedValue * 0.035


#### VAUL 2020 ####
PAR.2020.url <- "https://drive.google.com/drive/u/1/folders/1udve7khOM2EX0l8BZQK3yehb2HUbwFtR"
PAR_2020.prt1 <- drive_get(as_id(PAR.2020.url))
vaul_par_glist <- drive_ls(PAR_2020.prt1, pattern = "11616_005_002_VAUL_EndOfSeason.CSV")
walk(vaul_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul.par.2020.Data <- read.csv("11616_005_002_VAUL_EndOfSeason.CSV",
                               skip = 9, header = FALSE)
vaul.par.2020.Data <- vaul.par.2020.Data %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
vaul.par.2020.Data$DateTime <- paste(vaul.par.2020.Data$Date, vaul.par.2020.Data$Time, sep="")

vaul.par.2020.Data$DateTime <-  dmy_hms(vaul.par.2020.Data$DateTime)
vaul.par.2020.Data$DateTime <- force_tz(vaul.par.2020.Data$DateTime, "America/Anchorage")

#Calibrate logger 11616 to LICOR
vaul.par.2020.Data$CalibratedValue <- vaul.par.2020.Data$CalibratedValue * 0.032



#### MOOS 2020 ####
PAR.2020.url <- "https://drive.google.com/drive/u/1/folders/1udve7khOM2EX0l8BZQK3yehb2HUbwFtR"
PAR_2020.prt1 <- drive_get(as_id(PAR.2020.url))
moos_par_glist <- drive_ls(PAR_2020.prt1, pattern = "11617_004_002_MOOS_EndOfSeason.CSV")
walk(moos_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos.par.2020.Data <- read.csv("11617_004_002_MOOS_EndOfSeason.CSV",
                               skip = 9, header = FALSE)
moos.par.2020.Data <- moos.par.2020.Data %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
moos.par.2020.Data$DateTime <- paste(moos.par.2020.Data$Date, moos.par.2020.Data$Time, sep="")

moos.par.2020.Data$DateTime <-  dmy_hms(moos.par.2020.Data$DateTime)
moos.par.2020.Data$DateTime <- force_tz(moos.par.2020.Data$DateTime, "America/Anchorage")

#Calibrate logger 11617 to LICOR
moos.par.2020.Data$CalibratedValue <- moos.par.2020.Data$CalibratedValue * 0.037 



#### STRT 2020 ####
PAR.2020.url <- "https://drive.google.com/drive/u/1/folders/1udve7khOM2EX0l8BZQK3yehb2HUbwFtR"
PAR_2020.prt1 <- drive_get(as_id(PAR.2020.url))
strt_par_glist <- drive_ls(PAR_2020.prt1, pattern = "11620_001_002_STRT_EndOfSeason.CSV")
walk(strt_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt.par.2020.Data <- read.csv("11620_001_002_STRT_EndOfSeason.CSV",
                               skip = 9, header = FALSE)
strt.par.2020.Data <- strt.par.2020.Data %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
strt.par.2020.Data$DateTime <- paste(strt.par.2020.Data$Date, strt.par.2020.Data$Time, sep="")

strt.par.2020.Data$DateTime <-  dmy_hms(strt.par.2020.Data$DateTime)
strt.par.2020.Data$DateTime <- force_tz(strt.par.2020.Data$DateTime, "America/Anchorage")

#Calibrate logger 11620 to LICOR
strt.par.2020.Data$CalibratedValue <- strt.par.2020.Data$CalibratedValue * 0.036



#### FRCH 2020 ####
PAR.2020.url <- "https://drive.google.com/drive/u/1/folders/1udve7khOM2EX0l8BZQK3yehb2HUbwFtR"
PAR_2020.prt1 <- drive_get(as_id(PAR.2020.url))
frch_par_glist <- drive_ls(PAR_2020.prt1, pattern = "11615_003_002_FRCH_2_EndOfSeason.CSV")
walk(frch_par_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch.par.2020.Data <- read.csv("11615_003_002_FRCH_2_EndOfSeason.CSV",
                               skip = 9, header = FALSE)
frch.par.2020.Data <- frch.par.2020.Data %>%
  dplyr::rename(ScanNumber = V1, Date = V2, Time = V3, RawValue = V4, CalibratedValue = V5)

#Fix Date Time
frch.par.2020.Data$DateTime <- paste(frch.par.2020.Data$Date, frch.par.2020.Data$Time, sep="")

frch.par.2020.Data$DateTime <-  dmy_hms(frch.par.2020.Data$DateTime)
frch.par.2020.Data$DateTime <- force_tz(frch.par.2020.Data$DateTime, "America/Anchorage")

#Calibrate logger 11615 to LICOR
frch.par.2020.Data$CalibratedValue <- frch.par.2020.Data$CalibratedValue * 0.031




###### 2021 #####
PAR.2021.url <- "https://drive.google.com/drive/u/1/folders/1EPjHLDmfbCo5n12AKj7QpN2vXRCPQtg5"
PAR_2021.prt1 <- drive_get(as_id(PAR.2021.url))
par2021_glist <- drive_ls(PAR_2021.prt1, pattern = "all.dates.par.2021.csv")
walk(par2021_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
par2021.Data <- read.csv("all.dates.par.2021.csv",
                               skip = 0, header = TRUE)


# Sort into sites
poke.par2021.Data <- par2021.Data %>% filter(site == "poke")
vaul.par2021.Data <- par2021.Data %>% filter(site == "vaul")
strt.par2021.Data <- par2021.Data %>% filter(site == "strt")
moos.par2021.Data <- par2021.Data %>% filter(site == "moos")
frch.par2021.Data <- par2021.Data %>% filter(site == "frch")

poke.par2021.Data <- poke.par2021.Data %>%
  dplyr::rename(RawValue = V5)
vaul.par2021.Data <- vaul.par2021.Data %>%
  dplyr::rename(RawValue = V5)
strt.par2021.Data <- strt.par2021.Data %>%
  dplyr::rename(RawValue = V5)
moos.par2021.Data <- moos.par2021.Data %>%
  dplyr::rename(RawValue = V5)
frch.par2021.Data <- frch.par2021.Data %>%
  dplyr::rename(RawValue = V5)

poke.par2021.Data$DateTime <- as.POSIXct(poke.par2021.Data$DateTime)
vaul.par2021.Data$DateTime <- as.POSIXct(vaul.par2021.Data$DateTime)
strt.par2021.Data$DateTime <- as.POSIXct(strt.par2021.Data$DateTime)
moos.par2021.Data$DateTime <- as.POSIXct(moos.par2021.Data$DateTime)
frch.par2021.Data$DateTime <- as.POSIXct(frch.par2021.Data$DateTime)

#Calibrate loggers to LICOR
poke.par2021.Data$Calibrated.Value <- poke.par2021.Data$Calibrated.Value * 0.035

vaul.par2021.Data$Calibrated.Value <- vaul.par2021.Data$Calibrated.Value * 0.032

strt.par2021.Data$Calibrated.Value <- strt.par2021.Data$Calibrated.Value * 0.036

moos.par2021.Data$Calibrated.Value <- moos.par2021.Data$Calibrated.Value * 0.037 

frch.par2021.Data$Calibrated.Value <- frch.par2021.Data$Calibrated.Value * 0.031




#### Combine all years ####

#POKE
poke.par2021.Data <- poke.par2021.Data %>%
  select(Calibrated.Value, DateTime)

poke.par.2020.Data <- poke.par.2020.Data %>%
  select(CalibratedValue, DateTime) %>% dplyr::rename(Calibrated.Value = CalibratedValue)

poke.par.2019.Data <- poke.par.2019.Data %>%
  select(CalibratedValue, DateTime) %>% dplyr::rename(Calibrated.Value = CalibratedValue)

poke.combinded.par <- rbind(poke.par.2019.Data, poke.par.2020.Data, poke.par2021.Data)

write.csv(poke.combinded.par,"outputs/poke.combinded.par.csv", row.names = FALSE)


tiff("Plots/Poke_PAR_all_years.tiff", compression = "lzw", width = 1500, height =1000)
plot(poke.combinded.par$DateTime, poke.combinded.par$Calibrated.Value, main = "Poke PAR", ylab = "PAR (µmol of photons m-2 s-1)", xlab = "date")
dev.off()

#VAUL
vaul.par2021.Data <- vaul.par2021.Data %>%
  select(Calibrated.Value, DateTime)

vaul.par.2020.Data <- vaul.par.2020.Data %>%
  select(CalibratedValue, DateTime) %>% dplyr::rename(Calibrated.Value = CalibratedValue)

vaul.par.2019.Data <- vaul.par.2019.Data %>%
  select(CalibratedValue, DateTime) %>% dplyr::rename(Calibrated.Value = CalibratedValue)

vaul.combinded.par <- rbind(vaul.par.2019.Data, vaul.par.2020.Data, vaul.par2021.Data)

write.csv(vaul.combinded.par,"outputs/vaul.combinded.par.csv", row.names = FALSE)

tiff("Plots/Vaul_PAR_all_years.tiff", compression = "lzw", width = 1500, height =1000)
plot(vaul.combinded.par$DateTime, vaul.combinded.par$Calibrated.Value, main = "Vaul PAR", ylab = "PAR (µmol of photons m-2 s-1)", xlab = "date")
dev.off()

#STRT
strt.par2021.Data <- strt.par2021.Data %>%
  select(Calibrated.Value, DateTime)

strt.par.2020.Data <- strt.par.2020.Data %>%
  select(CalibratedValue, DateTime) %>% dplyr::rename(Calibrated.Value = CalibratedValue)

strt.par.2019.Data <- strt.par.2019.Data %>%
  select(CalibratedValue, DateTime) %>% dplyr::rename(Calibrated.Value = CalibratedValue)

strt.combinded.par <- rbind(strt.par.2019.Data, strt.par.2020.Data, strt.par2021.Data)

write.csv(strt.combinded.par,"outputs/strt.combinded.par.csv", row.names = FALSE)


tiff("Plots/Strt_PAR_all_years.tiff", compression = "lzw", width = 1500, height =1000)
plot(strt.combinded.par$DateTime, strt.combinded.par$Calibrated.Value, main = "Strt PAR", ylab = "PAR (µmol of photons m-2 s-1)", xlab = "date")
dev.off()



#MOOS
moos.par2021.Data <- moos.par2021.Data %>%
  select(Calibrated.Value, DateTime)

moos.par.2020.Data <- moos.par.2020.Data %>%
  select(CalibratedValue, DateTime) %>% dplyr::rename(Calibrated.Value = CalibratedValue)

moos.par.2019.Data <- moos.par.2019.Data %>%
  select(CalibratedValue, DateTime) %>% dplyr::rename(Calibrated.Value = CalibratedValue)

moos.combinded.par <- rbind(moos.par.2019.Data, moos.par.2020.Data, moos.par2021.Data)

write.csv(moos.combinded.par,"outputs/moos.combinded.par.csv", row.names = FALSE)


tiff("Plots/Moos_PAR_all_years.tiff", compression = "lzw", width = 1500, height =1000)
plot(moos.combinded.par$DateTime, moos.combinded.par$Calibrated.Value, main = "Moos PAR", ylab = "PAR (µmol of photons m-2 s-1)", xlab = "date")
dev.off()


#FRCH
frch.par2021.Data <- frch.par2021.Data %>%
  select(Calibrated.Value, DateTime)

frch.par.2020.Data <- frch.par.2020.Data %>%
  select(CalibratedValue, DateTime) %>% dplyr::rename(Calibrated.Value = CalibratedValue)

frch.par.2019.Data <- frch.par.2019.Data %>%
  select(CalibratedValue, DateTime) %>% dplyr::rename(Calibrated.Value = CalibratedValue)

frch.combinded.par <- rbind(frch.par.2019.Data, frch.par.2020.Data, frch.par2021.Data)

write.csv(frch.combinded.par,"outputs/frch.combinded.par.csv", row.names = FALSE)


tiff("Plots/Frch_PAR_all_years.tiff", compression = "lzw", width = 1500, height =1000)
plot(frch.combinded.par$DateTime, frch.combinded.par$Calibrated.Value, main = "Frch PAR", ylab = "PAR (µmol of photons m-2 s-1)", xlab = "date")
dev.off()


