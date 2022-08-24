# light Calibrations for MARG

#Craw
crawPar.url <- "https://drive.google.com/drive/u/1/folders/1k14Iky5K6UU8OMxjA4yalup7GPVFRws3"
CrawPar.1 <- drive_get(as_id(crawPar.url))
crawPar_glist <- drive_ls(CrawPar.1, pattern = "220712_5366_123_004.CSV")
walk(crawPar_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
craw.logger.Data <- read.csv("220712_5366_123_004.CSV",
                            skip = 7, header = TRUE, na.strings=c("","NA","blank"))

craw.logger.Data <- craw.logger.Data %>%
  dplyr::rename(time = RAW.VALUE)

craw.logger.Data <- craw.logger.Data %>%
  dplyr::rename(Date = X.1)

craw.logger.Data <- craw.logger.Data %>%
  dplyr::rename(Value1 = CALIBRATED.VALUE)

craw.logger.Data <- craw.logger.Data %>%
  dplyr::rename(Value2 = X.2)

craw.logger.Data <- craw.logger.Data %>%
  dplyr::rename(count = X)

#Fix Date Time
craw.logger.Data$DateTime <- paste(craw.logger.Data$Date, craw.logger.Data$time, sep="")

craw.logger.Data$DateTime <-  dmy_hms(craw.logger.Data$DateTime)
craw.logger.Data$DateTime <- force_tz(craw.logger.Data$DateTime, "America/Anchorage")

#Calibrate logger 6366 (also misnamed 5366 in places) to LICOR
craw.logger.Data$AZ.CalibratedValue <- craw.logger.Data$Value1 * 0.1588


#Full days for day after deployment to day before removal 
craw.logger.Data.NDS <- craw.logger.Data %>%  filter(DateTime >= "2022-06-14 00:00:00") %>% filter(DateTime <= "2022-07-11 23:45:00")


testcraw <- craw.logger.Data.NDS %>%
  mutate(DateTime = month(DateTime)) %>%
  group_by(DateTime) %>%
  summarize(mean_Daily_light = mean(AZ.CalibratedValue))

DailyMeanCraw <- craw.logger.Data.NDS %>% 
  group_by(as.Date(DateTime)) %>% 
  dplyr::summarize(Mean_light = mean(AZ.CalibratedValue, na.rm=TRUE))


DailyMaxCraw <- craw.logger.Data.NDS %>% 
  group_by(as.Date(DateTime)) %>% 
  dplyr::summarize(Max_Light = max(AZ.CalibratedValue, na.rm=TRUE))


mean_monthly_max_craw <- mean(DailyMaxCraw$Max_Light)
mean_monthly_mean_craw <- mean(DailyMeanCraw$Mean_light)

# light Calibrations for MARG

#Mast
mastPar.url <- "https://drive.google.com/drive/u/1/folders/1pPYgANOsaQbxsDEObbTenSYmJeiRsWld"
MastPar.1 <- drive_get(as_id(mastPar.url))
mastPar_glist <- drive_ls(MastPar.1, pattern = "220712_6365_123_004.CSV")
walk(mastPar_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
mast.logger.Data <- read.csv("220712_6365_123_004.CSV",
                             skip = 7, header = TRUE, na.strings=c("","NA","blank"))

mast.logger.Data <- mast.logger.Data %>%
  dplyr::rename(time = RAW.VALUE)

mast.logger.Data <- mast.logger.Data %>%
  dplyr::rename(Date = X.1)

mast.logger.Data <- mast.logger.Data %>%
  dplyr::rename(Value1 = CALIBRATED.VALUE)

mast.logger.Data <- mast.logger.Data %>%
  dplyr::rename(Value2 = X.2)

mast.logger.Data <- mast.logger.Data %>%
  dplyr::rename(count = X)

#Fix Date Time
mast.logger.Data$DateTime <- paste(mast.logger.Data$Date, mast.logger.Data$time, sep="")

mast.logger.Data$DateTime <-  dmy_hms(mast.logger.Data$DateTime)
mast.logger.Data$DateTime <- force_tz(mast.logger.Data$DateTime, "America/Anchorage")

#Calibrate logger 6366 (also misnamed 5366 in places) to LICOR
mast.logger.Data$AZ.CalibratedValue <- mast.logger.Data$Value1 * 0.1512


mast.logger.Data.NDS <- mast.logger.Data %>%  filter(DateTime >= "2022-06-14 00:00:00") %>% filter(DateTime <= "2022-07-11 23:45:00")


DailyMeanMast <- mast.logger.Data.NDS %>% 
  group_by(as.Date(DateTime)) %>% 
  dplyr::summarize(Mean_light = mean(AZ.CalibratedValue, na.rm=TRUE))


DailyMaxMast <- mast.logger.Data.NDS %>% 
  group_by(as.Date(DateTime)) %>% 
  dplyr::summarize(Max_Light = max(AZ.CalibratedValue, na.rm=TRUE))


mean_monthly_max_mast <- mean(DailyMaxMast$Max_Light)
mean_monthly_mean_mast <- mean(DailyMeanMast$Mean_light)



# light Calibrations for MARG

#Shov
shovPar.url <- "https://drive.google.com/drive/u/1/folders/16dNWKCifboRLimq_n6pVf_W8UfeYpWlu"
ShovPar.1 <- drive_get(as_id(shovPar.url))
shovPar_glist <- drive_ls(ShovPar.1, pattern = "220715_shov_7922_123_003.CSV")
walk(shovPar_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
shov.logger.Data <- read.csv("220715_shov_7922_123_003.CSV",
                             skip = 7, header = TRUE, na.strings=c("","NA","blank"))

shov.logger.Data <- shov.logger.Data %>%
  dplyr::rename(time = RAW.VALUE)

shov.logger.Data <- shov.logger.Data %>%
  dplyr::rename(Date = X.1)

shov.logger.Data <- shov.logger.Data %>%
  dplyr::rename(Value1 = CALIBRATED.VALUE)

shov.logger.Data <- shov.logger.Data %>%
  dplyr::rename(Value2 = X.2)

shov.logger.Data <- shov.logger.Data %>%
  dplyr::rename(count = X)

#Fix Date Time
shov.logger.Data$DateTime <- paste(shov.logger.Data$Date, shov.logger.Data$time, sep="")

shov.logger.Data$DateTime <-  dmy_hms(shov.logger.Data$DateTime)
shov.logger.Data$DateTime <- force_tz(shov.logger.Data$DateTime, "America/Anchorage")

#Calibrate logger 6366 (also misnamed 5366 in places) to LICOR
shov.logger.Data$AZ.CalibratedValue <- shov.logger.Data$Value1 * 0.1588

shov.logger.Data.NDS <- shov.logger.Data %>%  filter(DateTime >= "2022-06-15 00:00:00") %>% filter(DateTime <= "2022-07-14 23:45:00")



DailyMeanShov <- shov.logger.Data.NDS %>% 
  group_by(as.Date(DateTime)) %>% 
  dplyr::summarize(Mean_light = mean(AZ.CalibratedValue, na.rm=TRUE))


DailyMaxShov <- shov.logger.Data.NDS %>% 
  group_by(as.Date(DateTime)) %>% 
  dplyr::summarize(Max_Light = max(AZ.CalibratedValue, na.rm=TRUE))


mean_monthly_max_shov <- mean(DailyMaxShov$Max_Light)
mean_monthly_mean_shov <- mean(DailyMeanShov$Mean_light)







# light Calibrations for MARG

#Poke
pokePar.url <- "https://drive.google.com/drive/u/1/folders/1JW8NRBDT-I9oIOayp3i4kGef9cP3nO3k"
PokePar.1 <- drive_get(as_id(pokePar.url))
pokePar_glist <- drive_ls(PokePar.1, pattern = "220801_11619_125_POKE.CSV")
walk(pokePar_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke.logger.Data <- read.csv("220801_11619_125_POKE.CSV",
                             skip = 7, header = TRUE, na.strings=c("","NA","blank"))

poke.logger.Data <- poke.logger.Data %>%
  dplyr::rename(time = RAW.VALUE)

poke.logger.Data <- poke.logger.Data %>%
  dplyr::rename(Date = X.1)

poke.logger.Data <- poke.logger.Data %>%
  dplyr::rename(Value1 = CALIBRATED.VALUE)

poke.logger.Data <- poke.logger.Data %>%
  dplyr::rename(Value2 = X.2)

poke.logger.Data <- poke.logger.Data %>%
  dplyr::rename(count = X)

#Fix Date Time
poke.logger.Data$DateTime <- paste(poke.logger.Data$Date, poke.logger.Data$time, sep="")

poke.logger.Data$DateTime <-  dmy_hms(poke.logger.Data$DateTime)
poke.logger.Data$DateTime <- force_tz(poke.logger.Data$DateTime, "America/Anchorage")

#Calibrate logger 6366 (also misnamed 5366 in places) to LICOR
poke.logger.Data$AZ.CalibratedValue <- poke.logger.Data$Value1 * 0.035


poke.logger.Data.NDS <- poke.logger.Data %>%  filter(DateTime >= "2022-06-21 00:00:00") %>% filter(DateTime <= "2022-07-18 23:45:00")



DailyMeanPoke <- poke.logger.Data.NDS %>% 
  group_by(as.Date(DateTime)) %>% 
  dplyr::summarize(Mean_light = mean(AZ.CalibratedValue, na.rm=TRUE))


DailyMaxPoke <- poke.logger.Data.NDS %>% 
  group_by(as.Date(DateTime)) %>% 
  dplyr::summarize(Max_Light = max(AZ.CalibratedValue, na.rm=TRUE))


mean_monthly_max_poke <- mean(DailyMaxPoke$Max_Light)
mean_monthly_mean_poke <- mean(DailyMeanPoke$Mean_light)




vaulPar.url <- "https://drive.google.com/drive/u/1/folders/1JW8NRBDT-I9oIOayp3i4kGef9cP3nO3k"
VaulPar.1 <- drive_get(as_id(vaulPar.url))
vaulPar_glist <- drive_ls(VaulPar.1, pattern = "220801_11616_125_VAUL.CSV")
walk(vaulPar_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul.logger.Data <- read.csv("220801_11616_125_VAUL.CSV",
                             skip = 7, header = TRUE, na.strings=c("","NA","blank"))

vaul.logger.Data <- vaul.logger.Data %>%
  dplyr::rename(time = RAW.VALUE)

vaul.logger.Data <- vaul.logger.Data %>%
  dplyr::rename(Date = X.1)

vaul.logger.Data <- vaul.logger.Data %>%
  dplyr::rename(Value1 = CALIBRATED.VALUE)

vaul.logger.Data <- vaul.logger.Data %>%
  dplyr::rename(Value2 = X.2)

vaul.logger.Data <- vaul.logger.Data %>%
  dplyr::rename(count = X)

#Fix Date Time
vaul.logger.Data$DateTime <- paste(vaul.logger.Data$Date, vaul.logger.Data$time, sep="")

vaul.logger.Data$DateTime <-  dmy_hms(vaul.logger.Data$DateTime)
vaul.logger.Data$DateTime <- force_tz(vaul.logger.Data$DateTime, "America/Anchorage")

#Calibrate logger 6366 (also misnamed 5366 in places) to LICOR
vaul.logger.Data$AZ.CalibratedValue <- vaul.logger.Data$Value1 * 0.032

vaul.logger.Data.NDS <- vaul.logger.Data %>%  filter(DateTime >= "2022-06-21 00:00:00") %>% filter(DateTime <= "2022-07-18 23:45:00")



DailyMeanVaul <- vaul.logger.Data.NDS %>% 
  group_by(as.Date(DateTime)) %>% 
  dplyr::summarize(Mean_light = mean(AZ.CalibratedValue, na.rm=TRUE))


DailyMaxVaul <- vaul.logger.Data.NDS %>% 
  group_by(as.Date(DateTime)) %>% 
  dplyr::summarize(Max_Light = max(AZ.CalibratedValue, na.rm=TRUE))


mean_monthly_max_vaul <- mean(DailyMaxVaul$Max_Light)
mean_monthly_mean_vaul <- mean(DailyMeanVaul$Mean_light)




moosPar.url <- "https://drive.google.com/drive/u/1/folders/1JW8NRBDT-I9oIOayp3i4kGef9cP3nO3k"
MoosPar.1 <- drive_get(as_id(moosPar.url))
moosPar_glist <- drive_ls(MoosPar.1, pattern = "220802_11617_125_MOOS.CSV")
walk(moosPar_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos.logger.Data <- read.csv("220802_11617_125_MOOS.CSV",
                             skip = 7, header = TRUE, na.strings=c("","NA","blank"))

moos.logger.Data <- moos.logger.Data %>%
  dplyr::rename(time = RAW.VALUE)

moos.logger.Data <- moos.logger.Data %>%
  dplyr::rename(Date = X.1)

moos.logger.Data <- moos.logger.Data %>%
  dplyr::rename(Value1 = CALIBRATED.VALUE)

moos.logger.Data <- moos.logger.Data %>%
  dplyr::rename(Value2 = X.2)

moos.logger.Data <- moos.logger.Data %>%
  dplyr::rename(count = X)

#Fix Date Time
moos.logger.Data$DateTime <- paste(moos.logger.Data$Date, moos.logger.Data$time, sep="")

moos.logger.Data$DateTime <-  dmy_hms(moos.logger.Data$DateTime)
moos.logger.Data$DateTime <- force_tz(moos.logger.Data$DateTime, "America/Anchorage")

#Calibrate logger 6366 (also misnamed 5366 in places) to LICOR
moos.logger.Data$AZ.CalibratedValue <- moos.logger.Data$Value1 * 0.037


moos.logger.Data.NDS <- moos.logger.Data %>%  filter(DateTime >= "2022-06-22 00:00:00") %>% filter(DateTime <= "2022-07-17 23:45:00")



DailyMeanMoos <- moos.logger.Data.NDS %>% 
  group_by(as.Date(DateTime)) %>% 
  dplyr::summarize(Mean_light = mean(AZ.CalibratedValue, na.rm=TRUE))


DailyMaxMoos <- moos.logger.Data.NDS %>% 
  group_by(as.Date(DateTime)) %>% 
  dplyr::summarize(Max_Light = max(AZ.CalibratedValue, na.rm=TRUE))


mean_monthly_max_moos <- mean(DailyMaxMoos$Max_Light)
mean_monthly_mean_moos <- mean(DailyMeanMoos$Mean_light)




frchPar.url <- "https://drive.google.com/drive/u/1/folders/1JW8NRBDT-I9oIOayp3i4kGef9cP3nO3k"
FrchPar.1 <- drive_get(as_id(frchPar.url))
frchPar_glist <- drive_ls(FrchPar.1, pattern = "220802_11615_125_FRCH.CSV")
walk(frchPar_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch.logger.Data <- read.csv("220802_11615_125_FRCH.CSV",
                             skip = 7, header = TRUE, na.strings=c("","NA","blank"))

frch.logger.Data <- frch.logger.Data %>%
  dplyr::rename(time = RAW.VALUE)

frch.logger.Data <- frch.logger.Data %>%
  dplyr::rename(Date = X.1)

frch.logger.Data <- frch.logger.Data %>%
  dplyr::rename(Value1 = CALIBRATED.VALUE)

frch.logger.Data <- frch.logger.Data %>%
  dplyr::rename(Value2 = X.2)

frch.logger.Data <- frch.logger.Data %>%
  dplyr::rename(count = X)

#Fix Date Time
frch.logger.Data$DateTime <- paste(frch.logger.Data$Date, frch.logger.Data$time, sep="")

frch.logger.Data$DateTime <-  dmy_hms(frch.logger.Data$DateTime)
frch.logger.Data$DateTime <- force_tz(frch.logger.Data$DateTime, "America/Anchorage")

#Calibrate logger 6366 (also misnamed 5366 in places) to LICOR
frch.logger.Data$AZ.CalibratedValue <- frch.logger.Data$Value1 * 0.031

frch.logger.Data.NDS <- frch.logger.Data %>%  filter(DateTime >= "2022-06-22 00:00:00") %>% filter(DateTime <= "2022-07-17 23:45:00")




DailyMeanFrch <- frch.logger.Data.NDS %>% 
  group_by(as.Date(DateTime)) %>% 
  dplyr::summarize(Mean_light = mean(AZ.CalibratedValue, na.rm=TRUE))


DailyMaxFrch <- frch.logger.Data.NDS %>% 
  group_by(as.Date(DateTime)) %>% 
  dplyr::summarize(Max_Light = max(AZ.CalibratedValue, na.rm=TRUE))


mean_monthly_max_frch <- mean(DailyMaxFrch$Max_Light)
mean_monthly_mean_frch <- mean(DailyMeanFrch$Mean_light)










































