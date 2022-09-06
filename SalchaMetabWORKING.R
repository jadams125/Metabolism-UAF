
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
library(here)

#Salcha Metabolism Remade


Salcha.exo.2021 <- read.csv(file=file.path("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Salcha/EXO_data/from_internal_harddrive/processed/SALCHA.EXO.aord.csv"))

#DO, water temp, Time
Salcha.exo.2021 <- Salcha.exo.2021 %>% select(ODO.mgL.mn, Temp.C.mn, datetimeAK)


# Discharge and Depth
dischargeSalcha <- read.csv(here("dischargeSalchaTXT.csv"))

dischargeSalcha <- dischargeSalcha[-c(1), ]

dischargeSalcha$datetimeAK <- as.POSIXct(mdy_hm(dischargeSalcha$datetime))

dischargeSalcha$datetimeAK <- force_tz(dischargeSalcha$datetimeAK, "America/Anchorage")


# convert to Meters 

dischargeSalcha$X1761_00065 <- as.numeric(dischargeSalcha$X1761_00065) * 0.3048
dischargeSalcha$X1760_00060 <- as.numeric(dischargeSalcha$X1760_00060) * 0.0283168

dischargeSalcha <- dischargeSalcha %>% rename(depth = X1761_00065)
dischargeSalcha <- dischargeSalcha %>% rename(discharge = X1760_00060)

dischargeSalcha <- dischargeSalcha %>% select(discharge, depth, datetimeAK)

salchaData <- join(Salcha.exo.2021, dischargeSalcha, by = "datetimeAK")

salchaData$datetimeAK <- as.POSIXct(salchaData$datetimeAK) 

#Solar Time

salchaData$solar.time <- calc_solar_time(salchaData$datetimeAK, -146.9281)

salchaData$light <- calc_light(salchaData$solar.time, 64.47153, -146.9281)


#DO SAT

strt.2021.atmo.url <- "https://drive.google.com/drive/u/2/folders/1-om0nU42U8fNmeWtBrhM8WQTbGqBW8RN"
strtatmo <- drive_get(as_id(strt.2021.atmo.url))
test1strt2021_glist <- drive_ls(strtatmo, pattern = "20005934_STRT_ATMO_210930.csv")
walk(test1strt2021_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt2021atmo.Data <- read.csv("20005934_STRT_ATMO_210930.csv", 
                              skip = 1, header = TRUE)

names(strt2021atmo.Data)[3]= c("pressure.air")
names(strt2021atmo.Data)[2]= c("dateTimePre")



strt2021atmo.Data$datetimeAK <- as.POSIXct(strt2021atmo.Data$dateTimePre, format="%m/%d/%y %I:%M:%S %p", tz="America/Anchorage")


salchatest1 <- plyr::join(strt2021atmo.Data, salchaData, by = "datetimeAK")

# Salcha_DoD_and_USGS_2021 <- na.omit(Salcha_DoD_and_USGS_2021)


salchatest1$ModeledDOSat <- calc_DO_sat(salchatest1$Temp.C.mn ,salchatest1$pressure.air, model = "garcia-benson")


#metabolism

salchaData <- salchatest1

salchaData <- salchaData %>% rename(temp.water = Temp.C.mn)
salchaData <- salchaData %>% rename(DO.obs = ODO.mgL.mn)
salchaData <- salchaData %>% rename(DO.sat = ModeledDOSat)

final.salcha.DT <- na.omit(salchaData) %>%
  select(discharge, light, temp.water, DO.sat,DO.obs,depth,solar.time)

str(final.salcha.DT)


write.csv(final.salcha.DT, here("outputs", "correctedSalchaDT.csv"))


bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)



bayes_name

bayes_specs <- specs(bayes_name,
                     burnin_steps=1000, saved_steps=3000, n_cores=4,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
)


mm.test.salcha.new <- metab(bayes_specs, data=final.salcha.DT)


get_fit(mm.test.salcha.new)
