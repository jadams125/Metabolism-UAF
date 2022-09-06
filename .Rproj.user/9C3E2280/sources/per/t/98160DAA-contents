############# Clean Metab Model #######

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
#################################### Salcha ##################
?streamMetabolizer



Salcha.exo.2021 <- read.csv(file=file.path("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Salcha/EXO_data/from_internal_harddrive/processed/SALCHA.EXO.aord.csv"))

Salcha.exo.2021$datetimeAK <-  force_tz(as.POSIXct(Salcha.exo.2021$datetimeAK), tz = "America/Anchorage")

Salcha.exo.2021$min <-  force_tz(as.POSIXct(Salcha.exo.2021$min), tz = "America/Anchorage")

#not repeatable
# USGS_Salcha <- read.csv(file=file.path("C:/Users/jacob/OneDrive - University of Alaska/USGS_Salcha_2021.csv"))
# 
# USGS_Salcha$DateTime <- as.POSIXct(paste(USGS_Salcha$Date, USGS_Salcha$Time), format="%m/%d/%Y %H:%M")


library(dataRetrieval)

?dataRetrieval

vignette("dataRetrieval", package = "dataRetrieval")


discharge <- readNWISuv(siteNumber = "15484000",
                        parameterCd = "00060",
                        startDate ="2021-5-18", 
                        endDate = "2021-9-30", tz = "America/Anchorage")

#convert to meters

discharge$X_00060_00000.meters <- discharge$X_00060_00000 / 35.315


# USGS_Salcha$DateTime <- as.POSIXct(paste(USGS_Salcha$Date, USGS_Salcha$Time), format="%m/%d/%Y %H:%M")




depth <- readNWISuv(siteNumber = "15484000",
                        parameterCd = "00064",
                        startDate ="2021-5-18", 
                        endDate = "2021-9-30", tz = "America/Anchorage")

GageHeight <- readNWISuv(siteNumber = "15484000",
                    parameterCd = "00065",
                    startDate ="2021-5-18", 
                    endDate = "2021-9-30", tz = "America/Anchorage")

#Convert to Meters
GageHeight$depthMeters <- GageHeight$X_00065_00000 *0.3048




# 
# test <- readNWISuv(siteNumber = "15484000",
#                    parameterCd = "00000",
#                     startDate ="2021-5-18", 
#                     endDate = "2021-9-30", tz = "America/Anchorage")
# View(test)




View(parameterCdFile)





dailyDataAvailable <- whatNWISdata(siteNumber = "15484000", service = "dv", statCd = "00003")


Salcha.exo.2021$DateTime <- Salcha.exo.2021$datetimeAK

#UTC? 
# lubridate::force_tz(as.POSIXct(Salcha.exo.2021$DateTime), 'UTC')


# discharge$DateTime <- lubridate::force_tz(as.POSIXct(discharge$dateTime), 'UTC')
# GageHeight$DateTime <- lubridate::force_tz(as.POSIXct(GageHeight$dateTime), 'UTC')

discharge$DateTime <- as.POSIXct(discharge$dateTime)
discharge$DateTime <-  force_tz(as.POSIXct(discharge$DateTime), tz = "America/Anchorage")


GageHeight$DateTime <- as.POSIXct(GageHeight$dateTime)


#check Structure 
str(Salcha.exo.2021)
str(discharge)
str(USGS_Salcha)


Salcha_DoD_and_USGS_2021 <- na.omit(plyr::join(discharge, Salcha.exo.2021, by = "DateTime"))

Salcha_DoD_and_USGS_2021 <- na.omit(plyr::join(GageHeight, Salcha_DoD_and_USGS_2021, by = "DateTime"))


Salcha_DoD_and_USGS_2021



####################### Metabolism ###########


#Continuous DO Data, temp data 


Salcha_DoD_and_USGS_2021$ODO.mgL.mn <- as.numeric(Salcha_DoD_and_USGS_2021$ODO.mgL.mn)

ODO.Mgl.salcha.2021 <- aggregate(ODO.mgL.mn ~ DateTime, Salcha_DoD_and_USGS_2021, mean )

ODO.psat.salcha.2021 <- aggregate(as.numeric(ODO.Psat.mn) ~ DateTime, Salcha_DoD_and_USGS_2021, mean )

#Where is DO saturated data 

Temp.C.2021.salcha <- aggregate(Temp.C.mn ~ DateTime, Salcha_DoD_and_USGS_2021, mean )

Temp.C.2021.salcha

######Light data 
# 
# install.packages("devtools")
# library(devtools)
# #Use the devtools packge to install StreamLightUtils
# devtools::install_github("psavoy/StreamLightUtils")
# devtools::install_github("psavoy/StreamLight")
# 
# library("StreamLightUtils")
# library("StreamLight")
# 
# 
# ?stream_light(
#               )
# 
# 
# 
# 
# #Clip data: ast good at 13:30
# strt2020Data <- strt2020Data[c(1:11328),]
# names(strt2020Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")
# 
# #Calibrate to known LICOR values
# strt2020Data$Calibrated.Par.Value <- strt2020Data$Calibrated.Value * 0.036
# 
# 
# strt2020Data$DateTime <- as.POSIXct(paste(strt2020Data$Date, strt2020Data$time), format="%d/%m/%Y %H:%M:%S")
# 
# strt2020Data$DateTime <- lubridate::round_date(strt2020Data$DateTime, "15 minutes") 
# 
# strt2020Data$DateTime
# 
# strt.2020.ggplot <- ggplot(data=strt2020Data, aes(y=Calibrated.Par.Value, x=DateTime)) +
#   geom_point() + 
#   labs(x = "Date and Time", y = "Calibrated PAR sensor values")+
#   ggtitle("Stuart 2020 PAR Data")
# 
# strt.2020.ggplot
# 
# strt2020Data$Calibrated.Par.Value      
# 
# #Discharge
# 
# strt2020Discharge <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/LabLearning/strt.final.discharge.2020.csv") 
# 
# strt2020Discharge$DateTime <-  strt2020Discharge$Strt1comb.2020.DateTime
# 
# 
# ### Combine them all 
# 
# Final.Strt.2020 <- plyr::join(strt.dod.combined, ODO.Mgl.strt.2020, by = "DateTime")
# Final.Strt.2020 <- plyr::join(Final.Strt.2020, ODO.psat.strt.2020, by = "DateTime")
# Final.Strt.2020 <- plyr::join(Final.Strt.2020, Temp.C.2020.strt, by = "DateTime")
# Final.Strt.2020 <- plyr::join(Final.Strt.2020, strt2020Data, by = "DateTime")
# Final.Strt.2020 <- plyr::join(Final.Strt.2020, strt2020Discharge, by = "DateTime")
# 
# str(Final.Strt.2020)
# 
# Final.Strt.2020$DateTime <- as.POSIXct(Final.Strt.2020$DateTime)
# str(Final.Strt.2020)
# 
# Final.Strt.2020 <- na.omit(Final.Strt.2020)
# 
# ### Solar Time
# 
# lubridate::tz(Final.Strt.2020$DateTime) # yep, we want and have the code for EST
# Final.Strt.2020$solar.time <- streamMetabolizer::calc_solar_time(Final.Strt.2020$DateTime, longitude=-146.479933)
# str(Final.Strt.2020)
# 
# ### Stream Metabolizer 
# 
# library(streamMetabolizer)
# 
# 
# 
# names(Final.Strt.2020)[5]= c("depth")
# names(Final.Strt.2020)[6]= c("DO.obs")
# names(Final.Strt.2020)[7]= c("DO.sat")
# names(Final.Strt.2020)[8]= c("temp.water")
# names(Final.Strt.2020)[14]= c("light")
# names(Final.Strt.2020)[18]= c("discharge")
# 


## BREAKS CODE


# write.csv(Final.Strt.2020, file = "C:/Users/jacob/Downloads/FinalStrt2020.csv")
# 
# Final.Strt.2020 <- read.csv(file = "C:/Users/jacob/Downloads/FinalStrt2020.csv")
# 
# Final.Strt.2020$solar.time <- as.POSIXct(Final.Strt.2020$solar.time)
# 
# test.strt <- Final.Strt.2020
# 
# bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)
# bayes_name
# 
# bayes_specs <- specs(bayes_name, 
#                      burnin_steps=1000, saved_steps=500, n_cores=8, 
#                      GPP_daily_lower = 0, ER_daily_upper = 0)
# 
# data.strt.mm <- na.omit(test.strt) %>%
#   select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)
# 
# mm.test <- metab(bayes_specs, data=data.strt.mm)
# 
# 
# 
# predict_metab(mm.test)
# plot_metab_preds(mm.test)
# 
# 
# predict_DO(mm) %>% head()
# plot_DO_preds(mm)
# mcmc <- get_mcmc(mm)
# rstan::traceplot(mcmc, pars='K600_daily', nrow=3)
# 
# get_fit(mm)
# #measure Rhat as a measure of convergence, shouldn't be over 1.1 
# 
# pairs(mm)
# 
# plot_DO_preds(mm)
# 
# 


################### Stream Metabolizer Method
library(streamMetabolizer)

Salcha_DoD_and_USGS_2021$solar.time.salcha <-calc_solar_time(Salcha_DoD_and_USGS_2021$DateTime, -146.9281)

# #make it UTC
# Salcha_DoD_and_USGS_2021$UTC.test <- format(Salcha_DoD_and_USGS_2021$solar.time.salcha, tz="UTC", usetz=TRUE)

# #classifies it as UTC
# Salcha_DoD_and_USGS_2021$UTC.test <- lubridate::force_tz(as.POSIXct(Salcha_DoD_and_USGS_2021$UTC.test), 'UTC')

#model light at salcha
Salcha_DoD_and_USGS_2021$LightSalcha <- as.numeric(calc_light(Salcha_DoD_and_USGS_2021$solar.time.salcha, 64.47153, -146.9281))

str(Salcha_DoD_and_USGS_2021)


# Salcha_DoD_and_USGS_2021$DateTime <- Salcha_DoD_and_USGS_2021$solar.time.salcha


# calc_light(
#   Salcha_DoD_and_USGS_2021$DateTime,
#   64,
#   146,
#   max.PAR = u(2326, "umol m^-2 s^-1"),)


# 
# library(unitted)
# calc_light(u(Salcha_DoD_and_USGS_2021$DateTime), u(64, 'degN'), u(146, 'degE'), u(2326, 'umol m^-2 s^-1'))
library(dplyr)



################## DO Sat #########
?calc_air_pressure()

#method One
# Salcha_DoD_and_USGS_2021$SatConCalcualted <- (100*Salcha_DoD_and_USGS_2021$DO.obs)/(Salcha_DoD_and_USGS_2021$ODO.Psat.mn)

#method 2
library(googledrive)
library(purrr)

strt.2021.atmo.url <- "https://drive.google.com/drive/u/2/folders/1-om0nU42U8fNmeWtBrhM8WQTbGqBW8RN"
strtatmo <- drive_get(as_id(strt.2021.atmo.url))
test1strt2021_glist <- drive_ls(strtatmo, pattern = "20005934_STRT_ATMO_210930.csv")
walk(test1strt2021_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt2021atmo.Data <- read.csv("20005934_STRT_ATMO_210930.csv", 
                              skip = 1, header = TRUE)


names(strt2021atmo.Data)[3]= c("pressure.air")
names(strt2021atmo.Data)[2]= c("dateTimePre")



strt2021atmo.Data$DateTime <- as.POSIXct(strt2021atmo.Data$dateTimePre, format="%m/%d/%y %I:%M:%S %p", tz="America/Anchorage")



strt2021atmo.Data$DateTime <- as.POSIXct(strt2021atmo.Data$DateTime)
Salcha_DoD_and_USGS_2021$DateTime <- as.POSIXct(Salcha_DoD_and_USGS_2021$DateTime)


Salcha_DoD_and_USGS_2021 <- plyr::join(strt2021atmo.Data, Salcha_DoD_and_USGS_2021, by = "DateTime")

Salcha_DoD_and_USGS_2021 <- na.omit(Salcha_DoD_and_USGS_2021)


Salcha_DoD_and_USGS_2021$ModelDOSat <- calc_DO_sat(Salcha_DoD_and_USGS_2021$Temp.C.mn ,Salcha_DoD_and_USGS_2021$pressure.air, model = "garcia-benson")


# 
# strt2021atmo.Data$DateTime <- strt2021atmo.Data$datetime1
# 
# 
# object1 <- strt2021atmo.Data %>% inner_join(Salcha_DoD_and_USGS_2021, by=c("DateTime","DateTime"))
# 
# 
# newObject <- plyr::join(strt2021atmo.Data,Salcha_DoD_and_USGS_2021, by = "DateTime")
# 
# 
# str(Salcha_DoD_and_USGS_2021)
# 
# strt2021atmo.Data$datetime1 <- as.POSIXct(strt2021atmo.Data$dateTimePre, format="%m/%d/%y %I:%M:%S %p", tz="America/Anchorage")




#######################




# 
# lubridate::force_tz(as.POSIXct(Salcha_DoD_and_USGS_2021$dateTime), 'America/anchorage')





#Dont use?

# 
# posix.time.localtz <- 
# 
# Salcha_DoD_and_USGS_2021$solar.time <- streamMetabolizer::calc_solar_time(posix.time.localtz, longitude=-146)

#may break code... IDK
# Salcha_DoD_and_USGS_2021$solar.time <- lubridate::force_tz(as.POSIXct(Salcha_DoD_and_USGS_2021$dateTime), 'UTC')

#why is this here?
# Salcha_DoD_and_USGS_2021 <- Salcha_DoD_and_USGS_2021[!duplicated(as.list(Salcha_DoD_and_USGS_2021))]

Salcha_DoD_and_USGS_2021 <- Salcha_DoD_and_USGS_2021 %>%
  dplyr::rename(depth = X_00065_00000)




Salcha_DoD_and_USGS_2021 <- Salcha_DoD_and_USGS_2021 %>%
  dplyr::rename(DO.obs = ODO.mgL.mn)

# Salcha_DoD_and_USGS_2021 <- Salcha_DoD_and_USGS_2021 %>%
#   dplyr::rename(DO.pecent.sat = DO.sat)


Salcha_DoD_and_USGS_2021 <- Salcha_DoD_and_USGS_2021 %>%
  dplyr::rename(temp.water = Temp.C.mn)

Salcha_DoD_and_USGS_2021 <- Salcha_DoD_and_USGS_2021 %>%
  dplyr::rename(light = LightSalcha)

Salcha_DoD_and_USGS_2021 <- Salcha_DoD_and_USGS_2021 %>%
  dplyr::rename(discharge = X_00060_00000.meters)

Salcha_DoD_and_USGS_2021 <- Salcha_DoD_and_USGS_2021 %>%
  dplyr::rename(DO.sat = ModelDOSat)

Salcha_DoD_and_USGS_2021 <- Salcha_DoD_and_USGS_2021 %>%
  dplyr::rename(solar.time = solar.time.salcha)



# names(Final.Strt.2020)[6]= c("DO.obs")
# names(Final.Strt.2020)[7]= c("DO.sat")
# names(Final.Strt.2020)[8]= c("temp.water")
# names(Final.Strt.2020)[14]= c("light")
# names(Final.Strt.2020)[18]= c("discharge")


final.salcha.DT <- Salcha_DoD_and_USGS_2021 %>%
  select(discharge, light, temp.water, DO.sat,DO.obs,depth,solar.time)


str(final.salcha.DT)

# 


# final.salcha.DT$solar.time <- lubridate::force_tz(as.POSIXct(Salcha_DoD_and_USGS_2021$dateTime), tz = 'UTC')


bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)



bayes_name

bayes_specs <- specs(bayes_name,
                     burnin_steps=1000, saved_steps=3000, n_cores=8,
                     GPP_daily_lower = 0, ER_daily_upper = 0,
                     )

data.salcha.mm <- na.omit(final.salcha.DT) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

# data.salcha.mm$discharge <- as.character(data.salcha.mm$discharge)
# data.salcha.mm$discharge <- as.numeric(data.salcha.mm$discharge)
# 
# str(data.salcha.mm$discharge)

write.csv(data.salcha.mm, here("salcha2021.csv"))
data.salcha.mm2 <- read.csv(here("salcha2021.csv"))

data.salcha.mm2$solar.time <- as.POSIXct(data.salcha.mm2$solar.time, tz = "UTC")

data.salcha.mm2 <- na.omit(data.salcha.mm2) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

data.salcha.mm2 <- data.salcha.mm2 %>% filter(solar.time < "2021-09-30 04:43:54")



data.salcha.mm2$discharge <- 

mm.test.salcha <- metab(bayes_specs, data=data.salcha.mm2)





predict_metab(mm.test)
plot_metab_preds(mm.test)



### MLE test




# 
# 
# predict_DO(mm) %>% head()
# plot_DO_preds(mm)
# mcmc <- get_mcmc(mm)
# rstan::traceplot(mcmc, pars='K600_daily', nrow=3)
# 
# get_fit(mm)
# #measure Rhat as a measure of convergence, shouldn't be over 1.1
# 
# pairs(mm)
# 
# plot_DO_preds(mm)

