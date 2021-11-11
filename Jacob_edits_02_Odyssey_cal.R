# press Command+Option+O to collapse all sections and get an overview of the workflow! #

#### read me ####

#### libraries ####
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(plyr)
library(zoo)
library(xts)
library(forecast)
library(googledrive)



#(Jacob)
oddessy.data.2021.url <- "https://drive.google.com/drive/u/2/folders/1EPjHLDmfbCo5n12AKj7QpN2vXRCPQtg5"
oddessy2021 <- drive_get(as_id(oddessy.data.2021.url))
test1_glist <- drive_ls(oddessy2021, pattern = "all.dates.par.2021.csv")
walk(test1_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
Par2021Data <- read.csv("all.dates.par.2021.csv")


instrument_2020_PAR_calibration <- "https://drive.google.com/drive/u/2/folders/1Yclao7XQr7B7OeNRdavRfw3_DQ2JRoUT"
Par2020google <- drive_get(as_id(instrument_2020_PAR_calibration))
test2_glist <- drive_ls(Par2020google, pattern = "CR1000_Table1.dat")
Oddessy2021data<- walk(test2_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))




##(Jacob) will be read later

#### load reference data from 200530 ####

 #(Jacob) Use Alex's Method to format DAT file so it is useable
      importCSdata <- function(filename,RetOpt="data"){
        if(RetOpt=="info"){
          # bring in entire header of CSI TOA5 data file for metadata
          stn.info <- scan(file=filename,nlines=4,what=character(),sep="\r")
          return(stn.info)
        } else {
          # second line of header contains variable names
          header <- scan(file=filename,skip=1,nlines=1,what=character(),sep=",")
          # bring in data
          stn.data <- read.table(file=filename,skip=4,header=FALSE, na.strings=c("NAN"),sep=",")
          names(stn.data) <- header
          # add column of R-formatted date/timestamps
          stn.data$TIMESTAMP <- as.POSIXlt(strptime(stn.data$TIMESTAMP,"%Y-%m-%d %H:%M:%S"))
          return(stn.data)}
      }

#  The flux dens is the instantaneous umol/sec/m2 PAR and the total is mmol/m2/15min

ref_200530 = importCSdata("CR1000_Table1.dat") #(Jacob) Changed to our data file downloaded from google

ref_200530$datetime_AK = as.POSIXct(ref_200530$TIMESTAMP, tz="America/Anchorage")

# cut 1 hr off each end for logger set up/take down
ref_200530 = ref_200530[ref_200530$datetime_AK > as.POSIXct("2020-05-30 20:45:00") &
                          ref_200530$datetime_AK < as.POSIXct("2020-05-31 18:15:00"),]

ggplot(ref_200530, aes(datetime_AK, PAR_Tot_Tot)) + geom_line()
                                  

#### load Odyssey logger data from 200530 ####

  #### (Jacob) Our data from 2021 is not 200530, but the data used for the calibration is. 


  ####Download data from google








#### Old import method
#SN11615 = read.csv("Odyssey PAR calibration/2020/11615_cal.CSV",
                  # skip = 8, header = F)

#new import with google

#11615
  oddessy2020cal_drive <- drive_get(as_id(instrument_2020_PAR_calibration))
  oddessy2020cal_glist <- drive_ls(oddessy2020cal_drive, pattern = "11615_cal.CSV")
  walk(oddessy2020cal_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
  cal.11615_2020Data <- read.csv("11615_cal.CSV", skip = 8, header = F)
  
  cal.11615_2020Data = cal.11615_2020Data[,2:4]
  colnames(cal.11615_2020Data) = c("Date", "Time", "counts")
  cal.11615_2020Data$datetime_AK = paste(cal.11615_2020Data$Date, cal.11615_2020Data$Time, sep=" ")
  cal.11615_2020Data$datetime_AK = as.POSIXct(cal.11615_2020Data$datetime_AK, format="%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
  cal.11615_2020Data$SN = "SN11615"

#11616
  oddessy2020cal_drive <- drive_get(as_id(instrument_2020_PAR_calibration))
  oddessy2020cal_glist <- drive_ls(oddessy2020cal_drive, pattern = "11616_cal.CSV")
  walk(oddessy2020cal_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
  cal.11616_2020Data <- read.csv("11616_cal.CSV", skip = 8, header = F)
  
  cal.11616_2020Data = cal.11616_2020Data[,2:4]
  colnames(cal.11616_2020Data) = c("Date", "Time", "counts")
  cal.11616_2020Data$datetime_AK = paste(cal.11616_2020Data$Date, cal.11616_2020Data$Time, sep=" ")
  cal.11616_2020Data$datetime_AK = as.POSIXct(cal.11616_2020Data$datetime_AK, format="%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
  cal.11616_2020Data$SN = "SN11616"
  
  
  
#11617
  
  oddessy2020cal_drive <- drive_get(as_id(instrument_2020_PAR_calibration))
  oddessy2020cal_glist <- drive_ls(oddessy2020cal_drive, pattern = "11617_cal.CSV")
  walk(oddessy2020cal_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
  cal.11617_2020Data <- read.csv("11617_cal.CSV", skip = 8, header = F)
  
  cal.11617_2020Data = cal.11617_2020Data[,2:4]
  colnames(cal.11617_2020Data) = c("Date", "Time", "counts")
  cal.11617_2020Data$datetime_AK = paste(cal.11617_2020Data$Date, cal.11617_2020Data$Time, sep=" ")
  cal.11617_2020Data$datetime_AK = as.POSIXct(cal.11617_2020Data$datetime_AK, format="%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
  cal.11617_2020Data$SN = "SN11617"
  
#11618
  
  oddessy2020cal_drive <- drive_get(as_id(instrument_2020_PAR_calibration))
  oddessy2020cal_glist <- drive_ls(oddessy2020cal_drive, pattern = "11618_cal.CSV")
  walk(oddessy2020cal_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
  cal.11618_2020Data <- read.csv("11618_cal.CSV", skip = 8, header = F)
  
  cal.11618_2020Data = cal.11618_2020Data[,2:4]
  colnames(cal.11618_2020Data) = c("Date", "Time", "counts")
  cal.11618_2020Data$datetime_AK = paste(cal.11618_2020Data$Date, cal.11618_2020Data$Time, sep=" ")
  cal.11618_2020Data$datetime_AK = as.POSIXct(cal.11618_2020Data$datetime_AK, format="%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
  cal.11618_2020Data$SN = "SN11618"
  
  
#11619
  
  oddessy2020cal_drive <- drive_get(as_id(instrument_2020_PAR_calibration))
  oddessy2020cal_glist <- drive_ls(oddessy2020cal_drive, pattern = "11619_cal.CSV")
  walk(oddessy2020cal_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
  cal.11619_2020Data <- read.csv("11619_cal.CSV", skip = 8, header = F)
  
  cal.11619_2020Data = cal.11619_2020Data[,2:4]
  colnames(cal.11619_2020Data) = c("Date", "Time", "counts")
  cal.11619_2020Data$datetime_AK = paste(cal.11619_2020Data$Date, cal.11619_2020Data$Time, sep=" ")
  cal.11619_2020Data$datetime_AK = as.POSIXct(cal.11619_2020Data$datetime_AK, format="%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
  cal.11619_2020Data$SN = "SN11619"
  

#11620
  
  oddessy2020cal_drive <- drive_get(as_id(instrument_2020_PAR_calibration))
  oddessy2020cal_glist <- drive_ls(oddessy2020cal_drive, pattern = "11620_cal.CSV")
  walk(oddessy2020cal_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
  cal.11620_2020Data <- read.csv("11620_cal.CSV", skip = 8, header = F)
  
  cal.11620_2020Data = cal.11620_2020Data[,2:4]
  colnames(cal.11620_2020Data) = c("Date", "Time", "counts")
  cal.11620_2020Data$datetime_AK = paste(cal.11620_2020Data$Date, cal.11620_2020Data$Time, sep=" ")
  cal.11620_2020Data$datetime_AK = as.POSIXct(cal.11620_2020Data$datetime_AK, format="%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
  cal.11620_2020Data$SN = "SN11620"
  
  
#################  OLD CODE ###########  

# SN11616 = read.csv("Odyssey PAR calibration/2020/11616_cal.CSV",
#                    skip = 8, header = F)
# SN11616 = SN11616[,2:4]
# colnames(SN11616) = c("Date", "Time", "counts")
# SN11616$datetime_AK = paste(SN11616$Date, SN11616$Time, sep=" ")
# SN11616$datetime_AK = as.POSIXct(SN11616$datetime_AK, format="%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
# SN11616$SN = "SN11616"
# 
# SN11617 = read.csv("Odyssey PAR calibration/2020/11617_cal.CSV",
#                    skip = 8, header = F)
# SN11617 = SN11617[,2:4]
# colnames(SN11617) = c("Date", "Time", "counts")
# SN11617$datetime_AK = paste(SN11617$Date, SN11617$Time, sep=" ")
# SN11617$datetime_AK = as.POSIXct(SN11617$datetime_AK, format="%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
# SN11617$SN = "SN11617"
# 
# SN11618 = read.csv("Odyssey PAR calibration/2020/11618_cal.CSV",
#                    skip = 8, header = F)
# SN11618 = SN11618[,2:4]
# colnames(SN11618) = c("Date", "Time", "counts")
# SN11618$datetime_AK = paste(SN11618$Date, SN11618$Time, sep=" ")
# SN11618$datetime_AK = as.POSIXct(SN11618$datetime_AK, format="%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
# SN11618$SN = "SN11618"
# 
# SN11619 = read.csv("Odyssey PAR calibration/2020/11619_cal.CSV",
#                    skip = 8, header = F)
# SN11619 = SN11619[,2:4]
# colnames(SN11619) = c("Date", "Time", "counts")
# SN11619$datetime_AK = paste(SN11619$Date, SN11619$Time, sep=" ")
# SN11619$datetime_AK = as.POSIXct(SN11619$datetime_AK, format="%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
# SN11619$SN = "SN11619"
# 
# SN11620 = read.csv("Odyssey PAR calibration/2020/11620_cal.CSV",
#                    skip = 8, header = F)
# SN11620 = SN11620[,2:4]
# colnames(SN11620) = c("Date", "Time", "counts")
# SN11620$datetime_AK = paste(SN11620$Date, SN11620$Time, sep=" ")
# SN11620$datetime_AK = as.POSIXct(SN11620$datetime_AK, format="%d/%m/%Y %H:%M:%S", tz="America/Anchorage")
# SN11620$SN = "SN11620"

  
  #### (Jacob) Added new Rbind labels 
  
loggers_200530 = rbind(cal.11615_2020Data, cal.11616_2020Data, cal.11617_2020Data, cal.11618_2020Data, 
                       cal.11618_2020Data, cal.11619_2020Data, cal.11620_2020Data)
loggers_200530 = subset(loggers_200530, select=c("datetime_AK", "SN", "counts"))
# cut 1 hr off each end for logger set up/take down
loggers_200530 = loggers_200530[loggers_200530$datetime_AK > as.POSIXct("2020-05-30 20:45:00") &
                                  loggers_200530$datetime_AK < as.POSIXct("2020-05-31 18:15:00"),]

ggplot(loggers_200530, aes(datetime_AK, counts, color=SN)) + geom_line()

#### Join reference and logger data ####

combo_200530 = left_join(loggers_200530, ref_200530, by="datetime_AK")
any(is.na(combo_200530))

# plot
regression=function(combo_200530){
  #setting the regression function. 
  reg_fun<-lm(formula=combo_200530$PAR_Den_Avg~combo_200530$counts) #regression function
  #getting the slope, intercept, R square and adjusted R squared of 
  #the regression function (with 3 decimals).
  slope<-round(coef(reg_fun)[2],3)  
  intercept<-round(coef(reg_fun)[1],3) 
  R2<-round(as.numeric(summary(reg_fun)[8]),3)
  R2.Adj<-round(as.numeric(summary(reg_fun)[9]),3)
  c(slope,intercept,R2,R2.Adj)
}

regressions_data<-ddply(combo_200530,"SN",regression)
colnames(regressions_data)<-c ("SN","slope","intercept","R2","R2.Adj")

qplot(counts, PAR_Den_Avg, data = combo_200530, size=I(2))+
  geom_smooth(method="lm")+
  facet_wrap(SN ~ .)+
  ggtitle("Regressions")+
  geom_label(data=regressions_data, inherit.aes=FALSE, 
             aes(x = 20000, y = 2000,
                 label=paste("slope=",slope,","," ","intercept=",intercept,","," ")))+
  geom_label(data=regressions_data, inherit.aes=FALSE, 
             aes(x = 20000, y = 1500,
                 label=paste("R^2=",R2,","," ","R^2.Adj=",R2.Adj))) +
  ylab("PAR (umol/sec/m2)")
 + geom_label(aes(label=RECORD), hjust = 0, nudge_x = 0.1)

#### save joined reference and logger data ####

final_cal_dat_200530 = subset(combo_200530, select=c("datetime_AK","SN","counts","PAR_Den_Avg"))
names(final_cal_dat_200530) = c("datetime_AK", "SN", "counts", "PAR_umol.sec.m2")

write.csv(final_cal_dat_200530, "Odyssey PAR calibration/2020/final_cal_dat_200530.csv", row.names = F)



############################ 2021 data #########################################

###
# str(all.dates.par.2021.trimmed)
# 
# qplot( DateTime,Calibrated.Value, data = all.dates.par.2021.trimmed) +
#   facet_wrap(~site)+
#   geom_hline()







?read.csv
library(alr4)

library(ggplot2)

install.packages("libridate")
library(lubridate)

#French

frch.2021.url <- "https://drive.google.com/drive/u/2/folders/1nsg6femkXN40ikphPi-otbcqSGD1ijsQ"
frch2021 <- drive_get(as_id(frch.2021.url))
test1frch_glist <- drive_ls(frch2021, pattern = "FRCH_11615_00003_001_EndOfSeason.CSV")
walk(test1frch_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch2021Data <- read.csv("FRCH_11615_00003_001_EndOfSeason.CSV", 
                         skip = 9, header = FALSE)

frch2021Data <- frch2021Data[c(1:14084),]

names(frch2021Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

frch2021Data$DateTime <- as.POSIXct(paste(frch2021Data$Date, frch2021Data$time), format="%d/%m/%Y %H:%M:%S")
frch2021Data$DateTime


frch2021Data$regression.value <- frch2021Data$Calibrated.Value * 0.031

frch.ggplot <- ggplot(data=frch2021Data, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("French PAR Data")

frch.ggplot

#Poker
poke.2021.url <- "https://drive.google.com/drive/u/2/folders/1FBILijNS4ewze9O62ZFFclC86EnB0Iw_"
poke2021 <- drive_get(as_id(poke.2021.url))
test1poke_glist <- drive_ls(poke2021, pattern = "POKE_11619_002_001_EndOfSeason.CSV")
walk(test1poke_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke2021Data <- read.csv("POKE_11619_002_001_EndOfSeason.CSV", 
                         skip = 9, header = FALSE)
names(poke2021Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

poke2021Data$DateTime <- as.POSIXct(paste(poke2021Data$Date, poke2021Data$time), format="%d/%m/%Y %H:%M:%S")
poke2021Data$DateTime


poke2021Data$regression.value <- poke2021Data$Calibrated.Value * 0.035

poke.ggplot <- ggplot(data=poke2021Data, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("Poker PAR Data")

poke.ggplot

#Vault
vaul.2021.url <- "hhttps://drive.google.com/drive/u/2/folders/1AchdGUwd1G0BdQkd3sa82IjnO6Gpj9dG"
vaul2021 <- drive_get(as_id(vaul.2021.url))
test1vaul_glist <- drive_ls(vaul2021, pattern = "VAUL_11616_005_001_EndOfSeason.CSV")
walk(test1vaul_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul2021Data <- read.csv("VAUL_11616_005_001_EndOfSeason.CSV", 
                         skip = 9, header = FALSE)

vaul2021Data <- vaul2021Data[c(1:13436),]
names(vaul2021Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

vaul2021Data$DateTime <- as.POSIXct(paste(vaul2021Data$Date, vaul2021Data$time), format="%d/%m/%Y %H:%M:%S")
vaul2021Data$DateTime

vaul2021Data$regression.value <- vaul2021Data$Calibrated.Value * 0.032

vaul.ggplot <- ggplot(data=vaul2021Data, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("Vault PAR Data")

vaul.ggplot

#Moose
moos.2021.url <- "https://drive.google.com/drive/u/2/folders/1Wy9TfAOtQwI_ZbjDVCiGYSFN0qQb8qAF"
moos2021 <- drive_get(as_id(moos.2021.url))
test1moos_glist <- drive_ls(moos2021, pattern = "MOOS_11617_004_001_EndOfSeason.CSV")
walk(test1moos_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos2021Data <- read.csv("MOOS_11617_004_001_EndOfSeason.CSV", 
                         skip = 9, header = FALSE)


names(moos2021Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")
moos2021Data <- moos2021Data[c(1:14004),]

names(moos2021Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

moos2021Data$regression.value <- moos2021Data$Calibrated.Value * 0.037

moos2021Data$DateTime <- as.POSIXct(paste(moos2021Data$Date, moos2021Data$time), format="%d/%m/%Y %H:%M:%S")
moos2021Data$DateTime

moos.ggplot <- ggplot(data=moos2021Data, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("Moose PAR Data")

moos.ggplot

#Stuart
strt.2021.url <- "https://drive.google.com/drive/u/2/folders/1f2xiiH49WSYcJlQVZrCq_pp5g5dgE330"
strt2021 <- drive_get(as_id(strt.2021.url))
test1strt_glist <- drive_ls(strt2021, pattern = "STRT_11620_001_001_EndOfSeason.CSV")
walk(test1strt_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt2021Data <- read.csv("STRT_11620_001_001_EndOfSeason.CSV", 
                         skip = 9, header = FALSE)



strt2021Data <- strt2021Data[c(1:14006),]
names(strt2021Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

strt2021Data$regression.value <- strt2021Data$Calibrated.Value * 0.036

strt2021Data$DateTime <- as.POSIXct(paste(strt2021Data$Date, strt2021Data$time), format="%d/%m/%Y %H:%M:%S")
strt2021Data$DateTime

strt.ggplot <- ggplot(data=strt2021Data, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "Calibrated PAR sensor values")+
  ggtitle("Stuart PAR Data")

strt.ggplot


#All together 

strt2021Data$site <- "strt"
poke2021Data$site <- "poke"
moos2021Data$site <- "moos"
vaul2021Data$site <- "vaul"
frch2021Data$site <- "frch"


all.dates.par.2021 <- rbind(strt2021Data, poke2021Data, moos2021Data, vaul2021Data, frch2021Data)





ggplot(data=all.dates.par.2021, aes(y=regression.value, x=DateTime)) +
  geom_line() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("All sites PAR")+
  facet_wrap(~site)

write.csv(all.dates.par.2021, file="C:/Users/jacob/OneDrive/Documents/LabLearning/all.dates.par.2021.csv", row.names = FALSE)
getwd()




#--------------------


qplot(DateTime, Calibrated.Value, data = test.frch)+
  geom_abline(slope = 0.031, intercept = 0.237, col = "red")




