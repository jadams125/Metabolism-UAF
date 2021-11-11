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
# oddessy.data.2021.url <- "https://drive.google.com/drive/u/2/folders/1EPjHLDmfbCo5n12AKj7QpN2vXRCPQtg5"
# oddessy2021 <- drive_get(as_id(oddessy.data.2021.url))
# test1_glist <- drive_ls(oddessy2021, pattern = "all.dates.par.2021.csv")
# walk(test1_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# Par2021Data <- read.csv("all.dates.par.2021.csv")


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



### Calibration data
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


#Calibration continued
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



############################ Converting oddessey 2021 data to PAR values from above calibration curve #########################################


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
  ggtitle("All sites PAR 2021")+
  facet_wrap(~site)

write.csv(all.dates.par.2021, file="C:/Users/jacob/OneDrive/Documents/LabLearning/all.dates.par.2021.csv", row.names = FALSE)
getwd()




############ 2020 ############

#French

frch.2020.url <- "https://drive.google.com/drive/u/2/folders/1udve7khOM2EX0l8BZQK3yehb2HUbwFtR"
frch2020 <- drive_get(as_id(frch.2020.url))
test2frch_glist <- drive_ls(frch2020, pattern = "11618_006_002_FRCH_EndOfSeason.CSV")
walk(test2frch_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch2020Data <- read.csv("11618_006_002_FRCH_EndOfSeason.CSV", 
                         skip = 9, header = FALSE)

frch2020Data <- frch2020Data[c(1:12093),]

names(frch2020Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

frch2020Data$DateTime <- as.POSIXct(paste(frch2020Data$Date, frch2020Data$time), format="%d/%m/%Y %H:%M:%S")
frch2020Data$DateTime


frch2020Data$regression.value <- frch2020Data$Calibrated.Value * 0.032

frch2020.ggplot <- ggplot(data=frch2020Data, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("French PAR Data")

frch2020.ggplot

#Poker
poke.2020.url <- "https://drive.google.com/drive/u/2/folders/1udve7khOM2EX0l8BZQK3yehb2HUbwFtR"
poke2020 <- drive_get(as_id(poke.2020.url))
test2poke_glist <- drive_ls(poke2020, pattern = "11619_002_002_POKE_EndOfSeason.CSV")
walk(test2poke_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke2020Data <- read.csv("11619_002_002_POKE_EndOfSeason.CSV", 
                         skip = 9, header = FALSE)

poke2020Data <- poke2020Data[c(1:12681),]

names(poke2020Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

poke2020Data$DateTime <- as.POSIXct(paste(poke2020Data$Date, poke2020Data$time), format="%d/%m/%Y %H:%M:%S")
poke2020Data$DateTime


poke2020Data$regression.value <- poke2020Data$Calibrated.Value * 0.035

poke2020.ggplot <- ggplot(data=poke2020Data, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("Poker PAR Data 2020")

poke2020.ggplot

#Vault
vaul.2020.url <- "https://drive.google.com/drive/u/2/folders/1udve7khOM2EX0l8BZQK3yehb2HUbwFtR"
vaul2020 <- drive_get(as_id(vaul.2020.url))
test2vaul_glist <- drive_ls(vaul2020, pattern = "11616_005_002_VAUL_EndOfSeason.CSV")
walk(test2vaul_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul2020Data <- read.csv("11616_005_002_VAUL_EndOfSeason.CSV", 
                         skip = 9, header = FALSE)


vaul2020Data <- vaul2020Data[c(1:12563),]
names(vaul2020Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

vaul2020Data$DateTime <- as.POSIXct(paste(vaul2020Data$Date, vaul2020Data$time), format="%d/%m/%Y %H:%M:%S")
vaul2020Data$DateTime

vaul2020Data$regression.value <- vaul2020Data$Calibrated.Value * 0.032

vaul2020.ggplot <- ggplot(data=vaul2020Data, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("Vault PAR Data 2020")

vaul2020.ggplot

#Moose
moos.2020.url <- "https://drive.google.com/drive/u/2/folders/1udve7khOM2EX0l8BZQK3yehb2HUbwFtR"
moos2020 <- drive_get(as_id(moos.2020.url))
test2moos_glist <- drive_ls(moos2020, pattern = "11617_004_002_MOOS_EndOfSeason.CSV")
walk(test2moos_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos2020Data <- read.csv("11617_004_002_MOOS_EndOfSeason.CSV", 
                         skip = 9, header = FALSE)



moos2020Data <- moos2020Data[c(1:12205),]

names(moos2020Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

moos2020Data$regression.value <- moos2020Data$Calibrated.Value * 0.037

moos2020Data$DateTime <- as.POSIXct(paste(moos2020Data$Date, moos2020Data$time), format="%d/%m/%Y %H:%M:%S")
moos2020Data$DateTime

moos2020.ggplot <- ggplot(data=moos2020Data, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("Moose PAR Data 2020")

moos2020.ggplot

#Stuart
strt.2020.url <- "https://drive.google.com/drive/u/2/folders/1udve7khOM2EX0l8BZQK3yehb2HUbwFtR"
strt2020 <- drive_get(as_id(strt.2020.url))
test2strt_glist <- drive_ls(strt2020, pattern = "11620_001_002_STRT_EndOfSeason.CSV")
walk(test2strt_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt2020Data <- read.csv("11620_001_002_STRT_EndOfSeason.CSV", 
                         skip = 9, header = FALSE)



strt2020Data <- strt2020Data[c(1:11329),]
names(strt2020Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

strt2020Data$regression.value <- strt2020Data$Calibrated.Value * 0.036

strt2020Data$DateTime <- as.POSIXct(paste(strt2020Data$Date, strt2020Data$time), format="%d/%m/%Y %H:%M:%S")
strt2020Data$DateTime

strt2020.ggplot <- ggplot(data=strt2020Data, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "Calibrated PAR sensor values")+
  ggtitle("Stuart PAR Data 2020")

strt2020.ggplot


#All together 

strt2020Data$site <- "strt"
poke2020Data$site <- "poke"
moos2020Data$site <- "moos"
vaul2020Data$site <- "vaul"
frch2020Data$site <- "frch"


all.dates.par.2020 <- rbind(strt2020Data, poke2020Data, moos2020Data, vaul2020Data, frch2020Data)


ggplot(data=all.dates.par.2020, aes(y=regression.value, x=DateTime)) +
  geom_line() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("All sites PAR 2020")+
  facet_wrap(~site)

write.csv(all.dates.par.2020, file="C:/Users/jacob/OneDrive/Documents/LabLearning/all.dates.par.2021.csv", row.names = FALSE)
getwd()


################################# 2019 ##############################

#French

frch.2019.url <- "https://drive.google.com/drive/u/2/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
frch2019 <- drive_get(as_id(frch.2019.url))
test3frch_glist <- drive_ls(frch2019, pattern = "191010_11615_FRCH.CSV")
walk(test3frch_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch2019Data <- read.csv("191010_11615_FRCH.CSV", 
                         skip = 9, header = FALSE)

frch2019Data <- frch2019Data[c(10:15742),]

names(frch2019Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

frch2019Data$DateTime <- as.POSIXct(paste(frch2019Data$Date, frch2019Data$time), format="%d/%m/%Y %H:%M:%S")
frch2019Data$DateTime


frch2019Data$regression.value <- frch2019Data$Calibrated.Value * 0.031

frch2019.ggplot <- ggplot(data=frch2019Data, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("French PAR Data 2019")

frch2019.ggplot

#Poker
poke.2019.url <- "https://drive.google.com/drive/u/2/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
poke2019 <- drive_get(as_id(poke.2019.url))
test3poke_glist <- drive_ls(poke2019, pattern = "191017_11619_POKE.CSV")
walk(test3poke_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke2019Data <- read.csv("191017_11619_POKE.CSV", 
                         skip = 9, header = FALSE)

poke2019Data <- poke2019Data[c(1:14979),]

names(poke2019Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

poke2019Data$DateTime <- as.POSIXct(paste(poke2019Data$Date, poke2019Data$time), format="%d/%m/%Y %H:%M:%S")
poke2019Data$DateTime


poke2019Data$regression.value <- poke2019Data$Calibrated.Value * 0.035

poke2019.ggplot <- ggplot(data=poke2019Data, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("Poker PAR Data 2019")

poke2019.ggplot

#Vault
vaul.2019.url <- "https://drive.google.com/drive/u/2/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
vaul2019 <- drive_get(as_id(vaul.2019.url))
test3vaul_glist <- drive_ls(vaul2019, pattern = "191017_11616_VAUL.CSV")
walk(test3vaul_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul2019Data <- read.csv("191017_11616_VAUL.CSV", 
                         skip = 9, header = FALSE)


vaul2019Data <- vaul2019Data[c(1:15340),]
names(vaul2019Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

vaul2019Data$DateTime <- as.POSIXct(paste(vaul2019Data$Date, vaul2019Data$time), format="%d/%m/%Y %H:%M:%S")
vaul2019Data$DateTime

vaul2019Data$regression.value <- vaul2019Data$Calibrated.Value * 0.032

vaul2019.ggplot <- ggplot(data=vaul2019Data, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("Vault PAR Data 2019")

vaul2019.ggplot

#Moose
moos.2019.url <- "https://drive.google.com/drive/u/2/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
moos2019 <- drive_get(as_id(moos.2019.url))
test3moos_glist <- drive_ls(moos2019, pattern = "191022_11617_MOOS.CSV")
walk(test3moos_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos2019Data <- read.csv("191022_11617_MOOS.CSV", 
                         skip = 9, header = FALSE)



moos2019Data <- moos2019Data[c(91:16026),]

names(moos2019Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

moos2019Data$regression.value <- moos2019Data$Calibrated.Value * 0.037

moos2019Data$DateTime <- as.POSIXct(paste(moos2019Data$Date, moos2019Data$time), format="%d/%m/%Y %H:%M:%S")
moos2019Data$DateTime

moos2019.ggplot <- ggplot(data=moos2019Data, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("Moose PAR Data 2019")

moos2019.ggplot

#Stuart
### For some reason there are two light files that will need to be merged.

strt.2019.url <- "https://drive.google.com/drive/u/2/folders/11kWuwCF6pTXYolwYnxcsX6OcYyQeF35z"
strt2019 <- drive_get(as_id(strt.2019.url))

test3strt.1_glist <- drive_ls(strt2019, pattern = "190521_11620_STRT.CSV")
walk(test3strt_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt2019Data1 <- read.csv("190521_11620_STRT.CSV", 
                         skip = 9, header = FALSE)

test3strt.2_glist <- drive_ls(strt2019, pattern = "190829_11620_PAR_STRT.CSV")
walk(test3strt.2_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt2019Data2 <- read.csv("190829_11620_PAR_STRT.CSV", 
                         skip = 9, header = FALSE)

test3strt.3_glist <- drive_ls(strt2019, pattern = "191016_11620_PAR_STRT.CSV")
walk(test3strt.3_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt2019Data3 <- read.csv("191016_11620_PAR_STRT.CSV", 
                          skip = 9, header = FALSE)

strt2019Data3 <- strt2019Data3[c(1:2778),]


strt2019Data1$site <- "strt"
strt2019Data2$site <- "strt"
strt2019Data3$site <- "strt"
strt2019DataMerged <- rbind(strt2019Data1, strt2019Data2, strt2019Data3)

names(strt2019DataMerged)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")

strt2019DataMerged$regression.value <- strt2019DataMerged$Calibrated.Value * 0.036

strt2019DataMerged$DateTime <- as.POSIXct(paste(strt2019DataMerged$Date, strt2019DataMerged$time), format="%d/%m/%Y %H:%M:%S")
strt2019DataMerged$DateTime

strt2019.ggplot <- ggplot(data=strt2019DataMerged, aes(y=regression.value, x=DateTime)) +
  geom_point() + 
  labs(x = "Date and Time", y = "Calibrated PAR sensor values")+
  ggtitle("Stuart PAR Data 2019")

strt2019.ggplot


#All together 

strt2019DataMerged$site <- "strt"
poke2019Data$site <- "poke"
moos2019Data$site <- "moos"
vaul2019Data$site <- "vaul"
frch2019Data$site <- "frch"


all.dates.par.2019 <- rbind(strt2019DataMerged, poke2019Data, moos2019Data, vaul2019Data, frch2019Data)


ggplot(data=all.dates.par.2019, aes(y=regression.value, x=DateTime)) +
  geom_line() + 
  labs(x = "Date and Time", y = "PAR (µmol of photons m-2 s-1)")+
  ggtitle("All sites PAR 2019")+
  facet_wrap(~site)








