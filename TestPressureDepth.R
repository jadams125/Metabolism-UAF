

###  Jacob Adams
## Attempt at pressure x depth plots 
library(ggpubr)
install.packages("anytime")
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



################################## 2020 #############

STRTWRdata2020 <- read_sheet("https://docs.google.com/spreadsheets/d/10Y-b9zy-5E-0fHuKgDoXdnWMawjL7kmWiBh61KVpZls/edit#gid=1865448467")
STRTWRdata2020$Date <- ymd(STRTWRdata2020$Date)
STRTWRdata2020$`Depth (cm)` <-  as.numeric(STRTWRdata2020$`Depth (cm)`) 

#Fill NA times and make DateTime
STRTWRdata2020<- STRTWRdata2020 %>%
  fill(Time, .direction = "down")
STRTWRdata2020$DateTime <- as.POSIXct(paste(STRTWRdata2020$Date, STRTWRdata2020$Time), format="%Y-%m-%d %H:%M")
STRTWRdata2020$DateTime <- lubridate::round_date(STRTWRdata2020$DateTime, "15 minutes") 

dateDepthSTRT <- aggregate(`Depth (cm)` ~ DateTime, STRTWRdata2020, median)
dateDepthSTRT$DepthCM <-  dateDepthSTRT[,2]
dateDepthSTRT

str(dateDepthSTRT)



# #Pressure Data
# STRT.2020.PT.url1 <- "https://drive.google.com/drive/u/2/folders/1TWj16hvYu78dTk_aiSKyu2f0yl0KlibI"
# STRT.pt.20.1 <- drive_get(as_id(STRT.2020.PT.url1))
# strt.pt.20.1_glist <- drive_ls(STRT.pt.20.1, pattern = "20075857_STRT_STREAM1.csv")
# walk(strt.pt.20.1_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# strt.pt.Data.1 <- read.csv("20075857_STRT_STREAM1.csv", 
#                            skip = 1, header = TRUE)
# 
# strt.pt.Data.1
# 
# STRT.2020.PT.url2 <- "https://drive.google.com/drive/u/2/folders/1TWj16hvYu78dTk_aiSKyu2f0yl0KlibI"
# STRT.pt.20.2 <- drive_get(as_id(STRT.2020.PT.url2))
# strt.pt.20.2_glist <- drive_ls(STRT.pt.20.2, pattern = "20075856_STRT_STREAM2.csv")
# walk(strt.pt.20.2_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# strt.pt.Data.2 <- read.csv("20075856_STRT_STREAM2.csv", 
#                            skip = 1, header = TRUE)




strt.dod.combined <- read.csv(file=file.path("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/PT_data/2020/STRT/strt.pt.2020.csv"))

# 
# strt.pt.Data.1
# strt.pt.Data.2
# 
# 
# strt.pt.Data.1$DateTime <- mdy_hms(strt.pt.Data.1$Date.Time..GMT.08.00)
# strt.pt.Data.2$DateTime <- mdy_hms(strt.pt.Data.2$Date.Time..GMT.08.00)
# 
# strt.pt.Data.1$DateTime <- as.POSIXct(strt.pt.Data.1$DateTime, format='%I:%M %p')
# strt.pt.Data.2$DateTime <- as.POSIXct(strt.pt.Data.2$DateTime, format='%I:%M %p')




# ## create Date objects using base R
# strt.pt.Data.1$parsed <- strptime(strt.pt.Data.1$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# strt.pt.Data.1$DateTime <- format(strt.pt.Data.1$parsed, "%d-%m-%Y")
# 
# ## create Date objects using base R
# strt.pt.Data.2$parsed <- strptime(strt.pt.Data.2$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# strt.pt.Data.2$DateTime <- format(strt.pt.Data.2$parsed, "%d-%m-%Y")


# 
# 
# strt.pt.Data.2$Pressure2 <- strt.pt.Data.2$Abs.Pres..kPa..LGR.S.N..20075856..SEN.S.N..20075856..LBL..P.
# strt.pt.Data.1$Pressure1 <- strt.pt.Data.1$Abs.Pres..kPa..LGR.S.N..20075857..SEN.S.N..20075857..LBL..P.
# 
# 

# 
# library(data.table)
# 
# Strt2020pt <- plyr::join(strt.pt.Data.1, strt.pt.Data.2, by = c("DateTime"), type = "inner")


#EDIT: Using jakes data code

StrtPt_Depth <- plyr::join(strt.dod.combined, dateDepthSTRT, by = "DateTime")

nams <- colnames(StrtPt_Depth)
colnames(StrtPt_Depth) = make.names(nams, unique=TRUE)
# 
# 
# 
# StrtPt_Depth <- StrtPt_Depth %>% mutate(Avg=rowMeans(cbind(Pressure1, Pressure2), na.rm=T))
# StrtPt_Depth

# str(StrtPt_Depth$meanPressure.Avg)

StrtPt_Depth <- na.omit(StrtPt_Depth)

str(StrtPt_Depth)

StrtPt_Depth$DateTime <- as.POSIXct(StrtPt_Depth$DateTime)

strtPlot <- ggplot(StrtPt_Depth, aes(x=MeanWL, y=DepthCM, color = DateTime))+
  geom_point(size = 3)+
  ggtitle("Strt 2020 avg Water Level and Depth")+
  labs(x = "Average Water Level", y = "Depth (cm)") +
  geom_smooth(method=lm) + stat_poly_eq() 
  
strt.lm <- lm(DepthCM ~ MeanWL, data = StrtPt_Depth)
strt.lm

#Left to do: Clean data for N/A
#Clean data for all other plots online
#read as CSV instead of Sheets 
#add to dataset and Repo with PAR 
#make work with water level isnstead of pressure







#Poker 2020

POKEWRdata2020 <- read_sheet("https://docs.google.com/spreadsheets/d/1y6oDYc6bGpuTf7DawIje4n-XJyFs6yP8q_ofXiJNMo0/edit")
POKEWRdata2020$Date <- ymd(POKEWRdata2020$Date)
POKEWRdata2020$`Depth (cm)` <-  as.numeric(POKEWRdata2020$`Depth`) 

#Fill NA times and make DateTime
POKEWRdata2020<- POKEWRdata2020 %>%
  fill(Time, .direction = "down")
POKEWRdata2020$DateTime <- as.POSIXct(paste(POKEWRdata2020$Date, POKEWRdata2020$Time), format="%Y-%m-%d %H:%M")
POKEWRdata2020$DateTime <- lubridate::round_date(POKEWRdata2020$DateTime, "15 minutes") 

dateDepthPOKE <- aggregate(`Depth (cm)` ~ DateTime, POKEWRdata2020, median)
dateDepthPOKE$DepthCM <-  dateDepthPOKE[,2]
dateDepthPOKE

str(dateDepthPOKE)



# #Pressure Data
# POKE.2020.PT.url1 <- "https://drive.google.com/drive/u/2/folders/1CrINE0O9s7ILAUZkc9jguxHAvvTOD2NR"
# POKE.pt.20.1 <- drive_get(as_id(POKE.2020.PT.url1))
# poke.pt.20.1_glist <- drive_ls(POKE.pt.20.1, pattern = "10766894_POKE_stream_1.csv")
# walk(poke.pt.20.1_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# poke.pt.Data.1 <- read.csv("10766894_POKE_stream_1.csv", 
#                            skip = 1, header = TRUE)
# 
# poke.pt.Data.1
# 
# POKE.2020.PT.url2 <- "https://drive.google.com/drive/u/2/folders/1CrINE0O9s7ILAUZkc9jguxHAvvTOD2NR"
# POKE.pt.20.2 <- drive_get(as_id(POKE.2020.PT.url2))
# poke.pt.20.2_glist <- drive_ls(POKE.pt.20.2, pattern = "20574424_POKE_stream_2.csv")
# walk(poke.pt.20.2_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# poke.pt.Data.2 <- read.csv("20574424_POKE_stream_2.csv", 
#                            skip = 1, header = TRUE)
# 
# poke.pt.Data.1
# poke.pt.Data.2
# 
# 
# poke.pt.Data.1$DateTime <- mdy_hms(poke.pt.Data.1$Date.Time..GMT.08.00)
# poke.pt.Data.2$DateTime <- mdy_hms(poke.pt.Data.2$Date.Time..GMT.08.00)
# 
# poke.pt.Data.1$DateTime <- as.POSIXct(poke.pt.Data.1$DateTime, format='%I:%M %p')
# poke.pt.Data.2$DateTime <- as.POSIXct(poke.pt.Data.2$DateTime, format='%I:%M %p')
# 
# 

poke.dod.combined <- read.csv(file=file.path("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/PT_data/2020/POKE/poke.pt.2020.csv"))


# ## create Date objects using base R
# poke.pt.Data.1$parsed <- strptime(poke.pt.Data.1$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# poke.pt.Data.1$DateTime <- format(poke.pt.Data.1$parsed, "%d-%m-%Y")
# 
# ## create Date objects using base R
# poke.pt.Data.2$parsed <- strptime(poke.pt.Data.2$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# poke.pt.Data.2$DateTime <- format(poke.pt.Data.2$parsed, "%d-%m-%Y")

# poke.pt.Data.2[,3]
# 
# 
# poke.pt.Data.2$Pressure2 <- poke.pt.Data.2[,3]
# poke.pt.Data.1$Pressure1 <- poke.pt.Data.1[,3]





# 
# Poke2020pt <- plyr::join(poke.pt.Data.1, poke.pt.Data.2, by = c("DateTime"), type = "inner")


PokePt_Depth <- plyr::join(poke.dod.combined, dateDepthPOKE, by = "DateTime")

nams <- colnames(PokePt_Depth)
colnames(PokePt_Depth) = make.names(nams, unique=TRUE)



# PokePt_Depth <- PokePt_Depth %>% mutate(Avg=rowMeans(cbind(Pressure1, Pressure2), na.rm=T))
# PokePt_Depth

str(PokePt_Depth$meanPressure.Avg)

str(PokePt_Depth)
PokePt_Depth$DateTime <- as.POSIXct(PokePt_Depth$DateTime)


pokePlot <- ggplot(na.omit(PokePt_Depth), aes(x=MeanWL, y=DepthCM, color = DateTime))+
  geom_point(size = 3)+
  ggtitle("Poke 2020 Water Level and Depth")+
  labs(x = "Average Water Level", y = "Depth (cm)")+
  geom_smooth(method=lm) +
  scale_x_continuous(expand=c(0,0)) +stat_poly_eq()

pokePlot


#VAUL 2020


VAULWRdata2020 <- read_sheet("https://docs.google.com/spreadsheets/d/1uosRGDlMfj10salDwuAAmuhcomtQk8GsClvuVv2Ex9I/edit#gid=618638360")
VAULWRdata2020$Date <- ymd(VAULWRdata2020$Date)
VAULWRdata2020$`Depth (cm)` <-  as.numeric(VAULWRdata2020$`Depth (cm)`) 

#Fill NA times and make DateTime
VAULWRdata2020<- VAULWRdata2020 %>%
  fill(Time, .direction = "down")
VAULWRdata2020$DateTime <- as.POSIXct(paste(VAULWRdata2020$Date, VAULWRdata2020$Time), format="%Y-%m-%d %H:%M")
VAULWRdata2020$DateTime <- lubridate::round_date(VAULWRdata2020$DateTime, "15 minutes") 

dateDepthVAUL <- aggregate(`Depth (cm)` ~ DateTime, VAULWRdata2020, median)
dateDepthVAUL$DepthCM <-  dateDepthVAUL[,2]
dateDepthVAUL

str(dateDepthVAUL)


# 
# #Pressure Data
# VAUL.2020.PT.url1 <- "https://drive.google.com/drive/u/2/folders/1nZzsTzSxZFCWF2GYsH6_HnRdi2iiLofK"
# VAUL.pt.20.1 <- drive_get(as_id(VAUL.2020.PT.url1))
# vaul.pt.20.1_glist <- drive_ls(VAUL.pt.20.1, pattern = "20574422_VAUL_stream_1.csv")
# walk(vaul.pt.20.1_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# vaul.pt.Data.1 <- read.csv("20574422_VAUL_stream_1.csv", 
#                            skip = 1, header = TRUE)
# 
# vaul.pt.Data.1
# 
# VAUL.2020.PT.url2 <- "https://drive.google.com/drive/u/2/folders/1nZzsTzSxZFCWF2GYsH6_HnRdi2iiLofK"
# VAUL.pt.20.2 <- drive_get(as_id(VAUL.2020.PT.url2))
# vaul.pt.20.2_glist <- drive_ls(VAUL.pt.20.2, pattern = "20574420_VAUL_stream_2.csv")
# walk(vaul.pt.20.2_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# vaul.pt.Data.2 <- read.csv("20574420_VAUL_stream_2.csv", 
#                            skip = 1, header = TRUE)
# 
# vaul.pt.Data.1
# vaul.pt.Data.2
# 
# 
# vaul.pt.Data.1$DateTime <- mdy_hms(vaul.pt.Data.1$Date.Time..GMT.08.00)
# vaul.pt.Data.2$DateTime <- mdy_hms(vaul.pt.Data.2$Date.Time..GMT.08.00)
# 
# vaul.pt.Data.1$DateTime <- as.POSIXct(vaul.pt.Data.1$DateTime, format='%I:%M %p')
# vaul.pt.Data.2$DateTime <- as.POSIXct(vaul.pt.Data.2$DateTime, format='%I:%M %p')
# 



# ## create Date objects using base R
# vaul.pt.Data.1$parsed <- strptime(vaul.pt.Data.1$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# vaul.pt.Data.1$DateTime <- format(vaul.pt.Data.1$parsed, "%d-%m-%Y")
# 
# ## create Date objects using base R
# vaul.pt.Data.2$parsed <- strptime(vaul.pt.Data.2$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# vaul.pt.Data.2$DateTime <- format(vaul.pt.Data.2$parsed, "%d-%m-%Y")
# 
# vaul.pt.Data.2[,3]
# 
# 
# vaul.pt.Data.2$Pressure2 <- vaul.pt.Data.2[,3]
# vaul.pt.Data.1$Pressure1 <- vaul.pt.Data.1[,3]
# 




# 
# Vaul2020pt <- plyr::join(vaul.pt.Data.1, vaul.pt.Data.2, by = c("DateTime"), type = "inner")
vaul.dod.combined <- read.csv(file=file.path("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/PT_data/2020/VAUL/vaul.pt.2020.csv"))


VaulPt_Depth <- plyr::join(vaul.dod.combined, dateDepthVAUL, by = "DateTime")

nams <- colnames(VaulPt_Depth)
colnames(VaulPt_Depth) = make.names(nams, unique=TRUE)


# VaulPt_Depth <- VaulPt_Depth %>% mutate(Avg=rowMeans(cbind(Pressure1, Pressure2), na.rm=T))
# VaulPt_Depth

str(VaulPt_Depth$meanPressure.Avg)

VaulPt_Depth$DateTime <- as.POSIXct(VaulPt_Depth$DateTime)

vaulPlot <- ggplot(na.omit(VaulPt_Depth), aes(x=MeanWL, y=DepthCM, color = DateTime))+
  geom_point(size = 3)+
  ggtitle("Vaul 2020 avg Water Level and Depth")+
  labs(x = "Average Water Level", y = "Depth (cm)")+
  geom_smooth(method=lm) + stat_poly_eq()
  

vaulPlot



#MOOS 2020

MOOSWRdata2020 <- read_sheet("https://docs.google.com/spreadsheets/d/1w81pcBt9sT8TjRrQtBK7MW_rAMsJBDOH36AlWadNlw4/edit#gid=1807794565")
MOOSWRdata2020$Date <- ymd(MOOSWRdata2020$Date)
MOOSWRdata2020$`Depth (cm)` <-  as.numeric(MOOSWRdata2020$`Depth (cm)`) 

#Fill NA times and make DateTime
MOOSWRdata2020<- MOOSWRdata2020 %>%
  fill(Time, .direction = "down")
MOOSWRdata2020$DateTime <- as.POSIXct(paste(MOOSWRdata2020$Date, MOOSWRdata2020$Time), format="%Y-%m-%d %H:%M")
MOOSWRdata2020$DateTime <- lubridate::round_date(MOOSWRdata2020$DateTime, "15 minutes") 

dateDepthMOOS <- aggregate(`Depth (cm)` ~ DateTime, MOOSWRdata2020, median)
dateDepthMOOS$DepthCM <-  dateDepthMOOS[,2]
dateDepthMOOS

str(dateDepthMOOS)


# 
# #Pressure Data
# MOOS.2020.PT.url1 <- "https://drive.google.com/drive/u/2/folders/10iEEQn5LhX3sxD2rcwozzAjuxmSuWKuh"
# MOOS.pt.20.1 <- drive_get(as_id(MOOS.2020.PT.url1))
# moos.pt.20.1_glist <- drive_ls(MOOS.pt.20.1, pattern = "10710340_MOOS_stream_1.csv")
# walk(moos.pt.20.1_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# moos.pt.Data.1 <- read.csv("10710340_MOOS_stream_1.csv", 
#                            skip = 1, header = TRUE)
# 
# moos.pt.Data.1
# 
# MOOS.2020.PT.url2 <- "https://drive.google.com/drive/u/2/folders/10iEEQn5LhX3sxD2rcwozzAjuxmSuWKuh"
# MOOS.pt.20.2 <- drive_get(as_id(MOOS.2020.PT.url2))
# moos.pt.20.2_glist <- drive_ls(MOOS.pt.20.2, pattern = "20452210_MOOS_Stream_2.csv")
# walk(moos.pt.20.2_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# moos.pt.Data.2 <- read.csv("20452210_MOOS_Stream_2.csv", 
#                            skip = 1, header = TRUE)
# 
# moos.pt.Data.1
# moos.pt.Data.2
# 
# 
# moos.pt.Data.1$DateTime <- mdy_hms(moos.pt.Data.1$Date.Time..GMT.08.00)
# moos.pt.Data.2$DateTime <- mdy_hms(moos.pt.Data.2$Date.Time..GMT.08.00)
# 
# moos.pt.Data.1$DateTime <- as.POSIXct(moos.pt.Data.1$DateTime, format='%I:%M %p')
# moos.pt.Data.2$DateTime <- as.POSIXct(moos.pt.Data.2$DateTime, format='%I:%M %p')
# 
# 
# 
# 
# # ## create Date objects using base R
# # moos.pt.Data.1$parsed <- strptime(moos.pt.Data.1$DateTime, "%d/%m/%Y")
# # 
# # ## format them to spec
# # moos.pt.Data.1$DateTime <- format(moos.pt.Data.1$parsed, "%d-%m-%Y")
# # 
# # ## create Date objects using base R
# # moos.pt.Data.2$parsed <- strptime(moos.pt.Data.2$DateTime, "%d/%m/%Y")
# # 
# # ## format them to spec
# # moos.pt.Data.2$DateTime <- format(moos.pt.Data.2$parsed, "%d-%m-%Y")
# 
# moos.pt.Data.2[,3]
# 
# 
# moos.pt.Data.2$Pressure2 <- moos.pt.Data.2[,3]
# moos.pt.Data.1$Pressure1 <- moos.pt.Data.1[,3]



moos.dod.combined <- read.csv(file=file.path("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/PT_data/2020/MOOS/moos.pt.2020.csv"))


MoosPt_Depth <- plyr::join(moos.dod.combined, dateDepthMOOS, by = "DateTime")

nams <- colnames(MoosPt_Depth)
colnames(MoosPt_Depth) = make.names(nams, unique=TRUE)


# VaulPt_Depth <- VaulPt_Depth %>% mutate(Avg=rowMeans(cbind(Pressure1, Pressure2), na.rm=T))
# VaulPt_Depth

str(MoosPt_Depth$meanPressure.Avg)

MoosPt_Depth$DateTime <- as.POSIXct(MoosPt_Depth$DateTime)




moosPlot <- ggplot(na.omit(MoosPt_Depth), aes(x=MeanWL, y=DepthCM, color = DateTime))+
  geom_point(size = 3)+
  ggtitle("Moos 2020 avg Water Level and Depth")+
  labs(x = "Average Water Level", y = "Depth (cm)")+
  geom_smooth(method=lm) + stat_poly_eq()

moosPlot

# FRCH 2020

FRCHWRdata2020 <- read_sheet("https://docs.google.com/spreadsheets/d/1Fa4rjJWo3uzIrcsrOqBO2pwmyK5icKRZt-hKVTA5Lrk/edit#gid=1920619921")
FRCHWRdata2020$Date <- ymd(FRCHWRdata2020$Date)
FRCHWRdata2020$`Depth (cm)` <-  as.numeric(FRCHWRdata2020$`Depth (cm)`) 

#Fill NA times and make DateTime
FRCHWRdata2020<- FRCHWRdata2020 %>%
  fill(Time, .direction = "down")
FRCHWRdata2020$DateTime <- as.POSIXct(paste(FRCHWRdata2020$Date, FRCHWRdata2020$Time), format="%Y-%m-%d %H:%M")
FRCHWRdata2020$DateTime <- lubridate::round_date(FRCHWRdata2020$DateTime, "15 minutes") 

dateDepthFRCH <- aggregate(`Depth (cm)` ~ DateTime, FRCHWRdata2020, median)
dateDepthFRCH$DepthCM <-  dateDepthFRCH[,2]
dateDepthFRCH

str(dateDepthFRCH)

# 
# 
# #Pressure Data
# FRCH.2020.PT.url1 <- "https://drive.google.com/drive/u/2/folders/1-xZLou63OJTvw23xhWPO26x8VcZuv-xK"
# FRCH.pt.20.1 <- drive_get(as_id(FRCH.2020.PT.url1))
# frch.pt.20.1_glist <- drive_ls(FRCH.pt.20.1, pattern = "20005935_FRCH_stream_1.csv")
# walk(frch.pt.20.1_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# frch.pt.Data.1 <- read.csv("20005935_FRCH_stream_1.csv", 
#                            skip = 1, header = TRUE)
# 
# frch.pt.Data.1
# 
# FRCH.2020.PT.url2 <- "https://drive.google.com/drive/u/2/folders/1-xZLou63OJTvw23xhWPO26x8VcZuv-xK"
# FRCH.pt.20.2 <- drive_get(as_id(FRCH.2020.PT.url2))
# frch.pt.20.2_glist <- drive_ls(FRCH.pt.20.2, pattern = "10710335_FRCH_stream_2.csv")
# walk(frch.pt.20.2_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# frch.pt.Data.2 <- read.csv("10710335_FRCH_stream_2.csv", 
#                            skip = 1, header = TRUE)
# 
# frch.pt.Data.1
# frch.pt.Data.2
# 
# 
# frch.pt.Data.1$DateTime <- mdy_hms(frch.pt.Data.1$Date.Time..GMT.08.00)
# frch.pt.Data.2$DateTime <- mdy_hms(frch.pt.Data.2$Date.Time..GMT.08.00)
# 
# frch.pt.Data.1$DateTime <- as.POSIXct(frch.pt.Data.1$DateTime, format='%I:%M %p')
# frch.pt.Data.2$DateTime <- as.POSIXct(frch.pt.Data.2$DateTime, format='%I:%M %p')
# 
# 
# 
# 
# # ## create Date objects using base R
# # frch.pt.Data.1$parsed <- strptime(frch.pt.Data.1$DateTime, "%d/%m/%Y")
# # 
# # ## format them to spec
# # frch.pt.Data.1$DateTime <- format(frch.pt.Data.1$parsed, "%d-%m-%Y")
# # 
# # ## create Date objects using base R
# # frch.pt.Data.2$parsed <- strptime(frch.pt.Data.2$DateTime, "%d/%m/%Y")
# # 
# # ## format them to spec
# # frch.pt.Data.2$DateTime <- format(frch.pt.Data.2$parsed, "%d-%m-%Y")
# 
# frch.pt.Data.2[,3]
# 
# 
# frch.pt.Data.2$Pressure2 <- frch.pt.Data.2[,3]
# frch.pt.Data.1$Pressure1 <- frch.pt.Data.1[,3]
# 
# 
# 
# 
# 
# 
# Frch2020pt <- plyr::join(frch.pt.Data.1, frch.pt.Data.2, by = c("DateTime"), type = "inner")
frch.dod.combined <- read.csv(file=file.path("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/PT_data/2020/FRCH/frch.pt.2020.csv"))

FrchPt_Depth <- plyr::join(frch.dod.combined, dateDepthFRCH, by = "DateTime")


nams <- colnames(FrchPt_Depth)
colnames(FrchPt_Depth) = make.names(nams, unique=TRUE)

# FrchPt_Depth <- FrchPt_Depth %>% mutate(Avg=rowMeans(cbind(Pressure1, Pressure2), na.rm=T))
# FrchPt_Depth

str(FrchPt_Depth$meanPressure.Avg)


FrchPt_Depth$DateTime <- as.POSIXct(FrchPt_Depth$DateTime)

FrchPt_Depth_Filtered <- na.omit(FrchPt_Depth)[-2,]



frchPlot <- ggplot(na.omit(FrchPt_Depth_Filtered), aes(x=MeanWL, y=DepthCM, color = DateTime))+
  geom_point(size = 3)+
  ggtitle("Frch 2020 avg Water Level and Depth")+
  labs(x = "Average Water Level", y = "Depth (cm)")+
  geom_smooth(method=lm) + stat_poly_eq()

frchPlot

#All Together


FinalPlot <- ggarrange(strtPlot, pokePlot, moosPlot, vaulPlot, frchPlot,
          ncol = 3, nrow = 2, main = "Average Water Level and Depth")

FinalPlot


############################################ 2019 ######################################

?googlesheets4

STRTWRdata2019 <- read_sheet("https://docs.google.com/spreadsheets/d/1CCTMaCPFNaoQh2aew8h7vAjuc9SL6G5xx1kiE9Xy8yY/edit#gid=2101896361")
STRTWRdata2019$Date <- ymd(STRTWRdata2019$Date...1)
STRTWRdata2019$`Depth (cm)` <-  as.numeric(STRTWRdata2019$`Depth (cm)`) 

#Fill NA times and make DateTime
STRTWRdata2019<- STRTWRdata2019 %>%
  fill(Time, .direction = "down")

STRTWRdata2019<- STRTWRdata2019 %>%
  fill(Date, .direction = "down")

STRTWRdata2019$DateTime <- as.POSIXct(paste(STRTWRdata2019$Date, STRTWRdata2019$Time), format="%Y-%m-%d %H:%M")


dateDepthSTRT <- aggregate(`Depth (cm)` ~ DateTime, STRTWRdata2019, median)
dateDepthSTRT$DepthCM <-  dateDepthSTRT[,2]
dateDepthSTRT

str(dateDepthSTRT)



#Pressure Data
STRT.2019.PT.url1 <- "https://drive.google.com/drive/u/2/folders/1TWj16hvYu78dTk_aiSKyu2f0yl0KlibI"
STRT.pt.19.1 <- drive_get(as_id(STRT.2019.PT.url1))
strt.pt.19.1_glist <- drive_ls(STRT.pt.19.1, pattern = "20075857_STRT_STREAM1.csv")
walk(strt.pt.19.1_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt.pt.Data.1 <- read.csv("20075857_STRT_STREAM1.csv", 
                           skip = 1, header = TRUE)

strt.pt.Data.1

STRT.2019.PT.url2 <- "https://drive.google.com/drive/u/2/folders/1TWj16hvYu78dTk_aiSKyu2f0yl0KlibI"
STRT.pt.19.2 <- drive_get(as_id(STRT.2019.PT.url2))
strt.pt.19.2_glist <- drive_ls(STRT.pt.19.2, pattern = "20075856_STRT_STREAM2.csv")
walk(strt.pt.19.2_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt.pt.Data.2 <- read.csv("20075856_STRT_STREAM2.csv", 
                           skip = 1, header = TRUE)

strt.pt.Data.1
strt.pt.Data.2


strt.pt.Data.1$DateTime <- mdy_hms(strt.pt.Data.1$Date.Time..GMT.08.00)
strt.pt.Data.2$DateTime <- mdy_hms(strt.pt.Data.2$Date.Time..GMT.08.00)

strt.pt.Data.1$DateTime <- as.POSIXct(strt.pt.Data.1$DateTime, format='%I:%M %p')
strt.pt.Data.2$DateTime <- as.POSIXct(strt.pt.Data.2$DateTime, format='%I:%M %p')




# ## create Date objects using base R
# strt.pt.Data.1$parsed <- strptime(strt.pt.Data.1$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# strt.pt.Data.1$DateTime <- format(strt.pt.Data.1$parsed, "%d-%m-%Y")
# 
# ## create Date objects using base R
# strt.pt.Data.2$parsed <- strptime(strt.pt.Data.2$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# strt.pt.Data.2$DateTime <- format(strt.pt.Data.2$parsed, "%d-%m-%Y")




strt.pt.Data.2$Pressure2 <- strt.pt.Data.2$Abs.Pres..kPa..LGR.S.N..20075856..SEN.S.N..20075856..LBL..P.
strt.pt.Data.1$Pressure1 <- strt.pt.Data.1$Abs.Pres..kPa..LGR.S.N..20075857..SEN.S.N..20075857..LBL..P.




library(data.table)

Strt2019pt <- plyr::join(strt.pt.Data.1, strt.pt.Data.2, by = c("DateTime"), type = "inner")


StrtPt_Depth <- plyr::join(Strt2019pt, dateDepthSTRT, by = "DateTime")

nams <- colnames(StrtPt_Depth)
colnames(StrtPt_Depth) = make.names(nams, unique=TRUE)



StrtPt_Depth <- StrtPt_Depth %>% mutate(Avg=rowMeans(cbind(Pressure1, Pressure2), na.rm=T))
StrtPt_Depth

str(StrtPt_Depth$meanPressure.Avg)


strtPlot <- ggplot(na.omit(StrtPt_Depth), aes(x=Avg, y=DepthCM, color = DateTime))+
  geom_point(size = 3)+
  ggtitle("Strt 2019 avg Water Level and Depth")+
  labs(x = "Average Water Level", y = "Depth (cm)")+
  geom_smooth(method=lm)





#Left to do: Clean data for N/A
#Clean data for all other plots online
#read as CSV instead of Sheets 
#add to dataset and Repo with PAR 
#make work with water level isnstead of pressure







#Poker 2019

POKEWRdata2019 <- read_sheet("https://docs.google.com/spreadsheets/d/1y6oDYc6bGpuTf7DawIje4n-XJyFs6yP8q_ofXiJNMo0/edit")
POKEWRdata2019$Date <- ymd(POKEWRdata2019$Date)
POKEWRdata2019$`Depth (cm)` <-  as.numeric(POKEWRdata2019$`Depth`) 

#Fill NA times and make DateTime
POKEWRdata2019<- POKEWRdata2019 %>%
  fill(Time, .direction = "down")
POKEWRdata2019$DateTime <- as.POSIXct(paste(POKEWRdata2019$Date, POKEWRdata2019$Time), format="%Y-%m-%d %H:%M")


dateDepthPOKE <- aggregate(`Depth (cm)` ~ DateTime, POKEWRdata2019, median)
dateDepthPOKE$DepthCM <-  dateDepthPOKE[,2]
dateDepthPOKE

str(dateDepthPOKE)



#Pressure Data
POKE.2019.PT.url1 <- "https://drive.google.com/drive/u/2/folders/1CrINE0O9s7ILAUZkc9jguxHAvvTOD2NR"
POKE.pt.19.1 <- drive_get(as_id(POKE.2019.PT.url1))
poke.pt.19.1_glist <- drive_ls(POKE.pt.19.1, pattern = "10766894_POKE_stream_1.csv")
walk(poke.pt.19.1_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke.pt.Data.1 <- read.csv("10766894_POKE_stream_1.csv", 
                           skip = 1, header = TRUE)

poke.pt.Data.1

POKE.2019.PT.url2 <- "https://drive.google.com/drive/u/2/folders/1CrINE0O9s7ILAUZkc9jguxHAvvTOD2NR"
POKE.pt.19.2 <- drive_get(as_id(POKE.2019.PT.url2))
poke.pt.19.2_glist <- drive_ls(POKE.pt.19.2, pattern = "20574424_POKE_stream_2.csv")
walk(poke.pt.19.2_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke.pt.Data.2 <- read.csv("20574424_POKE_stream_2.csv", 
                           skip = 1, header = TRUE)

poke.pt.Data.1
poke.pt.Data.2


poke.pt.Data.1$DateTime <- mdy_hms(poke.pt.Data.1$Date.Time..GMT.08.00)
poke.pt.Data.2$DateTime <- mdy_hms(poke.pt.Data.2$Date.Time..GMT.08.00)

poke.pt.Data.1$DateTime <- as.POSIXct(poke.pt.Data.1$DateTime, format='%I:%M %p')
poke.pt.Data.2$DateTime <- as.POSIXct(poke.pt.Data.2$DateTime, format='%I:%M %p')




# ## create Date objects using base R
# poke.pt.Data.1$parsed <- strptime(poke.pt.Data.1$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# poke.pt.Data.1$DateTime <- format(poke.pt.Data.1$parsed, "%d-%m-%Y")
# 
# ## create Date objects using base R
# poke.pt.Data.2$parsed <- strptime(poke.pt.Data.2$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# poke.pt.Data.2$DateTime <- format(poke.pt.Data.2$parsed, "%d-%m-%Y")

poke.pt.Data.2[,3]


poke.pt.Data.2$Pressure2 <- poke.pt.Data.2[,3]
poke.pt.Data.1$Pressure1 <- poke.pt.Data.1[,3]






Poke2019pt <- plyr::join(poke.pt.Data.1, poke.pt.Data.2, by = c("DateTime"), type = "inner")


PokePt_Depth <- plyr::join(Poke2019pt, dateDepthPOKE, by = "DateTime")

nams <- colnames(PokePt_Depth)
colnames(PokePt_Depth) = make.names(nams, unique=TRUE)



PokePt_Depth <- PokePt_Depth %>% mutate(Avg=rowMeans(cbind(Pressure1, Pressure2), na.rm=T))
PokePt_Depth

str(PokePt_Depth$meanPressure.Avg)


pokePlot <- ggplot(na.omit(PokePt_Depth), aes(x=Avg, y=DepthCM, color = DateTime))+
  geom_point(size = 3)+
  ggtitle("Poke 2019 Water Level and Depth")+
  labs(x = "Average Water Level", y = "Depth (cm)")+
  geom_smooth(method=lm) +
  scale_x_continuous(expand=c(0,0))



#VAUL 2019


VAULWRdata2019 <- read_sheet("https://docs.google.com/spreadsheets/d/1uosRGDlMfj10salDwuAAmuhcomtQk8GsClvuVv2Ex9I/edit#gid=618638360")
VAULWRdata2019$Date <- ymd(VAULWRdata2019$Date)
VAULWRdata2019$`Depth (cm)` <-  as.numeric(VAULWRdata2019$`Depth (cm)`) 

#Fill NA times and make DateTime
VAULWRdata2019<- VAULWRdata2019 %>%
  fill(Time, .direction = "down")
VAULWRdata2019$DateTime <- as.POSIXct(paste(VAULWRdata2019$Date, VAULWRdata2019$Time), format="%Y-%m-%d %H:%M")


dateDepthVAUL <- aggregate(`Depth (cm)` ~ DateTime, VAULWRdata2019, median)
dateDepthVAUL$DepthCM <-  dateDepthVAUL[,2]
dateDepthVAUL

str(dateDepthVAUL)



#Pressure Data
VAUL.2019.PT.url1 <- "https://drive.google.com/drive/u/2/folders/1nZzsTzSxZFCWF2GYsH6_HnRdi2iiLofK"
VAUL.pt.19.1 <- drive_get(as_id(VAUL.2019.PT.url1))
vaul.pt.19.1_glist <- drive_ls(VAUL.pt.19.1, pattern = "20574422_VAUL_stream_1.csv")
walk(vaul.pt.19.1_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul.pt.Data.1 <- read.csv("20574422_VAUL_stream_1.csv", 
                           skip = 1, header = TRUE)

vaul.pt.Data.1

VAUL.2019.PT.url2 <- "https://drive.google.com/drive/u/2/folders/1nZzsTzSxZFCWF2GYsH6_HnRdi2iiLofK"
VAUL.pt.19.2 <- drive_get(as_id(VAUL.2019.PT.url2))
vaul.pt.19.2_glist <- drive_ls(VAUL.pt.19.2, pattern = "20574420_VAUL_stream_2.csv")
walk(vaul.pt.19.2_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul.pt.Data.2 <- read.csv("20574420_VAUL_stream_2.csv", 
                           skip = 1, header = TRUE)

vaul.pt.Data.1
vaul.pt.Data.2


vaul.pt.Data.1$DateTime <- mdy_hms(vaul.pt.Data.1$Date.Time..GMT.08.00)
vaul.pt.Data.2$DateTime <- mdy_hms(vaul.pt.Data.2$Date.Time..GMT.08.00)

vaul.pt.Data.1$DateTime <- as.POSIXct(vaul.pt.Data.1$DateTime, format='%I:%M %p')
vaul.pt.Data.2$DateTime <- as.POSIXct(vaul.pt.Data.2$DateTime, format='%I:%M %p')




# ## create Date objects using base R
# vaul.pt.Data.1$parsed <- strptime(vaul.pt.Data.1$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# vaul.pt.Data.1$DateTime <- format(vaul.pt.Data.1$parsed, "%d-%m-%Y")
# 
# ## create Date objects using base R
# vaul.pt.Data.2$parsed <- strptime(vaul.pt.Data.2$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# vaul.pt.Data.2$DateTime <- format(vaul.pt.Data.2$parsed, "%d-%m-%Y")

vaul.pt.Data.2[,3]


vaul.pt.Data.2$Pressure2 <- vaul.pt.Data.2[,3]
vaul.pt.Data.1$Pressure1 <- vaul.pt.Data.1[,3]






Vaul2019pt <- plyr::join(vaul.pt.Data.1, vaul.pt.Data.2, by = c("DateTime"), type = "inner")


VaulPt_Depth <- plyr::join(Vaul2019pt, dateDepthVAUL, by = "DateTime")

nams <- colnames(VaulPt_Depth)
colnames(VaulPt_Depth) = make.names(nams, unique=TRUE)


VaulPt_Depth <- VaulPt_Depth %>% mutate(Avg=rowMeans(cbind(Pressure1, Pressure2), na.rm=T))
VaulPt_Depth

str(VaulPt_Depth$meanPressure.Avg)


vaulPlot <- ggplot(na.omit(VaulPt_Depth), aes(x=Avg, y=DepthCM, color = DateTime))+
  geom_point(size = 3)+
  ggtitle("Vaul 2019 avg Water Level and Depth")+
  labs(x = "Average Water Level", y = "Depth (cm)")+
  geom_smooth(method=lm) 






#MOOS 2019

MOOSWRdata2019 <- read_sheet("https://docs.google.com/spreadsheets/d/1w81pcBt9sT8TjRrQtBK7MW_rAMsJBDOH36AlWadNlw4/edit#gid=1807794565")
MOOSWRdata2019$Date <- ymd(MOOSWRdata2019$Date)
MOOSWRdata2019$`Depth (cm)` <-  as.numeric(MOOSWRdata2019$`Depth (cm)`) 

#Fill NA times and make DateTime
MOOSWRdata2019<- MOOSWRdata2019 %>%
  fill(Time, .direction = "down")
MOOSWRdata2019$DateTime <- as.POSIXct(paste(MOOSWRdata2019$Date, MOOSWRdata2019$Time), format="%Y-%m-%d %H:%M")


dateDepthMOOS <- aggregate(`Depth (cm)` ~ DateTime, MOOSWRdata2019, median)
dateDepthMOOS$DepthCM <-  dateDepthMOOS[,2]
dateDepthMOOS

str(dateDepthMOOS)



#Pressure Data
MOOS.2019.PT.url1 <- "https://drive.google.com/drive/u/2/folders/10iEEQn5LhX3sxD2rcwozzAjuxmSuWKuh"
MOOS.pt.19.1 <- drive_get(as_id(MOOS.2019.PT.url1))
moos.pt.19.1_glist <- drive_ls(MOOS.pt.19.1, pattern = "10710340_MOOS_stream_1.csv")
walk(moos.pt.19.1_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos.pt.Data.1 <- read.csv("10710340_MOOS_stream_1.csv", 
                           skip = 1, header = TRUE)

moos.pt.Data.1

MOOS.2019.PT.url2 <- "https://drive.google.com/drive/u/2/folders/10iEEQn5LhX3sxD2rcwozzAjuxmSuWKuh"
MOOS.pt.19.2 <- drive_get(as_id(MOOS.2019.PT.url2))
moos.pt.19.2_glist <- drive_ls(MOOS.pt.19.2, pattern = "20452210_MOOS_Stream_2.csv")
walk(moos.pt.19.2_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos.pt.Data.2 <- read.csv("20452210_MOOS_Stream_2.csv", 
                           skip = 1, header = TRUE)

moos.pt.Data.1
moos.pt.Data.2


moos.pt.Data.1$DateTime <- mdy_hms(moos.pt.Data.1$Date.Time..GMT.08.00)
moos.pt.Data.2$DateTime <- mdy_hms(moos.pt.Data.2$Date.Time..GMT.08.00)

moos.pt.Data.1$DateTime <- as.POSIXct(moos.pt.Data.1$DateTime, format='%I:%M %p')
moos.pt.Data.2$DateTime <- as.POSIXct(moos.pt.Data.2$DateTime, format='%I:%M %p')




# ## create Date objects using base R
# moos.pt.Data.1$parsed <- strptime(moos.pt.Data.1$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# moos.pt.Data.1$DateTime <- format(moos.pt.Data.1$parsed, "%d-%m-%Y")
# 
# ## create Date objects using base R
# moos.pt.Data.2$parsed <- strptime(moos.pt.Data.2$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# moos.pt.Data.2$DateTime <- format(moos.pt.Data.2$parsed, "%d-%m-%Y")

moos.pt.Data.2[,3]


moos.pt.Data.2$Pressure2 <- moos.pt.Data.2[,3]
moos.pt.Data.1$Pressure1 <- moos.pt.Data.1[,3]






Moos2019pt <- plyr::join(moos.pt.Data.1, moos.pt.Data.2, by = c("DateTime"), type = "inner")


MoosPt_Depth <- plyr::join(Moos2019pt, dateDepthMOOS, by = "DateTime")

nams <- colnames(MoosPt_Depth)
colnames(MoosPt_Depth) = make.names(nams, unique=TRUE)

MoosPt_Depth <- MoosPt_Depth %>% mutate(Avg=rowMeans(cbind(Pressure1, Pressure2), na.rm=T))
MoosPt_Depth

str(MoosPt_Depth$meanPressure.Avg)


moosPlot <- ggplot(na.omit(MoosPt_Depth), aes(x=Avg, y=DepthCM, color = DateTime))+
  geom_point(size = 3)+
  ggtitle("Moos 2019 avg Water Level and Depth")+
  labs(x = "Average Water Level", y = "Depth (cm)")+
  geom_smooth(method=lm) 


# FRCH 2019

FRCHWRdata2019 <- read_sheet("https://docs.google.com/spreadsheets/d/1Fa4rjJWo3uzIrcsrOqBO2pwmyK5icKRZt-hKVTA5Lrk/edit#gid=1920619921")
FRCHWRdata2019$Date <- ymd(FRCHWRdata2019$Date)
FRCHWRdata2019$`Depth (cm)` <-  as.numeric(FRCHWRdata2019$`Depth (cm)`) 

#Fill NA times and make DateTime
FRCHWRdata2019<- FRCHWRdata2019 %>%
  fill(Time, .direction = "down")
FRCHWRdata2019$DateTime <- as.POSIXct(paste(FRCHWRdata2019$Date, FRCHWRdata2019$Time), format="%Y-%m-%d %H:%M")


dateDepthFRCH <- aggregate(`Depth (cm)` ~ DateTime, FRCHWRdata2019, median)
dateDepthFRCH$DepthCM <-  dateDepthFRCH[,2]
dateDepthFRCH

str(dateDepthFRCH)



#Pressure Data
FRCH.2019.PT.url1 <- "https://drive.google.com/drive/u/2/folders/1-xZLou63OJTvw23xhWPO26x8VcZuv-xK"
FRCH.pt.19.1 <- drive_get(as_id(FRCH.2019.PT.url1))
frch.pt.19.1_glist <- drive_ls(FRCH.pt.19.1, pattern = "20005935_FRCH_stream_1.csv")
walk(frch.pt.19.1_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch.pt.Data.1 <- read.csv("20005935_FRCH_stream_1.csv", 
                           skip = 1, header = TRUE)

frch.pt.Data.1

FRCH.2019.PT.url2 <- "https://drive.google.com/drive/u/2/folders/1-xZLou63OJTvw23xhWPO26x8VcZuv-xK"
FRCH.pt.19.2 <- drive_get(as_id(FRCH.2019.PT.url2))
frch.pt.19.2_glist <- drive_ls(FRCH.pt.19.2, pattern = "10710335_FRCH_stream_2.csv")
walk(frch.pt.19.2_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch.pt.Data.2 <- read.csv("10710335_FRCH_stream_2.csv", 
                           skip = 1, header = TRUE)

frch.pt.Data.1
frch.pt.Data.2


frch.pt.Data.1$DateTime <- mdy_hms(frch.pt.Data.1$Date.Time..GMT.08.00)
frch.pt.Data.2$DateTime <- mdy_hms(frch.pt.Data.2$Date.Time..GMT.08.00)

frch.pt.Data.1$DateTime <- as.POSIXct(frch.pt.Data.1$DateTime, format='%I:%M %p')
frch.pt.Data.2$DateTime <- as.POSIXct(frch.pt.Data.2$DateTime, format='%I:%M %p')




# ## create Date objects using base R
# frch.pt.Data.1$parsed <- strptime(frch.pt.Data.1$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# frch.pt.Data.1$DateTime <- format(frch.pt.Data.1$parsed, "%d-%m-%Y")
# 
# ## create Date objects using base R
# frch.pt.Data.2$parsed <- strptime(frch.pt.Data.2$DateTime, "%d/%m/%Y")
# 
# ## format them to spec
# frch.pt.Data.2$DateTime <- format(frch.pt.Data.2$parsed, "%d-%m-%Y")

frch.pt.Data.2[,3]


frch.pt.Data.2$Pressure2 <- frch.pt.Data.2[,3]
frch.pt.Data.1$Pressure1 <- frch.pt.Data.1[,3]






Frch2019pt <- plyr::join(frch.pt.Data.1, frch.pt.Data.2, by = c("DateTime"), type = "inner")


FrchPt_Depth <- plyr::join(Frch2019pt, dateDepthFRCH, by = "DateTime")


nams <- colnames(FrchPt_Depth)
colnames(FrchPt_Depth) = make.names(nams, unique=TRUE)

FrchPt_Depth <- FrchPt_Depth %>% mutate(Avg=rowMeans(cbind(Pressure1, Pressure2), na.rm=T))
FrchPt_Depth

str(FrchPt_Depth$meanPressure.Avg)


frchPlot <- ggplot(na.omit(FrchPt_Depth), aes(x=Avg, y=DepthCM, color = DateTime))+
  geom_point(size = 3)+
  ggtitle("Frch 2019 avg Water Level and Depth")+
  labs(x = "Average Water Level", y = "Depth (cm)")+
  geom_smooth(method=lm) 


#All Together


FinalPlot <- ggarrange(strtPlot, pokePlot, moosPlot, vaulPlot, frchPlot,
                       ncol = 3, nrow = 2, main = "Average Water Level and Depth")

FinalPlot

################################# Predicted Depth  ####



strt.lm.WL <- lm(DepthCM ~ MeanWL, data = StrtPt_Depth)
strt.lm.WL

strt.dod.combined$pred.strt.depth <- coef(strt.lm.WL)[2] * 
  strt.dod.combined$MeanWL + coef(strt.lm.WL)[1]

view(strt.dod.combined)


###################### Metabolism STRT ######

exo.all <- read.csv("C:/Users/jacob/Downloads/EXO.ALL.csv")

#Continuous Depth (m)
strt.dod.combined$depth.meters <- strt.dod.combined$pred.strt.depth / 100 

strt.dod.combined$depth.meters

#Continuous DO Data, temp data 
strt.exo <- exo.all %>%
  filter( site.ID == "STRT")


str(strt.exo)

strt.exo$DateTime <- as.POSIXct(strt.exo$DateTime)
str(strt.exo)

strt.exo$ODO.mgL <- as.numeric(strt.exo$ODO.mgL)

ODO.Mgl.strt.2020 <- aggregate(ODO.mgL ~ DateTime, strt.exo, mean )

ODO.psat.strt.2020 <- aggregate(as.numeric(ODO.Psat) ~ DateTime, strt.exo, mean )

        #Where is DO saturated data 

Temp.C.2020.strt <- aggregate(Temp.C ~ DateTime, strt.exo, mean )

Temp.C.2020.strt

######Light data 
      
      strt.2020.url <- "https://drive.google.com/drive/u/2/folders/1udve7khOM2EX0l8BZQK3yehb2HUbwFtR"
      strt2020 <- drive_get(as_id(strt.2020.url))
      test1strt2020_glist <- drive_ls(strt2020, pattern = "11620_001_002_STRT_EndOfSeason.CSV")
      walk(test1strt2020_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
      strt2020Data <- read.csv("11620_001_002_STRT_EndOfSeason.CSV", 
                               skip = 9, header = FALSE)
      
      
      #Clip data: ast good at 13:30
      strt2020Data <- strt2020Data[c(1:11328),]
      names(strt2020Data)[1:4]= c("Scan.Number", "Date","time", "Calibrated.Value")
      
      #Calibrate to known LICOR values
      strt2020Data$Calibrated.Par.Value <- strt2020Data$Calibrated.Value * 0.036
      
      
      strt2020Data$DateTime <- as.POSIXct(paste(strt2020Data$Date, strt2020Data$time), format="%d/%m/%Y %H:%M:%S")
      
      strt2020Data$DateTime <- lubridate::round_date(strt2020Data$DateTime, "15 minutes") 
      
      strt2020Data$DateTime
      
      strt.2020.ggplot <- ggplot(data=strt2020Data, aes(y=Calibrated.Par.Value, x=DateTime)) +
        geom_point() + 
        labs(x = "Date and Time", y = "Calibrated PAR sensor values")+
        ggtitle("Stuart 2020 PAR Data")
      
      strt.2020.ggplot

strt2020Data$Calibrated.Par.Value      
      
#Discharge

strt2020Discharge <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/LabLearning/strt.final.discharge.2020.csv") 

strt2020Discharge$DateTime <-  strt2020Discharge$Strt1comb.2020.DateTime


### Combine them all 

Final.Strt.2020 <- plyr::join(strt.dod.combined, ODO.Mgl.strt.2020, by = "DateTime")
Final.Strt.2020 <- plyr::join(Final.Strt.2020, ODO.psat.strt.2020, by = "DateTime")
Final.Strt.2020 <- plyr::join(Final.Strt.2020, Temp.C.2020.strt, by = "DateTime")
Final.Strt.2020 <- plyr::join(Final.Strt.2020, strt2020Data, by = "DateTime")
Final.Strt.2020 <- plyr::join(Final.Strt.2020, strt2020Discharge, by = "DateTime")

str(Final.Strt.2020)

Final.Strt.2020$DateTime <- as.POSIXct(Final.Strt.2020$DateTime)
str(Final.Strt.2020)

### Solar Time

lubridate::tz(Final.Strt.2020$DateTime) # yep, we want and have the code for EST
Final.Strt.2020$solar.time <- streamMetabolizer::calc_solar_time(Final.Strt.2020$DateTime, longitude=-146.479933)
str(Final.Strt.2020)

### Stream Metabolizer 

library(streamMetabolizer)



# names(Final.Strt.2020)[5]= c("depth")
# names(Final.Strt.2020)[6]= c("DO.obs")
# names(Final.Strt.2020)[7]= c("DO.sat")
# names(Final.Strt.2020)[8]= c("temp.water")
# names(Final.Strt.2020)[14]= c("light")
# names(Final.Strt.2020)[18]= c("discharge")

write.csv(Final.Strt.2020, file = "C:/Users/jacob/Downloads/FinalStrt2020.csv")

Final.Strt.2020 <- read.csv(file = "C:/Users/jacob/Downloads/FinalStrt2020.csv")

Final.Strt.2020$solar.time <- as.POSIXct(Final.Strt.2020$solar.time)

test.strt <- Final.Strt.2020

bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_name

bayes_specs <- specs(bayes_name, burnin_steps=1000, saved_steps=500, n_cores=8, GPP_daily_mu=3, GPP_daily_sigma=2)

data.strt.mm <- na.omit(test.strt) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

mm.test <- metab(bayes_specs, data=data.strt.mm)



predict_metab(mm.test)
plot_metab_preds(mm.test)


predict_DO(mm) %>% head()
plot_DO_preds(mm)
mcmc <- get_mcmc(mm)
rstan::traceplot(mcmc, pars='K600_daily', nrow=3)

get_fit(mm)
#measure Rhat as a measure of convergence, shouldn't be over 1.1 

pairs(mm)

plot_DO_preds(mm)
