##### Stitch Depth #####
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

#### 2019 ####

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




##### 2020 #####


PT.2020.url <- "https://drive.google.com/drive/u/1/folders/1xYaOxYwRJQmYt0qOzahZMmTZRKW3lTVX"
pt.2020.1 <- drive_get(as_id(PT.2020.url))
pt.2020_glist <- drive_ls(pt.2020.1, pattern = "all.pt.raw.csv")
walk(pt.2020_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
pt.2020.Data <- read.csv("all.pt.raw.csv",
                         skip = 0, header = TRUE)

pt.2020.Data <- pt.2020.Data %>% filter(Year == "2020")



#separate out into individual sites to create a mean PT depth 

frch.data1.2020 <- pt.2020.Data %>% filter(Site == "FRCH1")
frch.data2.2020 <- pt.2020.Data %>% filter(Site == "FRCH2")
frch.2020.pt <- inner_join(frch.data1.2020, frch.data2.2020, by = "DateTime")
frch.2020.pt$DateTime <- as.POSIXct(paste(frch.2020.pt$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
frch.2020.pt$AvgAbsDepth <- (frch.2020.pt$WaterLevel.x + frch.2020.pt$WaterLevel.x)/2


poke.data1.2020 <- pt.2020.Data %>% filter(Site == "POKE1")
poke.data2.2020 <- pt.2020.Data %>% filter(Site == "POKE2")
poke.2020.pt <- inner_join(poke.data1.2020, poke.data2.2020, by = "DateTime")
poke.2020.pt$DateTime <- as.POSIXct(paste(poke.2020.pt$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
poke.2020.pt$AvgAbsDepth <- (poke.2020.pt$WaterLevel.x + poke.2020.pt$WaterLevel.x)/2




strt.data1.2020 <- pt.2020.Data %>% filter(Site == "STRT1")
strt.data2.2020 <- pt.2020.Data %>% filter(Site == "STRT2")
strt.2020.pt <- inner_join(strt.data1.2020, strt.data2.2020, by = "DateTime")
strt.2020.pt$DateTime <- as.POSIXct(paste(strt.2020.pt$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
strt.2020.pt$AvgAbsDepth <- (strt.2020.pt$WaterLevel.x + strt.2020.pt$WaterLevel.x)/2


moos.data1.2020 <- pt.2020.Data %>% filter(Site == "MOOS1")
moos.data2.2020 <- pt.2020.Data %>% filter(Site == "MOOS2")
moos.2020.pt <- inner_join(moos.data1.2020, moos.data2.2020, by = "DateTime")
moos.2020.pt$DateTime <- as.POSIXct(paste(moos.2020.pt$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
moos.2020.pt$AvgAbsDepth <- (moos.2020.pt$WaterLevel.x + moos.2020.pt$WaterLevel.x)/2


vaul.data1.2020 <- pt.2020.Data %>% filter(Site == "VAUL1")
vaul.data1.2020$DateTime <- as.POSIXct(vaul.data1.2020$DateTime)
vaul.2020.pt <- vaul.data1.2020

vaul.2020.pt$AvgAbsDepth <- vaul.2020.pt$WaterLevel


###### 2021 #####

s