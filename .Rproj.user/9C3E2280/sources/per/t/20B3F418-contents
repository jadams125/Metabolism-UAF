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
library(data.table)

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


frch.2019.pt <- mutate(frch.2019.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, AbsPTDepth.y, AvgAbsDepth))

frch.2019.pt <- mutate(frch.2019.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, AbsPTDepth.x, AvgAbsDepth))



poke.data1 <- pt.2019.Data %>% filter(Site == "POKE1")
poke.data2 <- pt.2019.Data %>% filter(Site == "POKE2")
poke.2019.pt <- inner_join(poke.data1, poke.data2, by = "DateTime")
poke.2019.pt$DateTime <- as.POSIXct(paste(poke.2019.pt$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
poke.2019.pt$AvgAbsDepth <- (poke.2019.pt$AbsPTDepth.x + poke.2019.pt$AbsPTDepth.y)/2


poke.2019.pt <- mutate(poke.2019.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, AbsPTDepth.y, AvgAbsDepth))

poke.2019.pt <- mutate(poke.2019.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, AbsPTDepth.x, AvgAbsDepth))



strt.data1 <- pt.2019.Data %>% filter(Site == "STRT1")
strt.data2 <- pt.2019.Data %>% filter(Site == "STRT2")
strt.data2$DateTime <- as.POSIXct(paste(strt.data2$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
strt.data2$DateTime <- as.character(strt.data2$DateTime)
strt.2019.pt <- merge(strt.data2, strt.data1, by = "DateTime", all = TRUE)
strt.data2$DateTime <- as.POSIXct(strt.data2$DateTime)
strt.2019.pt$AvgAbsDepth <- (strt.2019.pt$AbsPTDepth.x + strt.2019.pt$AbsPTDepth.y)/2


strt.2019.pt <- mutate(strt.2019.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, AbsPTDepth.y, AvgAbsDepth))

strt.2019.pt <- mutate(strt.2019.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, AbsPTDepth.x, AvgAbsDepth))

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

frch.2020.pt$AvgAbsDepth <- (frch.2020.pt$WaterLevel.x + frch.2020.pt$WaterLevel.y)/2


frch.2020.pt <- mutate(frch.2020.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.y, AvgAbsDepth))

frch.2020.pt <- mutate(frch.2020.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.x, AvgAbsDepth))



poke.data1.2020 <- pt.2020.Data %>% filter(Site == "POKE1")
poke.data2.2020 <- pt.2020.Data %>% filter(Site == "POKE2")
poke.2020.pt <- inner_join(poke.data1.2020, poke.data2.2020, by = "DateTime")
poke.2020.pt$DateTime <- as.POSIXct(paste(poke.2020.pt$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
poke.2020.pt$AvgAbsDepth <- (poke.2020.pt$WaterLevel.x + poke.2020.pt$WaterLevel.y)/2


poke.2020.pt <- mutate(poke.2020.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.y, AvgAbsDepth))

poke.2020.pt <- mutate(poke.2020.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.x, AvgAbsDepth))



strt.data1.2020 <- pt.2020.Data %>% filter(Site == "STRT1")
strt.data2.2020 <- pt.2020.Data %>% filter(Site == "STRT2")
strt.2020.pt <- inner_join(strt.data1.2020, strt.data2.2020, by = "DateTime")
strt.2020.pt$DateTime <- as.POSIXct(paste(strt.2020.pt$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
strt.2020.pt$AvgAbsDepth <- (strt.2020.pt$WaterLevel.x + strt.2020.pt$WaterLevel.y)/2


strt.2020.pt <- mutate(strt.2020.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.y, AvgAbsDepth))

strt.2020.pt <- mutate(strt.2020.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.x, AvgAbsDepth))





moos.data1.2020 <- pt.2020.Data %>% filter(Site == "MOOS1")
moos.data1$DateTime <- as.POSIXct(paste(moos.data1$DateTimeGMT), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
moos.2020.pt <- moos.data1.2020



vaul.data1.2020 <- pt.2020.Data %>% filter(Site == "VAUL1")
vaul.data1.2020$DateTime <- as.POSIXct(vaul.data1.2020$DateTime)
vaul.2020.pt <- vaul.data1.2020

vaul.2020.pt$AvgAbsDepth <- vaul.2020.pt$WaterLevel


###### 2021 #####

#VAUL 
VAUL_PT_2021.url <- "https://drive.google.com/drive/u/1/folders/1F80dynCpIo87e5EalwjNprze5UnLiomX"
vaul_pt.2021.1 <- drive_get(as_id(VAUL_PT_2021.url))
vaul.pt2021_glist <- drive_ls(vaul_pt.2021.1, pattern = "vaul.pt.2021.csv")
walk(vaul.pt2021_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul_PT_2021.Data <- read.csv("vaul.pt.2021.csv",
                         skip = 0, header = TRUE)


vaul.2021.pt <- vaul_PT_2021.Data
# 
# vaul.data1.2021 <- vaul_PT_2021.Data %>% filter(Site == "VAUL1")
# vaul.data2.2021 <- vaul_PT_2021.Data %>% filter(Site == "VAUL2")
# vaul.2021.pt <- inner_join(vaul.data1.2021, vaul.data2.2021, by = "DateTime")
# vaul.2021.pt$DateTime <- as.POSIXct(paste(vaul.2021.pt$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
# vaul.2021.pt$AvgAbsDepth <- (vaul.2021.pt$WaterLevel.x + vaul.2021.pt$WaterLevel.y)/2
# 
# 
# vaul.2021.pt <- mutate(vaul.2021.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.y, AvgAbsDepth))
# 
# vaul.2021.pt <- mutate(vaul.2021.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.x, AvgAbsDepth))




#POKE

POKE_PT_2021.url <- "https://drive.google.com/drive/u/1/folders/1rOGiMGGMYzOoDcNQoJATxHARqR8Y1F1m"
poke_pt.2021.1 <- drive_get(as_id(POKE_PT_2021.url))
poke.pt2021_glist <- drive_ls(poke_pt.2021.1, pattern = "poke.pt.2021.csv")
walk(poke.pt2021_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke_PT_2021.Data <- read.csv("poke.pt.2021.csv",
                              skip = 0, header = TRUE)

poke.data1.2021 <- poke_PT_2021.Data %>% filter(Site == "POKE1")
poke.data2.2021 <- poke_PT_2021.Data %>% filter(Site == "POKE2")
poke.2021.pt <- inner_join(poke.data1.2021, poke.data2.2021, by = "DateTime")
poke.2021.pt$DateTime <- as.POSIXct(paste(poke.2021.pt$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
poke.2021.pt$AvgAbsDepth <- (poke.2021.pt$WaterLevel.x + poke.2021.pt$WaterLevel.y)/2


poke.2021.pt <- mutate(poke.2021.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.y, AvgAbsDepth))

poke.2021.pt <- mutate(poke.2021.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.x, AvgAbsDepth))



#FRCH

FRCH_PT_2021.url <- "https://drive.google.com/drive/u/1/folders/1pCn5m6WKsJv3ZzuWDvBr6iAp05bsGOWC"
frch_pt.2021.1 <- drive_get(as_id(FRCH_PT_2021.url))
frch.pt2021_glist <- drive_ls(frch_pt.2021.1, pattern = "frch.pt.2021.csv")
walk(frch.pt2021_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch_PT_2021.Data <- read.csv("frch.pt.2021.csv",
                              skip = 0, header = TRUE)

frch.data1.2021 <- frch_PT_2021.Data %>% filter(Site == "FRCH1")
frch.data2.2021 <- frch_PT_2021.Data %>% filter(Site == "FRCH2")
frch.2021.pt <- inner_join(frch.data1.2021, frch.data2.2021, by = "DateTime")
frch.2021.pt$DateTime <- as.POSIXct(paste(frch.2021.pt$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")

frch.2021.pt$AvgAbsDepth <- (frch.2021.pt$WaterLevel.x + frch.2021.pt$WaterLevel.y)/2


frch.2021.pt <- mutate(frch.2021.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.y, AvgAbsDepth))

frch.2021.pt <- mutate(frch.2021.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.x, AvgAbsDepth))




#MOOS

MOOS_PT_2021.url <- "https://drive.google.com/drive/u/1/folders/1Q14HB4khayh09dbTfFWGqHrkKaG1wXyN"
moos_pt.2021.1 <- drive_get(as_id(MOOS_PT_2021.url))
moos.pt2021_glist <- drive_ls(moos_pt.2021.1, pattern = "moos.pt.2021.csv")
walk(moos.pt2021_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos_PT_2021.Data <- read.csv("moos.pt.2021.csv",
                              skip = 0, header = TRUE)

moos.data1.2021 <- moos_PT_2021.Data %>% filter(Site == "MOOS1")
moos.data2.2021 <- moos_PT_2021.Data %>% filter(Site == "MOOS2")
moos.2021.pt <- inner_join(moos.data1.2021, moos.data2.2021, by = "DateTime")
moos.2021.pt$DateTime <- as.POSIXct(paste(moos.2021.pt$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
moos.2021.pt$AvgAbsDepth <- (moos.2021.pt$WaterLevel.x + moos.2021.pt$WaterLevel.y)/2


moos.2021.pt <- mutate(moos.2021.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.y, AvgAbsDepth))

moos.2021.pt <- mutate(moos.2021.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.x, AvgAbsDepth))


#STRT

STRT_PT_2021.url <- "https://drive.google.com/drive/u/1/folders/1-om0nU42U8fNmeWtBrhM8WQTbGqBW8RN"
strt_pt.2021.1 <- drive_get(as_id(STRT_PT_2021.url))
strt.pt2021_glist <- drive_ls(strt_pt.2021.1, pattern = "strt.pt.2021.csv")
walk(strt.pt2021_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt_PT_2021.Data <- read.csv("strt.pt.2021.csv",
                              skip = 0, header = TRUE)

strt.data1.2021 <- strt_PT_2021.Data %>% filter(Site == "STRT1")
strt.data2.2021 <- strt_PT_2021.Data %>% filter(Site == "STRT2")
strt.2021.pt <- inner_join(strt.data1.2021, strt.data2.2021, by = "DateTime")
strt.2021.pt$DateTime <- as.POSIXct(paste(strt.2021.pt$DateTime), format = "%Y-%m-%d %H:%M", tz = "America/Anchorage")
strt.2021.pt$AvgAbsDepth <- (strt.2021.pt$WaterLevel.x + strt.2021.pt$WaterLevel.x)/2

strt.2021.pt$AvgAbsDepth <- (strt.2021.pt$WaterLevel.x + strt.2021.pt$WaterLevel.y)/2


strt.2021.pt <- mutate(strt.2021.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.y, AvgAbsDepth))

strt.2021.pt <- mutate(strt.2021.pt, AvgAbsDepth = ifelse(is.na(AvgAbsDepth) == TRUE, WaterLevel.x, AvgAbsDepth))



#MY METHOD



# Poke 2019:

#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
poke.wr19_glist <- drive_ls(WR.19.1, pattern = "poke_2019_flowmeter_Q_for_R_JAA.csv")
walk(poke.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke_WR_19.Data <- read.csv("poke_2019_flowmeter_Q_for_R_JAA.csv",
                            skip = 1, header = TRUE, na.strings=c("","NA","blank"))

poke_WR_19.Data <- poke_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

poke_WR_19.Data <- poke_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


poke_WR_19.Data$datetimeAK <- as.POSIXct(paste(poke_WR_19.Data$Date, poke_WR_19.Data$Time), format="%m/%d/%Y %H:%M")


poke_WR_19.Data <- poke_WR_19.Data %>%
  select(Depth..cm., datetimeAK)

Poke_depth_19_WR <- ddply(na.omit(poke_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# poke_wr.lm <- lm(meanDepth~datetimeAK, Poke_depth_19)


Poke_depth_19_WR <- setDT(Poke_depth_19_WR)

Poke_depth_19_WR <- Poke_depth_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

poke.2019.pt <- poke.2019.pt %>%
  dplyr::rename(datetimeAK = DateTime)

poke.2019.pt <- setDT(poke.2019.pt)

setDT(Poke_depth_19)
setDT(poke.2019.pt)

poke.2019.pt$datetimeAK1 <- poke.2019.pt$datetimeAK

setkey( poke.2019.pt, datetimeAK )
setkey( Poke_depth_19_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_poke19 <- poke.2019.pt[ Poke_depth_19_WR, roll = "nearest" ]

rounded.dates_poke19_WR_PT <- rounded.dates_poke19 %>%
  select(datetimeAK, AvgAbsDepth, meanDepth)

#convert to meters
rounded.dates_poke19_WR_PT$meanDepth <- rounded.dates_poke19_WR_PT$meanDepth /100

poke_pt_wr_graph <- ggplot(rounded.dates_poke19_WR_PT, aes(AvgAbsDepth, meanDepth)) +
  geom_point()
# Add regression line
poke_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/poke_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


poke19_depth_mod <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_poke19_WR_PT)

summary(poke19_depth_mod)

plot(meanDepth~AvgAbsDepth, data = rounded.dates_poke19_WR_PT)
abline(poke19_depth_mod)

#extract slope of model and develop rating curve
poke.2019.pt$RatingCurveDepth <- poke19_depth_mod$coefficients[1]+(poke19_depth_mod$coefficients[2])*poke.2019.pt$AvgAbsDepth




# rounded.dates_poke19 <- rounded.dates_poke19 %>%
#   dplyr::rename(datetimeAK_old = datetimeAK)
# rounded.dates_poke19 <- rounded.dates_poke19 %>%
#   dplyr::rename(datetimeAK = datetimeAK1)
# 
# rounded.dates_poke19 <- rounded.dates_poke19 %>%
#   select(meanDepth, datetimeAK)
# 
# Poke.2019.pt_WR.Depth <- merge(poke.2019.pt, rounded.dates_poke19, by = "datetimeAK", all = TRUE)
# Poke.2019.pt_WR.Depth$meanDepth1 <- Poke.2019.pt_WR.Depth$meanDepth
# 
# testmod <- lm(meanDepth ~ datetimeAK, Poke.2019.pt_WR.Depth)
# 
# summary(testmod)
# 
# Poke.2019.pt_WR.Depth <- Poke.2019.pt_WR.Depth %>% 
#   mutate(predictedDepth = predict(testmod, .)) %>%
#   # Replace NA with pred in var1
#   mutate(meanDepth = ifelse(is.na(meanDepth), predictedDepth, meanDepth))
# 
# 
# plot(Poke.2019.pt_WR.Depth$AbsPTDepth , Poke.2019.pt_WR.Depth$predictedDepth)
# 
# 
# 
# 
# 
# 
# 
# 
# 

# # Vaul 2019:

#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
vaul.wr19_glist <- drive_ls(WR.19.1, pattern = "vaul_2019_flowmeter_Q_for_R_JAA.csv")
walk(vaul.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul_WR_19.Data <- read.csv("vaul_2019_flowmeter_Q_for_R_JAA.csv",
                            skip = 1, header = TRUE, na.strings=c("","NA","blank"))

vaul_WR_19.Data <- vaul_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

vaul_WR_19.Data <- vaul_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


vaul_WR_19.Data$datetimeAK <- as.POSIXct(paste(vaul_WR_19.Data$Date, vaul_WR_19.Data$Time), format="%m/%d/%Y %H:%M")


vaul_WR_19.Data <- vaul_WR_19.Data %>%
  select(Depth..cm., datetimeAK)

Vaul_depth_19_WR <- ddply(na.omit(vaul_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# vaul_wr.lm <- lm(meanDepth~datetimeAK, Vaul_depth_19)


Vaul_depth_19_WR <- setDT(Vaul_depth_19_WR)

Vaul_depth_19_WR <- Vaul_depth_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

vaul.2019.pt <- vaul.2019.pt %>%
  dplyr::rename(datetimeAK = DateTime)

vaul.2019.pt <- setDT(vaul.2019.pt)

setDT(Vaul_depth_19)
setDT(vaul.2019.pt)

vaul.2019.pt$datetimeAK1 <- vaul.2019.pt$datetimeAK

setkey( vaul.2019.pt, datetimeAK )
setkey( Vaul_depth_19_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_vaul19 <- vaul.2019.pt[ Vaul_depth_19_WR, roll = "nearest" ]

rounded.dates_vaul19_WR_PT <- rounded.dates_vaul19 %>%
  select(datetimeAK, AbsPTDepth, meanDepth)

#convert to meters
rounded.dates_vaul19_WR_PT$meanDepth <- rounded.dates_vaul19_WR_PT$meanDepth /100

vaul_pt_wr_graph <- ggplot(rounded.dates_vaul19_WR_PT, aes(AbsPTDepth, meanDepth)) +
  geom_point()
# Add regression line
vaul_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/vaul_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


vaul19_depth_mod <- lm(meanDepth~AbsPTDepth, data = rounded.dates_vaul19_WR_PT)

summary(vaul19_depth_mod)

plot(meanDepth~AbsPTDepth, data = rounded.dates_vaul19_WR_PT)
abline(vaul19_depth_mod)

#extract slope of model and develop rating curve
vaul.2019.pt$RatingCurveDepth <-vaul19_depth_mod$coefficients[1]+(vaul19_depth_mod$coefficients[2])*vaul.2019.pt$AbsPTDepth


# # Moos 2019:

#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
moos.wr19_glist <- drive_ls(WR.19.1, pattern = "moos_2019_flowmeter_Q_for_R_JAA.csv")
walk(moos.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos_WR_19.Data <- read.csv("moos_2019_flowmeter_Q_for_R_JAA.csv",
                            skip = 1, header = TRUE, na.strings=c("","NA","blank"))

moos_WR_19.Data <- moos_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

moos_WR_19.Data <- moos_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


moos_WR_19.Data$datetimeAK <- as.POSIXct(paste(moos_WR_19.Data$Date, moos_WR_19.Data$Time), format="%m/%d/%Y %H:%M")


moos_WR_19.Data <- moos_WR_19.Data %>%
  select(Depth..cm., datetimeAK)

Moos_depth_19_WR <- ddply(na.omit(moos_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# moos_wr.lm <- lm(meanDepth~datetimeAK, Moos_depth_19)


Moos_depth_19_WR <- setDT(Moos_depth_19_WR)

Moos_depth_19_WR <- Moos_depth_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

moos.2019.pt <- moos.2019.pt %>%
  dplyr::rename(datetimeAK = DateTime)

moos.2019.pt <- moos.2019.pt %>%
  dplyr::rename(AvgAbsDepth = AbsPTDepth)

moos.2019.pt <- setDT(moos.2019.pt)

setDT(Moos_depth_19)
setDT(moos.2019.pt)

moos.2019.pt$datetimeAK1 <- moos.2019.pt$datetimeAK

setkey( moos.2019.pt, datetimeAK )
setkey( Moos_depth_19_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_moos19 <- moos.2019.pt[ Moos_depth_19_WR, roll = "nearest" ]

rounded.dates_moos19_WR_PT <- rounded.dates_moos19 %>%
  select(datetimeAK, AvgAbsDepth, meanDepth)

#convert to meters
rounded.dates_moos19_WR_PT$meanDepth <- rounded.dates_moos19_WR_PT$meanDepth /100

moos_pt_wr_graph <- ggplot(rounded.dates_moos19_WR_PT, aes(AvgAbsDepth, meanDepth)) +
  geom_point()
# Add regression line
moos_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/moos_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


moos19_depth_mod <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_moos19_WR_PT)

summary(moos19_depth_mod)

plot(meanDepth~AvgAbsDepth, data = rounded.dates_moos19_WR_PT)
abline(moos19_depth_mod)

#extract slope of model and develop rating curve
moos.2019.pt$RatingCurveDepth <-moos19_depth_mod$coefficients[1]+(moos19_depth_mod$coefficients[2])*moos.2019.pt$AvgAbsDepth



# # Frch 2019:

#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
frch.wr19_glist <- drive_ls(WR.19.1, pattern = "frch_2019_flowmeter_Q_for_R_JAA.csv")
walk(frch.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch_WR_19.Data <- read.csv("frch_2019_flowmeter_Q_for_R_JAA.csv",
                            skip = 1, header = TRUE, na.strings=c("","NA","blank"))

frch_WR_19.Data <- frch_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

frch_WR_19.Data <- frch_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


frch_WR_19.Data$datetimeAK <- as.POSIXct(paste(frch_WR_19.Data$Date, frch_WR_19.Data$Time), format="%m/%d/%Y %H:%M")


frch_WR_19.Data <- frch_WR_19.Data %>%
  select(Depth..cm., datetimeAK)

Frch_depth_19_WR <- ddply(na.omit(frch_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# frch_wr.lm <- lm(meanDepth~datetimeAK, Frch_depth_19)


Frch_depth_19_WR <- setDT(Frch_depth_19_WR)

Frch_depth_19_WR <- Frch_depth_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

frch.2019.pt <- frch.2019.pt %>%
  dplyr::rename(datetimeAK = DateTime)

frch.2019.pt <- frch.2019.pt %>%
  dplyr::rename(AvgAbsDepth = AbsPTDepth)

frch.2019.pt <- setDT(frch.2019.pt)

setDT(Frch_depth_19)
setDT(frch.2019.pt)

frch.2019.pt$datetimeAK1 <- frch.2019.pt$datetimeAK

setkey( frch.2019.pt, datetimeAK )
setkey( Frch_depth_19_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_frch19 <- frch.2019.pt[ Frch_depth_19_WR, roll = "nearest" ]

rounded.dates_frch19_WR_PT <- rounded.dates_frch19 %>%
  select(datetimeAK, AvgAbsDepth, meanDepth)

#convert to meters
rounded.dates_frch19_WR_PT$meanDepth <- rounded.dates_frch19_WR_PT$meanDepth /100

frch_pt_wr_graph <- ggplot(rounded.dates_frch19_WR_PT, aes(AvgAbsDepth, meanDepth)) +
  geom_point()
# Add regression line
frch_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/frch_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


frch19_depth_mod <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_frch19_WR_PT)

summary(frch19_depth_mod)

plot(meanDepth~AvgAbsDepth, data = rounded.dates_frch19_WR_PT)
abline(frch19_depth_mod)

#extract slope of model and develop rating curve
frch.2019.pt$RatingCurveDepth <-frch19_depth_mod$coefficients[1]+(frch19_depth_mod$coefficients[2])*frch.2019.pt$AvgAbsDepth


# # Strt 2019:

#download flowmeter data
WR_19.url <- "https://drive.google.com/drive/u/1/folders/1bm62_JO1dKrFPyUCz8E88K5q7IHElXPP"
WR.19.1 <- drive_get(as_id(WR_19.url))
strt.wr19_glist <- drive_ls(WR.19.1, pattern = "STRT_2019_flowmeter_Q_for_R_JAA.csv")
walk(strt.wr19_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt_WR_19.Data <- read.csv("STRT_2019_flowmeter_Q_for_R_JAA.csv",
                             header = TRUE, na.strings=c("","NA","blank"))

strt_WR_19.Data <- strt_WR_19.Data %>% tidyr::fill(Date, .direction = ("down"))

strt_WR_19.Data <- strt_WR_19.Data %>% tidyr::fill(Time, .direction = ("down"))


strt_WR_19.Data$datetimeAK <- as.POSIXct(paste(strt_WR_19.Data$Date, strt_WR_19.Data$Time), format="%m/%d/%Y %H:%M")


strt_WR_19.Data <- strt_WR_19.Data %>%
  select(Depth..cm., datetimeAK)

Strt_depth_19_WR <- ddply(na.omit(strt_WR_19.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# strt_wr.lm <- lm(meanDepth~datetimeAK, Strt_depth_19)


Strt_depth_19_WR <- setDT(Strt_depth_19_WR)

Strt_depth_19_WR <- Strt_depth_19_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

strt.2019.pt <- strt.2019.pt %>%
  dplyr::rename(datetimeAK = DateTime)

strt.2019.pt <- strt.2019.pt %>%
  dplyr::rename(AvgAbsDepth = AbsPTDepth)

strt.2019.pt <- setDT(strt.2019.pt)

setDT(Strt_depth_19)
setDT(strt.2019.pt)

strt.2019.pt$datetimeAK <- as.POSIXct(strt.2019.pt$datetimeAK)

strt.2019.pt$datetimeAK1 <- strt.2019.pt$datetimeAK

setkey( strt.2019.pt, datetimeAK )
setkey( Strt_depth_19_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_strt19 <- strt.2019.pt[ Strt_depth_19_WR, roll = "nearest" ]

rounded.dates_strt19_WR_PT <- rounded.dates_strt19 %>%
  select(datetimeAK, AvgAbsDepth, meanDepth)

#convert to meters
rounded.dates_strt19_WR_PT$meanDepth <- rounded.dates_strt19_WR_PT$meanDepth /100

strt_pt_wr_graph <- ggplot(rounded.dates_strt19_WR_PT, aes(AvgAbsDepth, meanDepth)) +
  geom_point()
# Add regression line
strt_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/strt_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


strt19_depth_mod <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_strt19_WR_PT)

summary(strt19_depth_mod)

plot(meanDepth~AvgAbsDepth, data = rounded.dates_strt19_WR_PT)
abline(strt19_depth_mod)

#extract slope of model and develop rating curve
strt.2019.pt$RatingCurveDepth <-strt19_depth_mod$coefficients[1]+(strt19_depth_mod$coefficients[2])*strt.2019.pt$AvgAbsDepth





#### 2020 ####

# Poke 2020:

#download flowmeter data
WR_20_poke.url <- "https://drive.google.com/drive/u/1/folders/1S2L8Qg08AIhQo1ZdKdaxlJliz4bi1ttr"
WR.20_poke.1 <- drive_get(as_id(WR_20_poke.url))
poke.wr20_glist <- drive_ls(WR.20_poke.1, pattern = "R_Flowmeter Q calculation_POKE_for_R_JAA.csv")
walk(poke.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke_WR_20.Data <- read.csv("R_Flowmeter Q calculation_POKE_for_R_JAA.csv",
                            skip = 0, header = TRUE, na.strings=c("","NA","blank"))


poke_WR_20.Data <- poke_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))


poke_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(poke_WR_20.Data$ï..Date), poke_WR_20.Data$Time), format="%y%m%d %H:%M")

poke_WR_20.Data <- poke_WR_20.Data %>%
  dplyr::rename(Depth..cm. = Depth)

poke_WR_20.Data <- poke_WR_20.Data %>%
  select(Depth..cm., datetimeAK)

Poke_depth_20_WR <- ddply(na.omit(poke_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# poke_wr.lm <- lm(meanDepth~datetimeAK, Poke_depth_20)


Poke_depth_20_WR <- setDT(Poke_depth_20_WR)

Poke_depth_20_WR <- Poke_depth_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

poke.2020.pt <- poke.2020.pt %>%
  dplyr::rename(datetimeAK = DateTime)

poke.2020.pt <- setDT(poke.2020.pt)

setDT(Poke_depth_20)
setDT(poke.2020.pt)

poke.2020.pt$datetimeAK1 <- poke.2020.pt$datetimeAK

setkey( poke.2020.pt, datetimeAK )
setkey( Poke_depth_20_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_poke20 <- poke.2020.pt[ Poke_depth_20_WR, roll = "nearest" ]

rounded.dates_poke20_WR_PT <- rounded.dates_poke20 %>%
  select(datetimeAK, AvgAbsDepth, meanDepth)

#convert to meters
rounded.dates_poke20_WR_PT$meanDepth <- rounded.dates_poke20_WR_PT$meanDepth /100

poke_pt_wr_graph <- ggplot(rounded.dates_poke20_WR_PT, aes(AvgAbsDepth, meanDepth)) +
  geom_point()
# Add regression line
poke_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/poke_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


poke20_depth_mod <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_poke20_WR_PT)

summary(poke20_depth_mod)

plot(meanDepth~AvgAbsDepth, data = rounded.dates_poke20_WR_PT)
abline(poke20_depth_mod)

#extract slope of model and develop rating curve
poke.2020.pt$RatingCurveDepth <- poke20_depth_mod$coefficients[1]+ (poke20_depth_mod$coefficients[2])*poke.2020.pt$AvgAbsDepth






# Vaul 2020:

#download flowmeter data
WR_20_vaul.url <- "https://drive.google.com/drive/u/1/folders/1l-QIICuviZvugbNGrvoLfkCDgo1HyDMg"
WR.20_vaul.1 <- drive_get(as_id(WR_20_vaul.url))
vaul.wr20_glist <- drive_ls(WR.20_vaul.1, pattern = "R_Flowmeter Q calculation_VAUL_for_R_JAA.csv")
walk(vaul.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul_WR_20.Data <- read.csv("R_Flowmeter Q calculation_VAUL_for_R_JAA.csv",
                            skip = 0, header = TRUE, na.strings=c("","NA","blank"))


vaul_WR_20.Data <- vaul_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))


vaul_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(vaul_WR_20.Data$ï..Date), vaul_WR_20.Data$Time), format="%y%m%d %H:%M")

vaul_WR_20.Data <- vaul_WR_20.Data %>%
  dplyr::rename(Depth..cm. = Depth)

vaul_WR_20.Data <- vaul_WR_20.Data %>%
  select(Depth..cm., datetimeAK)

Vaul_depth_20_WR <- ddply(na.omit(vaul_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# vaul_wr.lm <- lm(meanDepth~datetimeAK, Vaul_depth_20)


Vaul_depth_20_WR <- setDT(Vaul_depth_20_WR)

Vaul_depth_20_WR <- Vaul_depth_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

vaul.2020.pt <- vaul.2020.pt %>%
  dplyr::rename(datetimeAK = DateTime)

vaul.2020.pt <- setDT(vaul.2020.pt)

setDT(Vaul_depth_20)
setDT(vaul.2020.pt)

vaul.2020.pt$datetimeAK1 <- vaul.2020.pt$datetimeAK

setkey( vaul.2020.pt, datetimeAK )
setkey( Vaul_depth_20_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_vaul20 <- vaul.2020.pt[ Vaul_depth_20_WR, roll = "nearest" ]

rounded.dates_vaul20_WR_PT <- rounded.dates_vaul20 %>%
  select(datetimeAK, AvgAbsDepth, meanDepth)

#convert to meters
rounded.dates_vaul20_WR_PT$meanDepth <- rounded.dates_vaul20_WR_PT$meanDepth /100

vaul_pt_wr_graph <- ggplot(rounded.dates_vaul20_WR_PT, aes(AvgAbsDepth, meanDepth)) +
  geom_point()
# Add regression line
vaul_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/vaul_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


vaul20_depth_mod <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_vaul20_WR_PT)

summary(vaul20_depth_mod)

plot(meanDepth~AvgAbsDepth, data = rounded.dates_vaul20_WR_PT)
abline(vaul20_depth_mod)

#extract slope of model and develop rating curve
vaul.2020.pt$RatingCurveDepth <- vaul20_depth_mod$coefficients[1]+ (vaul20_depth_mod$coefficients[2])*vaul.2020.pt$AvgAbsDepth




# Moos 2020:

#download flowmeter data
WR_20_moos.url <- "https://drive.google.com/drive/u/1/folders/1O28nv-6gsmC_xsAwUFRjjouY9hS29FtK"
WR.20_moos.1 <- drive_get(as_id(WR_20_moos.url))
moos.wr20_glist <- drive_ls(WR.20_moos.1, pattern = "R_Flowmeter Q calculation_MOOS_for_R_JAA.csv")
walk(moos.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos_WR_20.Data <- read.csv("R_Flowmeter Q calculation_MOOS_for_R_JAA.csv",
                            skip = 0, header = TRUE, na.strings=c("","NA","blank"))


moos_WR_20.Data <- moos_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))


moos_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(moos_WR_20.Data$ï..Date), moos_WR_20.Data$Time), format="%y%m%d %H:%M")

moos_WR_20.Data <- moos_WR_20.Data %>%
  dplyr::rename(Depth..cm. = Depth)

moos_WR_20.Data <- moos_WR_20.Data %>%
  select(Depth..cm., datetimeAK)

Moos_depth_20_WR <- ddply(na.omit(moos_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# moos_wr.lm <- lm(meanDepth~datetimeAK, Moos_depth_20)


Moos_depth_20_WR <- setDT(Moos_depth_20_WR)

Moos_depth_20_WR <- Moos_depth_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

moos.2020.pt <- moos.2020.pt %>%
  dplyr::rename(datetimeAK = DateTime)

moos.2020.pt <- setDT(moos.2020.pt)

setDT(Moos_depth_20)
setDT(moos.2020.pt)

moos.2020.pt$datetimeAK <- as.POSIXct(moos.2020.pt$datetimeAK)

moos.2020.pt$datetimeAK1 <- moos.2020.pt$datetimeAK


setkey( moos.2020.pt, datetimeAK )
setkey( Moos_depth_20_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_moos20 <- moos.2020.pt[ Moos_depth_20_WR, roll = "nearest" ]

rounded.dates_moos20_WR_PT <- rounded.dates_moos20 %>%
  select(datetimeAK, WaterLevel, meanDepth)

#convert to meters
rounded.dates_moos20_WR_PT$meanDepth <- rounded.dates_moos20_WR_PT$meanDepth /100

moos_pt_wr_graph <- ggplot(rounded.dates_moos20_WR_PT, aes(WaterLevel, meanDepth)) +
  geom_point()
# Add regression line
moos_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/moos_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


moos20_depth_mod <- lm(meanDepth~WaterLevel, data = rounded.dates_moos20_WR_PT)

summary(moos20_depth_mod)

plot(meanDepth~WaterLevel, data = rounded.dates_moos20_WR_PT)
abline(moos20_depth_mod)

#extract slope of model and develop rating curve
moos.2020.pt$RatingCurveDepth <- moos20_depth_mod$coefficients[1]+ (moos20_depth_mod$coefficients[2])*moos.2020.pt$WaterLevel




# Frch 2020:

#download flowmeter data
WR_20_frch.url <- "https://drive.google.com/drive/u/1/folders/1tlcGKOm11j4nPqgBeTa1Hj6DFTrbZTvy"
WR.20_frch.1 <- drive_get(as_id(WR_20_frch.url))
frch.wr20_glist <- drive_ls(WR.20_frch.1, pattern = "R_Flowmeter Q calculation_FRCH_for_R_JAA.csv")
walk(frch.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch_WR_20.Data <- read.csv("R_Flowmeter Q calculation_FRCH_for_R_JAA.csv",
                            skip = 0, header = TRUE, na.strings=c("","NA","blank"))


frch_WR_20.Data <- frch_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))


frch_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(frch_WR_20.Data$Date), frch_WR_20.Data$Time), format="%y%m%d %H:%M")

frch_WR_20.Data <- frch_WR_20.Data %>%
  dplyr::rename(Depth..cm. = Depth)

frch_WR_20.Data <- frch_WR_20.Data %>%
  select(Depth..cm., datetimeAK)

Frch_depth_20_WR <- ddply(na.omit(frch_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# frch_wr.lm <- lm(meanDepth~datetimeAK, Frch_depth_20)


Frch_depth_20_WR <- setDT(Frch_depth_20_WR)

Frch_depth_20_WR <- Frch_depth_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

frch.2020.pt <- frch.2020.pt %>%
  dplyr::rename(datetimeAK = DateTime)

frch.2020.pt <- setDT(frch.2020.pt)

setDT(Frch_depth_20)
setDT(frch.2020.pt)

frch.2020.pt$datetimeAK <- as.POSIXct(frch.2020.pt$datetimeAK)

frch.2020.pt$datetimeAK1 <- frch.2020.pt$datetimeAK


setkey( frch.2020.pt, datetimeAK )
setkey( Frch_depth_20_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_frch20 <- frch.2020.pt[ Frch_depth_20_WR, roll = "nearest" ]

rounded.dates_frch20_WR_PT <- rounded.dates_frch20 %>%
  select(datetimeAK, AvgAbsDepth, meanDepth)

#convert to meters
rounded.dates_frch20_WR_PT$meanDepth <- rounded.dates_frch20_WR_PT$meanDepth /100

frch_pt_wr_graph <- ggplot(rounded.dates_frch20_WR_PT, aes(AvgAbsDepth, meanDepth)) +
  geom_point()
# Add regression line
frch_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/frch_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


frch20_depth_mod <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_frch20_WR_PT)

summary(frch20_depth_mod)

plot(meanDepth~AvgAbsDepth, data = rounded.dates_frch20_WR_PT)
abline(frch20_depth_mod)

#extract slope of model and develop rating curve
frch.2020.pt$RatingCurveDepth <- frch20_depth_mod$coefficients[1]+ (frch20_depth_mod$coefficients[2])*frch.2020.pt$AvgAbsDepth




# Moos 2020:

#download flowmeter data
WR_20_moos.url <- "https://drive.google.com/drive/u/1/folders/1O28nv-6gsmC_xsAwUFRjjouY9hS29FtK"
WR.20_moos.1 <- drive_get(as_id(WR_20_moos.url))
moos.wr20_glist <- drive_ls(WR.20_moos.1, pattern = "R_Flowmeter Q calculation_MOOS_for_R_JAA.csv")
walk(moos.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos_WR_20.Data <- read.csv("R_Flowmeter Q calculation_MOOS_for_R_JAA.csv",
                            skip = 0, header = TRUE, na.strings=c("","NA","blank"))


moos_WR_20.Data <- moos_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))


moos_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(moos_WR_20.Data$ï..Date), moos_WR_20.Data$Time), format="%y%m%d %H:%M")

moos_WR_20.Data <- moos_WR_20.Data %>%
  dplyr::rename(Depth..cm. = Depth)

moos_WR_20.Data <- moos_WR_20.Data %>%
  select(Depth..cm., datetimeAK)

Moos_depth_20_WR <- ddply(na.omit(moos_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# moos_wr.lm <- lm(meanDepth~datetimeAK, Moos_depth_20)


Moos_depth_20_WR <- setDT(Moos_depth_20_WR)

Moos_depth_20_WR <- Moos_depth_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

moos.2020.pt <- moos.2020.pt %>%
  dplyr::rename(datetimeAK = DateTime)

moos.2020.pt <- setDT(moos.2020.pt)

setDT(Moos_depth_20)
setDT(moos.2020.pt)

moos.2020.pt$datetimeAK <- as.POSIXct(moos.2020.pt$datetimeAK)

moos.2020.pt$datetimeAK1 <- moos.2020.pt$datetimeAK


setkey( moos.2020.pt, datetimeAK )
setkey( Moos_depth_20_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_moos20 <- moos.2020.pt[ Moos_depth_20_WR, roll = "nearest" ]

rounded.dates_moos20_WR_PT <- rounded.dates_moos20 %>%
  select(datetimeAK, WaterLevel, meanDepth)

#convert to meters
rounded.dates_moos20_WR_PT$meanDepth <- rounded.dates_moos20_WR_PT$meanDepth /100

moos_pt_wr_graph <- ggplot(rounded.dates_moos20_WR_PT, aes(WaterLevel, meanDepth)) +
  geom_point()
# Add regression line
moos_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/moos_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


moos20_depth_mod <- lm(meanDepth~WaterLevel, data = rounded.dates_moos20_WR_PT)

summary(moos20_depth_mod)

plot(meanDepth~WaterLevel, data = rounded.dates_moos20_WR_PT)
abline(moos20_depth_mod)

#extract slope of model and develop rating curve
moos.2020.pt$RatingCurveDepth <- moos20_depth_mod$coefficients[1]+ (moos20_depth_mod$coefficients[2])*moos.2020.pt$WaterLevel




# Strt 2020:

#download flowmeter data
WR_20_strt.url <- "https://drive.google.com/drive/u/1/folders/1x_E4gaPvjRLDcrM8lN2ao4_o0bzdMBrb"
WR.20_strt.1 <- drive_get(as_id(WR_20_strt.url))
strt.wr20_glist <- drive_ls(WR.20_strt.1, pattern = "R_Flowmeter Q calculation_STRT_for_R_JAA.csv")
walk(strt.wr20_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt_WR_20.Data <- read.csv("R_Flowmeter Q calculation_STRT_for_R_JAA.csv",
                            skip = 0, header = TRUE, na.strings=c("","NA","blank"))


strt_WR_20.Data <- strt_WR_20.Data %>% tidyr::fill(Time, .direction = ("down"))


strt_WR_20.Data$datetimeAK <- as.POSIXct(paste(as.character(strt_WR_20.Data$ï..Date), strt_WR_20.Data$Time), format="%y%m%d %H:%M")

strt_WR_20.Data <- strt_WR_20.Data %>%
  dplyr::rename(Depth..cm. = Depth)

strt_WR_20.Data <- strt_WR_20.Data %>%
  select(Depth..cm., datetimeAK)

Strt_depth_20_WR <- ddply(na.omit(strt_WR_20.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# strt_wr.lm <- lm(meanDepth~datetimeAK, Strt_depth_20)


Strt_depth_20_WR <- setDT(Strt_depth_20_WR)

Strt_depth_20_WR <- Strt_depth_20_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

strt.2020.pt <- strt.2020.pt %>%
  dplyr::rename(datetimeAK = DateTime)

strt.2020.pt <- setDT(strt.2020.pt)

setDT(Strt_depth_20)
setDT(strt.2020.pt)

strt.2020.pt$datetimeAK <- as.POSIXct(strt.2020.pt$datetimeAK)

strt.2020.pt$datetimeAK1 <- strt.2020.pt$datetimeAK


setkey( strt.2020.pt, datetimeAK )
setkey( Strt_depth_20_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_strt20 <- strt.2020.pt[ Strt_depth_20_WR, roll = "nearest" ]

rounded.dates_strt20_WR_PT <- rounded.dates_strt20 %>%
  select(datetimeAK, AvgAbsDepth, meanDepth)

#convert to meters
rounded.dates_strt20_WR_PT$meanDepth <- rounded.dates_strt20_WR_PT$meanDepth /100

strt_pt_wr_graph <- ggplot(rounded.dates_strt20_WR_PT, aes(AvgAbsDepth, meanDepth)) +
  geom_point()
# Add regression line
strt_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/strt_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


strt20_depth_mod <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_strt20_WR_PT)

summary(strt20_depth_mod)

plot(meanDepth~AvgAbsDepth, data = rounded.dates_strt20_WR_PT)
abline(strt20_depth_mod)

#extract slope of model and develop rating curve
strt.2020.pt$RatingCurveDepth <- strt20_depth_mod$coefficients[1]+ (strt20_depth_mod$coefficients[2])*strt.2020.pt$AvgAbsDepth

#Curve is being skewed by a single point in november, maybe due to ice or other conditions. Plot below is without that point. It appears to be an outlier.

rounded.dates_strt20_WR_PT_2 <- rounded.dates_strt20_WR_PT[-c(9), ]

strt20_depth_mod2 <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_strt20_WR_PT_2)


strt_pt_wr_graph2 <- ggplot(rounded.dates_strt20_WR_PT_2, aes(AvgAbsDepth, meanDepth)) +
  geom_point()
# Add regression line
strt_pt_wr_graph2 + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/strt_pt_wr_graph2.png", width = 15, height = 10, units = "cm", scale = 1.3)


strt20_depth_mod2 <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_strt20_WR_PT_2)

summary(strt20_depth_mod2)

plot(meanDepth~AvgAbsDepth, data = rounded.dates_strt20_WR_PT_2)
abline(strt20_depth_mod2)

#extract slope of model and develop rating curve
strt.2020.pt$RatingCurveDepth2 <- strt20_depth_mod2$coefficients[1]+ (strt20_depth_mod2$coefficients[2])*strt.2020.pt$AvgAbsDepth


#add original back in to compare

strt.2020.pt$RatingCurveDepth <- strt20_depth_mod$coefficients[1]+ (strt20_depth_mod$coefficients[2])*strt.2020.pt$AvgAbsDepth

#it is bad. use original
strt.2020.pt$RatingCurveDepth <- strt.2020.pt$RatingCurveDepth2


#### 2021 ####

# Poke 2021:

#download flowmeter data
WR_21_poke.url <- "https://drive.google.com/drive/u/1/folders/18z6vSz6SE3DEvUVDyfM8I3gqkGaxQqOl"
WR.21_poke.1 <- drive_get(as_id(WR_21_poke.url))
poke.wr21_glist <- drive_ls(WR.21_poke.1, pattern = "R_Flowmeter Q calculation_POKE_for_R_JAA_2021.csv")
walk(poke.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
poke_WR_21.Data <- read.csv("R_Flowmeter Q calculation_POKE_for_R_JAA_2021.csv",
                            skip = 0, header = TRUE, na.strings=c("","NA","blank"))


poke_WR_21.Data <- poke_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))


poke_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(poke_WR_21.Data$ï..Date), poke_WR_21.Data$Time), format="%y%m%d %H:%M")

poke_WR_21.Data <- poke_WR_21.Data %>%
  dplyr::rename(Depth..cm. = Depth)

poke_WR_21.Data <- poke_WR_21.Data %>%
  select(Depth..cm., datetimeAK)

Poke_depth_21_WR <- ddply(na.omit(poke_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# poke_wr.lm <- lm(meanDepth~datetimeAK, Poke_depth_21)


Poke_depth_21_WR <- setDT(Poke_depth_21_WR)

Poke_depth_21_WR <- Poke_depth_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

poke.2021.pt <- poke.2021.pt %>%
  dplyr::rename(datetimeAK = DateTime)

poke.2021.pt <- setDT(poke.2021.pt)

setDT(Poke_depth_21)
setDT(poke.2021.pt)

poke.2021.pt$datetimeAK1 <- poke.2021.pt$datetimeAK

setkey( poke.2021.pt, datetimeAK )
setkey( Poke_depth_21_WR, datetimeAK )

#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_poke21 <- poke.2021.pt[ Poke_depth_21_WR, roll = "nearest" ]

rounded.dates_poke21_WR_PT <- rounded.dates_poke21 %>%
  select(datetimeAK, AvgAbsDepth, meanDepth)

#convert to meters
rounded.dates_poke21_WR_PT$meanDepth <- rounded.dates_poke21_WR_PT$meanDepth /100

poke_pt_wr_graph <- ggplot(rounded.dates_poke21_WR_PT, aes(AvgAbsDepth, meanDepth)) +
  geom_point()
# Add regression line
poke_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/poke_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


poke21_depth_mod <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_poke21_WR_PT)

summary(poke21_depth_mod)

plot(meanDepth~AvgAbsDepth, data = rounded.dates_poke21_WR_PT)
abline(poke21_depth_mod)

#extract slope of model and develop rating curve
poke.2021.pt$RatingCurveDepth <- poke21_depth_mod$coefficients[1]+ (poke21_depth_mod$coefficients[2])*poke.2021.pt$AvgAbsDepth



# Vaul 2021:

#download flowmeter data
WR_21_vaul.url <- "https://drive.google.com/drive/u/1/folders/13avby555rryYttGDgO8sKE7umZPmo5uB"
WR.21_vaul.1 <- drive_get(as_id(WR_21_vaul.url))
vaul.wr21_glist <- drive_ls(WR.21_vaul.1, pattern = "R_Flowmeter Q calculation_VAUL_for_R_JAA_2021.csv")
walk(vaul.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
vaul_WR_21.Data <- read.csv("R_Flowmeter Q calculation_VAUL_for_R_JAA_2021.csv",
                            skip = 0, header = TRUE, na.strings=c("","NA","blank"))


vaul_WR_21.Data <- vaul_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))


vaul_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(vaul_WR_21.Data$ï..Date), vaul_WR_21.Data$Time), format="%y%m%d %H:%M")

vaul_WR_21.Data <- vaul_WR_21.Data %>%
  dplyr::rename(Depth..cm. = Depth)

vaul_WR_21.Data <- vaul_WR_21.Data %>%
  select(Depth..cm., datetimeAK)

Vaul_depth_21_WR <- ddply(na.omit(vaul_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# vaul_wr.lm <- lm(meanDepth~datetimeAK, Vaul_depth_21)


Vaul_depth_21_WR <- setDT(Vaul_depth_21_WR)

Vaul_depth_21_WR <- Vaul_depth_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

vaul.2021.pt <- vaul.2021.pt %>%
  dplyr::rename(datetimeAK = DateTime)

vaul.2021.pt <- setDT(vaul.2021.pt)

setDT(Vaul_depth_21)
setDT(vaul.2021.pt)

vaul.2021.pt$datetimeAK1 <- vaul.2021.pt$datetimeAK

vaul.2021.pt$datetimeAK <- as.POSIXct(vaul.2021.pt$datetimeAK)

vaul.2021.pt$datetimeAK <- as.POSIXct(vaul.2021.pt$datetimeAK)

setkey( vaul.2021.pt, datetimeAK )
setkey( Vaul_depth_21_WR, datetimeAK )



#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_vaul21 <- vaul.2021.pt[Vaul_depth_21_WR, roll = "nearest" ]

rounded.dates_vaul21_WR_PT <- rounded.dates_vaul21 %>%
  select(datetimeAK, WaterLevel, meanDepth)

#convert to meters
rounded.dates_vaul21_WR_PT$meanDepth <- rounded.dates_vaul21_WR_PT$meanDepth /100

vaul_pt_wr_graph <- ggplot(rounded.dates_vaul21_WR_PT, aes(WaterLevel, meanDepth)) +
  geom_point()
# Add regression line
vaul_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/vaul_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


vaul21_depth_mod <- lm(meanDepth~WaterLevel, data = rounded.dates_vaul21_WR_PT)

summary(vaul21_depth_mod)

plot(meanDepth~WaterLevel, data = rounded.dates_vaul21_WR_PT)
abline(vaul21_depth_mod)

#extract slope of model and develop rating curve
vaul.2021.pt$RatingCurveDepth <- vaul21_depth_mod$coefficients[1]+ (vaul21_depth_mod$coefficients[2])*vaul.2021.pt$WaterLevel







# Moos 2021:

#download flowmeter data
WR_21_moos.url <- "https://drive.google.com/drive/u/1/folders/1-S_ixEutlA7RKfrvhi15aIBLrjYjg5O_"
WR.21_moos.1 <- drive_get(as_id(WR_21_moos.url))
moos.wr21_glist <- drive_ls(WR.21_moos.1, pattern = "R_Flowmeter Q calculation_MOOS_for_R_JAA_2021.csv")
walk(moos.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
moos_WR_21.Data <- read.csv("R_Flowmeter Q calculation_MOOS_for_R_JAA_2021.csv",
                            skip = 0, header = TRUE, na.strings=c("","NA","blank"))


moos_WR_21.Data <- moos_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))


moos_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(moos_WR_21.Data$ï..Date), moos_WR_21.Data$Time), format="%y%m%d %H:%M")

moos_WR_21.Data <- moos_WR_21.Data %>%
  dplyr::rename(Depth..cm. = Depth)

moos_WR_21.Data <- moos_WR_21.Data %>%
  select(Depth..cm., datetimeAK)

Moos_depth_21_WR <- ddply(na.omit(moos_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# moos_wr.lm <- lm(meanDepth~datetimeAK, Moos_depth_21)


Moos_depth_21_WR <- setDT(Moos_depth_21_WR)

Moos_depth_21_WR <- Moos_depth_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

moos.2021.pt <- moos.2021.pt %>%
  dplyr::rename(datetimeAK = DateTime)

moos.2021.pt <- setDT(moos.2021.pt)

setDT(Moos_depth_21)
setDT(moos.2021.pt)

moos.2021.pt$datetimeAK1 <- moos.2021.pt$datetimeAK

moos.2021.pt$datetimeAK <- as.POSIXct(moos.2021.pt$datetimeAK)

moos.2021.pt$datetimeAK <- as.POSIXct(moos.2021.pt$datetimeAK)

setkey( moos.2021.pt, datetimeAK )
setkey( Moos_depth_21_WR, datetimeAK )



#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_moos21 <- moos.2021.pt[Moos_depth_21_WR, roll = "nearest" ]

rounded.dates_moos21_WR_PT <- rounded.dates_moos21 %>%
  select(datetimeAK, AvgAbsDepth, meanDepth)

#convert to meters
rounded.dates_moos21_WR_PT$meanDepth <- rounded.dates_moos21_WR_PT$meanDepth /100

moos_pt_wr_graph <- ggplot(rounded.dates_moos21_WR_PT, aes(AvgAbsDepth, meanDepth)) +
  geom_point()
# Add regression line
moos_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/moos_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


moos21_depth_mod <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_moos21_WR_PT)

summary(moos21_depth_mod)

plot(meanDepth~AvgAbsDepth, data = rounded.dates_moos21_WR_PT)
abline(moos21_depth_mod)

#extract slope of model and develop rating curve
moos.2021.pt$RatingCurveDepth <- moos21_depth_mod$coefficients[1]+ (moos21_depth_mod$coefficients[2])*moos.2021.pt$AvgAbsDepth





# Frch 2021:

#download flowmeter data
WR_21_frch.url <- "https://drive.google.com/drive/u/1/folders/1MrFabu9Mzuv3v4naPl2-iCFaMj_DjkZG"
WR.21_frch.1 <- drive_get(as_id(WR_21_frch.url))
frch.wr21_glist <- drive_ls(WR.21_frch.1, pattern = "R_Flowmeter_Q_calulation_FRCH_for_R_JAA_2021.csv")
walk(frch.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
frch_WR_21.Data <- read.csv("R_Flowmeter_Q_calulation_FRCH_for_R_JAA_2021.csv",
                            skip = 0, header = TRUE, na.strings=c("","NA","blank"))


frch_WR_21.Data <- frch_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))


frch_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(frch_WR_21.Data$ï..Date), frch_WR_21.Data$Time), format="%y%m%d %H:%M")

frch_WR_21.Data <- frch_WR_21.Data %>%
  dplyr::rename(Depth..cm. = Depth)

frch_WR_21.Data <- frch_WR_21.Data %>%
  select(Depth..cm., datetimeAK)

Frch_depth_21_WR <- ddply(na.omit(frch_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# frch_wr.lm <- lm(meanDepth~datetimeAK, Frch_depth_21)


Frch_depth_21_WR <- setDT(Frch_depth_21_WR)

Frch_depth_21_WR <- Frch_depth_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

frch.2021.pt <- frch.2021.pt %>%
  dplyr::rename(datetimeAK = DateTime)

frch.2021.pt <- setDT(frch.2021.pt)

setDT(Frch_depth_21)
setDT(frch.2021.pt)

frch.2021.pt$datetimeAK1 <- frch.2021.pt$datetimeAK

frch.2021.pt$datetimeAK <- as.POSIXct(frch.2021.pt$datetimeAK)

frch.2021.pt$datetimeAK <- as.POSIXct(frch.2021.pt$datetimeAK)

setkey( frch.2021.pt, datetimeAK )
setkey( Frch_depth_21_WR, datetimeAK )



#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_frch21 <- frch.2021.pt[Frch_depth_21_WR, roll = "nearest" ]

rounded.dates_frch21_WR_PT <- rounded.dates_frch21 %>%
  select(datetimeAK, AvgAbsDepth, meanDepth)

#convert to meters
rounded.dates_frch21_WR_PT$meanDepth <- rounded.dates_frch21_WR_PT$meanDepth /100

frch_pt_wr_graph <- ggplot(rounded.dates_frch21_WR_PT, aes(AvgAbsDepth, meanDepth)) +
  geom_point()
# Add regression line
frch_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/frch_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


frch21_depth_mod <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_frch21_WR_PT)

summary(frch21_depth_mod)

plot(meanDepth~AvgAbsDepth, data = rounded.dates_frch21_WR_PT)
abline(frch21_depth_mod)

#extract slope of model and develop rating curve
frch.2021.pt$RatingCurveDepth <- frch21_depth_mod$coefficients[1]+ (frch21_depth_mod$coefficients[2])*frch.2021.pt$AvgAbsDepth






# Strt 2021:

#download flowmeter data
WR_21_strt.url <- "https://drive.google.com/drive/u/1/folders/1LTD4EFX3_Yas0ZCF8rKLl6dxSeZDvkDY"
WR.21_strt.1 <- drive_get(as_id(WR_21_strt.url))
strt.wr21_glist <- drive_ls(WR.21_strt.1, pattern = "R_Flowmeter Q calculation_STRT_for_R_JAA_2021.csv")
walk(strt.wr21_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
strt_WR_21.Data <- read.csv("R_Flowmeter Q calculation_STRT_for_R_JAA_2021.csv",
                            skip = 0, header = TRUE, na.strings=c("","NA","blank"))


strt_WR_21.Data <- strt_WR_21.Data %>% tidyr::fill(Time, .direction = ("down"))


strt_WR_21.Data$datetimeAK <- as.POSIXct(paste(as.character(strt_WR_21.Data$Date), strt_WR_21.Data$Time), format="%y%m%d %H:%M")

strt_WR_21.Data <- strt_WR_21.Data %>%
  dplyr::rename(Depth..cm. = Depth)

strt_WR_21.Data <- strt_WR_21.Data %>%
  select(Depth..cm., datetimeAK)

Strt_depth_21_WR <- ddply(na.omit(strt_WR_21.Data), .(datetimeAK), summarize, meanDepth = mean(as.numeric(Depth..cm.)))

# strt_wr.lm <- lm(meanDepth~datetimeAK, Strt_depth_21)


Strt_depth_21_WR <- setDT(Strt_depth_21_WR)

Strt_depth_21_WR <- Strt_depth_21_WR %>%
  dplyr::rename(datetimeAK = datetimeAK)

strt.2021.pt <- strt.2021.pt %>%
  dplyr::rename(datetimeAK = DateTime)

strt.2021.pt <- setDT(strt.2021.pt)

setDT(Strt_depth_21)
setDT(strt.2021.pt)

strt.2021.pt$datetimeAK1 <- strt.2021.pt$datetimeAK

strt.2021.pt$datetimeAK <- as.POSIXct(strt.2021.pt$datetimeAK)

strt.2021.pt$datetimeAK <- as.POSIXct(strt.2021.pt$datetimeAK)

setkey( strt.2021.pt, datetimeAK )
setkey( Strt_depth_21_WR, datetimeAK )



#WR was taken when EXO out of water. round depth point to nearest in data record
rounded.dates_strt21 <- strt.2021.pt[Strt_depth_21_WR, roll = "nearest" ]

rounded.dates_strt21_WR_PT <- rounded.dates_strt21 %>%
  select(datetimeAK, AvgAbsDepth, meanDepth)

#convert to meters
rounded.dates_strt21_WR_PT$meanDepth <- rounded.dates_strt21_WR_PT$meanDepth /100

strt_pt_wr_graph <- ggplot(rounded.dates_strt21_WR_PT, aes(AvgAbsDepth, meanDepth)) +
  geom_point()
# Add regression line
strt_pt_wr_graph + geom_smooth(method = lm) + xlab("Depth (PT)") +ylab ("Depth (WR)")

ggsave("plots/strt_pt_wr_graph.png", width = 15, height = 10, units = "cm", scale = 1.3)


strt21_depth_mod <- lm(meanDepth~AvgAbsDepth, data = rounded.dates_strt21_WR_PT)

summary(strt21_depth_mod)

plot(meanDepth~AvgAbsDepth, data = rounded.dates_strt21_WR_PT)
abline(strt21_depth_mod)

#extract slope of model and develop rating curve
strt.2021.pt$RatingCurveDepth <- strt21_depth_mod$coefficients[1]+ (strt21_depth_mod$coefficients[2])*strt.2021.pt$AvgAbsDepth









#### combine ####

poke.2019.depth <- poke.2019.pt %>%
  select(RatingCurveDepth, datetimeAK)

poke.2020.depth <- poke.2020.pt %>%
  select(RatingCurveDepth, datetimeAK)

poke.2021.depth <- poke.2021.pt %>%
  select(RatingCurveDepth, datetimeAK)

poke.depth <- rbind (poke.2019.depth, poke.2020.depth, poke.2021.depth)




vaul.2019.depth <- vaul.2019.pt %>%
  select(RatingCurveDepth, datetimeAK)

vaul.2020.depth <- vaul.2020.pt %>%
  select(RatingCurveDepth, datetimeAK)

vaul.2020.depth <- vaul.2020.depth %>% filter(datetimeAK <= "2020-10-14 03:45:00")

vaul.2021.depth <- vaul.2021.pt %>%
  select(RatingCurveDepth, datetimeAK)

vaul.depth <- rbind (vaul.2019.depth, vaul.2020.depth, vaul.2021.depth)



moos.2019.depth <- moos.2019.pt %>%
  select(RatingCurveDepth, datetimeAK)

moos.2020.depth <- moos.2020.pt %>%
  select(RatingCurveDepth, datetimeAK)

moos.2020.depth <- moos.2020.depth %>% filter(datetimeAK <= "2020-10-15 08:45:00")



moos.2021.depth <- moos.2021.pt %>%
  select(RatingCurveDepth, datetimeAK)

moos.depth <- rbind (moos.2019.depth, moos.2020.depth, moos.2021.depth)




frch.2019.depth <- frch.2019.pt %>%
  select(RatingCurveDepth, datetimeAK)

frch.2020.depth <- frch.2020.pt %>%
  select(RatingCurveDepth, datetimeAK)

frch.2020.depth <- frch.2020.depth %>% filter(datetimeAK <= "2020-10-15 02:30:00")


frch.2021.depth <- frch.2021.pt %>%
  select(RatingCurveDepth, datetimeAK)

frch.depth <- rbind (frch.2019.depth, frch.2020.depth, frch.2021.depth)



strt.2019.depth <- strt.2019.pt %>%
  select(RatingCurveDepth, datetimeAK)

strt.2020.depth <- strt.2020.pt %>%
  select(RatingCurveDepth, datetimeAK)

strt.2020.depth <- strt.2020.depth %>% filter(datetimeAK <= "2020-10-13 06:30:00")

strt.2021.depth <- strt.2021.pt %>%
  select(RatingCurveDepth, datetimeAK)

strt.depth <- rbind (strt.2019.depth, strt.2020.depth, strt.2021.depth)

