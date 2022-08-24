#### Combine and stitch discharge ####

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
library(here)

#Discharge data cleaned and scripted by Jake in DoD Discharge Repo


####### 2019 #######

Q2019.url <- "https://drive.google.com/drive/u/1/folders/1ww0WENY9u_iHbx5RvVuOp1po9RvtzC7j"
q.2019.prt1 <- drive_get(as_id(Q2019.url))
q.2019.glist <- drive_ls(q.2019.prt1, pattern = "Q_2019.csv")
walk(q.2019.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
Q_2019 <- read.csv("Q_2019.csv",)

#### Breaking up into sites ####
POKE_Q_2019 <- Q_2019 %>% filter(Site == "POKE")
VAUL_Q_2019 <- Q_2019 %>% filter(Site == "VAUL")
STRT_Q_2019 <- Q_2019 %>% filter(Site == "STRT")
MOOS_Q_2019 <- Q_2019 %>% filter(Site == "MOOS")
FRCH_Q_2019 <- Q_2019 %>% filter(Site == "FRCH")


#change poke discharge becuase of gaps
POKE_Q_2019 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/DoD_Discharge/Predicted_Discharge/2019/POKE/POKE.Q.csv")

VAUL_Q_2019$Q <- VAUL_Q_2019$MeanDischarge
STRT_Q_2019$Q <- STRT_Q_2019$MeanDischarge
MOOS_Q_2019$Q <- MOOS_Q_2019$MeanDischarge
FRCH_Q_2019$Q <- FRCH_Q_2019$MeanDischarge

#Select needed columns 
POKE.2019.Q <- POKE_Q_2019 %>%
  select(DateTime, Q, Site)

VAUL.2019.Q <- VAUL_Q_2019 %>%
  select(DateTime, Q, Site)

STRT.2019.Q <- STRT_Q_2019 %>%
  select(DateTime, Q, Site)

MOOS.2019.Q <- MOOS_Q_2019 %>%
  select(DateTime, Q, Site)

FRCH.2019.Q <- FRCH_Q_2019 %>%
  select(DateTime, Q, Site)


####### 2020 #######

#### Breaking up into sites ####
### POKE ###

poke_q_2020.url <- "https://drive.google.com/drive/u/1/folders/18UaoUOnZYKXo0Ea7BAyV2Ow7w3ArfS89"
poke_q_2020.prt1 <- drive_get(as_id(poke_q_2020.url))
poke.2020.q.glist <- drive_ls(poke_q_2020.prt1, pattern = "POKE.Q.csv")
walk(poke.2020.q.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
POKE.2020.Q <- read.csv("POKE.Q.csv",)

### VAUL ###
vaul_q_2020.url <- "https://drive.google.com/drive/u/1/folders/1SCdGe_2MUpoeNcpFI4kZbUa3FK-T4El0"
vaul_q_2020.prt1 <- drive_get(as_id(vaul_q_2020.url))
vaul.2020.q.glist <- drive_ls(vaul_q_2020.prt1, pattern = "VAUL.Q.csv")
walk(vaul.2020.q.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
VAUL.2020.Q <- read.csv("VAUL.Q.csv",)


### STRT ###

strt_q_2020.url <- "https://drive.google.com/drive/u/1/folders/1A4-Rw0ZQ0kAxKb7WkBm2VnDFT8fa1RsJ"
strt_q_2020.prt1 <- drive_get(as_id(strt_q_2020.url))
strt.2020.q.glist <- drive_ls(strt_q_2020.prt1, pattern = "STRT.Q.csv")
walk(strt.2020.q.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
STRT.2020.Q <- read.csv("STRT.Q.csv",)

### MOOS ###

moos_q_2020.url <- "https://drive.google.com/drive/u/1/folders/1ArCib8d1B4cBAeit-GBZG2Em8-jTBBmi"
moos_q_2020.prt1 <- drive_get(as_id(moos_q_2020.url))
moos.2020.q.glist <- drive_ls(moos_q_2020.prt1, pattern = "MOOS.Q.csv")
walk(moos.2020.q.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
MOOS.2020.Q <- read.csv("MOOS.Q.csv",)


### FRCH ###

frch_q_2020.url <- "https://drive.google.com/drive/u/1/folders/1X5ejz_Ia7jwqAk6jT8Xb9a5EknSV8bF-"
frch_q_2020.prt1 <- drive_get(as_id(frch_q_2020.url))
frch.2020.q.glist <- drive_ls(frch_q_2020.prt1, pattern = "FRCH.Q.csv")
walk(frch.2020.q.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
FRCH.2020.Q <- read.csv("FRCH.Q.csv",)


####### 2021 #######

#### Breaking up into sites ####
### POKE ###

poke_q_2021.url <- "https://drive.google.com/drive/u/1/folders/1KIgfC8CCUW7bj1DcecOtZzf85OYP6lI7"
poke_q_2021.prt1 <- drive_get(as_id(poke_q_2021.url))
poke.2021.q.glist <- drive_ls(poke_q_2021.prt1, pattern = "POKE.Q.csv")
walk(poke.2021.q.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
POKE.2021.Q <- read.csv("POKE.Q.csv",)

### VAUL ###
vaul_q_2021.url <- "https://drive.google.com/drive/u/1/folders/1USPfRoW9Pc_7-CWd6PDTQ2sYHBSUkvjI"
vaul_q_2021.prt1 <- drive_get(as_id(vaul_q_2021.url))
vaul.2021.q.glist <- drive_ls(vaul_q_2021.prt1, pattern = "VAUL.Q.csv")
walk(vaul.2021.q.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
VAUL.2021.Q <- read.csv("VAUL.Q.csv",)


### STRT ###

strt_q_2021.url <- "https://drive.google.com/drive/u/1/folders/1CUb8n8lmhRt4-Z0iiuWB9rcd-TRBJ1mI"
strt_q_2021.prt1 <- drive_get(as_id(strt_q_2021.url))
strt.2021.q.glist <- drive_ls(strt_q_2021.prt1, pattern = "STRT.Q.csv")
walk(strt.2021.q.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
STRT.2021.Q <- read.csv("STRT.Q.csv",)

### MOOS ###

moos_q_2021.url <- "https://drive.google.com/drive/u/1/folders/1IJ_l63IM_L7-5o1gZ_TbY-a1t52OoZXn"
moos_q_2021.prt1 <- drive_get(as_id(moos_q_2021.url))
moos.2021.q.glist <- drive_ls(moos_q_2021.prt1, pattern = "MOOS.Q.csv")
walk(moos.2021.q.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
MOOS.2021.Q <- read.csv("MOOS.Q.csv",)


### FRCH ###

frch_q_2021.url <- "https://drive.google.com/drive/u/1/folders/1tB71qTZa8Eq1W4lnVsHl-NWSKtRr5eZ4"
frch_q_2021.prt1 <- drive_get(as_id(frch_q_2021.url))
frch.2021.q.glist <- drive_ls(frch_q_2021.prt1, pattern = "FRCH.Q.csv")
walk(frch.2021.q.glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
FRCH.2021.Q <- read.csv("FRCH.Q.csv",)




########## Combine ##########
getwd()

#Poke
POKE.ALL.Q <- rbind(POKE.2019.Q, POKE.2020.Q, POKE.2021.Q)
POKE.ALL.Q$DateTime <-  as.POSIXct(POKE.ALL.Q$DateTime)



write.csv(POKE.ALL.Q,"Predicted_Discharge/Combined after 2019/POKE.ALL.Q.csv", row.names = FALSE)

tiff("Q_Plots/PokeQ_all_years.tiff", compression = "lzw", width = 1500, height =1000)
plot(POKE.ALL.Q$DateTime, POKE.ALL.Q$Q, main = "Poke Discharge", xlab = "Date", ylab = "Predicted Discahrge (m^3/s)")
dev.off()


#Vaul
VAUL.ALL.Q <- rbind(VAUL.2019.Q, VAUL.2020.Q, VAUL.2021.Q)
VAUL.ALL.Q$DateTime <-  as.POSIXct(VAUL.ALL.Q$DateTime)

write.csv(VAUL.ALL.Q,"Predicted_Discharge/Combined after 2019/VAUL.ALL.Q.csv", row.names = FALSE)

tiff("Q_Plots/VaulQ_all_years.tiff", compression = "lzw", width = 1500, height =1000)
plot(VAUL.ALL.Q$DateTime, VAUL.ALL.Q$Q, main = "Vaul Discharge", xlab = "Date", ylab = "Predicted Discahrge (m^3/s)")
dev.off()


#Strt
STRT.ALL.Q <- rbind(STRT.2019.Q, STRT.2020.Q, STRT.2021.Q)
STRT.ALL.Q$DateTime <-  as.POSIXct(STRT.ALL.Q$DateTime)

write.csv(STRT.ALL.Q,"Predicted_Discharge/Combined after 2019/STRT.ALL.Q.csv", row.names = FALSE)

tiff("Q_Plots/StrtQ_all_years.tiff", compression = "lzw", width = 1500, height =1000)
plot(STRT.ALL.Q$DateTime, STRT.ALL.Q$Q, main = "Strt Discharge", xlab = "Date", ylab = "Predicted Discahrge (m^3/s)")
dev.off()


#Moos
MOOS.ALL.Q <- rbind(MOOS.2019.Q, MOOS.2020.Q, MOOS.2021.Q)
MOOS.ALL.Q$DateTime <-  as.POSIXct(MOOS.ALL.Q$DateTime)

write.csv(MOOS.ALL.Q,"Predicted_Discharge/Combined after 2019/MOOS.ALL.Q.csv", row.names = FALSE)

tiff("Q_Plots/MoosQ_all_years.tiff", compression = "lzw", width = 1500, height =1000)
plot(MOOS.ALL.Q$DateTime, MOOS.ALL.Q$Q, main = "Moos Discharge", xlab = "Date", ylab = "Predicted Discahrge (m^3/s)")
dev.off()


#Frch
FRCH.ALL.Q <- rbind(FRCH.2019.Q, FRCH.2020.Q, FRCH.2021.Q)
FRCH.ALL.Q$DateTime <-  as.POSIXct(FRCH.ALL.Q$DateTime)

write.csv(FRCH.ALL.Q,"Predicted_Discharge/Combined after 2019/FRCH.ALL.Q.csv", row.names = FALSE)

tiff("Q_Plots/FrchQ_all_years.tiff", compression = "lzw", width = 1500, height =1000)
plot(FRCH.ALL.Q$DateTime, FRCH.ALL.Q$Q, main = "Frch Discharge", xlab = "Date", ylab = "Predicted Discahrge (m^3/s)")
dev.off()




