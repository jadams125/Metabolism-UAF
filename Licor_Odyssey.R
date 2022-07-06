#Light Plotting

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


#Light Data NEW Logger

#Logger startign with f5
epscorPAR.url <- "https://drive.google.com/drive/u/2/folders/1lfXbcLm47wgJzVaauOPK5iS7XgwMe9ap"
f5_part1 <- drive_get(as_id(epscorPAR.url))
f5_glist <- drive_ls(f5_part1, pattern = "jadams125@alaska.edu_F538FD770D95_1653444782959.csv")
walk(f5_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
f5.Data.1 <- read.csv("jadams125@alaska.edu_F538FD770D95_1653444782959.csv",
                           skip = 0, header = TRUE)


f5.Data.1$dateTime <-  dmy_hms(f5.Data.1$dateTime)
f5.Data.1$dateTime <- force_tz(f5.Data.1$dateTime, "America/Anchorage")




#LICOR PAR Data
epscorPAR.url <- "https://drive.google.com/drive/u/2/folders/1lfXbcLm47wgJzVaauOPK5iS7XgwMe9ap"
PAR_part1 <- drive_get(as_id(epscorPAR.url))
PAR_glist <- drive_ls(PAR_part1, pattern = "GOF_Table1_2022-05-23T11-05.dat")
walk(PAR_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
PAR.Data.1 <- read.csv("GOF_Table1_2022-05-23T11-05.dat",
                      skip = 3, header = TRUE)

PAR.Data.1$dateTime <-  as_datetime(PAR.Data.1$X)
PAR.Data.1$dateTime <- force_tz(PAR.Data.1$dateTime, "America/Anchorage")


LICOR_F5 <- na.omit(plyr::join(PAR.Data.1, f5.Data.1, by = "dateTime"))





plot(LICOR_F5$dateTime,LICOR_F5$data2, xlab="Date", ylab="COUNTS", main = "Counts from Odyssey #F5")

plot(LICOR_F5$dateTime,LICOR_F5$Avg, xlab="Date", ylab="Avg µmol of photons m-2 s-1", main = "Total PAR from LICOR")

plot(LICOR_F5$Avg, LICOR_F5$data2, xlab = "Avg µmol of photons m-2 s-1", ylab = "f5 Odyssey (Counts)")




Test1 <- LICOR_F5

# subset data - Sat
Test1 <- subset(Test1,
                         Test1$dateTime >= as.POSIXct('2022-05-22 00:00',
                                                tz = "America/Anchorage") &
                  Test1$dateTime <= as.POSIXct('2022-05-23 00:00',
                                                  tz = "America/Anchorage"))


plot(Test1$dateTime,Test1$data2, xlab="Date", ylab="COUNTS", main = "Counts from Odyssey #F5")

plot(Test1$dateTime,Test1$Avg, xlab="Date", ylab="LICOR (µmol of photons m-2 s-1)", main = "Total PAR from LICOR")

plot(Test1$Avg, Test1$data2, xlab = "LICOR (µmol of photons m-2 s-1)", ylab = "f5 Odyssey (Counts)", main = "midnight am to midnight (5/21-22)")






#################
###
#Logger startign with E2
epscorPAR.url <- "https://drive.google.com/drive/u/2/folders/1lfXbcLm47wgJzVaauOPK5iS7XgwMe9ap"
e2_part1 <- drive_get(as_id(epscorPAR.url))
e2_glist <- drive_ls(e2_part1, pattern = "jadams125@alaska.edu_E246BC5B819D_1653444799997.csv")
walk(e2_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
e2.Data.1 <- read.csv("jadams125@alaska.edu_E246BC5B819D_1653444799997.csv",
                      skip = 0, header = TRUE)


e2.Data.1$dateTime <-  dmy_hms(e2.Data.1$dateTime)
e2.Data.1$dateTime <- force_tz(e2.Data.1$dateTime, "America/Anchorage")




#LICOR PAR Data
epscorPAR.url <- "https://drive.google.com/drive/u/2/folders/1lfXbcLm47wgJzVaauOPK5iS7XgwMe9ap"
PAR_part1 <- drive_get(as_id(epscorPAR.url))
PAR_glist <- drive_ls(PAR_part1, pattern = "GOF_Table1_2022-05-23T11-05.dat")
walk(PAR_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
PAR.Data.1 <- read.csv("GOF_Table1_2022-05-23T11-05.dat",
                       skip = 3, header = TRUE)

PAR.Data.1$dateTime <-  as_datetime(PAR.Data.1$X)
PAR.Data.1$dateTime <- force_tz(PAR.Data.1$dateTime, "America/Anchorage")


LICOR_e2 <- na.omit(plyr::join(PAR.Data.1, e2.Data.1, by = "dateTime"))





plot(LICOR_e2$dateTime,LICOR_e2$data2, xlab="Date", ylab="COUNTS", main = "Counts from Odyssey #e2")

plot(LICOR_e2$dateTime,LICOR_e2$Avg, xlab="Date", ylab="Avg µmol of photons m-2 s-1", main = "Total PAR from LICOR")

plot(LICOR_e2$Avg, LICOR_e2$data2, xlab = "Avg µmol of photons m-2 s-1", ylab = "e2 Odyssey (Counts)", main = "Full weekend")




Test2 <- LICOR_e2

# subset data - Sat
Test2 <- subset(Test2,
                Test2$dateTime >= as.POSIXct('2022-05-22 00:00',
                                            tz = "America/Anchorage") &
                  Test2$dateTime <= as.POSIXct('2022-05-23 00:00',
                                              tz = "America/Anchorage"))


plot(Test2$dateTime,Test2$data2, xlab="Date", ylab="COUNTS", main = "Counts from Odyssey #E2")

plot(Test2$dateTime,Test2$Avg, xlab="Date", ylab="Avg µmol of photons m-2 s-1", main = "Average PAR from LICOR")

plot(Test2$Avg, Test2$data2, xlab = "Avg µmol of photons m-2 s-1", ylab = "f5 Odyssey (Counts)", main = "midnight to midnight (5/21-5/22)")









#################
###
#Logger startign with E4
epscorPAR.url <- "https://drive.google.com/drive/u/2/folders/1lfXbcLm47wgJzVaauOPK5iS7XgwMe9ap"
e4_part1 <- drive_get(as_id(epscorPAR.url))
e4_glist <- drive_ls(e4_part1, pattern = "jadams125@alaska.edu_E4B8C8919534_1653444792727.csv")
walk(e4_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
e4.Data.1 <- read.csv("jadams125@alaska.edu_E4B8C8919534_1653444792727.csv",
                      skip = 0, header = TRUE)


e4.Data.1$dateTime <-  dmy_hms(e4.Data.1$dateTime)
e4.Data.1$dateTime <- force_tz(e4.Data.1$dateTime, "America/Anchorage")




#LICOR PAR Data
epscorPAR.url <- "https://drive.google.com/drive/u/2/folders/1lfXbcLm47wgJzVaauOPK5iS7XgwMe9ap"
PAR_part1 <- drive_get(as_id(epscorPAR.url))
PAR_glist <- drive_ls(PAR_part1, pattern = "GOF_Table1_2022-05-23T11-05.dat")
walk(PAR_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
PAR.Data.1 <- read.csv("GOF_Table1_2022-05-23T11-05.dat",
                       skip = 3, header = TRUE)

PAR.Data.1$dateTime <-  as_datetime(PAR.Data.1$X)
PAR.Data.1$dateTime <- force_tz(PAR.Data.1$dateTime, "America/Anchorage")


LICOR_e4 <- na.omit(plyr::join(PAR.Data.1, e4.Data.1, by = "dateTime"))





plot(LICOR_e4$dateTime,LICOR_e4$data2, xlab="Date", ylab="COUNTS", main = "Counts from Odyssey #e2")

plot(LICOR_e4$dateTime,LICOR_e4$Avg, xlab="Date", ylab="LICOR (µmol of photons m-2 s-1)", main = "Total PAR from LICOR")

plot(LICOR_e4$Avg, LICOR_e4$data2, xlab = "LICOR (µmol of photons m-2 s-1)", ylab = "e4 Odyssey (Counts)")




Test3 <- LICOR_e4

# subset data - Sat
Test3 <- subset(Test3,
                Test3$dateTime >= as.POSIXct('2022-05-22 00:00',
                                            tz = "America/Anchorage") &
                  Test3$dateTime <= as.POSIXct('2022-05-23 00:00',
                                              tz = "America/Anchorage"))


plot(Test3$dateTime,Test3$data2, xlab="Date", ylab="COUNTS", main = "Counts from Odyssey #F5")

plot(Test3$dateTime,Test3$Avg, xlab="Date", ylab="LICOR (µmol of photons m-2 s-1)", main = "Total PAR from LICOR")

plot(Test3$Avg, Test3$data2, xlab = "LICOR (µmol of photons m-2 s-1)", ylab = "e4 Odyssey (Counts)", main = "midnight to midnight (5/21-5/22)")




#################
###
#Logger startign with C1
epscorPAR.url <- "https://drive.google.com/drive/u/2/folders/1lfXbcLm47wgJzVaauOPK5iS7XgwMe9ap"
c1_part1 <- drive_get(as_id(epscorPAR.url))
c1_glist <- drive_ls(c1_part1, pattern = "jadams125@alaska.edu_C1D8AFCAC00D_1653444807985.csv")
walk(c1_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
c1.Data.1 <- read.csv("jadams125@alaska.edu_C1D8AFCAC00D_1653444807985.csv",
                      skip = 0, header = TRUE)


c1.Data.1$dateTime <-  dmy_hms(c1.Data.1$dateTime)
c1.Data.1$dateTime <- force_tz(c1.Data.1$dateTime, "America/Anchorage")




#LICOR PAR Data
epscorPAR.url <- "https://drive.google.com/drive/u/2/folders/1lfXbcLm47wgJzVaauOPK5iS7XgwMe9ap"
PAR_part1 <- drive_get(as_id(epscorPAR.url))
PAR_glist <- drive_ls(PAR_part1, pattern = "GOF_Table1_2022-05-23T11-05.dat")
walk(PAR_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
PAR.Data.1 <- read.csv("GOF_Table1_2022-05-23T11-05.dat",
                       skip = 3, header = TRUE)

PAR.Data.1$dateTime <-  as_datetime(PAR.Data.1$X)
PAR.Data.1$dateTime <- force_tz(PAR.Data.1$dateTime, "America/Anchorage")


LICOR_c1 <- na.omit(plyr::join(PAR.Data.1, c1.Data.1, by = "dateTime"))





plot(LICOR_c1$dateTime,LICOR_c1$data2, xlab="Date", ylab="COUNTS", main = "Counts from Odyssey #c1")

plot(LICOR_c1$dateTime,LICOR_c1$Avg, xlab="Date", ylab="Avg µmol of photons m-2 s-1", main = "Total PAR from LICOR")

plot(LICOR_c1$Avg, LICOR_c1$data2, xlab = "Avg µmol of photons m-2 s-1", ylab = "c1 Odyssey (Counts)")




Test4 <- LICOR_c1

# subset data - Sat
Test4 <- subset(Test4,
                Test4$dateTime >= as.POSIXct('2022-05-22 00:00',
                                             tz = "America/Anchorage") &
                  Test4$dateTime <= as.POSIXct('2022-05-23 00:00',
                                               tz = "America/Anchorage"))


plot(Test4$dateTime,Test4$data2, xlab="Date", ylab="COUNTS", main = "Counts from Odyssey #c1")

plot(Test4$dateTime,Test4$Avg, xlab="Date", ylab="Avg µmol of photons m-2 s-1", main = "Total PAR from LICOR")

plot(Test4$Avg, Test4$data2, xlab = "Avg µmol of photons m-2 s-1", ylab = "c1 Odyssey (Counts)", main = "midnight to midnight (5/21-5/22)")



#All 4 New Loggers
plot(Test1$dateTime, Test1$data2)
plot(Test2$dateTime, Test2$data2)
plot(Test3$dateTime, Test3$data2)
plot(Test4$dateTime, Test4$data2)





###############################
####### AZ Loggers 

Az.loggers.url <- "https://drive.google.com/drive/u/2/folders/1JW8NRBDT-I9oIOayp3i4kGef9cP3nO3k"
AZ_part1 <- drive_get(as_id(Az.loggers.url))
AZ1_glist <- drive_ls(AZ_part1, pattern = "220526_7922.CSV")
walk(AZ1_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
AZ.7922.Data.1 <- read.csv("220526_7922.CSV",
                       skip = 8, header = FALSE)


AZ.7922.Data.1$dateTime <- paste(AZ.7922.Data.1$V2, AZ.7922.Data.1$V3, sep="")

PAR.Data.1$dateTime <-  as_datetime(AZ.7922.Data.1$dateTime)
PAR.Data.1$dateTime <- force_tz(PAR.Data.1$dateTime, "America/Anchorage")

