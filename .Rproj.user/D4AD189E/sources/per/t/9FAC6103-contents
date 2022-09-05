# Make Figures with data 
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
library(ggpubr)
library(period.apply)
#start in DO stitch and other stitch scripts
#run script 1 again if it does not work



##### FRCH ####
#DO and Temp Data
All.years.frch.MESSY

All.years.frch.MESSY <- All.years.frch.MESSY %>%
  select(temp.water, ODO.Psat, ODO.Ploc, DO.obs, datetimeAK)

All.years.frch.MESSY$datetimeAK <- as.POSIXct(All.years.frch.MESSY$datetimeAK)

#discharge
FRCH.ALL.Q

FRCH.ALL.Q <- FRCH.ALL.Q %>%
  dplyr::rename(datetimeAK = DateTime)


#already in AKDT
FRCH.ALL.Q$datetimeAK <- as.POSIXct(FRCH.ALL.Q$datetimeAK)

setDT(FRCH.ALL.Q)[, datetimeAK := datetimeAK[1L], 
          by = cumsum(as.POSIXlt(datetimeAK, format = "%m/%d/%Y %H:%M")$min %% 15 == 0)]

FRCH.ALL.Q <- aggregate(Q ~ datetimeAK, data=FRCH.ALL.Q, FUN=mean)

FRCH.ALL.Q <- FRCH.ALL.Q %>%
  dplyr::rename(discharge = Q)

#air Pressure
frch.ap.data

frch.ap.data <- frch.ap.data %>%
  dplyr::rename(datetimeAK = DateTime)

frch.ap.data$datetimeAK <- as.POSIXct(frch.ap.data$datetimeAK)


#light
frch.combinded.par

frch.combinded.par <- frch.combinded.par %>%
  dplyr::rename(datetimeAK = DateTime)

frch.combinded.par <- frch.combinded.par %>%
  dplyr::rename(light = Calibrated.Value)

frch.combinded.par$datetimeAK <- as.POSIXct(frch.combinded.par$datetimeAK)


#depth rating curve
frch.depth

frch.depth <- frch.depth %>%
  dplyr::rename(depth = RatingCurveDepth)

frch.depth$datetimeAK <- as.POSIXct(frch.depth$datetimeAK)

setDT(frch.depth)[, datetimeAK := datetimeAK[1L], 
                  by = cumsum(as.POSIXlt(datetimeAK, format = "%m/%d/%Y %H:%M")$min %% 15 == 0)]

frch.depth <- aggregate(depth ~ datetimeAK, data=frch.depth, FUN=mean)


#Combine

FRCH.comb <- merge(All.years.frch.MESSY, FRCH.ALL.Q, 
                   # frch.depth, frch.combinded.par,frch.ap.data, 
                   by = "datetimeAK", all = TRUE)
FRCH.comb <- merge(FRCH.comb, frch.depth,
                   # frch.combinded.par,frch.ap.data, 
                   by = "datetimeAK", all = TRUE)
FRCH.comb <- merge(FRCH.comb, frch.combinded.par,
                   # frch.ap.data, 
                   by = "datetimeAK", all = TRUE)
FRCH.comb <- merge(FRCH.comb, frch.ap.data,
                   # , 
                   by = "datetimeAK", all = TRUE)

FRCH.comb$DO.sat <- calc_DO_sat(FRCH.comb$temp.water, FRCH.comb$air.pressure.mbar, model = "garcia-benson")

FRCH.comb$solar.time <- calc_solar_time(FRCH.comb$datetimeAK,-146.915323)

FRCH.comb <- distinct(FRCH.comb)

FRCH.comb

#remove outliers
FRCH.comb <- FRCH.comb[-c(23773,23774)]
FRCH.comb <- FRCH.comb[-c(29660)]
FRCH.comb <- FRCH.comb[-c(25015,25016,25017)]

write.csv(FRCH.comb, here("outputs", "frch.comb.csv"))

# ALL YEARS
frch.plot1 <- FRCH.comb %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "French ODO, 2019 - 2021")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge='m^3/sec', air.pressure.mbar = 'mbar')
frchPlot2 <- FRCH.comb %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/FRCH_comb_2019-2021.pdf", height= 8.5)
ggarrange(frch.plot1, frchPlot2, ncol = 1, nrow = 2)
dev.off()


#2019

#
frch.plot1.19 <- FRCH.comb %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "French ODO, 2019")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
frchPlot2.19 <- FRCH.comb %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/FRCH_comb.2019.pdf", height= 8.5)
ggarrange(frch.plot1.19, frchPlot2.19, ncol = 1, nrow = 2)
dev.off()

#2020

#
frch.plot1.20 <- FRCH.comb %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "French ODO, 2020")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
frchPlot2.20 <- FRCH.comb %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>%
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light', 'discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/FRCH_comb.2020.pdf", height= 8.5)
ggarrange(frch.plot1.20, frchPlot2.20, ncol = 1, nrow = 2)
dev.off()

#2021

#
frch.plot1.21 <- FRCH.comb %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "French ODO, 2121")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
frchPlot2.21 <- FRCH.comb %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', "air.pressure.mbar")),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/FRCH_comb.2021.pdf", height= 8.5)
ggarrange(frch.plot1.21, frchPlot2.21, ncol = 1, nrow = 2)
dev.off()





# ##### FRCH ####
# 
# #FRCH
# #DO and Temp Data
# All.years.frch.MESSY
# 
# All.years.frch.MESSY <- All.years.frch.MESSY %>%
#   select(temp.water, ODO.Psat, ODO.Ploc, DO.obs, datetimeAK)
# 
# All.years.frch.MESSY$datetimeAK <- as.POSIXct(All.years.frch.MESSY$datetimeAK)
# 
# #discharge
# FRCH.ALL.Q
# 
# FRCH.ALL.Q <- FRCH.ALL.Q %>%
#   dplyr::rename(datetimeAK = DateTime)
# 
# 
# 
# FRCH.ALL.Q$datetimeAK <- as.POSIXct(FRCH.ALL.Q$datetimeAK)
# 
# setDT(FRCH.ALL.Q)[, datetimeAK := datetimeAK[1L], 
#                   by = cumsum(as.POSIXlt(datetimeAK, format = "%m/%d/%Y %H:%M")$min %% 15 == 0)]
# 
# FRCH.ALL.Q <- aggregate(Q ~ datetimeAK, data=FRCH.ALL.Q, FUN=mean)
# 
# FRCH.ALL.Q <- FRCH.ALL.Q %>%
#   dplyr::rename(discharge = Q)
# 
# #air Pressure
# frch.ap.data
# 
# frch.ap.data <- frch.ap.data %>%
#   dplyr::rename(datetimeAK = DateTime)
# 
# frch.ap.data$datetimeAK <- as.POSIXct(frch.ap.data$datetimeAK)
# 
# 
# #light
# frch.combinded.par
# 
# frch.combinded.par <- frch.combinded.par %>%
#   dplyr::rename(datetimeAK = DateTime)
# 
# frch.combinded.par <- frch.combinded.par %>%
#   dplyr::rename(light = Calibrated.Value)
# 
# frch.combinded.par$datetimeAK <- as.POSIXct(frch.combinded.par$datetimeAK)
# 
# 
# #depth rating curve
# frch.depth
# 
# frch.depth <- frch.depth %>%
#   dplyr::rename(depth = RatingCurveDepth)
# 
# frch.depth$datetimeAK <- as.POSIXct(frch.depth$datetimeAK)
# 
# setDT(frch.depth)[, datetimeAK := datetimeAK[1L], 
#                   by = cumsum(as.POSIXlt(datetimeAK, format = "%m/%d/%Y %H:%M")$min %% 15 == 0)]
# 
# frch.depth <- aggregate(depth ~ datetimeAK, data=frch.depth, FUN=mean)
# 
# 
# #Combine
# 
# FRCH.comb <- merge(All.years.frch.MESSY, FRCH.ALL.Q, 
#                    # frch.depth, frch.combinded.par,frch.ap.data, 
#                    by = "datetimeAK", all = TRUE)
# FRCH.comb <- merge(FRCH.comb, frch.depth,
#                    # frch.combinded.par,frch.ap.data, 
#                    by = "datetimeAK", all = TRUE)
# FRCH.comb <- merge(FRCH.comb, frch.combinded.par,
#                    # frch.ap.data, 
#                    by = "datetimeAK", all = TRUE)
# FRCH.comb <- merge(FRCH.comb, frch.ap.data,
#                    # , 
#                    by = "datetimeAK", all = TRUE)
# 
# FRCH.comb$DO.sat <- calc_DO_sat(FRCH.comb$temp.water, FRCH.comb$air.pressure.mbar, model = "garcia-benson")
# 
# FRCH.comb$solar.time <- calc_solar_time(FRCH.comb$datetimeAK,-146.915323)
# 
# FRCH.comb <- distinct(FRCH.comb)
# 
# 
# # ALL YEARS
# frch.plot1 <- FRCH.comb %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "French ODO, 2019 - 2021")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge='m^3/sec', air.pressure.mbar = 'mbar')
# frchPlot2 <- FRCH.comb %>% 
#   select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
#   gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light','discharge', 'air.pressure.mbar')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')
# 
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/FRCH_comb_2019-2021.pdf", height= 8.5)
# ggarrange(frch.plot1, frchPlot2, ncol = 1, nrow = 2)
# dev.off()
# 
# 
# #2019
# 
# #
# frch.plot1.19 <- FRCH.comb %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "French ODO, 2019")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
# frchPlot2.19 <- FRCH.comb %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
#   gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light','discharge', 'air.pressure.mbar')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')
# 
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/FRCH_comb.2019.pdf", height= 8.5)
# ggarrange(frch.plot1.19, frchPlot2.19, ncol = 1, nrow = 2)
# dev.off()
# 
# #2020
# 
# #
# frch.plot1.20 <- FRCH.comb %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "French ODO, 2020")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
# frchPlot2.20 <- FRCH.comb %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>%
#   select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
#   gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light', 'discharge', 'air.pressure.mbar')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')
# 
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/FRCH_comb.2020.pdf", height= 8.5)
# ggarrange(frch.plot1.20, frchPlot2.20, ncol = 1, nrow = 2)
# dev.off()
# 
# #2021
# 
# #
# frch.plot1.21 <- FRCH.comb %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "French ODO, 2021")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
# frchPlot2.21 <- FRCH.comb %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
#   gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light','discharge', "air.pressure.mbar")),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')
# 
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/FRCH_comb.2021.pdf", height= 8.5)
# ggarrange(frch.plot1.21, frchPlot2.21, ncol = 1, nrow = 2)
# dev.off()
# 
# 
# 
# 
# 
# 
##### MOOS ####

#MOOS
#DO and Temp Data
All.years.moos.MESSY

All.years.moos.MESSY <- All.years.moos.MESSY %>%
  select(temp.water, ODO.Psat, ODO.Ploc, DO.obs, datetimeAK)

All.years.moos.MESSY$datetimeAK <- as.POSIXct(All.years.moos.MESSY$datetimeAK)

#discharge
MOOS.ALL.Q

MOOS.ALL.Q <- MOOS.ALL.Q %>%
  dplyr::rename(datetimeAK = DateTime)



MOOS.ALL.Q$datetimeAK <- as.POSIXct(MOOS.ALL.Q$datetimeAK)

setDT(MOOS.ALL.Q)[, datetimeAK := datetimeAK[1L], 
                  by = cumsum(as.POSIXlt(datetimeAK, format = "%m/%d/%Y %H:%M")$min %% 15 == 0)]

MOOS.ALL.Q <- aggregate(Q ~ datetimeAK, data=MOOS.ALL.Q, FUN=mean)

MOOS.ALL.Q <- MOOS.ALL.Q %>%
  dplyr::rename(discharge = Q)

#air Pressure
moos.ap.data

moos.ap.data <- moos.ap.data %>%
  dplyr::rename(datetimeAK = DateTime)

moos.ap.data$datetimeAK <- as.POSIXct(moos.ap.data$datetimeAK)


#light
moos.combinded.par

moos.combinded.par <- moos.combinded.par %>%
  dplyr::rename(datetimeAK = DateTime)

moos.combinded.par <- moos.combinded.par %>%
  dplyr::rename(light = Calibrated.Value)

moos.combinded.par$datetimeAK <- as.POSIXct(moos.combinded.par$datetimeAK)


#depth rating curve
moos.depth

moos.depth <- moos.depth %>%
  dplyr::rename(depth = RatingCurveDepth)

moos.depth$datetimeAK <- as.POSIXct(moos.depth$datetimeAK)

setDT(moos.depth)[, datetimeAK := datetimeAK[1L], 
                  by = cumsum(as.POSIXlt(datetimeAK, format = "%m/%d/%Y %H:%M")$min %% 15 == 0)]

moos.depth <- aggregate(depth ~ datetimeAK, data=moos.depth, FUN=mean)


#Combine

MOOS.comb <- merge(All.years.moos.MESSY, MOOS.ALL.Q, 
                   # moos.depth, moos.combinded.par,moos.ap.data, 
                   by = "datetimeAK", all = TRUE)
MOOS.comb <- merge(MOOS.comb, moos.depth,
                   # moos.combinded.par,moos.ap.data, 
                   by = "datetimeAK", all = TRUE)
MOOS.comb <- merge(MOOS.comb, moos.combinded.par,
                   # moos.ap.data, 
                   by = "datetimeAK", all = TRUE)
MOOS.comb <- merge(MOOS.comb, moos.ap.data,
                   # , 
                   by = "datetimeAK", all = TRUE)

MOOS.comb$DO.sat <- calc_DO_sat(MOOS.comb$temp.water, MOOS.comb$air.pressure.mbar, model = "garcia-benson")

MOOS.comb$solar.time <- calc_solar_time(MOOS.comb$datetimeAK,-146.915323)

MOOS.comb <- distinct(MOOS.comb)

write.csv(MOOS.comb, here("outputs", "moos.comb.csv"))

# ALL YEARS
moos.plot1 <- MOOS.comb %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Moose ODO, 2019 - 2021")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge='m^3/sec', air.pressure.mbar = 'mbar')
moosPlot2 <- MOOS.comb %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/MOOS_comb_2019-2021.pdf", height= 8.5)
ggarrange(moos.plot1, moosPlot2, ncol = 1, nrow = 2)
dev.off()


#2019

#
moos.plot1.19 <- MOOS.comb %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Moose ODO, 2019")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
moosPlot2.19 <- MOOS.comb %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/MOOS_comb.2019.pdf", height= 8.5)
ggarrange(moos.plot1.19, moosPlot2.19, ncol = 1, nrow = 2)
dev.off()

#2020

#
moos.plot1.20 <- MOOS.comb %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Moose ODO, 2020")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
moosPlot2.20 <- MOOS.comb %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>%
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light', 'discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/MOOS_comb.2020.pdf", height= 8.5)
ggarrange(moos.plot1.20, moosPlot2.20, ncol = 1, nrow = 2)
dev.off()

#2021

#
moos.plot1.21 <- MOOS.comb %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Moose ODO, 2021")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
moosPlot2.21 <- MOOS.comb %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', "air.pressure.mbar")),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/MOOS_comb.2021.pdf", height= 8.5)
ggarrange(moos.plot1.21, moosPlot2.21, ncol = 1, nrow = 2)
dev.off()







##### STRT ####

#STRT
#DO and Temp Data
All.years.strt.MESSY

All.years.strt.MESSY <- All.years.strt.MESSY %>%
  select(temp.water, ODO.Psat, ODO.Ploc, DO.obs, datetimeAK)

All.years.strt.MESSY$datetimeAK <- as.POSIXct(All.years.strt.MESSY$datetimeAK)

#discharge
STRT.ALL.Q

STRT.ALL.Q <- STRT.ALL.Q %>%
  dplyr::rename(datetimeAK = DateTime)

STRT.ALL.Q <- STRT.ALL.Q %>%
  dplyr::rename(Q = discharge)


STRT.ALL.Q$datetimeAK <- as.POSIXct(STRT.ALL.Q$datetimeAK)

# setDT(STRT.ALL.Q)[, datetimeAK := datetimeAK[1L], 
#                   by = cumsum(as.POSIXlt(datetimeAK, format = "%m/%d/%Y %H:%M")$min %% 15 == 0)]

STRT.ALL.Q <- dplyr::mutate(STRT.ALL.Q, t2 = cut.POSIXt(x = datetimeAK, breaks = "15 mins")) %>% 
  dplyr::group_by(t2) %>% 
  dplyr::summarise(am = mean(Q, na.rm = T))

STRT.ALL.Q <- STRT.ALL.Q %>%
  dplyr::rename(datetimeAK = t2)

STRT.ALL.Q <- STRT.ALL.Q %>%
  dplyr::rename(Q = am)

STRT.ALL.Q <- aggregate(Q ~ datetimeAK, data=STRT.ALL.Q, FUN=mean)

STRT.ALL.Q <- STRT.ALL.Q %>%
  dplyr::rename(discharge = Q)


STRT.ALL.Q$datetimeAK <- as.POSIXct(STRT.ALL.Q$datetimeAK)

#air Pressure
strt.ap.data

strt.ap.data <- strt.ap.data %>%
  dplyr::rename(datetimeAK = DateTime)

strt.ap.data$datetimeAK <- as.POSIXct(strt.ap.data$datetimeAK)


#light
strt.combinded.par

strt.combinded.par <- strt.combinded.par %>%
  dplyr::rename(datetimeAK = DateTime)

strt.combinded.par <- strt.combinded.par %>%
  dplyr::rename(light = Calibrated.Value)

strt.combinded.par$datetimeAK <- as.POSIXct(strt.combinded.par$datetimeAK)

strt.combinded.par$datetimeAK <- lubridate::round_date(strt.combinded.par$datetimeAK, "15 minutes") 



#depth rating curve
strt.depth <- strt.depth %>% filter(datetimeAK >= "2019-05-21 14:45:00")


strt.depth <- strt.depth %>%
  dplyr::rename(depth = RatingCurveDepth)

strt.depth$datetimeAK <- as.POSIXct(strt.depth$datetimeAK)

setDT(strt.depth)[, datetimeAK := datetimeAK[1L], 
                  by = cumsum(as.POSIXlt(datetimeAK, format = "%m/%d/%Y %H:%M")$min %% 15 == 0)]

strt.depth <- aggregate(depth ~ datetimeAK, data=strt.depth, FUN=mean)


#Combine

STRT.comb <- merge(All.years.strt.MESSY, STRT.ALL.Q, 
                   # strt.depth, strt.combinded.par,strt.ap.data, 
                   by = "datetimeAK", all = TRUE)
STRT.comb <- merge(STRT.comb, strt.depth,
                   # strt.combinded.par,strt.ap.data, 
                   by = "datetimeAK", all = TRUE)
STRT.comb <- merge(STRT.comb, strt.combinded.par,
                   # strt.ap.data, 
                   by = "datetimeAK", all = TRUE)

test1 <- strt.ap.data[-c(1)]

STRT.comb <- merge(STRT.comb, test1,
                   # , 
                   by = "datetimeAK", all = TRUE)

STRT.comb$DO.sat <- calc_DO_sat(STRT.comb$temp.water, STRT.comb$air.pressure.mbar, model = "garcia-benson")

STRT.comb$datetimeAK <- as.POSIXct(STRT.comb$datetimeAK) 

STRT.comb$solar.time <- calc_solar_time(STRT.comb$datetimeAK,-146.915323)

STRT.comb$light[STRT.comb$datetimeAK >= '2019-08-23 14:15:00' & STRT.comb$datetimeAK <= '2019-09-17 12:00:00'] <- NA

STRT.comb$datetimeAK <- as.character(STRT.comb$datetimeAK)


light2020formax <- STRT.comb %>% filter(datetimeAK >= '2020-00-00 14:15:00' & datetimeAK <= '2021-00-00 00:00:00')
max(light2020formax$light, na.rm = TRUE)

light2021formax <- STRT.comb %>% filter(STRT.comb$datetimeAK >= '2021-00-00 14:15:00' & STRT.comb$datetimeAK <= '2022-00-00 00:00:00')
max(light2021formax$light, na.rm = TRUE)



STRT.comb$modeled.light <- calc_light(STRT.comb$solar.time, 64.754280, -146.477647, max.PAR = 1932.084)


# STRT.comb$light[STRT.comb$datetimeAK >= '2019-08-23 14:15:00' & STRT.comb$datetimeAK <= '2019-09-17 12:00:00'] <- STRT.comb$modeled.light


STRT.comb.fill.light <- STRT.comb %>% filter(STRT.comb$datetimeAK >= '2019-08-23 14:15:00' & STRT.comb$datetimeAK <= '2019-09-17 12:00:00') %>% 
  mutate(light = coalesce(light,modeled.light))


STRT.comb.1 <- STRT.comb %>% filter(STRT.comb$datetimeAK < '2019-08-23 14:15:00')

STRT.comb.3 <- STRT.comb %>% filter(STRT.comb$datetimeAK > '2019-09-17 12:00:00')

STRT.comb <- rbind(STRT.comb.1, STRT.comb.fill.light, STRT.comb.3)



STRT.comb <- distinct(STRT.comb)

get.wd()
write.csv(STRT.comb, here("outputs", "strt.comb.csv"))


# ALL YEARS
strt.plot1 <- STRT.comb %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Stuart ODO, 2019 - 2021")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge='m^3/sec', air.pressure.mbar = 'mbar')
strtPlot2 <- STRT.comb %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/STRT_comb_2019-2021.pdf", height= 8.5)
ggarrange(strt.plot1, strtPlot2, ncol = 1, nrow = 2)
dev.off()


#2019

#
strt.plot1.19 <- STRT.comb %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Stuart ODO, 2019")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
strtPlot2.19 <- STRT.comb %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/STRT_comb.2019.pdf", height= 8.5)
ggarrange(strt.plot1.19, strtPlot2.19, ncol = 1, nrow = 2)
dev.off()

#2020

#
strt.plot1.20 <- STRT.comb %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Stuart ODO, 2020")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
strtPlot2.20 <- STRT.comb %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>%
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light', 'discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/STRT_comb.2020.pdf", height= 8.5)
ggarrange(strt.plot1.20, strtPlot2.20, ncol = 1, nrow = 2)
dev.off()

#2021

#
strt.plot1.21 <- STRT.comb %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Stuart ODO, 2021")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
strtPlot2.21 <- STRT.comb %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', "air.pressure.mbar")),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/STRT_comb.2021.pdf", height= 8.5)
ggarrange(strt.plot1.21, strtPlot2.21, ncol = 1, nrow = 2)
dev.off()



##### POKE ####

#POKE
#DO and Temp Data
All.years.poke.MESSY

All.years.poke.MESSY <- All.years.poke.MESSY %>%
  select(temp.water, ODO.Psat, ODO.Ploc, DO.obs, datetimeAK)

All.years.poke.MESSY$datetimeAK <- as.POSIXct(All.years.poke.MESSY$datetimeAK)

#discharge
POKE.ALL.Q

POKE.ALL.Q <- POKE.ALL.Q %>%
  dplyr::rename(datetimeAK = DateTime)



POKE.ALL.Q$datetimeAK <- as.POSIXct(POKE.ALL.Q$datetimeAK)

setDT(POKE.ALL.Q)[, datetimeAK := datetimeAK[1L], 
                  by = cumsum(as.POSIXlt(datetimeAK, format = "%m/%d/%Y %H:%M")$min %% 15 == 0)]

POKE.ALL.Q <- aggregate(Q ~ datetimeAK, data=POKE.ALL.Q, FUN=mean)

POKE.ALL.Q <- POKE.ALL.Q %>%
  dplyr::rename(discharge = Q)

#air Pressure
poke.ap.data

poke.ap.data <- poke.ap.data %>%
  dplyr::rename(datetimeAK = DateTime)

poke.ap.data$datetimeAK <- as.POSIXct(poke.ap.data$datetimeAK)


#light
poke.combinded.par

poke.combinded.par <- poke.combinded.par %>%
  dplyr::rename(datetimeAK = DateTime)

poke.combinded.par <- poke.combinded.par %>%
  dplyr::rename(light = Calibrated.Value)

poke.combinded.par$datetimeAK <- as.POSIXct(poke.combinded.par$datetimeAK)


#depth rating curve
poke.depth

poke.depth <- poke.depth %>%
  dplyr::rename(depth = RatingCurveDepth)

poke.depth$datetimeAK <- as.POSIXct(poke.depth$datetimeAK)

setDT(poke.depth)[, datetimeAK := datetimeAK[1L], 
                  by = cumsum(as.POSIXlt(datetimeAK, format = "%m/%d/%Y %H:%M")$min %% 15 == 0)]

poke.depth <- aggregate(depth ~ datetimeAK, data=poke.depth, FUN=mean)


#Combine

POKE.comb <- merge(All.years.poke.MESSY, POKE.ALL.Q, 
                   # poke.depth, poke.combinded.par,poke.ap.data, 
                   by = "datetimeAK", all = TRUE)
POKE.comb <- merge(POKE.comb, poke.depth,
                   # poke.combinded.par,poke.ap.data, 
                   by = "datetimeAK", all = TRUE)
POKE.comb <- merge(POKE.comb, poke.combinded.par,
                   # poke.ap.data, 
                   by = "datetimeAK", all = TRUE)
POKE.comb <- merge(POKE.comb, poke.ap.data,
                   # , 
                   by = "datetimeAK", all = TRUE)

POKE.comb$DO.sat <- calc_DO_sat(POKE.comb$temp.water, POKE.comb$air.pressure.mbar, model = "garcia-benson")

POKE.comb$solar.time <- calc_solar_time(POKE.comb$datetimeAK,-146.915323)

POKE.comb <- distinct(POKE.comb)

write.csv(POKE.comb, here("outputs", "poke.comb.csv"))

# ALL YEARS
poke.plot1 <- POKE.comb %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Poker ODO, 2019 - 2021")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge='m^3/sec', air.pressure.mbar = 'mbar')
pokePlot2 <- POKE.comb %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/POKE_comb_2019-2021.pdf", height= 8.5)
ggarrange(poke.plot1, pokePlot2, ncol = 1, nrow = 2)
dev.off()


#2019

#
poke.plot1.19 <- POKE.comb %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Poker ODO, 2019")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
pokePlot2.19 <- POKE.comb %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/POKE_comb.2019.pdf", height= 8.5)
ggarrange(poke.plot1.19, pokePlot2.19, ncol = 1, nrow = 2)
dev.off()

#2020

#
poke.plot1.20 <- POKE.comb %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Poker ODO, 2020")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
pokePlot2.20 <- POKE.comb %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>%
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light', 'discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/POKE_comb.2020.pdf", height= 8.5)
ggarrange(poke.plot1.20, pokePlot2.20, ncol = 1, nrow = 2)
dev.off()

#2021

#
poke.plot1.21 <- POKE.comb %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Poker ODO, 2021")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
pokePlot2.21 <- POKE.comb %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', "air.pressure.mbar")),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/POKE_comb.2021.pdf", height= 8.5)
ggarrange(poke.plot1.21, pokePlot2.21, ncol = 1, nrow = 2)
dev.off()




##### VAUL ####

#VAUL
#DO and Temp Data
All.years.vaul.MESSY

All.years.vaul.MESSY <- All.years.vaul.MESSY %>%
  select(temp.water, ODO.Psat, ODO.Ploc, DO.obs, datetimeAK)

All.years.vaul.MESSY$datetimeAK <- as.POSIXct(All.years.vaul.MESSY$datetimeAK)

#discharge
VAUL.ALL.Q

VAUL.ALL.Q <- VAUL.ALL.Q %>%
  dplyr::rename(datetimeAK = DateTime)

VAUL.ALL.Q <- VAUL.ALL.Q %>%
  dplyr::rename(Q = discharge)


VAUL.ALL.Q$datetimeAK <- as.POSIXct(VAUL.ALL.Q$datetimeAK)

setDT(VAUL.ALL.Q)[, datetimeAK := datetimeAK[1L], 
                  by = cumsum(as.POSIXlt(datetimeAK, format = "%m/%d/%Y %H:%M")$min %% 15 == 0)]

VAUL.ALL.Q <- aggregate(Q ~ datetimeAK, data=VAUL.ALL.Q, FUN=mean)

VAUL.ALL.Q <- VAUL.ALL.Q %>%
  dplyr::rename(discharge = Q)

#air Pressure
vaul.ap.data

vaul.ap.data <- vaul.ap.data %>%
  dplyr::rename(datetimeAK = DateTime)

vaul.ap.data$datetimeAK <- as.POSIXct(vaul.ap.data$datetimeAK)


#light
vaul.combinded.par

vaul.combinded.par <- vaul.combinded.par %>%
  dplyr::rename(datetimeAK = DateTime)

vaul.combinded.par <- vaul.combinded.par %>%
  dplyr::rename(light = Calibrated.Value)

vaul.combinded.par$datetimeAK <- as.POSIXct(vaul.combinded.par$datetimeAK)


#depth rating curve
vaul.depth

vaul.depth <- vaul.depth %>%
  dplyr::rename(depth = RatingCurveDepth)

vaul.depth$datetimeAK <- as.POSIXct(vaul.depth$datetimeAK)

setDT(vaul.depth)[, datetimeAK := datetimeAK[1L], 
                  by = cumsum(as.POSIXlt(datetimeAK, format = "%m/%d/%Y %H:%M")$min %% 15 == 0)]

vaul.depth <- aggregate(depth ~ datetimeAK, data=vaul.depth, FUN=mean)


#Combine

VAUL.comb <- merge(All.years.vaul.MESSY, VAUL.ALL.Q, 
                   # vaul.depth, vaul.combinded.par,vaul.ap.data, 
                   by = "datetimeAK", all = TRUE)
VAUL.comb <- merge(VAUL.comb, vaul.depth,
                   # vaul.combinded.par,vaul.ap.data, 
                   by = "datetimeAK", all = TRUE)
VAUL.comb <- merge(VAUL.comb, vaul.combinded.par,
                   # vaul.ap.data, 
                   by = "datetimeAK", all = TRUE)
VAUL.comb <- merge(VAUL.comb, vaul.ap.data,
                   # , 
                   by = "datetimeAK", all = TRUE)


VAUL.comb <- distinct(VAUL.comb)

VAUL.comb$solar.time <- calc_solar_time(VAUL.comb$datetimeAK,-146.915323)

VAUL.comb$DO.sat <- calc_DO_sat(VAUL.comb$temp.water, VAUL.comb$air.pressure.mbar, model = "garcia-benson")


VAUL.comb <- distinct(VAUL.comb)

write.csv(VAUL.comb, here("outputs", "vaul.comb.csv"))

# ALL YEARS
vaul.plot1 <- VAUL.comb %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Vault ODO, 2019 - 2021")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge='m^3/sec', air.pressure.mbar = 'mbar')
vaulPlot2 <- VAUL.comb %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/VAUL_comb_2019-2021.pdf", height= 8.5)
ggarrange(vaul.plot1, vaulPlot2, ncol = 1, nrow = 2)
dev.off()


#2019

#
vaul.plot1.19 <- VAUL.comb %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Vault ODO, 2019")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
vaulPlot2.19 <- VAUL.comb %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/VAUL_comb.2019.pdf", height= 8.5)
ggarrange(vaul.plot1.19, vaulPlot2.19, ncol = 1, nrow = 2)
dev.off()

#2020

#
vaul.plot1.20 <- VAUL.comb %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Vault ODO, 2020")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
vaulPlot2.20 <- VAUL.comb %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>%
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light', 'discharge', 'air.pressure.mbar')),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/VAUL_comb.2020.pdf", height= 8.5)
ggarrange(vaul.plot1.20, vaulPlot2.20, ncol = 1, nrow = 2)
dev.off()

#2021

#
vaul.plot1.21 <- VAUL.comb %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO')) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable') + labs(title = "Vault ODO, 2021")

labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)', discharge= 'm^3/sec', air.pressure.mbar = 'mbar')
vaulPlot2.21 <- VAUL.comb %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
  select(solar.time, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  gather(type, value, depth, temp.water, light, discharge, air.pressure.mbar) %>%
  mutate(
    type=ordered(type, levels=c('depth','temp.water','light','discharge', "air.pressure.mbar")),
    units=ordered(labels[type], unname(labels))) %>%
  ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/VAUL_comb.2021.pdf", height= 8.5)
ggarrange(vaul.plot1.21, vaulPlot2.21, ncol = 1, nrow = 2)
dev.off()


##### OLD CODE #####


# 
# 
# #2019
# png(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testFRCH_SM_DO.2019.png")
# frch.all %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "French ODO, 2019")
# dev.off()
# 
# 
# #2020
# png(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testFRCH_SM_DO.2020.png")
# frch.all %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "French ODO, 2020")
# dev.off()
# 
# #2021
# png(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testFRCH_SM_DO.2021.png")
# frch.all %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "French ODO, 2021")
# dev.off()
# 
# 
# 
# #Moos
# 
# moos.ap.data <- moos.ap.data %>%
#   dplyr::rename(datetimeAK = DateTime)
# 
# 
# moos.all <- merge(All.years.moos.MESSY,moos.ap.data, by = "datetimeAK",all = TRUE)
# 
# #rename some 
# moos.all <- moos.all %>%
#   dplyr::rename(temp.water = Temp.C)
# 
# moos.all <- moos.all %>%
#   dplyr::rename(DO.obs = ODO.mgL)
# 
# 
# 
# 
# moos.all$DO.sat <- calc_DO_sat(moos.all$temp.water, moos.all$air.pressure.mbar, model = "garcia-benson")
# 
# 
# moos.all$solar.time <- calc_solar_time(moos.all$datetimeAK,-147.052814)
# 
# png(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testMOOS_SM_DO.png")
# 
# moos.all %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Moose ODO, 2019 - 2021")
# dev.off()
# #2019
# png(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testMOOS_SM_DO.2019.png")
# moos.all %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Moose ODO, 2019")
# dev.off()
# 
# 
# #2020
# png(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testMOOS_SM_DO.2020.png")
# moos.all %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Moose ODO, 2020")
# dev.off()
# 
# #2021
# png(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testMOOS_SM_DO.2021.png")
# moos.all %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Moose ODO, 2021")
# dev.off()
# 
# 
# #Poke
# 
# poke.ap.data <- poke.ap.data %>%
#   dplyr::rename(datetimeAK = DateTime)
# 
# 
# poke.all <- merge(All.years.poke.MESSY,poke.ap.data, by = "datetimeAK",)
# 
# #rename some 
# poke.all <- poke.all %>%
#   dplyr::rename(temp.water = Temp.C)
# 
# poke.all <- poke.all %>%
#   dplyr::rename(DO.obs = ODO.mgL)
# 
# 
# 
# 
# poke.all$DO.sat <- calc_DO_sat(poke.all$temp.water, poke.all$air.pressure.mbar, model = "garcia-benson")
# 
# 
# poke.all$solar.time <- calc_solar_time(poke.all$datetimeAK,-147.052814)
# 
# png(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testPOKE_SM_DO.png")
# 
# poke.all %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Poker ODO, 2019 - 2021")
# dev.off()
# 
# #2019
# png(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testPOKE_SM_DO.2019.png")
# poke.all %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Poker ODO, 2019")
# dev.off()
# 
# 
# #2020
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testPOKE_SM_DO.2020.pdf")
# poke.all %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Poker ODO, 2020")
# dev.off()
# 
# #2021
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testPOKE_SM_DO.2021.pdf")
# poke.all %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Poker ODO, 2021")
# dev.off()
# 
# 
# #Strt
# strt.ap.data <- strt.ap.data %>%
#   dplyr::rename(datetimeAK = DateTime)
# 
# 
# strt.all <- merge(All.years.strt.MESSY,strt.ap.data, by = "datetimeAK",all = TRUE)
# 
# #rename some 
# strt.all <- strt.all %>%
#   dplyr::rename(temp.water = Temp.C)
# 
# strt.all <- strt.all %>%
#   dplyr::rename(DO.obs = ODO.mgL)
# 
# 
# 
# 
# strt.all$DO.sat <- calc_DO_sat(strt.all$temp.water, strt.all$air.pressure.mbar, model = "garcia-benson")
# 
# 
# strt.all$solar.time <- calc_solar_time(strt.all$datetimeAK,-147.052814)
# 
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testSTRT_SM_DO.pdf")
# 
# strt.all %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Strt ODO, 2019 - 2021")
# dev.off()
# #2019
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testSTRT_SM_DO.2019.pdf")
# strt.all %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Stuart ODO, 2019")
# dev.off()
# 
# 
# #2020
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testSTRT_SM_DO.2020.pdf")
# strt.all %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Stuart ODO, 2020")
# dev.off()
# 
# #2021
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testSTRT_SM_DO.2021.pdf")
# strt.all %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Stuart ODO, 2021")
# dev.off()
# 
# 
# 
# #Vaul
# vaul.ap.data <- vaul.ap.data %>%
#   dplyr::rename(datetimeAK = DateTime)
# 
# 
# vaul.all <- merge(All.years.vaul.MESSY,vaul.ap.data, by = "datetimeAK",all = TRUE)
# 
# #rename some 
# vaul.all <- vaul.all %>%
#   dplyr::rename(temp.water = Temp.C)
# 
# vaul.all <- vaul.all %>%
#   dplyr::rename(DO.obs = ODO.mgL)
# 
# vaul.all<- vaul.all[-c(22786),]
# 
# 
# vaul.all$DO.sat <- calc_DO_sat(vaul.all$temp.water, vaul.all$air.pressure.mbar, model = "garcia-benson")
# 
# 
# vaul.all$solar.time <- calc_solar_time(vaul.all$datetimeAK,-147.052814)
# 
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testVAUL_SM_DO.pdf")
# 
# 
# 
# vaul.all %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Vaul ODO, 2019 - 2021")
# dev.off()
# #2019
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testVAUL_SM_DO.2019.pdf")
# vaul.all %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Vault ODO, 2019")
# dev.off()
# 
# 
# #2020
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testVAUL_SM_DO.2020.pdf")
# vaul.all %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Vault ODO, 2020")
# dev.off()
# 
# #2021
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testVAUL_SM_DO.2021.pdf")
# vaul.all %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
#   mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
#   select(solar.time, starts_with('DO')) %>%
#   gather(type, DO.value, starts_with('DO')) %>%
#   mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
#   ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable') + labs(title = "Vault ODO, 2021")
# dev.off()
# 
# 
# 
# 
# ######################################################################
# #Light, Depth, Water temp
# 
# #MOOS
# 
# 
# moos.combinded.par <- moos.combinded.par %>%
#   dplyr::rename(light = Calibrated.Value)
# 
# moos.combinded.par <- moos.combinded.par %>%
#   dplyr::rename(datetimeAK = DateTime)
# 
# moos.all <- merge(moos.combinded.par, moos.all,  by = "datetimeAK",all = TRUE)
# 
# 
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testMOOS_SM_light_temp_depth.pdf")
# 
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# moos.all %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "MOOS, 2019 - 2021")
# 
# dev.off()
# 
# #2019
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testMOOS_SM_light_temp_depth.2019.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# moos.all %>% filter(datetimeAK >= "2019-01-01 00:00:00") %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "MOOS, 2019")
# 
# dev.off()
# 
# #2020
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testMOOS_SM_light_temp_depth.2020.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# moos.all %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "MOOS, 2020")
# 
# dev.off()
# 
# #2021
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testMOOS_SM_light_temp_depth.2021.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# moos.all %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "MOOS, 2021")
# 
# dev.off()
# 
# 
# 
# 
# 
# #FRCH
# 
# 
# frch.combinded.par <- frch.combinded.par %>%
#   dplyr::rename(light = Calibrated.Value)
# 
# frch.combinded.par <- frch.combinded.par %>%
#   dplyr::rename(datetimeAK = DateTime)
# 
# frch.all <- merge(frch.combinded.par, frch.all,  by = "datetimeAK", all = TRUE)
# 
# 
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testFRCH_SM_light_temp_depth.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# frch.all %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "FRCH, 2019 - 2021")
# 
# dev.off()
# #2019
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testFRCH_SM_light_temp_depth.2019.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# frch.all %>% filter(datetimeAK >= "2019-01-01 00:00:00") %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "FRCH, 2019")
# 
# dev.off()
# 
# #2020
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testFRCH_SM_light_temp_depth.2020.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# frch.all %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "FRCH, 2020")
# 
# dev.off()
# 
# #2021
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testFRCH_SM_light_temp_depth.2021.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# frch.all %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "FRCH, 2021")
# 
# dev.off()
# 
# 
# 
# 
# 
# #POKE
# 
# 
# poke.combinded.par <- poke.combinded.par %>%
#   dplyr::rename(light = Calibrated.Value)
# 
# poke.combinded.par <- poke.combinded.par %>%
#   dplyr::rename(datetimeAK = DateTime)
# 
# poke.all <- merge(poke.combinded.par, poke.all,  by = "datetimeAK",all = TRUE)
# 
# 
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testPOKE_SM_light_temp_depth.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# poke.all %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "POKE, 2019 - 2021")
# 
# dev.off()
# 
# #2019
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testPOKE_SM_light_temp_depth.2019.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# poke.all %>% filter(datetimeAK >= "2019-01-01 00:00:00") %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "POKE, 2019")
# 
# dev.off()
# 
# #2020
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testPOKE_SM_light_temp_depth.2020.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# poke.all %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "POKE, 2020")
# 
# dev.off()
# 
# #2021
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testPOKE_SM_light_temp_depth.2021.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# poke.all %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "POKE, 2021")
# 
# dev.off()
# 
# 
# 
# 
# 
# 
# #VAUL
# 
# 
# vaul.combinded.par <- vaul.combinded.par %>%
#   dplyr::rename(light = Calibrated.Value)
# 
# vaul.combinded.par <- vaul.combinded.par %>%
#   dplyr::rename(datetimeAK = DateTime)
# 
# vaul.all <- merge(vaul.combinded.par, vaul.all,  by = "datetimeAK",all = TRUE)
# 
# 
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testVAUL_SM_light_temp_depth.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# vaul.all %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "VAUL, 2019 - 2021")
# 
# dev.off()
# #2019
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testVAUL_SM_light_temp_depth.2019.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# vaul.all %>% filter(datetimeAK >= "2019-01-01 00:00:00") %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "VAUL, 2019")
# 
# dev.off()
# 
# #2020
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testVAUL_SM_light_temp_depth.2020.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# vaul.all %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "VAUL, 2020")
# 
# dev.off()
# 
# #2021
# pdf(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testVAUL_SM_light_temp_depth.2021.pdf")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# vaul.all %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "VAUL, 2021")
# 
# dev.off()
# 
# 
# 
# #STRT
# 
# 
# strt.combinded.par <- strt.combinded.par %>%
#   dplyr::rename(light = Calibrated.Value)
# 
# strt.combinded.par <- strt.combinded.par %>%
#   dplyr::rename(datetimeAK = DateTime)
# 
# strt.combinded.par$datetimeAK <- lubridate::round_date(strt.combinded.par$datetimeAK, "15 minutes")
# 
# 
# 
# strt.all <- merge(strt.combinded.par, strt.all,  by = "datetimeAK", all = TRUE)
# 
# 
# png(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testSTRT_SM_light_temp_depth.png")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# strt.all %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "STRT, 2019 - 2021")
# 
# dev.off()
# #2019
# png(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testSTRT_SM_light_temp_depth.2019.png")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# strt.all %>% filter(datetimeAK >= "2019-01-01 00:00:00") %>% filter(datetimeAK <= "2020-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "STRT, 2019")
# 
# dev.off()
# 
# #2020
# png(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testSTRT_SM_light_temp_depth.2020.png")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# strt.all %>% filter(datetimeAK >= "2020-01-01 00:00:00") %>% filter(datetimeAK <= "2021-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "STRT, 2020")
# 
# dev.off()
# 
# #2021
# png(file = "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Plots/testSTRT_SM_light_temp_depth.2021.png")
# 
# labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
# strt.all %>% filter(datetimeAK >= "2021-01-01 00:00:00") %>% filter(datetimeAK <= "2022-01-01 00:00:00") %>% 
#   select(solar.time, depth, temp.water, light) %>%
#   gather(type, value, depth, temp.water, light) %>%
#   mutate(
#     type=ordered(type, levels=c('depth','temp.water','light')),
#     units=ordered(labels[type], unname(labels))) %>%
#   ggplot(aes(x=solar.time, y=value, color=type)) + geom_line() + 
#   facet_grid(units ~ ., scale='free_y') + theme_bw() +
#   scale_color_discrete('variable')+ labs(title = "STRT, 2021")
# 
# dev.off()
# 
# 
# 
# 
# 
