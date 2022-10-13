
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

library(imputeTS)
library(itsmr)
library(purrr)


day <- correctedSalchaDT %>% filter(as.character(solar.time) >= "2021-08-18 03:58:54" & as.character(solar.time) < "2021-08-20 03:58:54") 

TS

ggplot(data=day, aes(x=solar.time, y=DO.obs)) + geom_point()

ggplot(data=correctedSalchaDT, aes(x=solar.time, y=DO.obs)) + geom_point()

yearTD <- rbind(burnin1000_2021_may_june18_salcha_extra, burnin1000_2021_june_july18_salcha_extra,burnin1000_2021_july_june18_salcha_extra, burnin1000_2021_aug_sept18_salcha_extra)





gpp <- ggplot(data=yearTD, aes(x=date, y=GPP_mean)) + geom_point(color = "chartreuse4") + geom_errorbar(aes(ymin=GPP_mean-GPP_sd, ymax=GPP_mean+GPP_sd), width=.2, position=position_dodge(0.05), color = "chartreuse4")

gpp

er <- ggplot(data=yearTD, aes(x=date, y=ER_mean)) + geom_point(color = "firebrick3") + geom_errorbar(aes(ymin=ER_mean-ER_sd, ymax=ER_mean+ER_sd), width=.2, position=position_dodge(0.05), color = "firebrick3") + ylim(-15, 1)


rhat <- ggplot(data=yearTD, aes(x=date)) + geom_point(aes(y=GPP_Rhat, colour = "GPP")) + geom_point(aes(y=ER_Rhat, colour = "ER")) +
  labs(y = "RHAT") + theme(legend.position="bottom")
rhat



grid.arrange(gpp, er,rhat, nrow=3)





