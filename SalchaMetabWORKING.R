
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


#Salcha Metabolism Remade


Salcha.exo.2021 <- read.csv(file=file.path("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Salcha/EXO_data/from_internal_harddrive/processed/SALCHA.EXO.aord.csv"))

ts_2021 <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/ts_2021.csv")

# setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Salcha/EXO_data/from_internal_harddrive/")

test <- full_join(ts_2021, Salcha.exo.2021, by = "datetimeAK")

write.csv(test, "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/salcha.2021.full.ts.csv")

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/")

EXO.list <- list.files(path="Metabolism-UAF", 
                       recursive=F, 
                       pattern="salcha.2021.full.ts.csv", 
                       full.names=TRUE)

EXO.cl <- lapply(EXO.list, 
                 read.csv, 
                 stringsAsFactors=FALSE, 
                 header = TRUE)


setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/")


# Format datetime
EXO.cl <- lapply(EXO.cl, function(x) {
  mutate(x, datetimeAK = as.POSIXct(datetimeAK, format = "%Y-%m-%d %H:%M:%S", tz= "America/Anchorage"))
})


### Fill gaps in Turbidity ###
anyNA(EXO.cl, recursive = TRUE)
lapply(EXO.cl, summary)

# Make univariate time series, covert to zoo, then to ts #
turb.only <- lapply(EXO.cl, '[', (c("datetimeAK", "Turbidity.FNU.mn")))

turb.ts <- lapply(turb.only, function(x) {read.zoo(x, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
})

turb.xts <- lapply(turb.ts, function(x) {as.xts(x)
})

# Apply auto.arima (default model in call to na_kalman) and kalman filter (default = smooth) to impute missing values using na.kalman from imputeTS #
turb.int <- lapply(turb.xts, function(x) {na_kalman(x)
})

anyNA(turb.int, recursive = TRUE)

# revert xts to dataframe
turb.ma.int = lapply(turb.int, function(x) {data.frame(datetimeAK=time(x), turb.ma.int=as.matrix(x))
})

turb.ma.int <- lapply(turb.ma.int, setNames, c("datetimeAK", "Turb.FNU.ma.int"))

# Rejoin to instrument record (EXO.cl)
EXO.cl.int <- map2(EXO.cl, turb.ma.int, ~merge(.x, .y, by = "datetimeAK"))

## Compare instrument record and with imputed turbidity time series
# to dataframe for compatibility with ggplot
EXO.cl.int.df <- bind_rows(EXO.cl.int, .id = "Site")


### Fill gaps in Temperature ###
anyNA(EXO.cl, recursive = TRUE)
lapply(EXO.cl, summary)

# Make univariate time series, covert to zoo, then to ts #
Temp.only <- lapply(EXO.cl, '[', (c("datetimeAK", "Temp.C.mn")))

Temp.ts <- lapply(Temp.only, function(x) {read.zoo(x, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
})

Temp.xts <- lapply(Temp.ts, function(x) {as.xts(x)
})


# Apply auto.arima (default model in call to na_kalman) and kalman filter (default = smooth) to impute missing values using na.kalman from imputeTS #
# type = "level" added to circumvent error with optim ("need finite values of 'fn'")
Temp.int <- lapply(Temp.xts, function(x) {na_kalman(x)
})

anyNA(Temp.int, recursive = TRUE)

# revert xts to dataframe
Temp.int = lapply(Temp.int, function(x) {data.frame(datetimeAK=time(x), Temp.C.int=as.matrix(x))
})

Temp.int <- lapply(Temp.int, setNames, c("datetimeAK", "Temp.C.int"))

# Rejoin to instrument record (EXO.cl)
EXO.cl.int <- map2(EXO.cl.int, Temp.int, ~merge(.x, .y, by = "datetimeAK"))

## Compare instrument record and with imputed turbidity time series
# to dataframe for compatibility with ggplot
EXO.cl.int.df <- bind_rows(EXO.cl.int, .id = "Site")

Temp.int.pl <- EXO.cl.int.df %>% 
  ggplot(aes(x = datetimeAK, y = Temp.C.mn)) +
  geom_point(color = "blue") +
  #geom_line(color = "blue") +
  geom_line(aes(x = datetimeAK, y = Temp.C.int), color = "green") +
  xlim(as.POSIXct(c("2021-05-01", "2021-10-27"))) +
  facet_wrap(~Site)

Temp.int.pl
# 
# ############### psat addition
# 
# 
# setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/")
# 
# EXO.list <- list.files(path="Metabolism-UAF", 
#                        recursive=F, 
#                        pattern="salcha.2021.full.ts.csv", 
#                        full.names=TRUE)
# 
# EXO.cl <- lapply(EXO.list, 
#                  read.csv, 
#                  stringsAsFactors=FALSE, 
#                  header = TRUE)
# 
# 
# setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/")
# 
# ### Fill gaps in Temperature ###
# anyNA(EXO.cl, recursive = TRUE)
# lapply(EXO.cl, summary)
# 
# # Make univariate time series, covert to zoo, then to ts #
# psat.only <- lapply(EXO.cl, '[', (c("datetimeAK", "ODO.Psat.mn")))
# 
# psat.ts <- lapply(psat.only, function(x) {read.zoo(x, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
# })
# 
# psat.xts <- lapply(psat.ts, function(x) {as.xts(x)
# })
# 
# 
# # Apply auto.arima (default model in call to na_kalman) and kalman filter (default = smooth) to impute missing values using na.kalman from imputeTS #
# # type = "level" added to circumvent error with optim ("need finite values of 'fn'")
# psat.int <- lapply(psat.xts, function(x) {na_kalman(x, type = "level")
# })
# 
# anyNA(psat.int, recursive = TRUE)
# 
# # revert xts to dataframe
# psat.int = lapply(psat.int, function(x) {data.frame(datetimeAK=time(x), psat.C.int=as.matrix(x))
# })
# 
# psat.int <- lapply(psat.int, setNames, c("datetimeAK", "ODO.Psat.int"))
# 
# # Rejoin to instrument record (EXO.cl)
# EXO.cl.int <- map2(EXO.cl.int, psat.int, ~merge(.x, .y, by = "datetimeAK"))
# 
# ## Compare instrument record and with imputed turbidity time series
# # to dataframe for compatibility with ggplot
# EXO.cl.int.df <- bind_rows(EXO.cl.int, .id = "Site")
# # 
# # Temp.int.pl <- EXO.cl.int.df %>% 
# #   ggplot(aes(x = datetimeAK, y = ODO.Psat.mn)) +
# #   geom_point(color = "blue") +
# #   #geom_line(color = "blue") +
# #   geom_line(aes(x = datetimeAK, y = ODO.Psat.int), color = "green") +
# #   xlim(as.POSIXct(c("2021-05-01", "2021-10-27"))) +
# #   facet_wrap(~Site)
# # 
# # Temp.int.pl
# 
# 
# 
# 
# 
# 
# ##############










### Fill gaps in Temperature ###
anyNA(EXO.cl, recursive = TRUE)
lapply(EXO.cl, summary)

# Make univariate time series, covert to zoo, then to ts #
DOmgl.only <- lapply(EXO.cl, '[', (c("datetimeAK", "ODO.mgL.mn")))

DOmgl.ts <- lapply(DOmgl.only, function(x) {read.zoo(x, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
})

DOmgl.xts <- lapply(DOmgl.ts, function(x) {as.xts(x)
})


# Apply auto.arima (default model in call to na_kalman) and kalman filter (default = smooth) to impute missing values using na.kalman from imputeTS #
# type = "level" added to circumvent error with optim ("need finite values of 'fn'")
DOmgl.int <- lapply(DOmgl.xts, function(x) {na_kalman(x, type = "level")
})

anyNA(DOmgl.int, recursive = TRUE)

# revert xts to dataframe
DOmgl.int = lapply(DOmgl.int, function(x) {data.frame(datetimeAK=time(x), DOmgl.C.int=as.matrix(x))
})

DOmgl.int <- lapply(DOmgl.int, setNames, c("datetimeAK", "ODO.mgL.int"))

# Rejoin to instrument record (EXO.cl)
EXO.cl.int <- map2(EXO.cl.int, DOmgl.int, ~merge(.x, .y, by = "datetimeAK"))

## Compare instrument record and with imputed turbidity time series
# to dataframe for compatibility with ggplot
EXO.cl.int.df <- bind_rows(EXO.cl.int, .id = "Site")

ODOmgL.int.pl <- EXO.cl.int.df %>% 
  ggplot(aes(x = datetimeAK, y = ODO.mgL.mn)) +
  geom_point(color = "blue") +
  #geom_line(color = "blue") +
  geom_line(aes(x = datetimeAK, y = ODO.mgL.int), color = "green") +
  xlim(as.POSIXct(c("2021-05-01", "2021-10-27"))) +
  facet_wrap(~Site)

ODOmgL.int.pl




#Light

CRREL <- read.csv("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Crell_met_PAR.csv", skip = 5)
# CRREL <- read_csv(here("Crell_met_PAR.csv"), skip = 5)

CRREL$datetimeAK <- strptime(CRREL$Time, '%m/%d/%Y %H:%M')

CRREL$datetimeAK <- as.POSIXct(as.character(CRREL$datetimeAK))

CRREL <- CRREL %>% rename(ParWm2 = CPCRW.CRREL.Main.Met.Station..PAR_Den_AVG.W.m2.)

Salcha.exo.2021$datetimeAK <- as.POSIXct(Salcha.exo.2021$datetimeAK)

ts_2021$datetimeAK <- as.POSIXct(ts_2021$datetimeAK, tz = "America/Anchorage")


CRREL_SALCHA1 <- full_join(ts_2021, CRREL, by = "datetimeAK")


write.csv(CRREL_SALCHA1, "C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/Crell_met_PAR2.csv")


setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/")

light.list <- list.files(path="Metabolism-UAF", 
                         recursive=F, 
                         pattern="Crell_met_PAR2", 
                         full.names=TRUE)

light.cl <- lapply(light.list, 
                   read.csv, 
                   stringsAsFactors=FALSE, 
                   header = TRUE)


# Format datetime
light.cl <- lapply(light.cl, function(x) {
  mutate(x, datetimeAK = as.POSIXct(datetimeAK, format = "%Y-%m-%d %H:%M:%S", tz= "America/Anchorage"))
})


### Fill gaps in Light ###
anyNA(light.cl, recursive = TRUE)
lapply(light.cl, summary)

# Make univariate time series, covert to zoo, then to ts #
light.only <- lapply(light.cl, '[', (c("datetimeAK", "ParWm2")))

light.ts <- lapply(light.only, function(x) {read.zoo(x, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
})

light.xts <- lapply(light.ts, function(x) {as.xts(x)
})

# Apply auto.arima (default model in call to na_kalman) and kalman filter (default = smooth) to impute missing values using na.kalman from imputeTS #
light.int <- lapply(light.xts, function(x) {na_kalman(x)
})

anyNA(light.int, recursive = TRUE)

# revert xts to dataframe
light.ma.int = lapply(light.int, function(x) {data.frame(datetimeAK=time(x), turb.ma.int=as.matrix(x))
})

light.ma.int <- lapply(light.ma.int, setNames, c("datetimeAK", "ParWm2.int"))


# Rejoin to instrument record (EXO.cl)
EXO.cl.int <- map2(EXO.cl.int, light.ma.int, ~merge(.x, .y, by = "datetimeAK"))

## Compare instrument record and with imputed turbidity time series
# to dataframe for compatibility with ggplot
EXO.cl.int.df <- bind_rows(EXO.cl.int, .id = "Site")


light.int.pl <- EXO.cl.int.df %>% 
  ggplot(aes(x = datetimeAK, y = ParWm2.int)) +
  geom_line(color = "blue") +
  #geom_line(color = "blue") +
  # geom_line(aes(x = datetimeAK, y = ParWm2.int), color = "green") +
  # xlim(as.POSIXct(c("2021-05-01", "2021-10-27"))) +
  facet_wrap(~Site)

light.int.pl



# Discharge and Depth
dischargeSalcha <- read.csv("Metabolism-UAF/dischargeSalchaTXT.csv")


dischargeSalcha <- dischargeSalcha[-c(1), ]

dischargeSalcha$datetimeAK <- as.POSIXct(mdy_hm(dischargeSalcha$datetime))

dischargeSalcha$datetimeAK <- force_tz(dischargeSalcha$datetimeAK, "America/Anchorage")

dischargeSalcha <- dischargeSalcha %>% rename(depth = X1761_00065)
dischargeSalcha <- dischargeSalcha %>% rename(discharge = X1760_00060)

dischargeSalcha <- dischargeSalcha %>% select(discharge, depth, datetimeAK)

# convert to Meters 

dischargeSalcha$depth <- as.numeric(dischargeSalcha$depth) * 0.3048
dischargeSalcha$discharge <- as.numeric(dischargeSalcha$discharge) * 0.0283168


dischargeSalcha <- full_join(ts_2021, dischargeSalcha, by = "datetimeAK")

write.csv(dischargeSalcha, ("C:/Users/jacob/OneDrive - University of Alaska/GitHub/Metabolism-UAF/q_data.csv"))



setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/")

Q.list <- list.files(path="Metabolism-UAF", 
                         recursive=F, 
                         pattern="q_data.csv", 
                         full.names=TRUE)

Q.cl <- lapply(Q.list, 
                   read.csv, 
                   stringsAsFactors=FALSE, 
                   header = TRUE)


# Format datetime
Q.cl <- lapply(Q.cl, function(x) {
  mutate(x, datetimeAK = as.POSIXct(datetimeAK, format = "%Y-%m-%d %H:%M:%S", tz= "America/Anchorage"))
})


### Fill gaps in Light ###
anyNA(Q.cl, recursive = TRUE)
lapply(Q.cl, summary)

# Make univariate time series, covert to zoo, then to ts #
Q.only <- lapply(Q.cl, '[', (c("datetimeAK", "discharge")))

Q.ts <- lapply(Q.only, function(x) {read.zoo(x, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
})

Q.xts <- lapply(Q.ts, function(x) {as.xts(x)
})

# Apply auto.arima (default model in call to na_kalman) and kalman filter (default = smooth) to impute missing values using na.kalman from imputeTS #
Q.int <- lapply(Q.xts, function(x) {na_kalman(x, type = "level")
})

anyNA(Q.int, recursive = TRUE)

# revert xts to dataframe
Q.ma.int = lapply(Q.int, function(x) {data.frame(datetimeAK=time(x), turb.ma.int=as.matrix(x))
})

Q.ma.int <- lapply(Q.ma.int, setNames, c("datetimeAK", "Q.int"))


# Rejoin to instrument record (EXO.cl)
EXO.cl.int <- map2(EXO.cl.int, Q.ma.int, ~merge(.x, .y, by = "datetimeAK"))

## Compare instrument record and with imputed turbidity time series
# to dataframe for compatibility with ggplot
EXO.cl.int.df <- bind_rows(EXO.cl.int, .id = "Site")


Q.int.pl <- EXO.cl.int.df %>% 
  ggplot(aes(x = datetimeAK, y = Q.int)) +
  geom_line(color = "blue") +
  #geom_line(color = "blue") +
  # geom_line(aes(x = datetimeAK, y = ParWm2.int), color = "green") +
  # xlim(as.POSIXct(c("2021-05-01", "2021-10-27"))) +
  facet_wrap(~Site)

Q.int.pl







#depth

setwd("C:/Users/jacob/OneDrive - University of Alaska/GitHub/")

depth.list <- list.files(path="Metabolism-UAF", 
                     recursive=F, 
                     pattern="q_data.csv", 
                     full.names=TRUE)

depth.cl <- lapply(depth.list, 
               read.csv, 
               stringsAsFactors=FALSE, 
               header = TRUE)


# Format datetime
depth.cl <- lapply(depth.cl, function(x) {
  mutate(x, datetimeAK = as.POSIXct(datetimeAK, format = "%Y-%m-%d %H:%M:%S", tz= "America/Anchorage"))
})


### Fill gaps in Light ###
anyNA(depth.cl, recursive = TRUE)
lapply(depth.cl, summary)

# Make univariate time series, covert to zoo, then to ts #
depth.only <- lapply(depth.cl, '[', (c("datetimeAK", "depth")))

depth.ts <- lapply(depth.only, function(x) {read.zoo(x, index.column=1, format="%Y-%m-%d %H:%M:%S", tz="America/Anchorage")
})

depth.xts <- lapply(depth.ts, function(x) {as.xts(x)
})

# Apply auto.arima (default model in call to na_kalman) and kalman filter (default = smooth) to impute missing values using na.kalman from imputeTS #
depth.int <- lapply(depth.xts, function(x) {na_kalman(x, type = "level")
})

anyNA(depth.int, recursive = TRUE)

# revert xts to dataframe
depth.ma.int = lapply(depth.int, function(x) {data.frame(datetimeAK=time(x), turb.ma.int=as.matrix(x))
})

depth.ma.int <- lapply(depth.ma.int, setNames, c("datetimeAK", "depth.int"))


# Rejoin to instrument record (EXO.cl)
EXO.cl.int <- map2(EXO.cl.int, depth.ma.int, ~merge(.x, .y, by = "datetimeAK"))

## Compare instrument record and with imputed turbidity time series
# to dataframe for compatibility with ggplot
EXO.cl.int.df <- bind_rows(EXO.cl.int, .id = "Site")


depth.int.pl <- EXO.cl.int.df %>% 
  ggplot(aes(x = datetimeAK, y = depth.int)) +
  geom_line(color = "blue") +
  #geom_line(color = "blue") +
  # geom_line(aes(x = datetimeAK, y = ParWm2.int), color = "green") +
  # xlim(as.POSIXct(c("2021-05-01", "2021-10-27"))) +
  facet_wrap(~Site)

depth.int.pl


#Gap filled Data frame:
#DO, water temp, Time
Salcha.exo.2021 <- EXO.cl.int.df %>% select(ODO.mgL.int, Temp.C.int, datetimeAK, ParWm2.int, Q.int, depth.int)


anyNA(Salcha.exo.2021, recursive = TRUE)
lapply(Salcha.exo.2021, summary)

Salcha.exo.2021$datetimeAK <- force_tz(as.POSIXct(Salcha.exo.2021$datetimeAK), "America/Anchorage")


Salcha.exo.2021$ODO.mgL.int <- as.numeric(Salcha.exo.2021$ODO.mgL.int)
Salcha.exo.2021$Temp.C.int <- as.numeric(Salcha.exo.2021$Temp.C.int)



# 
# #Join them together
# salchaData <- join(Salcha.exo.2021, dischargeSalcha, by = "datetimeAK")


salchaData <- Salcha.exo.2021

salchaData$datetimeAK <- as.POSIXct(salchaData$datetimeAK) 

#Solar Time and Light

salchaData$solar.time <- calc_solar_time(salchaData$datetimeAK, -146.9281)

salchaData$light <- calc_light(salchaData$solar.time, 64.47153, -146.9281)





bayes_specs <- specs(bayes_name,
                     burnin_steps=10000, saved_steps=5000, n_cores=8,
                     # GPP_daily_lower = 0, ER_daily_upper = 0,
                    
)

bayes_specs

mm.test.salcha.new <- metab(bayes_specs, data=final.salcha.DT)


test <- get_fit(mm.test.salcha.new)

test


testkbin <- test$KQ_binned
view(testkbin)

view(test$overall)
view(test$daily)
view(test$inst)

view(test$KQ_overall)



dev.off()
dev.set(dev.next())

mcmc3 <- get_mcmc(mm.test.salcha.new)
output79 <- rstan::traceplot(mcmc3, pars = "K600_daily" , nrow=3)
output79

output78 <- rstan::traceplot(mcmc3, pars = "GPP_daily" , nrow=3)
output78

output75 <- rstan::traceplot(mcmc3, pars = "ER_daily" , nrow=3)
output75


test9 <- get_params(mm.test.salcha.new)

here()

# write.csv(test9, here("burnin1000_2021_july-june18_salcha.csv"))

write.csv(test$daily, here("burnin1000_2021_aug-sept18_salcha_extra.csv"))


ggplot(data=test9, aes(x=GPP.daily, y=ER.daily)) + geom_point()
ggplot(data=test9, aes(x=GPP.daily, y=K600.daily)) + geom_point()
ggplot(data=test9, aes(x=ER.daily, y=K600.daily)) + geom_point()



# # defining start date
# start_date <- ymd_hms("2021/07/01 00:00:00")
# 
# # defining end date
# end_date <- ymd_hms("2021/08/01 00:00:00")
# 
# # generating range of dates
# range <- seq(start_date, end_date,"days")

