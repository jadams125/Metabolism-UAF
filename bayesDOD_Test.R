
library(streamMetabolizer)
library(here)


#trial Bayes metab run


bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_name

bayes_name_AppEtAL <- "b_Kb_oipi_tr_plrckm.stan"
bayes_name_AppEtAL
#MOOS
bayes_specs <- specs(bayes_name, 
                     burnin_steps=2000, saved_steps=1000, n_cores=8, n_chains = 4, 
                     GPP_daily_lower = 0, ER_daily_upper = 0
                     )

MOOS.comb <- read.csv(here("outputs", "moos.comb.csv"))
MOOS.comb$solar.time <- as.POSIXct(MOOS.comb$solar.time, tz = "UTC")

# moos.2019 <-  MOOS.comb %>% filter(datetimeAK < "2019-12-31 00:00:00")

data.moos.mm <- na.omit(MOOS.comb) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)





mm.test.moos <- metab(bayes_specs, data=data.moos.mm.19)


# # Warning message:
# In metab_fun(specs = specs, data = data, data_daily = data_daily,  :
#                Modeling failed
#              Warnings:
#                There were 4 chains where the estimated Bayesian Fraction of Missing Information was low. See
#              https://mc-stan.org/misc/warnings.html#bfmi-low
#              Examine the pairs() plot to diagnose sampling problems  Errors:
#                cannot allocate vector of size 8 Kb

Predict.moos <- predict_metab(mm.test.moos)
fit.moos <- get_fit(mm.test.moos)
fit.moos

pairs(fit.moos)

saveRDS(fit.moos, "outputs/moos.fit.rds")

fit.moos.df <- data.frame(fit.moos)

#save fits
daily.fit.moos <- fit.moos$daily
write.csv(daily.fit.moos,"outputs/daily.fit.moos.csv", row.names = FALSE)

overall.fit.moos <- fit.moos$overall
write.csv(overall.fit.moos,"outputs/overall.fit.moos.csv", row.names = FALSE)

inst.fit.moos <-  fit.moos$inst
write.csv(inst.fit.moos,"outputs/inst.fit.moos.csv", row.names = FALSE)

KQ_overall.moos <- fit.moos$KQ_overall
write.csv(KQ_overall.moos,"outputs/KQ_overall.moos.csv", row.names = FALSE)

KQ_binned.moos <- fit.moos$KQ_binned
write.csv(KQ_binned.moos,"outputs/KQ_binned.moos.csv", row.names = FALSE)

moos.fit.warnings <- fit.moos$warnings
writeLines(moos.fit.warnings, "outputs/moos.fit.warnings.txt")

moos.fit.errors <- fit.moos$errors
writeLines(moos.fit.errors, "outputs/moos.fit.errors.txt")


get_fitting_time(mm.test.moos)

do.preds.moos <- predict_DO(mm.test.moos)

png("plots/DO_preds_moos.png", width = 2000, height = 2000, res = 300)
plot_DO_preds(predict_DO(mm.test.moos))
dev.off()



moos.metab.plot.1921 <- plot_metab_preds(mm.test.moos, style = "ggplot2")
moos.metab.plot.1921 + labs(title="moos metab 2019- 2021")

str(mm.test.moos)
view(mm.test.moos)

Predict.moos <- data.frame(Predict.moos)
write.csv(Predict.moos,"outputs/moos.metab.1921.csv", row.names = FALSE)




#POKE
POKE.comb <- read.csv(here("outputs", "poke.comb.csv"))
data.poke.mm <- na.omit(POKE.comb) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

data.poke.mm$solar.time <- as.POSIXct(data.poke.mm$solar.time, tz = "UTC",)

mm.test.poke <- metab(bayes_specs, data=data.poke.mm)

Predict.poke <- predict_metab(mm.test.poke)
poke.metab.plot.1921 <- plot_metab_preds(mm.test.poke, style = "ggplot2")
poke.metab.plot.1921 + labs(title="poke metab 2019- 2021")

str(mm.test.poke)
view(mm.test.poke)

Predict.poke <- data.frame(Predict.poke)
write.csv(Predict.poke,"outputs/poke.metab.1921.csv", row.names = FALSE)


#FRCH
setwd(here())
FRCH.comb <- read.csv(here("outputs", "frch.comb.csv"))
data.frch.mm <- na.omit(FRCH.comb) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

data.frch.mm$solar.time <- as.POSIXct(data.frch.mm$solar.time, tz = "UTC")


mm.test.frch <- metab(bayes_specs, data=data.frch.mm)

Predict.frch <- predict_metab(mm.test.frch)
frch.metab.plot.1921 <- plot_metab_preds(mm.test.frch, style = "ggplot2")
frch.metab.plot.1921 + labs(title="frch metab 2019- 2021")

str(mm.test.frch)
view(mm.test.frch)

Predict.frch <- data.frame(Predict.frch)
write.csv(Predict.frch,"outputs/frch.metab.1921.csv", row.names = FALSE)




#VAUL
data.vaul.mm <- na.omit(VAUL.comb) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

mm.test.vaul <- metab(bayes_specs, data=data.vaul.mm)

Predict.vaul <- predict_metab(mm.test.vaul)
vaul.metab.plot.1921 <- plot_metab_preds(mm.test.vaul, style = "ggplot2")
vaul.metab.plot.1921 + labs(title="vaul metab 2019- 2021")

str(Predict.vaul)
view(Predict.vaul)

setwd(here())
getwd()
here()

Predict.vaul <- data.frame(Predict.vaul)
write.csv(Predict.vaul,"outputs/vaul.metab.1921.csv", row.names = FALSE)



#STRT
data.strt.mm <- na.omit(STRT.comb) %>%
  select(solar.time, depth, DO.obs, DO.sat, temp.water, light, discharge)

mm.test.strt <- metab(bayes_specs, data=data.strt.mm)

Predict.strt <- predict_metab(mm.test.strt)
strt.metab.plot.1921 <- plot_metab_preds(mm.test.strt, style = "ggplot2")
strt.metab.plot.1921 + labs(title="strt metab 2019- 2021")

str(mm.test.strt)
view(mm.test.strt)
