# model_refpts.R - Estimate reference points (benchmark)
# 2020_sol.27.4_assessment/model_refpts.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# https://github.com/ices-tools-prod/msy
devtools::install_github("ices-tools-prod/msy")

library(msy)
library(FLCore)

# LOAD AAP run sol.27.4 2020

load("sol274.RData")

# SETTINGS 

Fs <- seq(0, 1.5, length=51)
nsamp <- 2000

# USE 5 y for selex and biology
bio.years <- c(-4, -0) + dims(sol)$maxyear
sel.years <- c(-4, -0) + dims(sol)$maxyear

# REMOVE no years
remove.years <- NULL

# FIT all models

srfit0 <- eqsr_fit(sol, nsamp = nsamp, models = c("Segreg", "Ricker", "Bevholt"))

eqsr_plot(srfit0)

# NOTE Segreg CHOSEN, no biological support for Ricker

# FIT segreg to obtain BLIM & BPA (Type 2)

srfit1 <- eqsr_fit(sol, nsamp = nsamp,
  models = "Segreg")

Blim <- srfit1[["sr.det"]][,"b"]

# PA from cv=0.2, exp(1.645 * 0.2)

pa <- exp(1.645 * 0.2)

Bpa <- Blim * pa

# SIMULATE all models w/10 y, Fcv=Fphi=0, Btrigger=0
srsim1 <- eqsim_run(srfit1,
  bio.years = bio.years, sel.years = sel.years,
  Fcv = 0, Fphi = 0,
  Btrigger=0, Blim = Blim, Bpa = Bpa,
  Fscan = Fs,
  verbose = FALSE)

# EXTRACT Flim and Fpa
Flim <- srsim1$Refs2["catF", "F50"]
Fpa <- Flim / pa

# FIT all models, REMOVE last remove.years
srfit2 <- eqsr_fit(sol, nsamp = nsamp,
  models = "Segreg",
  remove.years=remove.years)

# SIMULATE, Fcv=0.212, Fphi=0.423 (WKMSYREF4)
srsim2 <- eqsim_run(srfit2,
  bio.years = bio.years, sel.years = sel.years,
  bio.const = FALSE, sel.const = FALSE,
  Fcv=0.212, Fphi=0.423,
  Btrigger=0, Blim = Blim, Bpa = Bpa,
  Fscan = Fs,
  verbose = FALSE)

cFmsy <- srsim2$Refs2["lanF", "medianMSY"]
F05 <- srsim2$Refs2["catF", "F05"]

# SIMULATE for Btrigger
srsim3 <- eqsim_run(srfit2,
  bio.years = bio.years, sel.years = sel.years,
  bio.const = FALSE, sel.const = FALSE,
  Fcv = 0, Fphi = 0,
  Btrigger=0, Blim = Blim, Bpa = Bpa,
  Fscan = Fs,
  verbose = FALSE)

# Btrigger < Bpa -> Bpa
x <- srsim3$rbp[srsim3$rbp$variable=="Spawning stock biomass", ]
cBtrigger <- x[which(abs(x$Ftarget - cFmsy) == min(abs(x$Ftarget - cFmsy))), "p05"]

# SIMULATE
srsim4 <- eqsim_run(srfit2,
  bio.years = bio.years, sel.years = sel.years,
  bio.const = FALSE, sel.const = FALSE,
  Fcv=0.212, Fphi=0.423,
  Btrigger=cBtrigger, Blim = Blim, Bpa = Bpa,
  Fscan = seq(0, 1.2, len = 40),
  verbose = FALSE)

F05 <- srsim4$Refs2["catF", "F05"]

# If F05 < Fmsy, then Fmsy = F05
if(cFmsy > F05) {
  Fmsy <- F05
} else {
  Fmsy <- cFmsy
}

# IF Btrigger < Bpa, then Btrigger = Bpa, then redo srsim4
# OR IF Fbar 5yr != Fmsy

if(cBtrigger < Bpa | all(tail(fbar(sol), 5) > Fmsy)) {

  Btrigger <- Bpa

  srsim4 <- eqsim_run(srfit2,
    bio.years = bio.years, sel.years = sel.years,
    bio.const = FALSE, sel.const = FALSE,
    Fcv=0.212, Fphi=0.423,
    Btrigger=Btrigger, Blim = Blim, Bpa = Bpa,
    Fscan = seq(0, 1.2, len = 40),
    verbose = FALSE)

  cFmsy <- srsim4$Refs2["lanF", "medianMSY"]
  F05 <- srsim4$Refs2["catF", "F05"]

  # If F05 < Fmsy, then Fmsy = F05
  if(cFmsy > F05) {
    Fmsy <- F05
  }
}

# FMSY (low - upp) w/o Btrigger
lFmsy <- srsim3$Refs2["lanF", "Medlower"]
uFmsy <- srsim3$Refs2["lanF", "Medupper"]

# REFPTS

refpts <- FLPar(Btrigger=Btrigger, Fmsy=Fmsy, Blim=Blim, Bpa=Bpa,
  Flim=Flim, Fpa=Fpa, lFmsy=lFmsy, uFmsy=uFmsy, F05=F05,
  units=c("t", "f", rep("t", 2), rep("f", 4), rep("t", 3)))

# SENSITIVITY

# sel.years

years <- setNames(lapply(seq(2009, by=-1, length=10), seq, by=9, length=2),
  seq(2018, by=-1, length=10))

# RUN w/srsim2 conditions
sens_sel.years <- parallel::mclapply(years, eqsim_run, fit=srfit2,
  bio.years = bio.years, bio.const = FALSE, sel.const = FALSE,
  Fcv=0.212, Fphi=0.423, Btrigger=0, Blim = Blim, Bpa = Bpa,
  Fscan = Fs, verbose = FALSE, mc.cores=3)

sensel_fmsy <- cbind(do.call(rbind, lapply(sens_sel.years, function(x) {
  data.frame(Fmsy05=x$Refs2['lanF', 'Medlower'],
    Fmsy95=x$Refs2['lanF', 'Medupper'],
    FmsyMed=x$Refs2['lanF', 'medianMSY'])
  })), year=names(years))

# bio.years

sens_bio.years <- parallel::mclapply(years, eqsim_run, fit=srfit2,
  sel.years = sel.years, bio.const = FALSE, sel.const = FALSE,
  Fcv=0.212, Fphi=0.423, Btrigger=0, Blim = Blim, Bpa = Bpa,
  Fscan = Fs, verbose = FALSE, mc.cores=3)

senbiol_fmsy <- cbind(do.call(rbind, lapply(sens_bio.years, function(x) {
  data.frame(Fmsy05=x$Refs2['lanF', 'Medlower'],
    Fmsy95=x$Refs2['lanF', 'Medupper'],
    FmsyMed=x$Refs2['lanF', 'medianMSY'])
  })), year=names(years))

sens <- list(sel.years=sensel_fmsy, bio.years=senbiol_fmsy)

# SAVE

save(sens, refpts, file="sol_refpts.RData", compress="xz")
