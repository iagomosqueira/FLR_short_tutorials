# 07-tutorial.R - DESC
# 07 - Stochastic projections, simulation and error/07-tutorial.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# install.packages(c("FLasher", "ggplotFL"), repos=structure(
#   c(CRAN="https://cloud.r-project.org/", FLR="https://flr-project.org/R")))

# Load required packages

library(FLasher)
library(ggplotFL)

# Load example data: ple4

load("cjm.RData")

# ARGS

yrs <- 2020:2040

catch(fisheries, by="fishery")

cbfis <- abind(lapply(fisheries, function(y)
  catch(y[[1]])[, ac(2019),,,,1]))
ratio <- c(cbfis/max(cbfis))

targ <- FLQuant(0.1, dimnames=list(year=yrs))

fwctrl <- fwdControl(c(
  lapply(yrs, function(y) list(year=y, biol=1, minAge=2, maxAge=6,
    quant="fbar", value=unlist(targ[, ac(y)]))),
  lapply(yrs, function(y) list(year=y, relYear=y, fishery=1, catch=1,
    relFishery=2, relCatch=1, quant="catch", value=ratio[1])),
  lapply(yrs, function(y) list(year=y, relYear=y, fishery=3, catch=1,
    relFishery=2, relCatch=1, quant="catch", value=ratio[3])),
  lapply(yrs, function(y) list(year=y, relYear=y, fishery=4, catch=1,
    relFishery=2, relCatch=1, quant="catch", value=ratio[4]))
  )
)

tes <- fwd(biol, fisheries, control=fwctrl)

plot(tes$biol)

plot(tes$fisheries)

plot(FLQuants(lapply(tes$fisheries, catch, 1)))

catch(tes$fisheries)











# --- CONDITION future biology and fishery

# - Weights, maturity, time of spawning (*.wt, mat, m.spwn)
# - Selectivity, discards ratio, timing of fishing (harvest,
#   discards.n / landings.n, harvest.spwn)

# SRR

srr <- fmle(transform(as.FLSR(ple4, model=bevholt), ssb=ssb, rec=rec))

plot(srr)

# --- PREPARE for forecasting

# CALL stf()
#  - 20 years ahead.
#  - wts, fbar disc ratio: means of 3 last years

fut <- stf(ple4, nyears=20, wts.nyears=3, fbar.nyears=3, disc.nyears=3)

fut <- propagate(fut, 300)


# --- SET future SRR deviances

# PLOT log residuals

plot(residuals(srr)) + geom_hline(yintercept=0)

# LogNormal deviances, with mean and SD as SRR residuals

devsrr <- rlnorm(300, FLQuant(mean(residuals(srr)),
  dimnames=list(age=1, year=2018:2037)), sd(residuals(srr)))

plot(devsrr)

# RESAMPLE past residuals

samp <- residuals(srr)[, sample(seq(dim(srr)[2]), 20, replace=TRUE)]
dimnames(samp) <- list(year=seq(2018, 2037))

devsamp <- rlnorm(300, samp, sd(residuals(srr)))

plot(FLQuants(LN=devsrr, RESAMP=devsamp))


# --- PROJECT for F=0

control <- fwdControl(year=2018:2037, quant="f", value=0)

pf0 <- fwd(fut, sr=srr, control=control, deviances=devsrr)

plot(pf0, control)


# --- PROJECT for F status quo

control <- fwdControl(year=2018:2037, quant="f", value=fbar(ple4)[, '2017'])

pfsq <- fwd(fut, sr=srr, control=control, deviances=devsrr)

plot(FLStocks(F0=pf0, Fsq=pfsq))


# --- PROJECT for constant catch

control <- fwdControl(year=2018:2037, quant="catch",
  value=yearMeans(catch(ple4)[, ac(2015:2017)]))

pcsq <- fwd(fut, sr=srr, control=control, deviances=devsrr)

plot(FLStocks(F0=pf0, Fsq=pfsq, Csq=pcsq))


# --- PROJECT for decreasing catch, 10% per year

control <- fwdControl(year=2018:2037, quant="catch",
  value=c(catch(ple4)[, '2017']) * 0.9 ^ (1:20))

pcde <- fwd(fut, sr=srr, control=control, deviances=devsrr)

plot(FLStocks(F0=pf0, Fsq=pfsq, Csq=pcsq, Cde=pcde))


# --- project for decreasing catch, 20% using relYear

control <- fwdControl(year=2018:2037, quant="catch",
  value=0.8, relYear=2017:2036)

pcre <- fwd(fut, sr=srr, control=control, deviances=devsrr)

plot(FLStocks(f0=pf0, fsq=pfsq, csq=pcsq, cde=pcde, cre=pcre))


# --- project for increasing catch, but min SSB

control <- fwdControl(
  list(year=2018:2037, quant="catch", value=1, relYear=2017:2036),
  list(year=2018:2037, quant="ssb_end", min=7e5))

pcin <- fwd(fut, sr=srr, control=control, deviances=devsrr)

plot(pcin)

# No. iters where target was not achieved

iterSums(catch(pcin)[, ac(2018:2037)] / catch(pcin)[, ac(2017:2036)] != 1)

plot(FLStocks(f0=pf0, fsq=pfsq, csq=pcsq, cde=pcde, cre=pcre, cin=pcin))


# --- project using stochastic targets

control <- fwdControl(
  lapply(2018:2037, function(x)
    list(year=x, quant="fbar", value=rnorm(300, 1, 0.4), relYear=2017)))

psto <- fwd(fut, sr=srr, control=control, deviances=devsrr)

plot(psto, control)

# No. iters where
iterSums(fbar(psto)[, ac(2018:2037)] != c(fbar(psto)[, '2017']))


# TUTORIAL at <https://flr-project.org/doc/Forecasting_on_the_Medium_Term_for_advice_using_FLasher.html>
