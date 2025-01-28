# 04-tutorial.R - DESC
# 04 - Short Term Forecasting with FLasher/04-tutorial.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# install.packages("FLasher", repos=structure(c(CRAN="https://cloud.r-project.org/",
#   FLR="https://flr-project.org/R")))

# Load required packages

library(FLasher)
library(ggplotFL)

# Load example data: ple4

data(ple4)


# --- PREPARE for forecasting

# CALL stf()
#  - 3 years ahead.
#  - wts, fbar disc ratio: means of 3 last years

fut <- stf(ple4, nyears=3, wts.nyears=3, fbar.nyears=3, disc.nyears=3)

fut <- fwdWindow(ple4, end=2020)


# CHECK averages

yearMeans(stock.wt(ple4)[, ac(2015:2017)]) %/% stock.wt(fut)[, ac(2018:2020)]

# TODO: ALTER future stock.wt with a decreasing 10% trend


# SETUP stock-recruits relationship (geomean of all but last 3 years)

gmrec <- exp(mean(log(window(rec(ple4), end=-3))))

srr <- predictModel(model=rec~a, params=FLPar(a=gmrec))


# --- PROJECTIONS

# 1. SETUP projection target for 2018: F status quo

control <- fwdControl(
  list(year=2018, quant="fbar", value=0.21)
)

# RUN fwd

fsq <- fwd(fut, sr=srr, control=control)

# CHECK results

fbar(fsq)[, ac(2018:2020)]

compare(fsq, control)

# OBTAIN outputs

catch(fsq)[, "2018"]

landings(fsq)[, "2018"]

ssb(fsq)[, "2018"]


# 2. SETUP projections for 2018:2020 with different values

control <- fwdControl(
  list(year=2018:2020, quant="fbar",
    value=c(fbar(ple4)[, "2017"]) * c(1, 0.6, 0.4))
)

fde <- fwd(fut, sr=srr, control=control)

plot(window(fde, start=2000)) +
  geom_vline(xintercept=2018, colour="gray")

compare(fde, control)

# TODO: Forecast the effect of no fishing


# 3. SETUP projections for 2018:2020 with different targets: 2018 TAC + FMSY

control <- fwdControl(
  list(year=2018, quant="catch", value=112643),
  list(year=2019:2020, quant="fbar", value=0.21)
)

fmp <- fwd(fut, sr=srr, control=control)

plot(window(fmp, start=2000)) +
  geom_vline(xintercept=2018, colour="gray")

compare(fmp, control)


# COMPARE them all

plot(FLStocks(Fsq=fsq, Fde=fde, Fmp=fmp)) +
  xlim(c(2000, NA)) +
  geom_vline(xintercept=2018, colour="gray")


# 4. SETUP projection with limits: !catch_y > catch_y-1 * 1.15

control <- fwdControl(
  list(year=2018:2020, quant="fbar", value=0.40),
  list(year=2018:2020, quant="catch", max=1.25, min=0.75, relYear=2017:2019)
)

fli <- fwd(fut, sr=srr, control=control)

plot(window(fli, start=2000)) +
  geom_vline(xintercept=2018, colour="gray")

# DEBUG compare(fli, control)

# CHECK results

fbar(fli)[, ac(2018:2020)]

catch(fli)[, ac(2018:2020)] / catch(fli)[, ac(2017:2019)] 


# --- UNCERTAINTY

fus <- propagate(fut, 300)

# 1. SRR deviances in the future

control <- fwdControl(
  list(year=2018:2020, quant="fbar", value=0.21),
)

srdevs <- rlnorm(300, rec(fus[, ac(2018:2020)]) %=% 0, 1)

fde <- fwd(fus, sr=srr, control=control, deviances=srdevs)

plot(window(fde, start=2000)) +
  geom_vline(xintercept=2018, colour="gray")

# 2. In stock

# EXTEND future stock along iter dimension

stock.wt(fus) <- rlnorm(300, log(stock.wt(fut)), 0.25)
catch.wt(fus) <- rlnorm(300, log(catch.wt(fut)), 0.25)
landings.wt(fus) <- rlnorm(300, log(landings.wt(fut)), 0.25)

control <- fwdControl(year=2018:2020, quant="fbar", value=0.21)

fst <- fwd(fus, sr=srr, control=control)

plot(window(fst, start=2000)) +
  geom_vline(xintercept=2018, colour="gray")

# 3. In targets

control <- fwdControl(
  list(year=2018, quant="catch", value=rlnorm(300, log(125000), 0.3)),
  list(year=2019, quant="catch", value=rlnorm(300, log(125000), 0.3)),
  list(year=2020, quant="catch", value=rlnorm(300, log(125000), 0.3))
)

fsc <- fwd(fus, sr=srr, control=control)

plot(window(fsc, start=2000)) +
  geom_vline(xintercept=2018, colour="gray")

# 4. In SRR

srs <- predictModel(model=rec~a, params=FLPar(a=rlnorm(300, log(9e5), 0.4)))

fsr <- fwd(fus, sr=srs, control=control)

plot(window(fsr, start=2000)) +
  geom_vline(xintercept=2018, colour="gray")


# --- ERRORS

# 1. Targets might not be achievable

control <- fwdControl(
  year=2018:2020, quant="ssb_end", value=c(1.5e6, 2e6, 3e6)
)

fhi <- fwd(fut, sr=srr, control=control)

compare(fhi, control)

ssb_end(fhi)[, ac(2017:2020)]

fbar(fhi)[, ac(2017:2020)]

plot(window(fhi, start=2000)) +
  geom_vline(xintercept=2018, colour="gray")

# 2. Timing of spawning limits ssb targets

m.spwn(ple4)

control <- fwdControl(year=2018:2019, quant="ssb_spawn", value=7.5e5)

fsb <- fwd(fut, sr=srr, control=control)

ssb(fsb)[, ac(2017:2020)]


# --- EXTRA: Running multiple scenarios

# SETUP a list of fwdcontrol objects

controls <- list(
  Fsq = fwdControl(
    list(year=2018:2020, quant="fbar", value=c(fbar(ple4)[, "2017"]))),
  Fsq050 = fwdControl(
    list(year=2018:2020, quant="fbar", value=c(fbar(ple4)[, "2017"]) * 0.5)),
  Fsq075 = fwdControl(
    list(year=2018:2020, quant="fbar", value=c(fbar(ple4)[, "2017"]) * 0.75)),
  Fsq125 = fwdControl(
    list(year=2018:2020, quant="fbar", value=c(fbar(ple4)[, "2017"]) * 1.25)),
  Fsq150 = fwdControl(
    list(year=2018:2020, quant="fbar", value=c(fbar(ple4)[, "2017"]) * 1.50))
)

# lapply fwd

frs <- lapply(controls, function(x) fwd(object=fut, sr=srr, control=x))

# convert to FLStocks to plot

plot(FLStocks(frs))
