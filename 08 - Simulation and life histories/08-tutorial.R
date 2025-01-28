# 08-tutorial.R - Simulation and life histories
# 08 - Simulation and life histories/08-tutorial.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


install.packages("FLife", repos=structure(
  c(CRAN="https://cloud.r-project.org/",
  FLR="https://flr.r-universe.dev")))

library(FLife)
library(FLasher)
library(ggplotFL)

# LOAD teleost dataset

data(teleost)

# An FLPar object with 145 spp.

teleost

dimnames(teleost)

# GET cod

codlh <- teleost[, "Gadus morhua"]

# DEBUG: t0 should be negative

codlh['t0'] <- codlh['t0'] * -1

# linf, k, t0: VB growth params
# l50 length at 50% mature
# a, b: scaling factor and exponent of length weight relationship

# SET age range

ages <- FLQuant(1:25, dimnames=list(age=1:25))

# EXAMPLE lH function: Von Bertalanffy growth curve

vonB(ages, codlh)

ggplot(vonB(ages, codlh), aes(x=age, y=data)) +
  geom_line() + geom_point() + xlab("Age") + ylab("Length (cm)")

# CREATE complete set of params

codpars <- lhPar(codlh)

# CHECK weight-at-age

ggplot(len2wt(vonB(ages, codlh), codpars)/1000, aes(x=age, y=data)) +
  geom_line() + geom_point() + xlab("Age") + ylab("Weight (kg)")

# Selectivity: double normal, sel1=peak age, sel2=left var, srel3=right var

codpars <- lhPar(codlh, sel1=3, sel2=1, sel3=45)

ggplot(dnormal(ages, lhPar(codlh, sel1=3, sel2=1, sel3=25)),
  aes(x=age, y=data)) +
  geom_line() + geom_point() +
  xlab("Age") + ylab("Selectivity") + ylim(c(0,1))

# TEST alternative selex: later peak and drop with age.

ggplot(dnormal(ages, lhPar(codlh, sel1=8, sel2=3, sel3=5)),
  aes(x=age, y=data)) +
  geom_line() + geom_point() +
  xlab("Age") + ylab("Selectivity") + ylim(c(0,1))

# Steepness: s=0.75

codpars <- lhPar(codlh, sel1=3, sel2=1, sel3=25, s=0.75)

# Virgin biomass: v=250,000

codpars <- lhPar(codlh, sel1=3, sel2=1, sel3=25, s=0.75, v=2e5)

# GENERATE equilibrium population according to pars

eql <- lhEql(codpars, sr="ricker", m="gislason",
  range=c(min=1, max=25, minfbar=3, maxfbar=6, plusgroup=25))

# CHECK M-at-age

m(eql)[, 1]

# RETURNS an FLBRP for equilibrium conditions

eql

# DEBUG: Fmax is wrong, drop
refpts(eql) <- refpts(eql)[1:4,]

plot(eql)

# PLOT m, selex, mat and wt at age
ggplot(metrics(eql,
  list(M=m, Selex=catch.sel, Maturity=mat, Weigth=catch.wt))) +
  geom_line(aes(age, data)) + facet_wrap(~qname, scale="free")+
  scale_x_continuous(limits=c(1, 25))

# CREATE population at all Fbar levels

cod <- as(eql, "FLStock")

plot(cod)

# POPULATION at F=0

ggplot(stock.n(cod)[, 1], aes(x=age, y=data)) + geom_line() +
  xlab("Age") + ylab("Abundance (1000s)")

# CHOOSE initial F level (not 0, better Fbar=0.03)
cod <- cod[, 2]

# SET weights as kg
cod <- transform(cod, catch.wt=catch.wt/1000, catch.wt=catch.wt/1000,
  landings.wt=landings.wt/1000, discards.wt=discards.wt/1000)

units(cod) <- standardUnits(cod)

# EXTEND stock for 100 y

om <- fwdWindow(cod, eql, end=101)

# GET SRR

srr <- predictModel(model=model(eql), params=params(eql))

# CREATE trajectories and (fast) forward

Pfmsy <- ffwd(om, sr=srr,
  control=fwdControl(year=3:101, value=c(fmsy(eql)), quant="f"))

plot(Pfmsy)

# FORECAST with SRR uncertainty

om <- propagate(om, 300)

lndev06 <- rlnorm(300, FLQuant(0, dimnames=list(year=3:101)), 0.6)

Pfmsy <- ffwd(om, sr=srr, deviances=lndev06,
  control=fwdControl(year=3:101, value=c(fmsy(eql)), quant="f"))

plot(Pfmsy)

# ADD structural uncertainty on B0

simcod <- function(v) {

  pars <- lhPar(codlh, sel1=3, sel2=1, sel3=25, s=0.75, v=v)
  
  eql <- lhEql(pars, range=c(min=1, max=25, minfbar=3, maxfbar=6, plusgroup=25))

  # SELECT 'almost' unfished
  stk <- as(eql, "FLStock")[,2]

  # SET wts in kg
  stk <- transform(stk, catch.wt=catch.wt/1000, catch.wt=catch.wt/1000,
  landings.wt=landings.wt/1000, discards.wt=discards.wt/1000)

  units(stk) <- standardUnits(stk)

  stk <- fwdWindow(stk, eql, end=101)
  srr <- list(model=model(eql), params=params(eql))

  return(list(stock=stk, sr=srr))
}

# APPLY for v = U(1e5, 2e5)

sim <- lapply(runif(50, 1e5, 3e5), simcod)

# COMBINE into single FLStock

om <- Reduce(combine, lapply(sim, "[[", "stock"))

# AR1 SRR deviances
lndev06 <- rlnormar1(50, 0.6, rho=0.4, years=3:101)

plot(lndev06)

# FWD
omA <- fwd(om, sr=srr, deviances=lndev06,
  control=fwdControl(year=3:101, value=c(fmsy(eql)), quant="f"))

plot(omA)

# Dynamic trajectories

library(FLRef)

ftraj <- fudc(om, fhi=2, flo=0.9, fref=fmsy(eql), sigmaF=0.25)

plot(ftraj)

omB <- fwd(om, sr=srr, deviances=lndev06, fbar=ftraj)

plot(omA, omB)


# NOW, What other uncertainties would you introduce?


