# 05-tutorial.R - DESC
# 05 - Reference points - FLBRP, eqsim.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# install.packages("FLBRP", repos=structure(c(CRAN="https://cloud.r-project.org/",
#   FLR="https://flr-project.org/R")))

# Load required packages

library(FLBRP)
library(FLSRTMB)

# LOAD AASP run sol.27.4 2020

load("sol274.RData")

plot(sol)

# --- Fit a SRR

# EXTRACT ssb and rec from FLStock

srr <- as.FLSR(sol)

summary(srr)

# SET model

model(srr) <- ricker()

# A formula

model(srr)

# A loglikehood function

logl(srr)

# FIT using MLE

srr <- fmle(srr, fixed=list(s=0.85, spr0=mean(spr0y(sol))))
srr <- fmle(srr)
srr <- srrTMB(srr, spr0=spr0y(sol))

# INSPECT fit

plot(srr)

# evaluate likelihood surface
profile(srr)

# CREATE FLBR object from FLStock and FLSR

solrp <- FLBRP(sol, sr=srr)

summary(solrp)

# CALCULATE refpts

solrp <- brp(solrp)

# PLOT output

plot(solrp)

plot(solrp, obs=TRUE)

# OBTAIN refpts

refpts(solrp)

# EXTRACT refpts to use

msyrps <- FLPars(
  F=FLPar(FMSY=fmsy(solrp)),
  SSB=FLPar(BMSY=bmsy(solrp)))

# PLOT with refpts

plot(sol, metrics=list(SSB=ssb, F=fbar)) +
  geom_flpar(data=msyrps, x=1957)

# EXTRA -- PRETTY good yield

rgesol <- msyRange(solrp,range=0.05)


# --- EXTRACTING other results: YPR

fypr <- FLQuants(YPR=ypr(solrp), F=fbar(solrp))

ggplot(model.frame(fypr), aes(x=F, y=YPR)) +
  geom_line()


# --- CREATE an equilibrium population

stock <- as(solrp, "FLStock")

# years are really F levels (0 - Fcrash)

plot(stock)

fbar(stock)

# PLOT abundances at age for increasing F levels

ggplot(stock.n(as(solrp, "FLStock"))[, c(10, 20, 30, 40, 50, 60)]) +
  geom_col(aes(x=age, y=data)) +
  facet_wrap(~year, labeller = as_labeller(setNames(c(paste0("F = ",
      round(fbar(solrp)[, c(10, 20, 30, 40, 50, 60)], 2))),
      nm=c(10, 20, 30, 40, 50, 60)))) +
  ylab("Abundances (1000s)") + xlab("Age") +
  scale_x_continuous(breaks=seq(2, 10, by=2))

ggplot(harvest(as(solrp, "FLStock"))[, c(10, 20, 30, 40, 50, 60)]) +
  geom_col(aes(x=age, y=data)) +
  facet_wrap(~year, labeller = as_labeller(setNames(c(paste0("F = ",
  round(fbar(solrp)[, c(10, 20, 30, 40, 50, 60)], 2))),
  nm=c(10, 20, 30, 40, 50, 60)))) +
  ylab("Abundances (1000s)") + xlab("Age") +
  scale_x_continuous(breaks=seq(2, 10, by=2))

# GET a population at a given F level, e.g. Fbar ~ 0.20

pop <- stock[, 26]

# pop can be extended and projected

library(FLasher)

# TRICK, change names of pop, so 1st year is 1
dimnames(pop) <- list(year=1)

# EXTEND for 99 years
pop <- fwdWindow(pop, solrp, end=100)

# GENERATE F random walk
set.seed(844)
fs <- 0.20 + c(0, cumsum(rnorm(98, 0, 0.03)))

# fwd
fut <- fwd(pop, sr=srr, control=fwdControl(year=2:100, quant="fbar", value=fs))

plot(fut)
