# 06-tutorial.R - DESC
# 06 - Stock assessment methods in FLR

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# INSTALL

install.packages(c("FLXSA", "FLa4a", "FLSAM"),
  repos=structure(c(CRAN="https://cloud.r-project.org/",
    FLR="https://flr-project.org/R")))

library(ggplotFL)

# LOAD data

data(ple4)

data(ple4.indices)

# SELECT only two indices, makes it faster and simpler

indices <- ple4.indices[c("BTS-Combined (ISIS and TRIDENS)", "SNS")]


# --- FLXSA

library(FLXSA)

# BASIC fit

fitxsa <- FLXSA(ple4, indices=indices)

# INSPECT output

fitxsa

# UPDATE stock (stock.n, harvest)

stxsa <- ple4 + fitxsa

# PLOT and compare with AAP SA (ple4)

plot(stxsa)

plot(FLStocks(AAP=ple4, XSA=stxsa))

# SET XSA options

ctlxsa <- FLXSA.control(shk.f=FALSE)

fitxsa <- FLXSA(ple4, indices=indices, control=ctlxsa)

plot(FLStocks(XSA=stxsa, XSA2=ple4 + fitxsa))

# EXTRACT diagnostics

diagnostics(fitxsa)


# --- FLa4a

# https://flr-project.org/doc/Statistical_catch_at_age_models_in_FLa4a.html

library(FLa4a)


# DEFAULT fit

fit <- sca(ple4, indices)

fit

# BASIC diagnostics

resid <- residuals(fit, ple4, indices)

# RESIDUALS time series per source

plot(resid)

# BUBBLEPLOT

bubbles(resid)

# QQPLOT of normality in residuals

qqmath(resid)

# MODEL fits
# catch
plot(fit, ple4)

# indices
plot(fit, indices)

# Akaike AIC
AIC(fit)

# UPDATE

run1 <- ple4 + fit


# SUBMODELS

fits <- list(basic=fit)


# 1. SET submodels for a separable model

# fmod by age and year
fmod <- ~ factor(age) + factor(year)

# catchability submodels, one per index
qmod <- list(~ factor(age), ~factor(age))

srmod <- ~ factor(year)

fit <- sca(stock = ple4, indices = indices, fmodel=fmod, qmodel=qmod, srmodel=srmod)

fits$separable <- fit


# 2. USE splines in fmod

fmod <- ~ s(age, k=4) + s(year, k = 20)

fit <- sca(stock = ple4, indices = indices, fmodel=fmod, qmodel=qmod, srmodel=srmod)

fits$splinef <- fit


# 3. MODEL age-year interactions using tensor splines

fmod <- ~ te(age, year, k = c(4,20))

fit <- sca(stock = ple4, indices = indices, fmodel=fmod, qmodel=qmod, srmodel=srmod)

fits$te <- fit


# 4. MODEL trends in catchability with time

qmod <- list( ~ s(age, k=4) + year,  ~ s(age, k=4) + year)

fit <- sca(stock = ple4, indices = indices, fmodel=fmod, qmodel=qmod, srmodel=srmod)

fits$qtime <- fit


# 5. Beverton & Holt SRR

srmod <- ~ bevholt(CV=0.1)

fit <- sca(stock = ple4, indices = indices, fmodel=fmod, qmodel=qmod, srmodel=srmod)

fits$bevholt <- fit


# COMPARE runs

runs <- FLStocks(lapply(fits, "+", e1=ple4))

plot(runs)

# AICs
lapply(fits, AIC)

# BICs
lapply(fits, BIC)

plot(FLStocks(XSA=stxsa, a4a=runs$splinef, AAP=ple4))

# RETRO & PREDICTION skill

library(a4adiags)

# RUN retrospective hindcast
xval <- a4ahcxval(ple4, indices)

plotXval(xval$indices)

# PLOT runstest
plotRunstest(index(fit), lapply(indices, index))


# --- FLSAM

library(FLSAM)

type(indices[[1]]) <- "number"
type(indices[[2]]) <- "number"


# CREATE control
control <- FLSAM.control(ple4, indices)

# ADD survey correlation

# BTS
control@cor.obs["BTS-Combined (ISIS and TRIDENS)", 1:8] <- c(rep(101, 2), rep(102, 6))
# SNS
control@cor.obs["SNS", 1:6] <- c(rep(101, 2), rep(102, 4)) +101
control@cor.obs.Flag[2:3] <- as.factor("AR")

# F random walks
control@f.vars["catch unique",] <- c(1, 2, rep(3, 6), rep(4, 2))

# F correlation structure
control@cor.F <- 2

# Observation variances
control@obs.vars["catch unique",] <- c(1, 2, 3, 3, rep(4, 6))
control@obs.vars["BTS-Combined (ISIS and TRIDENS)", ac(1:9)] <- 
  c(101, rep(102, 4), rep(103, 2), rep(104, 2))
control@obs.vars["SNS", ac(1:7)] <- c(101, rep(102, 3), rep(103, 3)) + 100

# Catchabilities
control@catchabilities["BTS-Combined (ISIS and TRIDENS)", ac(1:9)] <- c(1,1,2,3,4,5,5,6,6)
control@catchabilities["SNS", ac(1:7)] <- c(1,1,1,2,2,2,2) +100

control <- update(control)

control@residuals <- FALSE

# RUN SAM
sam <- FLSAM(ple4, indices, control)

# PLOT results
plot(sam)

runsam <- sam + ple4

plot(FLStocks(XSA=stxsa, AAP=ple4, SAM=runsam))


# --- AAP

library(AAP)

# LOAD SOL.27.4 data: sol4 (FLStock), indices (FLIndices)
data(sol4)

# SETUP control object
control <- AAP.control(pGrp=TRUE, qplat.surveys=8, qplat.Fmatrix=9,
  Sage.knots=6, Fage.knots=8, Ftime.knots=28, mcmc=FALSE)

# RUN aap
fit <- aap(sol4, indices[c("BTS", "SNS")], control=control)

# UPDATE FLStock with results (harvest and stock.n)
run <- fit + sol4

# PLOT
plot(run)

# PLOT uncertainty
plot(fit)

# RUN retro

years <- setNames(nm=seq(2019, by=-1, length=6))

retro <- FLStocks(lapply(setNames(nm=years), function(i)
  aap(window(sol4, end=i), lapply(indices[c("BTS", "SNS")], window, end=i),
    control=control) + window(sol4, end=i)))

# PLOT retro
plot(retro, metrics=list(SSB=ssb, F=fbar))

# CREATE mohnMatrix
mm <- mohnMatrix(retro, "fbar")

# COMPUTE Mohn's rho

library(icesAdvice)

mohn(mm)

mohn(mohnMatrix(retro, fbar))

mohn(mohnMatrix(retro, ssb))

# COMPUTE prediction skill

library(doParallel)
# ONLY in Linux
registerDoParallel(3)

# RUN hindcasting for indices
sxval <- aaphcxval(sol4, indices[c("BTS", "SNS")], control = control)

# PLOT xval
plotXval(sxval$indices)
