# 00-tutorial.R - DESC
# 00 - Introduction and setup. Dissecting an example
# Copyright Iago MOSQUEIRA (WMR), 2024
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# --- SETUP


# INSTALL packages

install.packages(c("mse", "mseviz"), repos=c(FLR="https://flr.r-universe.dev"),
  CRAN="https://cloud.r-project.org/")

# CLONE repository


# --- EXPLORE MSE

# Load required packages

library(mse)

# LOAD example FLom object

data(sol274)

# INSPECT om

summary(om)

plot(om)

# DECONSTRUCT om

slotNames(om)

# @stock
stock(om)

# @sr
sr(om)

# @refpts
refpts(om)

#@projection
projection(om)
