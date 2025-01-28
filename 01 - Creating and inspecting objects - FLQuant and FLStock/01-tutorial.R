# 01-tutorial.R - DESC
# 01 - Creating and inspecting objects: FLQuant and FLStock/01-tutorial.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# Load required packages

library(FLCore)
library(ggplotFL)


# --- An FLStock: ple4

# Load example data: ple4

data(ple4)

# summary(ple4)

summary(ple4)

# Same result if we type the object name, show(ple4)

ple4

# Call default plot() method

plot(ple4)

# Access FLStock slots

catch(ple4)

catch.n(ple4)

# Call calculation methods

ssb(ple4)

fbar(ple4)


# --- Creating FLQuant objects: FLQuant(), as.FLQuant().

# The class constructor

FLQuant()

# It can take a single value 

FLQuant(0.2)

# A vector (along `year` dimension)

FLQuant(c(9, 10, 2, 3, 2, 9), dimnames=list(age=0:5, year=2000))

# A matrix

ma <- matrix(rlnorm(50), ncol=10, nrow=5)

FLQuant(ma, quant="age")

# An array

ar <- array(rlnorm(100), dim=c(10, 5, 2))

FLQuant(ar)


# --- The inside of FLQuant: quant, dim, dimnames, units.

# An FLQuant contains both data and metadata

flq <- catch.n(ple4)

# Get the object dimensions

dim(flq)

# The object dimnames

dimnames(flq)

# And the units of measurement

units(flq)


# --- Creating your own FLStock: from file to R to FLR (and back).

# 1. From one or more CSV files

# Load a catch-at-age (age x year) matrix from a CSV file

dat <- read.csv("data/catch_numbers.csv", row.names=1)

# Loads as data.frame, need to convert to matrix

is(dat)

can <- as.matrix(dat)

catch.n <- FLQuant(can, dimnames=list(age=1:7, year = 1957:2011), units="1000")
landings.n <- FLQuant(can, dimnames=list(age=1:7, year = 1957:2011), units="1000")

# Starting an Flstock with a single slot

her <- FLStock(catch.n=catch.n, landings.n=landings.n, name="HER")


# 2. Loading from a set of VPA Suite-format files

her <- readFLStock("data/her-irlw/index.txt")

summary(her)

# NOTE Object still requires setting discards, calculating total catches, etc.

# --- uom, standardUnits

# `units` are free character values, but they work with each other

units(catch.n(ple4))
units(catch.wt(ple4))

catch.n(ple4) * catch.wt(ple4)

m(ple4) + harvest(ple4)

# Assign standard units to all FLStock slot

standardUnits(her)

units(her) <- standardUnits(her)


# --- Object validity (validObject) and verification (verify).

# S4 classes have a validity function

validObject(flq)

validObject(ple4)

# FLStock has a verify method that tells us about possible issues

verify(ple4)

landings.wt(ple4)

ruleset(ple4)[[1]]

# verify can be extended with your own rule(s), as a formula

verify(ple4, no0catch=~catch == 0)

# --- Manipulating objects (more to come)

# [ subsetting, by position or name

flq[1:2, as.character(1960:1970)]

# Also for FLStock

ple4[9:10, ac(2000:2005)]

# window for years, always as names

window(ple4, end=1988)


# --- Some other ways of loading data.

# "If you can bring it into R, you can load it into FLR"


# --- as.data.frame, metrics, model.frame

# Multidimensional FLR object can be converted to 2D

# 6D FLQuant gives a 7 column data.frame

as.data.frame(catch.n(ple4))

dat <- as.data.frame(catch.n(ple4), drop=TRUE)

as.FLQuant(dat)

# drop=TRUE to output only columns with information

head(as.data.frame(catch.n(ple4), drop=TRUE))

# with a date column (of "POSIXct", see ?ISOdate)

as.data.frame(catch.n(ple4), drop=TRUE, date=TRUE)

# a column for the cohort

as.data.frame(catch.n(ple4), drop=TRUE, cohort=TRUE)

# and the units of measurement

as.data.frame(catch.n(ple4), drop=TRUE, units=TRUE)


# metrics() can be used to extract elements and results from FLStock

met <- metrics(ple4, list(SSB=ssb, F=fbar))

met

plot(met)

met <- metrics(ple4, list(N=stock.n, F=harvest))

# Conversion to a table can be done in both long

as.data.frame(met)

# or wide formats

model.frame(met)


# A full FLStock can be stored as CSV file

dat <- as.data.frame(ple4)

write.csv(dat, file="ple4.csv", row.names=FALSE)

# and the retrieved and reconstructed

dat <- read.csv("ple4.csv")

stk <- as.FLStock(dat)

# NOTE the stock is missing units, and minfbar/maxfbar in range

range(stk, c("minfbar", "maxfbar")) <- c(2, 6)

units(stk) <- standardUnits(stk)
